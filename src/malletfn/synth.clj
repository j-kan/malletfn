(ns malletfn.synth
 ; (:use clojure.contrib.seq)
  (:use     malletfn.corpusutil)
  (:import (cc.mallet.types Alphabet))
  (:import (cc.mallet.types LabelAlphabet))
  (:import (cc.mallet.util Randoms)))

;(set! *warn-on-reflection* true)

(def *randoms* (new Randoms 90210))

(defstruct discrete  :type :pp)
(defstruct dirichlet :type :pp :magnitude)

(defn make-mallet-dirichlet 
  "a dirichlet distribution based on the mallet Dirichlet class"
  [p]
  (new cc.mallet.types.Dirichlet p))

(defn make-dirichlet [as]
    (let [alphas    (double-array as)
          magnitude (reduce + alphas)] 
      (struct-map dirichlet :type :dirichlet :pp (map #(/ % magnitude) alphas) :magnitude magnitude)))

(defn make-symmetric-dirichlet [alpha dim]
  (make-dirichlet (repeat dim alpha)))

(defn make-discrete 
  ([pp categories] 
    (let [denom (reduce + pp)] 
      (struct-map discrete :type :discrete :pp (map #(/ % denom) pp) :categories categories)))
  ([pp]
    (make-discrete pp (range (count pp)))))


(defn make-feature-distribution [feature-map]
  (make-discrete (repeat (count feature-map) 1.0)
                 (map (fn [[features alphas]] {:alphas alphas :features features} ) 
                      (seq feature-map))))


(defmulti sample-from 
  (fn [head & tail] (or (:type head) (type head))))

(defmethod sample-from :dirichlet [dir]
  (let [{ magnitude :magnitude
          alphas    :pp } dir
        gammas (map #(max 0.0001 (.nextGamma *randoms* (* % magnitude) 1)) 
                    alphas)
        sum    (reduce + gammas)]
    (make-discrete (map #(/ % sum) gammas))))

(defmethod sample-from cc.mallet.types.Dirichlet [dir]
  (.nextDistribution dir))



(defn- sample-from-discrete-aux 
  ([uniformsample categories distribution]
    (loop [rnd uniformsample
           [c & cc] categories
           [p & pp] distribution]
      (cond 
       (nil? pp) c
       (< rnd p) c
       :else (recur (- rnd p) cc pp))))
  ([distrib]
    (let [{ categories :categories
            pp         :pp } distrib]
      (sample-from-discrete-aux (.nextUniform *randoms*) categories pp))))

(defmethod sample-from :discrete [distrib]
   (sample-from-discrete-aux distrib))

(defmethod sample-from (Class/forName "[D") 
  ([p]
   (sample-from-discrete-aux (.nextUniform *randoms*) (range (alength p)) p))
  ([p categories]
   (sample-from-discrete-aux (.nextUniform *randoms*) categories p)))

(defmethod sample-from clojure.lang.ISeq 
  ([p]
   (sample-from-discrete-aux (.nextUniform *randoms*) (range (count p)) p))
  ([p categories]
   (sample-from-discrete-aux (.nextUniform *randoms*) categories p)))


(defn sample-thetas-with-alphas
  "(lazily) sample an infinite sequence of multinomials from a dirichlet with the given alphas"
  [alphas]
  (let [dirichlet-alpha (make-dirichlet alphas)]
    (repeatedly #(sample-from dirichlet-alpha))))

(defn sample-thetas-with-symmetric-alpha 
  "(lazily) sample an infinite sequence of multinomials from a dirichlet with symmetric alpha"
  [num-topics symmetric-alpha]
  (sample-thetas-with-alphas (repeat num-topics symmetric-alpha)))


(defn sample-theta-with-features [feature-distribution]
  (let [{ features :features
          alphas   :alphas   } (sample-from feature-distribution)
        dirichlet-alpha (make-dirichlet alphas)]
    (assoc (sample-from dirichlet-alpha) :features features)))

(defn sample-thetas-with-features [feature-distribution]
  "(lazily) sample an infinite sequence of multinomials from a dirichlet subject to the given feature distribution"
  (repeatedly #(sample-theta-with-features feature-distribution)))


(defn sample-phi [topic-vocabulary alphabet symmetric-beta]
  (let [beta             (repeat (count topic-vocabulary) (double symmetric-beta))
        dirichlet-beta   (make-dirichlet (double-array beta))
        alphabet-indices (.lookupIndices alphabet (to-array topic-vocabulary) false)
        dict             (zipmap alphabet-indices (:pp (sample-from dirichlet-beta)))]
    (make-discrete (map #(or (dict %) 0)
                        (range (.size alphabet))))))

(defn sample-phis [topic-vocabularies alphabet symmetric-beta]
  (map #(sample-phi % alphabet symmetric-beta) topic-vocabularies))




(defn sample-doc-lengths
  "(lazily) sample an infinite sequence of doc lengths from a Poisson distribution with parameter eta" 
  [eta]
  (repeatedly #(.nextPoisson *randoms* eta)))


(defn sample-doc-zs 
  "sample topic indices for all the words in a document of a given length"
  [theta doc-length]
  (take doc-length (repeatedly #(sample-from theta))))

(defn sample-doc-ws 
  "given a distribution (phis) and a sequence of topic indices (zs) for one document, 
   sample a sequence of document token indices"
  [phis zs]
  (map (fn [z] (sample-from (nth phis z)))
       zs))

(defn sample-document-indices
  "sample a document, returning both the token indices as well as the topic indices"
  [doc-length theta phis]
  (let [zs (sample-doc-zs theta doc-length)
        ws (sample-doc-ws phis zs)]
    [ws zs]))
  
(defn sample-docs-zs 
  "sample topic indices for all the words in a corpus of documents.
   The document length is sampled from a Poisson distribution with parameter eta.
   The thetas can be defined lazily."
  [thetas eta]
  (map (fn [theta doc-length] (sample-doc-zs theta doc-length)) 
       thetas
       (sample-doc-lengths eta)))


(defn sample-docs-ws
  "given a distribution (phis) and a sequence of sequences of topic indices (zzs), 
   sample a sequence of sequences of document token indices"
  [phis zzs]
  (map (fn [zs] (sample-doc-ws phis zs))
       zzs))

(defn sample-corpus-indices
  "sample a document corpus, returning both the token indices as well as the topic indices"
  [eta thetas phis]
  (let [zzs (sample-docs-zs thetas eta)
        wws (sample-docs-ws phis zzs)]
    [wws zzs]))

(defn ws2words 
  "transform a sequence of token indices to ta sequence of tokens"
  [ws alphabet]
  (map #(.lookupObject alphabet %) ws))

(defn sample-corpus-as-words [topics num-docs eta symmetric-alpha symmetric-beta]
    (let [num-topics (count topics)
          alphabet   (make-alphabet (apply concat topics))
          num-types  (.size alphabet)              ;(def num-types  (count (set (apply concat topics))))
          thetas     (sample-thetas-with-symmetric-alpha num-topics num-docs symmetric-alpha)
          phis       (sample-phis topics alphabet symmetric-beta)
          [wws zzs]  (sample-corpus-indices eta thetas phis)]
      (map #(ws2words % alphabet)  
           (sample-corpus-indices eta thetas phis))))

(defn sample-corpus-as-words-with-features [topics num-docs eta feature-distribution symmetric-beta]
    (let [num-topics (count topics)
          alphabet   (make-alphabet (apply concat topics))
          num-types  (.size alphabet)              ;(def num-types  (count (set (apply concat topics))))
          thetas     (take num-docs (sample-thetas-with-features feature-distribution))
          features   (map :features thetas)
          phis       (sample-phis topics alphabet symmetric-beta)
          [wws zzs]  (sample-corpus-indices eta thetas phis)]
      [(map #(ws2words % alphabet) wws)
       features
       zzs] ))

(defn derive-doc-features [doc-as-words]
  (map (fn [[i cnt]] (str (char (+ i (int \a))))) 
    (filter (fn [[i cnt]] (> cnt 3)) 
      (seq
        (zipmap 
          (range 26) 
          (vec-frequencies (vec (map #(round-int 5 (- (int (first (seq %))) (int \a))) 
                                     doc-as-words)) 26))))))

(defn make-corpus-instance-list-with-features
 
  ([words features] 
		(make-instance-list 
		  (make-instance-pipe
		    (new cc.mallet.pipe.TargetStringToFeatures)
				(new cc.mallet.pipe.Input2CharSequence)
				(new cc.mallet.pipe.CharSequence2TokenSequence)
				(new cc.mallet.pipe.TokenSequence2FeatureSequence))
		  (mallet-iterator-with-features words features)))

  ([[words features]] 
      (make-corpus-instance-list-with-features words features))

  ([topics num-docs eta feature-distribution symmetric-beta]
    (let [[words features _] (sample-corpus-as-words-with-features 
                               topics num-docs eta feature-distribution symmetric-beta)]
      (make-corpus-instance-list-with-features words features))))


(defn make-corpus-instance-list
 
  ([words] 
    (make-corpus-instance-list-with-features 
      words
      (map derive-doc-features words)))

  ([topics num-docs eta symmetric-alpha symmetric-beta]
      (make-corpus-instance-list 
        (sample-corpus-as-words topics num-docs eta symmetric-alpha symmetric-beta))))


(def topics 
  (map #(sort (seq (.split % "\\s+")))  
    [ "eggplant artichoke tomato onion cucumber carrot asparagus zucchini broccoli lettuce cabbage cauliflower spinach avocado potato leek kale basil arugula radicchio"
      "soap toothbrush towel bath slippers shampoo razor robe floormat icebucket pillow lotion conditioner tissue toilet blowdryer mirror"
      "piano violin keyboard flute trumpet organ cello saxophone banjo clarinet recorder oboe cornet sopranino tympani bassoon euphonium piccolo daxophone stritch manzello"
      "red purple green black blue white brown pink yellow gray orange silver gold navy grey beige bronze tan tomato carrot turquoise bone mauve"]))

(def topics 
  (map #(sort (seq (.split % "\\s+")))  
    [ "eggplant artichoke tomato onion cucumber" ; carrot asparagus zucchini broccoli lettuce cabbage cauliflower spinach avocado potato leek kale basil arugula radicchio"
      "soap toothbrush towel bath slippers"      ; shampoo razor robe floormat icebucket pillow lotion conditioner tissue toilet blowdryer mirror"
      "piano violin keyboard flute trumpet"      ; organ cello saxophone banjo clarinet recorder oboe cornet sopranino tympani bassoon euphonium piccolo daxophone stritch manzello"
      "red purple green black blue"              ; white brown pink yellow gray orange silver gold navy grey beige bronze tan tomato carrot turquoise bone mauve"
      ]))
  


(def feature-map
  {["pollan"]     [5.0  0.1  0.1  2.0]
   ["scriabin"]   [0.1  0.1  5.0  1.0]
   ["koolhaas"]   [0.1  1.0  0.1  3.0]
   ["kandinsky"]  [0.1  0.1  3.0  5.0]
   ["richter"]    [1.0  1.0  1.0  5.0]
   ["hilton"]     [0.1  5.0  0.1  4.0]
   ["hentoff"]    [0.1  0.1  5.0  1.0]
   ["stewart"]    [3.0  3.0  0.1  3.0]})

(def num-docs 10)
(def eta      8)
(def symmetric-alpha 1.0)
(def symmetric-beta  0.25)


(def feature-distribution (make-feature-distribution feature-map))

(defn corpus-as-words      [] (sample-corpus-as-words topics num-docs eta symmetric-alpha symmetric-beta))
(defn corpus-instance-list [] (make-corpus-instance-list (corpus-as-words)))

(defn corpus-as-words-with-features      [] 
  (sample-corpus-as-words-with-features topics num-docs eta feature-distribution symmetric-beta))

(defn corpus-instance-list-with-features []
  (let [[words features zzs] (corpus-as-words-with-features)] 
    [(make-corpus-instance-list-with-features words features) zzs]))

(defn make-dmr-synth-corpus []
  (let [[corpus tas]      (corpus-instance-list-with-features)
        num-topics        (count topics)
        topic-assignments (map (fn [instance ta] 
                                 {:type             :topic-assignment
                                  :instance         instance
                                  :topic-assignment ta})
                            	corpus tas)]
    {:type :dmr-synth-corpus
     :corpus corpus
     :num-topics num-topics
     :topic-assignments topic-assignments}))

;;;;;;;;;;;;

(defn- topic-index-to-alphabet-index [i topic-vocab alphabet]
  (.lookupIndex alphabet (nth topic-vocab i) false))

(defn- topic-indices-to-alphabet-indices [indices topic-vocab alphabet]
  (.lookupIndices alphabet 
    (to-array (map #(nth topic-vocab %) indices))
    false))

(defn- topic-vocabulary-to-alphabet-indices [topic-vocab alphabet]
  (.lookupIndices alphabet 
    (to-array topic-vocab)
    false))




(comment
  (in-ns 'malletfn.synth)

  (def alpha (take num-topics (repeat (double 0.01))))
  (def beta  (take num-types  (repeat (double 0.01))))  ; (comment (/ eta num-types))

  (def dirichlet-alpha (make-dirichlet (double-array alpha)))
  (def dirichlet-beta  (make-dirichlet (double-array beta)))

  (make-dirichlet (double-array (:pp (make-discrete [11 111 11 11]))))

  
  (take 10 (sample-thetas-with-symmetric-alpha 4 10.0))
  (take 3  (sample-thetas-with-features feature-distribution))
  
  (def author (first (sample-thetas-with-features feature-distribution)))
  (take 10 (repeatedly #(sample-from author)))

  (map :features (take 10  (sample-thetas-with-features feature-distribution)))

  (take 3 (sample-docs-zs (sample-thetas-with-symmetric-alpha 4 200.0) 7))

  (take 10 (sample-docs-zs (sample-thetas-with-features feature-distribution) 7))

  (def num-topics (count topics))
  (def num-types  (count (set (apply concat topics))))
  (def alphabet (make-alphabet (apply concat topics)))
  (def thetas   (sample-thetas-with-features feature-distribution))
  (def features (map :features thetas))
  
  (first thetas)
  (first features)
  
  (def phis     (sample-phis topics alphabet symmetric-beta))
 
  ;(def corpus (corpus-as-words-with-features))
  ;(def dmr-instance-list (corpus-instance-list-with-features))
  
  (def corpus-words (corpus-as-words-with-features))
  (def corpus-with-topics (corpus-instance-list-with-features))
  (def corpus (first corpus-with-topics))
  (def corpus-topic-assignments (second corpus-with-topics))
  
  (use 'clojure.contrib.seq-utils)

    (doc refer)
  (indexed corpus-topic-assignments)
  (def inst1 (first corpus))
  
  (map (fn [a b] (+ a b)) [1 2 3] [4 5 6])
  
  (def dmr-synth-corpus (make-dmr-synth-corpus))
  
  (.getTarget (:instance (first (:topic-assignments dmr-synth-corpus))))
  (:topic-assignment (first (:topic-assignments dmr-synth-corpus)))
  
  (defn token-alphabet [corpus]
    (-> inst1 .getData .getAlphabet))
  
  (defn feature-alphabet [corpus]
    (-> inst1 .getTarget .getAlphabet))
  
  (defn topic-alphabet [num-topics]
    (let [alphabet (new cc.mallet.types.LabelAlphabet)]
      (doseq [i (range num-topics)] 
        (.lookupIndex alphabet (str "topic" i)))
      alphabet))

  (-> inst1 .getTarget)
  (-> inst1 .getSource)
  (-> inst1 .getName)
  
  (token-alphabet corpus)
  (feature-alphabet corpus)
  (.size (topic-alphabet 4))
  
 )