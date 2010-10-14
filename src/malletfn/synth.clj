(ns malletfn.synth
 ; (:use clojure.contrib.seq)
  (:use     malletfn.corpusutil)
  (:import (cc.mallet.types Alphabet))
  (:import (cc.mallet.util Randoms)))

;(set! *warn-on-reflection* true)

(def *randoms* (new Randoms 90210))

(defstruct discrete  :type :pp)
(defstruct dirichlet :type :pp :magnitude)

(defn make-mallet-dirichlet 
  "a dirichlet distribution based on the mallet Dirichlet class"
  [p]
  (new cc.mallet.types.Dirichlet p))

(defn make-dirichlet [alphas]
    (let [magnitude (reduce + alphas)] 
      (struct-map dirichlet :type :dirichlet :pp (map #(/ % magnitude) alphas) :magnitude magnitude)))

(defn make-symmetric-dirichlet [alpha dim]
  (make-dirichlet (repeat dim (double alpha))))

(defn make-discrete 
  ([pp categories] 
    (let [denom (reduce + pp)] 
      (struct-map discrete :type :discrete :pp (map #(/ % denom) pp) :categories categories)))
  ([pp]
    (make-discrete pp (range (count pp)))))

(defn make-discrete-with-features [pp features]
  (assoc (make-discrete pp) :features features))

(defn make-feature-distribution [feature-map]
  (make-discrete (repeat (count feature-map) 1.0)
                 (map (fn [[features pp]] (make-discrete-with-features pp features)) 
                      (seq feature-map))))


(defmulti sample-from 
  (fn [head & tail] (or (:type head) (type head))))

(defmethod sample-from :dirichlet [dir]
  (let [{ magnitude :magnitude
          pp        :pp } dir
        gammas (map #(max 0.0001 (.nextGamma *randoms* (* % magnitude) 1)) 
                    pp)
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




;(def alpha (take num-topics (repeat (double 0.01))))
;(def beta  (take num-types  (repeat (double 0.01))))  ; (comment (/ eta num-types))

;(def dirichlet-alpha (dirichlet (double-array alpha)))
;(def dirichlet-beta  (dirichlet (double-array beta)))

(defn sample-thetas-with-hyperprior [num-docs hyperprior]
  (let [dirichlet-alpha (make-dirichlet (double-array (:pp hyperprior)))]
    (take num-docs (repeatedly #(sample-from dirichlet-alpha)))))

(defn sample-thetas-with-symmetric-alpha [num-topics num-docs symmetric-alpha]
  (sample-thetas-with-hyperprior 
    num-docs 
    (make-discrete (repeat num-topics (double symmetric-alpha)))))


(defn sample-theta-with-features [feature-distribution]
  (let [{ features :features
          alpha    :pp       } (sample-from feature-distribution)
        dirichlet-alpha (make-dirichlet (double-array alpha))]
    (assoc (sample-from dirichlet-alpha) :features features)))

(defn sample-thetas-with-features [num-docs feature-distribution]
  (take num-docs 
    (repeatedly #(sample-theta-with-features feature-distribution))))

;(map sample-from (sample-thetas-with-features 10 blah))
;(map :features (sample-thetas-with-features 10 blah))


(defn sample-phi [topic-vocabulary alphabet symmetric-beta]
  (let [beta             (repeat (count topic-vocabulary) (double symmetric-beta))
        dirichlet-beta   (make-dirichlet (double-array beta))
        alphabet-indices (.lookupIndices alphabet (to-array topic-vocabulary) false)
        dict             (zipmap alphabet-indices (:pp (sample-from dirichlet-beta)))]
    (make-discrete (map #(or (dict %) 0)
                        (range (.size alphabet))))))

(defn sample-phis [topic-vocabularies alphabet symmetric-beta]
  (map #(sample-phi % alphabet symmetric-beta) topic-vocabularies))



(defn sample-doc-zs [theta doc-length]
  (take doc-length (repeatedly #(sample-from theta))))

(defn sample-docs-zs [thetas eta]
  (map (fn [theta doc-length] (sample-doc-zs theta doc-length)) 
       thetas
     (repeatedly #(.nextPoisson *randoms* eta))))

(defn sample-doc-ws [phis zs]
  (map (fn [z] (sample-from (nth phis z)))
       zs))

(defn sample-docs-ws [phis zzs]
  (map (fn [zs] (sample-doc-ws phis zs))
       zzs))

(defn sample-corpus-indices [eta thetas phis] 
  (sample-docs-ws phis (sample-docs-zs thetas eta)))

(defn ws2words [ws alphabet]
  (map #(.lookupObject alphabet %) ws))

(defn sample-corpus-as-words [topics num-docs eta symmetric-alpha symmetric-beta]
    (let [num-topics (count topics)
          alphabet   (make-alphabet (apply concat topics))
          num-types  (.size alphabet)              ;(def num-types  (count (set (apply concat topics))))
          thetas     (sample-thetas-with-symmetric-alpha num-topics num-docs symmetric-alpha)
          phis       (sample-phis topics alphabet symmetric-beta)]
      (map #(ws2words % alphabet)  
           (sample-corpus-indices eta thetas phis))))

(defn sample-corpus-as-words-with-features [topics num-docs eta feature-distribution symmetric-beta]
    (let [num-topics (count topics)
          alphabet   (make-alphabet (apply concat topics))
          num-types  (.size alphabet)              ;(def num-types  (count (set (apply concat topics))))
          thetas     (sample-thetas-with-features num-docs feature-distribution)
          features   (map :features thetas)
          phis       (sample-phis topics alphabet symmetric-beta)]
      [(map #(ws2words % alphabet)  
           (sample-corpus-indices eta thetas phis))
       features] ))

(defn derive-doc-features [doc-as-words]
  (map (fn [[i cnt]] (str (char (+ i (int \a))))) 
    (filter (fn [[i cnt]] (> cnt 3)) 
      (seq
        (zipmap 
          (range 26) 
          (vec-frequencies (vec (map #(round-int 5 (- (int (first (seq %))) (int \a))) 
                                     doc-as-words)) 26))))))

(defn make-corpus-instance-list
 
  ([words] 
    (make-corpus-instance-list-with-features 
      words
      (map derive-doc-features words)))

  ([topics num-docs eta symmetric-alpha symmetric-beta]
      (make-corpus-instance-list 
        (sample-corpus-as-words topics num-docs eta symmetric-alpha symmetric-beta))))

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
    (let [[words features] (sample-corpus-as-words-with-features 
                              topics num-docs eta feature-distribution symmetric-beta)]
      (make-corpus-instance-list-with-features words features))))

(def topics 
  (map #(sort (seq (.split % "\\s+")))  
    ["eggplant artichoke tomato onion cucumber carrot asparagus zucchini broccoli lettuce cabbage cauliflower spinach avocado potato leek kale basil arugula radicchio"
     "soap toothbrush towel bath slippers shampoo razor robe floormat icebucket pillow lotion conditioner tissue toilet blowdryer mirror"
     "piano violin keyboard flute trumpet organ cello saxophone banjo clarinet recorder oboe cornet sopranino tympani bassoon euphonium piccolo daxophone stritch manzello"
     "red purple green black blue white brown pink yellow gray orange silver gold navy grey beige bronze tan tomato carrot turquoise bone mauve"]))

(def feature-map
  {["pollan"]     [5.0  0.1  0.1  2.0]
   ["scriabin"]   [0.1  0.1  5.0  1.0]
   ["koolhaas"]   [0.1  1.0  0.1  3.0]
   ["kandinsky"]  [0.1  0.1  3.0  5.0]
   ["richter"]    [1.0  1.0  1.0  5.0]
   ["hilton"]     [0.1  5.0  0.1  4.0]
   ["hentoff"]    [0.1  0.1  5.0  1.0]
   ["stewart"]    [3.0  3.0  0.1  3.0]})

;(def num-topics (count topics))
;(def num-types  (count (set (apply concat topics))))

(def num-docs 5000)
(def eta      50)
(def symmetric-alpha 1.0)
(def symmetric-beta  0.25)

;(def alphabet (make-alphabet (apply concat topics)))
;(def thetas   (sample-thetas num-topics num-docs 1))
;(def phis     (sample-phis topics alphabet 0.25))

(def feature-distribution (make-feature-distribution feature-map))

(defn corpus-as-words      [] (sample-corpus-as-words topics num-docs eta symmetric-alpha symmetric-beta))
(defn corpus-instance-list [] (make-corpus-instance-list (corpus-as-words)))

(defn corpus-as-words-with-features      [] 
  (sample-corpus-as-words-with-features topics num-docs eta feature-distribution symmetric-beta))

(defn corpus-instance-list-with-features [] 
  (make-corpus-instance-list-with-features (corpus-as-words-with-features)))

;(def corpus (corpus-as-words-with-features))
;(def dmr-instance-list (corpus-instance-list-with-features))
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



