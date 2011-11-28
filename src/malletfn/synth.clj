(ns malletfn.synth
  (:use     malletfn.corpusutil)
  (:import (cc.mallet.types Alphabet))
  (:import (cc.mallet.types LabelAlphabet))
  (:import (cc.mallet.util  Randoms)))

;(set! *warn-on-reflection* true)

(def ^:dynamic *randoms* (new Randoms 90210))


(defstruct discrete  :type :pp)
(defstruct dirichlet :type :pp :magnitude)

(defn- make-mallet-dirichlet 
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
  (let [{features :features
         alphas   :alphas   } (sample-from feature-distribution)
        dirichlet-alpha (make-dirichlet alphas)]
    (assoc (sample-from dirichlet-alpha) :features features)))

(defn sample-thetas-with-features
  "(lazily) sample an infinite sequence of multinomials from a dirichlet subject to the given feature distribution"
  [feature-distribution]
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
   The thetas can be defined lazily, in which case the corpus is an infinitely long 
   sequence of documents."
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


(defn sample-corpus-as-words 
  "sample an infinitely long corpus of documents, 
   where the documents are represented as sequences of words"
  
  [{:keys [topics eta symmetric-alpha symmetric-beta] 
    :or {eta 20
         symmetric-alpha 1.0
         symmetric-beta  0.25}}] 
  (let [alphabet   (make-alphabet (apply concat topics))
        thetas     (sample-thetas-with-symmetric-alpha (count topics) symmetric-alpha)
        phis       (sample-phis topics alphabet symmetric-beta)
        [wws zzs]  (sample-corpus-indices eta thetas phis)]
    (map #(ws2words % alphabet) wws)))


(defn sample-corpus-as-words-with-features
  "sample an infinitely long corpus of documents, 
   where the documents are represented as sequences of words and associated features"
  
  [{:keys [topics eta feature-map symmetric-beta] 
    :or {eta 20
         symmetric-beta  0.25}}] 
  (let [alphabet   (make-alphabet (apply concat topics))
        thetas     (sample-thetas-with-features (make-feature-distribution feature-map))
        phis       (sample-phis topics alphabet symmetric-beta)
        [wws zzs]  (sample-corpus-indices eta thetas phis)]
    (partition 3 (interleave 
                   (map #(ws2words % alphabet) wws)
                   (map :features thetas)
                   zzs))))

(defn- filter-out-topic-assignments 
  [words-features-tas] (map butlast words-features-tas))


(defn make-corpus-instance-list-with-features
  "turn a seq of pairs of words and features into a mallet instance list"
  
  ([words features]     ;  [[words1 words2 ...] [features1 features2 ... ]]
    (make-corpus-instance-list-with-features 
      (partition 2 (interleave words features))))

  ([words-and-features] ;  [[words1 features1] [words2 features2] ...]
    (make-instance-list 
      (make-instance-pipe
        (new cc.mallet.pipe.TargetStringToFeatures)
        (new cc.mallet.pipe.Input2CharSequence)
        (new cc.mallet.pipe.CharSequence2TokenSequence)
        (new cc.mallet.pipe.TokenSequence2FeatureSequence))
      (mallet-iterator-with-features words-and-features))))


(defn- make-mallet-instance-seq [the-seq]
  (map (fn [[words features tas]] 
           (struct-map mallet-instance :data words :target features :name tas :source features)) 
       the-seq))

(defn make-corpus-instance-list
  "turn a seq of triples of words, features, and topic assignments into a mallet instance list"
  
  [the-seq] ;  [[words1 features1 topic-assignmemts1] [words2 features2 topic-assignmemts2] ...]
  (make-instance-list 
    (make-instance-pipe
      (new cc.mallet.pipe.TargetStringToFeatures)
      (new cc.mallet.pipe.Input2CharSequence)
      (new cc.mallet.pipe.CharSequence2TokenSequence)
      (new cc.mallet.pipe.TokenSequence2FeatureSequence))
    (mallet-instance-iterator (make-mallet-instance-seq the-seq))))


(defn make-dmr-synth-corpus-with-features
  [topics feature-map num-docs]
  (let [generator     (sample-corpus-as-words-with-features 
                        {:topics topics 
                         :feature-map feature-map})
        instance-list (make-corpus-instance-list-with-features 
                        (take num-docs (filter-out-topic-assignments generator))) ]
    {:type       :dmr-synth-corpus
     :corpus     instance-list
     :num-topics (count topics)}))

(defn make-dmr-synth-corpus
  [topics feature-map num-docs]
  (let [generator     (sample-corpus-as-words-with-features 
                        {:topics topics 
                         :feature-map feature-map})
        instance-list (make-corpus-instance-list 
                        (take num-docs generator)) ]
    {:type       :dmr-synth-corpus
     :corpus     instance-list
     :num-topics (count topics)}))


(def full-topics 
  (map #(sort (seq (.split % "\\s+")))  
       ["eggplant artichoke tomato onion cucumber carrot asparagus zucchini broccoli lettuce cabbage cauliflower spinach avocado potato leek kale basil arugula radicchio"
        "soap toothbrush towel bath slippers shampoo razor robe floormat icebucket pillow lotion conditioner tissue toilet blowdryer mirror"
        "piano violin keyboard flute trumpet organ cello saxophone banjo clarinet recorder oboe cornet sopranino tympani bassoon euphonium piccolo daxophone stritch manzello"
        "red purple green black blue white brown pink yellow gray orange silver gold navy grey beige bronze tan tomato carrot turquoise bone mauve"]))

(def abbreviated-topics 
  (map #(sort (seq (.split % "\\s+")))  
       ["eggplant artichoke tomato onion cucumber" ; carrot asparagus zucchini broccoli lettuce cabbage cauliflower spinach avocado potato leek kale basil arugula radicchio"
        "soap toothbrush towel bath slippers"      ; shampoo razor robe floormat icebucket pillow lotion conditioner tissue toilet blowdryer mirror"
        "piano violin keyboard flute trumpet"      ; organ cello saxophone banjo clarinet recorder oboe cornet sopranino tympani bassoon euphonium piccolo daxophone stritch manzello"
        "red purple green black blue"              ; white brown pink yellow gray orange silver gold navy grey beige bronze tan tomato carrot turquoise bone mauve"
        ]))
  
(def author-feature-map
  {["pollan"]     [5.0  0.1  0.1  2.0]
   ["scriabin"]   [0.1  0.1  5.0  1.0]
   ["koolhaas"]   [0.1  1.0  0.1  3.0]
   ["kandinsky"]  [0.1  0.1  3.0  5.0]
   ["richter"]    [1.0  1.0  1.0  5.0]
   ["hilton"]     [0.1  5.0  0.1  4.0]
   ["hentoff"]    [0.1  0.1  5.0  1.0]
   ["stewart"]    [3.0  3.0  0.1  3.0]})

(defn dmr-synth-corpus [num-docs] 
  (make-dmr-synth-corpus full-topics author-feature-map num-docs))

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
