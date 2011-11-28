(ns malletfn.topicmodel

  ^{:doc    "Runs a Mallet ParallelTopicModel using Clojure."
    :author "jkan" }

  (:use     malletfn.fileutil)  
  (:use     malletfn.corpusutil)  
  (:use     malletfn.synth)
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
;  (:import (cc.mallet.topics ParallelTopicModel)))
  (:import (edu.umass.cs.mallet.users.kan.topics ParallelTopicModel)))


(defstruct lda-options :class :corpus :topics :iterations :threads)
(def lda-default-options {:iterations 1000
                          :threads    1 })

(defmulti make-model-options :class)

(defmethod make-model-options ParallelTopicModel [args]
  (let [options    (merge lda-default-options args)
        rootname   (or (:rootname options) (basename (:corpus options)))
        alpha      (or (:alpha options) (/ 50.0   (:topics options)))
        beta       (or (:beta  options)  0.01)
        outputdir  (format "%s-%d-iterations-%d-topics-%f-alpha-%f-beta-%d-threads-mallet" 
                           rootname (:iterations options) (:topics options) alpha beta (:threads options))]
    (merge options { :rootname  rootname :alpha alpha :beta beta :outputdir outputdir })))


(defstruct topic-model :parameter-estimator :options :output-file-fn)

(defn make-model [& args]
  (let [options     (make-model-options (assoc (apply hash-map args) :class ParallelTopicModel))
        lda         (new ParallelTopicModel (:topics options) (:alpha options) (:beta options))
        output-file (fn [filename]
                      (let [dir (new File (:outputdir options))]
                        (.mkdirs dir)
                        (new File dir filename)))]
    (doto lda
      (.setRandomSeed       90210)
      ;(.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     100 20)
      (.setNumIterations    (:iterations options))
      (.setOptimizeInterval 50)
      (.setBurninPeriod     200)
      (.setNumThreads       (:threads options)))
    (merge options {:parameter-estimator lda :output-file-fn output-file})))


(defn add-model-instances [model instance-list] 
  (.addInstances (:parameter-estimator model) instance-list))



(defn estimate-model [model]
  (.estimate (:parameter-estimator model)))

(defmulti write-model-results :class)

(defmethod write-model-results ParallelTopicModel [model]
  (let [output-file (:output-file-fn model)
        lda         (:parameter-estimator model)]
      (doto lda
        (.printTopWords        (output-file "topic-keys.txt") 20 false)
        (.printDocumentTopics  (output-file "doc-topics.txt"))
        (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
        (.printState           (output-file "state.gz")))
      (serialize-object lda    (output-file "lda-model.ser"))))

(defn run-model [m instance-list]
  (println (type (:parameter-estimator m)))
  (add-model-instances m instance-list)
  (estimate-model m) 
  (write-model-results m)
  m)


(defn run-lda []
  (run-model (make-model :corpus "resources/lda-full.ser" 
                         :topics 16 
                         :iterations 2000 
                         :threads 1)))

(defn run-synth-lda [corpus]
  (run-model (make-model :rootname "resources/new-synthetic" 
                         :topics 4 
                         :iterations 5000 
                         :threads 1 
                         :alpha 2.0 
                         :beta 0.5)
             corpus))

(defn run-synth-lda-with-alpha-beta [corpus alpha beta]
  (println "alpha: " alpha "beta: " beta)
  (run-model (make-model :rootname "synthetic" 
                         :topics 4 
                         :iterations 1000 
                         :threads 1 
                         :alpha alpha 
                         :beta beta)
             corpus))

(defn run-synth-lda-with-param-search []
  (let [corpus (dmr-synth-corpus 5000)
        alphas (range 0.5 3.5 1.0)
        betas  (range 0.25 1.25 0.25)]
    (time
      (map (fn [beta] 
             (map (fn [alpha] 
                      (do (run-synth-lda-with-alpha-beta (:corpus corpus) alpha beta)
                          [alpha beta])) 
                  alphas)) 
           betas))))


;;------- topic inference ----------;;

(defn get-inferencer [lda]
  (let [inferencer  (.getInferencer (:parameter-estimator lda))]
    (.setRandomSeed inferencer 90210)
    inferencer))

(defn write-inferred-distributions 
  "wrapper for mallet TopicInferencer method, with defaults"
  [inferencer instances filename & {:keys [iterations sample-interval burn-in doc-topics-threshold doc-topics-max]         
                                    :or   {iterations 100
 								                           sample-interval 10
								                           burn-in 10
								                           doc-topics-threshold 0.0
								                           doc-topics-max -1}}]
  (.setRandomSeed inferencer 90210)
  (.writeInferredDistributions 
    inferencer instances
    (new java.io.File filename)
    iterations sample-interval burn-in doc-topics-threshold doc-topics-max))

(defn ^doubles get-sampled-distribution 
  [inferencer instance & {:keys [iterations sample-interval burn-in]         
                          :or   {iterations 100
                                 sample-interval 10
                                 burn-in 10}}]
  (.getSampledDistribution inferencer instance iterations sample-interval burn-in))


;;------- testing (repl) ----------;;

(comment
  (def synth-corpus (dmr-synth-corpus 5000))
  (def split-corpus (partition-instance-list 
                      (:corpus synth-corpus) [90 10] *randoms*))
  (def training-corpus (first split-corpus))
  (def test-corpus     (last split-corpus))
  (def lda (run-synth-lda training-corpus))
  (def inferencer  (get-inferencer lda))

  (def mlda (malletfn.mtopicmodel/run-synth-lda malletfn.topicmodel/training-corpus))
  (def mlda malletfn.mtopicmodel/mlda)
  (def minferencer  (get-inferencer mlda))
  
  (.getData (first test-corpus))
  (.getSource (first test-corpus))
  (.getName (first test-corpus))
  (.getTarget (first test-corpus))
  
  (write-inferred-distributions inferencer test-corpus "test-doc-topics.txt")
  (write-inferred-distributions inferencer training-corpus "training-doc-topics.txt")
  
  (def test-topic-distributions 
    (map #(seq (get-sampled-distribution inferencer %)) 
         test-corpus))

  (def test-authors 
    (map #(.getSource %) test-corpus))
  
  
  (.getSource (first test-corpus))
  
  (seq2str (malletfn.corpusutil/word-seq (first test-corpus)))
    
  (apply vectorized-mean
         (take 1000 (repeatedly #(get-sampled-distribution inferencer (first test-corpus) :iterations 100))))

  (apply vectorized-mean
         (take 100 (repeatedly #(get-sampled-distribution inferencer (first test-corpus) :iterations 1000))))

  (apply vectorized-mean
         (take 10 (repeatedly #(get-sampled-distribution inferencer (first test-corpus) :iterations 10000))))

  (seq (get-sampled-distribution inferencer (first test-corpus) :iterations 100000))

  (seq (get-sampled-distribution inferencer (second test-corpus)))
  
  (take 4 test-topic-distributions)
  (take 4 test-authors)

  ;(def lda (make-lda "resources/docs.ser" 1000 8))
  ;(def lda (make-lda "resources/rhinoplastfm.ser" 1000 16))
  (def lda (run-lda))
  (run-synth-lda-with-param-search)
  
  '())


;;------- doc topic assignment ----------;;

(defn doc-name [doc-topic-assignment]
  (.toString (.getName (.instance doc-topic-assignment))))

(defn doc-word-sequence [doc-topic-assignment]
  "mallet FeatureSequence for word tokens"
  (.getData (.instance doc-topic-assignment)))

(defn doc-word-features [doc-topic-assignment]
  "word token indices as seq"
  (let [word-seq (doc-word-sequence doc-topic-assignment)
        size     (.size word-seq)]
    (take size (.getFeatures word-seq))))

(defn doc-word-alphabet [doc-topic-assignment]
  "mallet Alphabet for word types"
  (.getAlphabet (doc-word-sequence doc-topic-assignment)))

(defn doc-topic-sequence [doc-topic-assignment]
  "mallet LabelSequence for topics"
  (.topicSequence doc-topic-assignment))

(defn doc-topic-features [doc-topic-assignment]
  "int array of topic assignments for doc word tokens"
  (.getFeatures (.topicSequence doc-topic-assignment)))
  
(defn doc-content [doc-topic-assignment]
  "seq of word tokens"
  (let [word-alphabet (doc-word-alphabet doc-topic-assignment)]
    (map #(.lookupObject word-alphabet %) 
         (doc-word-features doc-topic-assignment))))

(defn doc-content-str [doc-topic-assignment]
  (apply str (interpose " " (doc-content doc-topic-assignment))))

(defn doc-c-topics [doc-topic-assignment]
  (let [topics     (doc-topic-features doc-topic-assignment)
        num-topics (.size (.getAlphabet (doc-topic-sequence doc-topic-assignment)))
        inc-vec    (fn [v i] (assoc v i (inc (nth v i))))]
    (reduce (fn [v t] (inc-vec v t)) 
            (vec (int-array num-topics 0)) 
            topics)))

(defn doc-p-topics [doc-topic-assignment]
  (let [c    (doc-c-topics doc-topic-assignment)
        norm (reduce + 0 c)]
    (map #(/ % norm) c)))
