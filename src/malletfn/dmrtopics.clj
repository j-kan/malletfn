(ns malletfn.dmrtopics
  (:require malletfn.mongo)  
  (:use     malletfn.topicmodel)
  (:use     malletfn.fileutil)
  (:use     malletfn.corpusutil)
  (:use     malletfn.synth)
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
  ;(:import (cc.mallet.topics DMRTopicModel)))
  (:import (edu.umass.cs.mallet.users.kan.topics DMRTopicModel)))


(defn sort-features [maxent]
  (let [features (.getAlphabet maxent)
        sorted-i (sort-indices features)]
    (concat (seq (.lookupObjects features (int-array sorted-i)))
            ["<default>"])))
           
(defn get-feature-weight [maxent feature-i topic-i]
  (let [features (.getAlphabet maxent)]
    (aget (.getParameters maxent) 
          (+ (* topic-i (+ (.size features) 1)) 
             feature-i))))

(defn get-topic-feature-weights [maxent topic-i]
  (let [features   (.getAlphabet maxent)
        parameters (.getParameters maxent)
        num-features (+ 1 (.size features))]
    (map #(aget parameters
                (+ % (* topic-i num-features)))
         (range num-features))))

(defn get-feature-weights [maxent feature-i]
  (let [features   (.getAlphabet maxent)
        feature    (if (= feature-i (.size features))
                       "<def>"
                       (.lookupObject features feature-i))
        parameters (.getParameters maxent)
        num-topics (.size (.getLabelAlphabet maxent))
        num-features (+ 1 (.size features))]
    [feature (map #(aget parameters
                         (+ feature-i (* % num-features)))
                  (range num-topics))]))

;(get-feature-weights maxent 21)

(defn get-topic-feature-weights-sorted [maxent topic-i]
  (let [features   (.getAlphabet maxent)
        parameters (.getParameters maxent)
        num-features (+ 1 (.size features))
        sorted-i   (sort-indices features)]
    (map (fn [i] [(.lookupObject features i)
                  (aget parameters
                         (+ i (* topic-i num-features)))])
         sorted-i)))

(defn get-feature-weights-sorted [maxent]
  (map #(get-topic-feature-weights-sorted maxent %) 
       (range (.size (.getLabelAlphabet maxent)))))

(defn print-features-topic-rows [maxent outstream]
  (map (fn [s] (.println outstream 
                 (seq2str (map (fn [[fname wt]] 
                                   (format " %s:%8.5f" fname wt)) 
                               s))))
       (get-feature-weights-sorted maxent)))

;(defn str-topic-names [dmr]
;  (format " %6s %s" fname (seq2str (map #(format " %8.5f" %) wts))));TODO

(defn str-features-sorted [maxent]
  (let [features     (.getAlphabet maxent)
        topics       (.getLabelAlphabet maxent)
        num-topics   (.size topics)
        num-features (+ 1 (.size features))]
    (map (fn [feature-i] 
           (let [[fname wts] (get-feature-weights maxent feature-i)]
              (format " %10s %s" fname (seq2str (map #(format " %8.5f" %) wts))))) 
         (conj (sort-indices features) (.size features)))))

(defn print-features-sorted [maxent pstream]
  (dorun (map (fn [feature-str] 
                  (.println pstream (str feature-str))) 
              (str-features-sorted maxent))))

(defn write-features-sorted [dmr file]
  (with-file-output-stream file
    (fn [fos] (print-features-sorted (.getRegressionParameters dmr) fos))))


;(write-features-sorted dmr "features.txt")


(defn dmr-instance-pipe []
  (make-instance-pipe
    (new cc.mallet.pipe.TargetStringToFeatures)
    (new cc.mallet.pipe.Input2CharSequence)
    (new cc.mallet.pipe.CharSequenceRemoveHTML) 
    (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
    (new cc.mallet.pipe.TokenSequenceLowercase)
    (.addStopWords 
      (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
      (into-array extra-stop-words))
    (new cc.mallet.pipe.TokenSequence2FeatureSequence)))


(defn years-from-summary [summary]
  (map #(Integer/parseInt %) 
       (re-seq #"\b(?:1[5-9]\d\d)|(?:20[01]\d)\b" (or summary ""))))

(defn decades-from-years [s]
  (set (map (partial round-int 10) s)))

(defn epoch-from-year [year]
  (if (> year 1900)
    [(round-int 10 year)  (+ 5  (round-int 10  (- year 5)))]
    [(round-int 100 year) (+ 50 (round-int 100 (- year 50)))]))

(defn beta-date-features-for-year [year]
  (letfn [(df [min-year max-year year]
            (let [d (/ (- year min-year) (- max-year min-year))]
              [(format "d_%d_a=%f" min-year (java.lang.Math/log d))
               (format "d_%d_b=%f" min-year (java.lang.Math/log (- 1 d)))]))]
    (apply concat 
      (for [min-year [1500 1900 1960] :when (> year min-year)]
        (df min-year 2010 year)))))

(defn features-from-years [extractor s]
   (seq2str (set (flatten (map extractor s)))))

(defn beta-date-features [summary]
  (features-from-years beta-date-features-for-year (years-from-summary summary)))

(defn date-features [summary]
  (features-from-years epoch-from-year (years-from-summary summary)))


(defn dmr-instance-from-mongo-result
  "assumes that your mongo query includes fields 'name' and 'content'"
  [item]
  (let [name    (.get item "name")
        summary (or (.get item "content") "")
        years   (date-features summary)]
    (new Instance summary years name name)))

(defn dmr-instance-list
  "builds a DMR-ready Mallet InstanceList from a mongo query"
   [query]
   (instance-list-from-mongo 
     (dmr-instance-pipe)
     (malletfn.mongo/mongo-query 
       rhinoplast-bio query 
       ["name" "content"]
       dmr-instance-from-mongo-result)))

(defmethod model-instance-list-from-mongo DMRTopicModel [model file] 
  (dmr-instance-list (or (:query model) "")))


(defmethod make-model-options DMRTopicModel [args]
  (let [options    (merge lda-default-options args)
        rootname   (or (:rootname options) (basename (:corpus options)))
        outputdir  (format "%s-%d-iterations-%d-topics-%d-threads-dmr-jkan" 
                           rootname (:iterations options) (:topics options) (:threads options))]
    (merge options { :rootname rootname :outputdir outputdir })))


(defn make-dmr [& args]
  (let [options     (make-model-options (assoc (apply hash-map args) :class DMRTopicModel))
        dmr         (new DMRTopicModel (:topics options))
        output-file (fn [filename]
                      (let [dir (new File (:outputdir options))]
                        (.mkdirs dir)
                        (new File dir filename)))]
    (doto dmr
      (.setRandomSeed       90210)
      ;(.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     100 20)
      (.setNumIterations    (:iterations options))
      (.setOptimizeInterval 20)
      (.setBurninPeriod     20)
      (.setNumThreads       (:threads options))
      ;(.setNumBatches       8)
      ;(.setInitialStep      0.001)
      ;(.setMetaStep         0.002)
      )
    (merge options {:parameter-estimator dmr :output-file-fn output-file})))

(defmethod write-model-results DMRTopicModel [model]
  (let [output-file (:output-file-fn model)
        dmr         (:parameter-estimator model)]
    (doto dmr
      (.printTopWords        (output-file "topic-keys.txt") 20 false)
      (.printDocumentTopics  (output-file "doc-topics.txt"))
      (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
      (.writeParameters      (output-file "parameters.txt"))
      (.printState           (output-file "state.gz")))
    (write-features-sorted dmr (output-file "feature-wts.txt"))
    (serialize-object dmr    (output-file "dmr-model.ser"))))


(defn load-dmr [& args] 
  (let [options (apply hash-map args)
        dmr     (DMRTopicModel/read (File. (:outputdir options) "dmr-model.ser"))]
    dmr))

(defn run-dmr []
  (run-model (make-dmr :corpus     "resources/dmr-full-by-overlapping-epoch.ser" 
                       :topics     16 
                       :iterations 2000 
                       :threads    1)))

(defn run-dmr-small []
  (run-model (make-dmr :corpus     "resources/dmr-full-by-overlapping-epoch.ser" 
                       :topics     8 
                       :iterations 1000 
                       :threads    1)))

(defn run-synth-dmr [corpus]
  (run-model (make-dmr :rootname "resources/synthetic-dmr-" 
                       :iterations 1000 
                       :topics 4 
                       :threads 1)
             corpus))

