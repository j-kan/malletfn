(ns malletfn.dmrtopics
  (:require malletfn.mongo)  
  (:use     malletfn.topicmodel)
  (:use     malletfn.fileutil)
  (:use     malletfn.corpusutil)
  (:use     malletfn.synth)
  (:import (malletfn.pipe.iterator.SeqIterator)) 
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
  (:import (cc.mallet.pipe.iterator FileIterator))
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
              (format " %6s %s" fname (seq2str (map #(format " %8.5f" %) wts))))) 
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
  (new cc.mallet.pipe.SerialPipes 
    (into-array cc.mallet.pipe.Pipe
      [(new cc.mallet.pipe.TargetStringToFeatures)
       (new cc.mallet.pipe.Input2CharSequence)
       (new cc.mallet.pipe.CharSequenceRemoveHTML) 
       (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
       (new cc.mallet.pipe.TokenSequenceLowercase)
       (.addStopWords 
         (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
         (into-array extra-stop-words))
       (new cc.mallet.pipe.TokenSequence2FeatureSequence)])))

(defn years-from-summary [summary]
  (map #(Integer/parseInt %) 
       (re-seq #"\b(?:1[5-9]\d\d)|(?:20[01]\d)\b" (or summary ""))))

(defn decades-from-years [s]
  (set (map (partial round-int 10) s)))

(defn beta-date-features [year]
  (letfn [(df [min-year max-year year]
            (let [d (/ (- year min-year) (- max-year min-year))]
              [(format "d_%d_a=%f" min-year (java.lang.Math/log d))
               (format "d_%d_b=%f" min-year (java.lang.Math/log (- 1 d)))]))]
    (apply concat 
      (for [min-year [1500 1900 1960] :when (> year min-year)]
        (df min-year 2010 year)))))

(defn date-features [summary]
  (seq2str (decades-from-years (years-from-summary summary))))


(defn dmr-instance-from-mongo-result
  "assumes that your mongo query includes fields 'name' and 'content'"
  [item]
  (let [name    (.get item "name")
        summary (or (.get item "content") "")
        years   (date-features summary)]
    (new Instance summary years name name)))

(defn dmr-instance-list [query]
  (instance-list-from-mongo 
              (dmr-instance-pipe)
              (malletfn.mongo/mongo-query 
                rhinoplast-bio query 
                ["name" "content"]
                dmr-instance-from-mongo-result)))

;(def dmr-query "{'name' : /^Sun/}")
;(def dmr-list (dmr-instance-list "{'name' : /^Sun/}"))
;(.getTarget (nth dmr-list 5))
; (dmr-instance-list "{'content' : /\\bexperimental\\b/}")
; (let [instance-list (dmr-instance-list "{'name' : /^The /}")]


(defn- dmr-load-from-mongo [query file] 
  (let [instance-list (dmr-instance-list query)]
     (serialize-object instance-list file)
     instance-list))

(defn- load-instances [file]
  (if (.exists file)
    (InstanceList/load file)
    (dmr-load-from-mongo "" file))) 

;"{'content' : /\\bexperimental\\b/}"

(defmethod make-model-params DMRTopicModel [params]
  (let [options    (merge lda-defaults params)
        rootname   (or (:rootname options) (basename (:corpus options)))
        outputdir  (format "%s-%d-iterations-%d-topics-%d-threads-dmr-jkan" 
                           rootname (:iterations options) (:topics options) (:threads options))]
    (merge options { :rootname rootname :outputdir outputdir })))


(defn make-dmr [& args]
  
  (let [params      (make-model-params (assoc (apply hash-map args) :class DMRTopicModel))
        dmr         (new DMRTopicModel (:topics params))
        output-file (fn [filename]
                      (let [dir (new File (:outputdir params))]
                        (.mkdirs dir)
                        (new File dir filename)))]

    (defn dmr-load-instances 
      ([]              (.addInstances dmr (load-instances (new File (:corpus params)))))
      ([instance-list] (.addInstances dmr instance-list)))
    
    (defn dmr-estimate []
      (.estimate dmr))
    
    (defn dmr-write-results []
      (doto dmr
        (.printTopWords        (output-file "topic-keys.txt") 20 false)
        (.printDocumentTopics  (output-file "doc-topics.txt"))
        (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
        (.writeParameters      (output-file "parameters.txt"))
        (.printState           (output-file "state.gz")))
      (write-features-sorted dmr (output-file "feature-wts.txt"))
      (serialize-object dmr    (output-file "dmr-model.ser")))
    
    (defn dmr-run
      ([] (dmr-load-instances)
          (dmr-estimate) 
          (dmr-write-results))
      ([instance-list]
          (dmr-load-instances instance-list)
          (dmr-estimate) 
          (dmr-write-results)))

    (doto dmr
      (.setRandomSeed       90210)
      ;(.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     100 20)
      (.setNumIterations    (:iterations params))
      (.setOptimizeInterval 20)
      (.setBurninPeriod     20)
      (.setNumThreads       (:threads params))
      )))


(defn load-dmr [& args] 
  (let [params (apply hash-map args)
        dmr    (DMRTopicModel/read (File. (:outputdir params) "dmr-model.ser"))]
    dmr))

(defn run-dmr []
  (let [dmr (make-dmr :corpus     "resources/dmr-full-by-decade.ser" 
                      :iterations 1000 
                      :topics 8 
                      :threads 1)]
    (println (type dmr))
    (dmr-run)
    dmr))


(def synthetic-corpus (corpus-instance-list-with-features))

(defn run-synth-dmr []
  (let [dmr (make-dmr :rootname "resources/synthetic-dmr-" 
                      :iterations 1000 
                      :topics 4 
                      :threads 1)]
    (println (type dmr))
    (dmr-run synthetic-corpus) 
    dmr))

;; (def dmr (run-synth-dmr))

; (def dmr (load-dmr :corpus "resources/dmr-full-by-decade.ser" :iterations 1000 :topics 8 :threads 1))
; (def dmr (run-dmr))	
; (get-feature-weights-sorted (.getRegressionParameters dmr))
;
;(write-features-sorted dmr "feature-wts.txt")

;(def maxent (.getRegressionParameters dmr))
;(.getParameters maxent)
;(.getNumParameters maxent)
;
;(import (cc.mallet.topics DMRTopicModel))