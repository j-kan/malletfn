(ns malletfn.topicmodel
  (:require malletfn.mongo)  
  (:use     malletfn.fileutil)  
  (:use     malletfn.corpusutil)  
  (:use     malletfn.synth)
  (:import (malletfn.pipe.iterator.SeqIterator)) 
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
  (:import (cc.mallet.pipe.iterator FileIterator))
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
;  (:import (cc.mallet.topics ParallelTopicModel)))
  (:import (edu.umass.cs.mallet.users.kan.topics ParallelTopicModel)))

 
(defn instance-list-from-mongo [pipe query-result]
  (make-instance-list (new malletfn.pipe.iterator.SeqIterator query-result)))




(def extra-stop-words
  ["href" "http" "www" "music" "fm" "bbcode" "rel" "nofollow" 
   "artist" "title" "tag" "class" "album" "unknown" "span" 
   "strong" "em" "track" "ndash" "ul" "ol" "li"])


(defn- make-instance-pipe []
  (new cc.mallet.pipe.SerialPipes 
    (into-array cc.mallet.pipe.Pipe
      [(new cc.mallet.pipe.Input2CharSequence)
       (new cc.mallet.pipe.CharSequenceRemoveHTML) 
       (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
       (new cc.mallet.pipe.TokenSequenceLowercase)
       (.addStopWords 
         (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
         (into-array extra-stop-words))
       (new cc.mallet.pipe.TokenSequence2FeatureSequence)])))

(defn- instance-from-mongo-result
  "assumes that your mongo query includes fields 'name' and 'content'"
  [item]
  (let [name    (.get item "name")
        summary (.get item "content")]
    (new Instance (or summary name) "" name name)))

(def rhinoplast-bio (malletfn.mongo/mongo-collection "rhinoplast" "bio"))
(def rhinoplast-query "")

; (make-instance-pipe)
; (defn make-iterator [])

; (def rhinoplast-query "{'name' : /^Sun/}")

; (map #(.get % "name") (seq rhinoquery-result))
; (map instance-from-mongo-result (seq rhinoquery-result))

;(defn mongo-connection [dbname collname]
;  (.getCollection (.getDB (new Mongo) dbname) collname))

;(def instance-list (new cc.mallet.types.InstanceList (make-instance-pipe)))
;(def instance-iter (new malletfn.pipe.iterator.SeqIterator rhinoquery-result))


(defn- load-from-mongo [file] 
  (let [instance-list 
          (instance-list-from-mongo 
              (make-instance-pipe)
              (malletfn.mongo/mongo-query 
                rhinoplast-bio rhinoplast-query 
                ["name" "content"]
                instance-from-mongo-result))]
     (serialize-object instance-list file)
     instance-list))

(defn- load-instances [file]
  (if (.exists file)
    (InstanceList/load file)
    (load-from-mongo file)))


(defstruct lda-params :class :corpus :topics :iterations :threads)
(def lda-defaults { :iterations 1000
                    :threads    1 })

(defmulti make-model-params :class)

(defmethod make-model-params ParallelTopicModel [params]
  (let [options    (merge lda-defaults params)
        rootname   (or (:rootname options) (basename (:corpus options)))
        alpha      (or (:alpha options) (/ 50.0   (:topics options)))
        beta       (or (:beta  options)  0.01)
        outputdir  (format "%s-%d-iterations-%d-topics-%f-alpha-%f-beta-%d-threads-mallet" 
                           rootname (:iterations options) (:topics options) alpha beta (:threads options))]
    (merge options { :rootname  rootname :alpha alpha :beta beta :outputdir outputdir })))


(defstruct topic-model :inferencer :params :output-file-fn)

(defn make-model [& args]
  (let [params      (make-model-params (assoc (apply hash-map args) :class ParallelTopicModel))
        lda         (new ParallelTopicModel (:topics params) (:alpha params) (:beta params))
        output-file (fn [filename]
                      (let [dir (new File (:outputdir params))]
                        (.mkdirs dir)
                        (new File dir filename)))]
    (doto lda
      (.setRandomSeed       90210)
      ;(.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     100 20)
      (.setNumIterations    (:iterations params))
      (.setOptimizeInterval 50)
      (.setBurninPeriod     200)
      (.setNumThreads       (:threads params)))
    (merge params {:inferencer lda :output-file-fn output-file})))
  
(defn load-model-instances 
  ([model]               (.addInstances (:inferencer model) (load-instances (new File (:corpus model)))))
  ([model instance-list] (.addInstances (:inferencer model) instance-list)))

(defn estimate-model [model]
  (.estimate (:inferencer model)))

(defmulti write-model-results :class)

(defmethod write-model-results ParallelTopicModel [model]
  (let [output-file (:output-file-fn model)
        lda         (:inferencer model)]
      (doto lda
        (.printTopWords        (output-file "topic-keys.txt") 20 false)
        (.printDocumentTopics  (output-file "doc-topics.txt"))
        (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
        (.printState           (output-file "state.gz")))
      (serialize-object lda    (output-file "lda-model.ser"))))

(defn run-model 
  ([m] 
      (load-model-instances m)
      (estimate-model m) 
      (write-model-results m))
  ([m instance-list]
      (load-model-instances m instance-list)
      (estimate-model m) 
      (write-model-results m)))


(defn run-lda []
  (let [lda (make-model :corpus "resources/dmr-full-by-decade.ser" 
                        :topics 16 
                        :iterations 2000 
                        :threads 1)]
    (println (type lda))
    (run-model lda)
    lda))

(defn run-synth-lda [corpus]
  (let [lda (make-model :rootname "resources/new-synthetic" 
                        :topics 4 
                        :iterations 1000 
                        :threads 1 
                        :alpha 2.0 
                        :beta 0.5)]
    (println (type lda))
    (run-model lda corpus)
    lda))

(defn run-synth-lda-with-alpha-beta [corpus alpha beta]
  (let [lda (make-model :rootname "synthetic" 
                        :topics 4 
                        :iterations 1000 
                        :threads 1 
                        :alpha alpha 
                        :beta beta)]
    (println (type lda) alpha beta)
    (run-model lda corpus)
    lda))

(defn run-synth-lda-with-param-search []
  (let [corpus (corpus-instance-list-with-features)
        alphas (range 0.5 3.5 1.0)
        betas  (range 0.25 1.25 0.25)]
    (time
      (map (fn [beta] 
             (map (fn [alpha] 
                      (do (run-synth-lda-with-alpha-beta corpus alpha beta)
                          [alpha beta])) 
                  alphas)) 
           betas))))


(def lda (make-model :rootname "resources/new-synthetic" 
                        :topics 4 
                        :iterations 1000 
                        :threads 1 
                        :alpha 2.0 
                        :beta 0.5))
;; (def lda (make-lda "resources/docs.ser" 1000 8))
;; (def lda (make-lda "resources/rhinoplastfm.ser" 1000 16))
;; (def lda (run-lda))
;; (def corpus (corpus-instance-list-with-features))
;; (def lda (run-synth-lda corpus))
;; (run-synth-lda-with-param-search)


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
