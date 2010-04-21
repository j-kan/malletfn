(ns malletfn.topicmodel
  (:require malletfn.mongo)  
  (:import (malletfn.pipe.iterator.SeqIterator)) 
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
  (:import (cc.mallet.pipe.iterator FileIterator))
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
  (:import (com.mongodb.util.JSON))
  (:import (edu.umass.cs.mallet.users.kan.topics ParallelTopicModel)))

(defn basename [filename]
  (first (.split filename "\\.")))

;;(basename "lda-model.ser")

(defn serialize-object [obj file]
  (let [oos (new java.io.ObjectOutputStream
              (new java.io.BufferedOutputStream
                (new java.io.FileOutputStream file)))]
    (try (.writeObject oos obj)
      (finally (.close oos)))))

(defn deserialize-object [file]
  (let [ois (new java.io.ObjectInputStream
              (new java.io.FileInputStream file))]
    (try (.readObject ois)
      (finally (.close ois)))))

(def extra-stop-words
  ["href" "http" "www" "music" "fm" "bbcode" "rel" "nofollow" 
   "artist" "title" "tag" "class" "album" "unknown" "span" 
   "strong" "em" "track" "ndash" "ul" "ol" "li"])


(defn make-instance-pipe []
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


(defn instance-from-mongo-result
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


(defn instance-list-from-mongo [query-result]
  (let [instance-list (new cc.mallet.types.InstanceList (make-instance-pipe))]
    (.addThruPipe instance-list 
                  (new malletfn.pipe.iterator.SeqIterator query-result))
    instance-list))
    

(defn load-from-mongo [file] 
  (let [instance-list 
          (instance-list-from-mongo 
              (malletfn.mongo/mongo-query 
                rhinoplast-bio rhinoplast-query 
                ["name" "content"]
                instance-from-mongo-result))]
     (serialize-object instance-list file)
     instance-list))

(defn load-instances [file]
  (if (.exists file)
    (InstanceList/load file)
    (load-from-mongo file)))

(defn lda-params [input-file num-iterations num-topics]
  (let [basename   (basename input-file)
        alpha      (/ 50.0 num-topics)
        beta       0.01
        outputdir  (format "%s-%d-iterations-%d-topics-%f-alpha-%f-beta" basename num-iterations num-topics alpha beta)]
    [basename alpha beta outputdir]))


(defn make-lda [input-file num-iterations num-topics]
  
  (let [[basename alpha beta outputdir] (lda-params input-file num-iterations num-topics)
        lda                             (new ParallelTopicModel num-topics alpha beta)]
    
    (defn output-file [filename] 
      (let [dir (new File outputdir)]
        (.mkdirs dir)
        (new File dir filename)))

    (defn lda-load-instances []
      (.addInstances lda (load-instances (new File input-file))))
    
    (defn lda-estimate []
      (.estimate lda))
    
    (defn lda-write-results []
      (doto lda
        (.printTopWords        (output-file "topic-keys.txt") 20 false)
        (.printDocumentTopics  (output-file "doc-topics.txt"))
        (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
        (.printState           (output-file "state.gz")))
      (serialize-object lda    (output-file "lda-model.ser")))
    
    (doto lda
      (.setRandomSeed       90210)
      (.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     100 20)
      (.setNumIterations    num-iterations)
      (.setOptimizeInterval 50)
      (.setBurninPeriod     200)
      (.setNumThreads       1))))


; (def lda (make-lda "resources/docs.ser" 1000 8))
 
;; (def lda (make-lda "resources/rhinoplastfm.ser" 1000 16))
;(lda-load-instances)
;(lda-estimate)
;(lda-write-results)
