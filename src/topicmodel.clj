(ns topicmodel
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector InstanceList Alphabet))
  (:import (cc.mallet.pipe.iterator FileIterator))
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
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


(defn load-from-mongo [file]
  '())

(defn load-instances [file]
  (if (.exists file)
    (InstanceList/load file)
    (load-from-mongo file)))

(defn lda-params [input-file num-iterations num-topics]
  (let [basename   (basename input-file)
        alpha      (/ 50.0 num-topics)
        beta       0.01
        outputdir  (format "resources/%s-%d-iterations-%d-topics-%f-alpha-%f-beta" basename num-iterations num-topics alpha beta)]
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


;; (def lda (make-lda "rhinoplastfm.ser" 1000 16))
;; (lda-load-instances)
;; (lda-estimate)
;; (lda-write-results)
