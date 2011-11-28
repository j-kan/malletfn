(ns malletfn.rhinoplastfm
  (:require malletfn.mongo)  
  (:require malletfn.fileutil)  
  (:use     malletfn.corpusutil)  
  (:require malletfn.dmrtopics)  
  (:import (cc.mallet.types Instance InstanceList))
  ;  (:import (cc.mallet.topics ParallelTopicModel DMRTopicModel)))
  (:import (edu.umass.cs.mallet.users.kan.topics ParallelTopicModel DMRTopicModel)))


(def extra-stop-words
  ["href" "http" "www" "music" "fm" "bbcode" "rel" "nofollow" 
   "artist" "title" "tag" "class" "album" "unknown" "span" 
   "strong" "em" "track" "ndash" "ul" "ol" "li"])

(defn instance-list-from-mongo 
  ([query-result]
    (instance-list-from-mongo 
      (make-instance-pipe
        (new cc.mallet.pipe.Input2CharSequence)
        (new cc.mallet.pipe.CharSequenceRemoveHTML) 
        (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
        (new cc.mallet.pipe.TokenSequenceLowercase)
        (.addStopWords 
          (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
          (into-array extra-stop-words))
        (new cc.mallet.pipe.TokenSequence2FeatureSequence))
      query-result))
  ([pipe query-result]
    {:pre  [(and pipe query-result)]}
    (make-instance-list pipe (mallet-iterator query-result))))


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


(defmulti model-instance-list-from-mongo :class)

(defmethod model-instance-list-from-mongo ParallelTopicModel [model file] 
  (instance-list-from-mongo 
    (malletfn.mongo/mongo-query 
      rhinoplast-bio rhinoplast-query 
      ["name" "content"]
      instance-from-mongo-result)))

(defn load-from-mongo [model file] 
  (let [instance-list (model-instance-list-from-mongo model file)]
    (serialize-object instance-list file)
    instance-list))

(defn load-model-instances 
  "either loads from an existing serialized instance corpus or build it from scratch from a mongo query"
  [model]
  (let [file (new java.io.File (:corpus model))]
    (if (.exists file)
      (InstanceList/load file)
      (load-from-mongo model file))))



;;;======dmr========;;;

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
     (malletfn.dmrtopics/dmr-instance-pipe)
     (malletfn.mongo/mongo-query 
       rhinoplast-bio query 
       ["name" "content"]
       dmr-instance-from-mongo-result)))

(defmethod model-instance-list-from-mongo DMRTopicModel [model file] 
  (dmr-instance-list (or (:query model) "")))


