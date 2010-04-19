(ns mallejure.mongo
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
  (:import (com.mongodb.util JSON)))

(defn mongo-json-field-spec [fields]
  (str "{" 
       (apply str
         (interpose "," (map #(format "'%s':'%d'" %1 %2) 
                             fields 
                             (iterate inc 0)))) 
       "}"))

(defn mongo-collection [dbname collname]
  (.getCollection (.getDB (new Mongo) dbname) collname))

(defn mongo-query [collection jsonquery fields fun]
  (map fun 
    (seq 
      (.find collection 
        (JSON/parse jsonquery)
        (JSON/parse (mongo-json-field-spec fields))))))



;(def rhinoplast-bio (mongo-collection "rhinoplast" "bio"))
;(first (seq (mongo-query rhinoplast-bio "{'name':'Sun Ra'}" ["name" "content"])))
