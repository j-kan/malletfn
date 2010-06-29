(ns malletfn.corpusutil
  (:import (cc.mallet.types Alphabet))
  (:import (cc.mallet.util Randoms)))

(defmulti sort-indices class)

(defmethod sort-indices (type (to-array [])) [s]
  (let [indices (range (alength s))]
    (sort-by #(aget s %) indices)))

(defmethod sort-indices (type (double-array [])) [#^doubles s]
  (let [indices (range (alength s))]
    (sort-by #(aget s %) indices)))

(defmethod sort-indices cc.mallet.types.Alphabet [a]
  (sort-indices (.toArray a)))

(defmethod sort-indices clojure.lang.PersistentVector [s]
  (let [indices (range (count s))]
    (sort-by #(nth s %) indices)))

(defmethod sort-indices clojure.lang.ISeq [s]
  (let [indices (range (count s))]
    (sort-by #(nth s %) indices)))

;(defn asort-indices [a]
;  (let [indices (range (alength a))]
;    (sort-by #(aget a %) indices)))


(defn seq2str [s]
  (apply str (interpose " " s)))


(defn- inc-vec [#^ints v i] 
  (let [i (int i)] 
    (assoc v i (inc (nth v i)))))

(defn vec-frequencies 
  "builds a vector containing frequency counts of the given int array"
  [#^ints coll num-buckets]
  (reduce inc-vec 
          (vec (int-array num-buckets 0)) coll))

(defn round-int [base n]
  (* base (int (/ n base))))


(defn make-alphabet [words]
  (new Alphabet (to-array words)))
  
(defn make-instance-list [pipe iterator]
  (let [instance-list (new cc.mallet.types.InstanceList pipe)]
    (.addThruPipe instance-list iterator)
    instance-list))

