(ns malletfn.corpusutil 

  ^{:doc    "Utilities to work with Mallet corpora using Clojure."
    :author "jkan" }

  (:use clojure.test)
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

(defn make-instance-pipe [& pipes]
  (new cc.mallet.pipe.SerialPipes
    (into-array cc.mallet.pipe.Pipe pipes)))



(defn mallet-iterator 
  "Adapts a Clojure sequence to work as a Mallet Instance iterator"
  [some-seq]
  (let [state (ref some-seq)]
    (reify java.util.Iterator
      (hasNext [this] 
        (not (empty? @state)))
      (next [this] 
        (let [[car & cdr] @state]
          (dosync (ref-set state cdr))
          (println car)
          (if (instance? cc.mallet.types.Instance car)
            car
            (new cc.mallet.types.Instance (apply str (interpose " " car)) nil nil nil))))
      (remove [this] 
        (let [[car & cdr] @state]
          (dosync (ref-set state cdr)))))))


(defn mallet-iterator-with-features 
  "Adapts two parallel Clojure sequences, 
   the first containing data and the second containing features, 
   to work as a Mallet Instance iterator"
  ([data-and-features-seq]
    (let [state (ref (seq data-and-features-seq))]
      (reify java.util.Iterator

        (hasNext [this] 
                 (not (empty? @state)))

        (next [this] 
              (let [[[data features] & cdr] @state
                    str-data                (apply str (interpose " " data))
                    str-features            (apply str (interpose " " features))]
                (dosync (ref-set state cdr))
                ;(println (cons :doc data) (cons :features features))
                (new cc.mallet.types.Instance str-data str-features nil str-features)))

        (remove [this] 
                (let [[_ _ & cdr] @state]
                  (dosync (ref-set state cdr)))))))
  
  ([data-seq features-seq]
    (mallet-iterator-with-features 
      (partition 2 (interleave data-seq features-seq)))))




(deftest test-mallet-iterator
  (let [ms (mallet-iterator '(["a" "b" "c"] ["d" "e" "f"]))
        test-next
           (fn [it] 
             (do
               (is (.hasNext it))
               (.next it)))
        i1 (test-next ms)
        i2 (test-next ms)]
    (is (= "a b c" (.getData i1)))
    (is (= "d e f" (.getData i2)))
    (is (not (.hasNext ms)))))
  

(deftest test-mallet-iterator-with-features
  (let [ms (mallet-iterator-with-features 
             [["a" "b" "c"] ["d" "e" "f"]]
             [["early"] ["late"]])
        test-next
           (fn [it] 
             (do
               (is (.hasNext it))
               (.next it)))
        i1 (test-next ms)
        i2 (test-next ms)]
    (is (= "a b c" (.getData i1)))
    (is (= "d e f" (.getData i2)))
    (is (not (.hasNext ms)))))
  

(run-tests)


