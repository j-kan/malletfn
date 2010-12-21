(ns malletfn.pipe.iterator.SeqWithFeaturesIterator
  (:gen-class
     :extends cc.mallet.pipe.iterator.EmptyInstanceIterator
     :prefix "seq-with-features-"
     :init init
     :constructors {[clojure.lang.ISeq clojure.lang.ISeq] []}
     :state state))

(defn seq-with-features-init [data-seq features-seq]
  [[] (ref (seq (zipmap data-seq features-seq)))])

(defn seq-with-features-hasNext [this]
  (boolean @(.state this)))
 
(defn seq-with-features-next [this]
  (let [[car & cdr]     @(.state this)
        [data features] car
        str-data        (apply str (interpose " " data))
        str-features    (apply str (interpose " " features))]
    (dosync (ref-set (.state this) cdr))
    ;(comment (println (cons :doc data) (cons :features features)))
    (new cc.mallet.types.Instance str-data str-features nil str-features)))

