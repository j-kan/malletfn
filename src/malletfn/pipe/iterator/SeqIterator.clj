(ns malletfn.pipe.iterator.SeqIterator
  (:gen-class
     :extends cc.mallet.pipe.iterator.EmptyInstanceIterator
     :init init
     :constructors {[clojure.lang.ISeq] []}
     :state state))

(defn -init [some-seq]
  [[] (ref some-seq)])

(defn -hasNext [this]
  (boolean @(.state this)))
 
(defn -next [this]
  (let [[car & cdr] @(.state this)]
    (dosync (ref-set (.state this) cdr))
    car))

