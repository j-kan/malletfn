(ns malletfn.testdmrtopics
  (:use clojure.test)
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:require malletfn.dmrtopics)
  (:import (edu.umass.cs.mallet.users.kan.topics DMRTopicModel)))

(defn count-features [features alphabet]
    (let [counter (new cc.mallet.types.FeatureCounter alphabet)]
      (doseq [label (iterator-seq (.iterator features))] 
        (.increment counter (.toString label)))
      (.toFeatureVector counter)))

(defn make-dmr-optimizable [dmr num-batches] ;  regular-prior-variance intercept-prior-variance
  (let [dmrtopicmodel     (:inferencer dmr)
        topic-assignments (.getData dmrtopicmodel)
        feature-alphabet  (.getTargetAlphabet (.instance (first topic-assignments)))
        topic-alphabet    (.getTopicAlphabet (:inferencer dmr))
        num-features      (inc (.size feature-alphabet))
        num-topics        (:topics dmr)
        param-pipe        (doto (new cc.mallet.pipe.Noop)
                            (.setDataAlphabet feature-alphabet)
                            (.setTargetAlphabet topic-alphabet))
        param-instances   (new cc.mallet.types.InstanceList param-pipe) 
        maxent-classifier (new cc.mallet.classify.MaxEnt 
                            param-pipe 
                            (double-array (* num-features num-topics))) ]
    
    (doseq [[i ta] (indexed topic-assignments)]
      (let [target (.getTarget (.instance ta))]
        (when target
          (.add param-instances (new cc.mallet.types.Instance 
                                  target 
                                  (count-features 
                                    (iterator-seq (.iterator (.topicSequence ta)))
                                    topic-alphabet) 
                                  i 
                                  i)))))
          
    (doto (new edu.umass.cs.mallet.users.kan.topics.DMROptimizable 
            param-instances num-batches maxent-classifier)
      (.setRegularGaussianPriorVariance 0.5)
      (.setInterceptGaussianPriorVariance 100.0))))



(defn do-optimize [[dmr-optimizable optimizer]]
  (comment (doto dmr-optimizable
             (.setRegularGaussianPriorVariance dmr-optimizable 0.5)
             (.setInterceptGaussianPriorVariance dmr-optimizable 100.0)))
  (.optimize optimizer)
  (doseq [strfeats (malletfn.dmrtopics/str-features-sorted (.getClassifier dmr-optimizable))]
    (println strfeats))
  dmr-optimizable)
        
(defn make-lbfgs [dmr]
  (let [dmro (make-dmr-optimizable dmr 1)]
    [dmro (new cc.mallet.optimize.LimitedMemoryBFGS dmro)]))

(defn make-sma [dmr]
  (let [random      (new java.util.Random)
        num-batches 4
        dmro        (make-dmr-optimizable dmr num-batches)
        opt         (new edu.umass.cs.mallet.users.kan.topics.StochasticOptimizer dmro num-batches random)]
    [(doto dmro
       (.setRegularGaussianPriorVariance 0.5)
       (.setInterceptGaussianPriorVariance 100.0))
     (doto opt
       (.setInitialStep 0.002)
       (.setMu 0.005))]))
  


(comment
  
	(def corpus (malletfn.synth/corpus-instance-list-with-features))
	(def dmr    (time (malletfn.dmrtopics/run-synth-dmr corpus)))
 
  (doseq [strfeats (malletfn.dmrtopics/str-features-sorted (.getRegressionParameters (:inferencer dmr)))]
    (println strfeats))
  
	(malletfn.dmrtopics/str-features-sorted (.getRegressionParameters (:inferencer dmr)))
 
  (time (do-optimize (make-lbfgs dmr)))
  (time (do-optimize (make-sma dmr)))

  (import 'edu.umass.cs.mallet.users.kan.topics.StochasticOptimizer)
 
  (def dmr-random (new java.util.Random))
  (.nextInt dmr-random)
  
 
  (def instances (.getInstanceList dmro-sma))
  
  (first instances)
  
  (.getData (first instances))
  (.getTarget (first instances))
  (.getName (first instances))
  (.getSource (first instances))
)
