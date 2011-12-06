(ns malletfn.twentynewsgroups
  (:use (malletfn fileutil
                  corpusutil
                  topicmodel
                  dmrtopics
                  [synth :only [*randoms*]])))

(defn- make-iterator [rootdir]
  (new cc.mallet.pipe.iterator.FileIterator 
       (. (new java.io.File rootdir) listFiles) 
       cc.mallet.pipe.iterator.FileIterator/LAST_DIRECTORY))
  
(def instance-pipe
  (make-instance-pipe
    (new cc.mallet.pipe.TargetStringToFeatures)
    (new cc.mallet.pipe.Input2CharSequence)
    (new cc.mallet.pipe.CharSubsequence cc.mallet.pipe.CharSubsequence/SKIP_HEADER) 
    (new cc.mallet.pipe.CharSequenceRemoveUUEncodedBlocks)
    (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
    (new cc.mallet.pipe.TokenSequenceLowercase)
    (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
    (new cc.mallet.pipe.TokenSequence2FeatureSequence)))


(def training-instances 
  {:type   :20-newsgroups
   :corpus (make-instance-list instance-pipe (make-iterator "resources/20news-bydate-train"))})


(comment test-instances 
  {:type   :20-newsgroups
   :corpus (make-instance-list instance-pipe (make-iterator "resources/20news-bydate-test"))})


(defn test-dmr-20 [corpus threads] 
  (let [[training-corpus test-corpus] (partition-instance-list (or (:corpus corpus) corpus) 
                                                               [90 10] *randoms*)]
    (run-model (make-dmr 
                 :rootname (str "resources/dmr-" (name (:type corpus))) 
                 :topics 20
                 :iterations 1000 
                 :threads threads
                 :iteration-trace-fn (eval-corpus-fn training-corpus test-corpus))
               training-corpus)))


(defn test-lda-20 [corpus threads] 
  (let [[training-corpus test-corpus] (partition-instance-list (or (:corpus corpus) corpus) 
                                                               [90 10] *randoms*)]
    (run-model (make-model 
                 :rootname (str "resources/lda-" (name (:type corpus))) 
                 :topics 20
                 :iterations 1000 
                 :threads threads
                 :iteration-trace-fn (eval-corpus-fn training-corpus test-corpus))
               training-corpus)))


    

(comment           
  ;==== repl worksheet

  (def lda [])
  (def twmg-lda (test-lda-20 training-instances 3))
  (def twng-dmr (test-dmr-20 training-instances 1))
  (def inferencer (get-inferencer twmg-dmr))

  (count training-instances)
  (def topic-indices
    (reverse (sort-indices (get-sampled-distribution inferencer (second training-instances)))))

 (seq  (get-sampled-distribution inferencer (second training-instances)))
       
   (word-seq (second training-instances))
  (.getTarget (second training-instances))
  
  (first training-instances)
  (count-tokens (first training-instances))
  (seq2str (word-seq (nth training-instances 4)))
 
  
  (.size training-instances)
  (seq2str (word-seq (.getTarget (first training-instances))))
  
  (def fv      (.getTarget (first training-instances)))
  
  (word-seq fv)
  (seq (.getIndices fv))
  (seq (.getValues fv)))