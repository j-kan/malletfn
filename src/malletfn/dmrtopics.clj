(ns malletfn.dmrtopics
  (:use     malletfn.topicmodel)
  (:use     malletfn.fileutil)
  (:use     malletfn.corpusutil)
  (:use     malletfn.synth)
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector Instance InstanceList Alphabet))
  ;(:import (cc.mallet.topics DMRTopicModel)))
  (:import (edu.umass.cs.mallet.users.kan.topics DMRTopicModel)))


(defn sort-features [maxent]
  (let [features (.getAlphabet maxent)
        sorted-i (sort-indices features)]
    (concat (seq (.lookupObjects features (int-array sorted-i)))
            ["<default>"])))
           
(defn get-feature-weight [maxent feature-i topic-i]
  (let [features (.getAlphabet maxent)]
    (aget (.getParameters maxent) 
          (+ (* topic-i (+ (.size features) 1)) 
             feature-i))))

(defn get-topic-feature-weights [maxent topic-i]
  (let [features   (.getAlphabet maxent)
        parameters (.getParameters maxent)
        num-features (+ 1 (.size features))]
    (map #(aget parameters
                (+ % (* topic-i num-features)))
         (range num-features))))

(defn get-feature-weights [maxent feature-i]
  (let [features   (.getAlphabet maxent)
        feature    (if (= feature-i (.size features))
                       "<def>"
                       (.lookupObject features feature-i))
        parameters (.getParameters maxent)
        num-topics (.size (.getLabelAlphabet maxent))
        num-features (+ 1 (.size features))]
    [feature (map #(aget parameters
                         (+ feature-i (* % num-features)))
                  (range num-topics))]))

;(get-feature-weights maxent 21)

(defn get-topic-feature-weights-sorted [maxent topic-i]
  (let [features   (.getAlphabet maxent)
        parameters (.getParameters maxent)
        num-features (+ 1 (.size features))
        sorted-i   (sort-indices features)]
    (map (fn [i] [(.lookupObject features i)
                  (aget parameters
                         (+ i (* topic-i num-features)))])
         sorted-i)))

(defn get-feature-weights-sorted [maxent]
  (map #(get-topic-feature-weights-sorted maxent %) 
       (range (.size (.getLabelAlphabet maxent)))))

(defn print-features-topic-rows [maxent outstream]
  (map (fn [s] (.println outstream 
                 (seq2str (map (fn [[fname wt]] 
                                   (format " %s:%8.5f" fname wt)) 
                               s))))
       (get-feature-weights-sorted maxent)))

;(defn str-topic-names [dmr]
;  (format " %6s %s" fname (seq2str (map #(format " %8.5f" %) wts))));TODO

(defn str-features-sorted [maxent]
  (let [features     (.getAlphabet maxent)
        topics       (.getLabelAlphabet maxent)
        num-topics   (.size topics)
        num-features (+ 1 (.size features))]
    (map (fn [feature-i] 
           (let [[fname wts] (get-feature-weights maxent feature-i)]
              (format " %10s %s" fname (seq2str (map #(format " %8.5f" %) wts))))) 
         (conj (sort-indices features) (.size features)))))

(defn print-features-sorted [maxent pstream]
  (dorun (map (fn [feature-str] 
                  (.println pstream (str feature-str))) 
              (str-features-sorted maxent))))

(defn write-features-sorted [dmr file]
  (with-file-output-stream file
    (fn [fos] (print-features-sorted (.getRegressionParameters dmr) fos))))


;(write-features-sorted dmr "features.txt")


(defn dmr-instance-pipe [extra-stop-words]
  (make-instance-pipe
    (new cc.mallet.pipe.TargetStringToFeatures)
    (new cc.mallet.pipe.Input2CharSequence)
    (new cc.mallet.pipe.CharSequenceRemoveHTML) 
    (new cc.mallet.pipe.CharSequence2TokenSequence "(?:\\p{L}|\\p{N})+")
    (new cc.mallet.pipe.TokenSequenceLowercase)
    (.addStopWords 
      (new cc.mallet.pipe.TokenSequenceRemoveStopwords false false) 
      (into-array extra-stop-words))
    (new cc.mallet.pipe.TokenSequence2FeatureSequence)))



(def dmr-default-options {:iterations           1000
                          :threads              1 
                          :random-seed          90210
                          :optimize-interval    20
                          :burn-in              20
                          :show-topics-interval 100
                          :show-words-per-topic 20 })


(defmethod make-model-options DMRTopicModel [args]
  (let [options    (merge dmr-default-options args)
        rootname   (or (:rootname options) (basename (:corpus options)))
        outputdir  (format "%s-%d-iterations-%d-topics-%d-threads-dmr-jkan" 
                           rootname (:iterations options) (:topics options) (:threads options))]
    (merge options { :rootname rootname :outputdir outputdir })))


(defn make-dmr [& args]
  (let [{:keys [topics outputdir iterations threads random-seed optimize-interval burn-in show-topics-interval show-words-per-topic iteration-trace-fn] 
         :or   {iteration-trace-fn (fn [model iteration] 
                                     (when (zero? (rem iteration show-topics-interval))
                                      	(println "iteration " iteration (.modelLogLikelihood model)))) }
         :as   options} (make-model-options (assoc (apply hash-map args) 
                                                   :class DMRTopicModel))
        output-file (fn [filename]
                      (let [dir (new File outputdir)]
                        (.mkdirs dir)
                        (new File dir filename)))
        
        dmr (proxy [DMRTopicModel] [topics]
              (traceEndIteration [iteration]
                                 (iteration-trace-fn this iteration)
                                 (proxy-super traceEndIteration iteration)))]
    (doto dmr
      (.setRandomSeed       random-seed)
      ;(.setProgressLogFile  (output-file "progress.txt"))
      (.setTopicDisplay     show-topics-interval show-words-per-topic)
      (.setNumIterations    iterations)
      (.setOptimizeInterval optimize-interval)
      (.setBurninPeriod     burn-in)
      (.setNumThreads       threads))
      ;(.setNumBatches       8)
      ;(.setInitialStep      0.001)
      ;(.setMetaStep         0.002)
    
    (merge options {:parameter-estimator dmr :output-file-fn output-file})))


(defmethod write-model-results DMRTopicModel [model]
  (let [output-file (:output-file-fn model)
        dmr         (:parameter-estimator model)]
    (doto dmr
      (.printTopWords        (output-file "topic-keys.txt") 20 false)
      (.printDocumentTopics  (output-file "doc-topics.txt"))
      (.printTypeTopicCounts (output-file "word-topic-counts.txt"))
      (.writeParameters      (output-file "parameters.txt"))
      (.printState           (output-file "state.gz")))
    (write-features-sorted dmr (output-file "feature-wts.txt"))
    (serialize-object dmr    (output-file "dmr-model.ser"))))


(defn load-dmr [& args] 
  (let [options (apply hash-map args)
        dmr     (DMRTopicModel/read (File. (:outputdir options) "dmr-model.ser"))]
    dmr))

(defn run-dmr []
  (run-model (make-dmr :corpus     "resources/dmr-full-by-overlapping-epoch.ser" 
                       :topics     16 
                       :iterations 2000 
                       :threads    1)))

(defn run-dmr-small []
  (run-model (make-dmr :corpus     "resources/dmr-full-by-overlapping-epoch.ser" 
                       :topics     8 
                       :iterations 1000 
                       :threads    1)))

(defn run-synth-dmr [corpus]
  (run-model (make-dmr :rootname "resources/synthetic-dmr-" 
                       :iterations 1000 
                       :topics 4 
                       :threads 1)
             corpus))


;;------- test ----------;;

(defn test-dmr [corpus] 
  (let [[training-corpus test-corpus] (partition-instance-list (or (:corpus corpus) corpus) 
                                                               [90 10] *randoms*)
        training-token-count          (count-tokens training-corpus)
        test-token-count              (count-tokens test-corpus)
        eval-corpus                   (fn [lda iteration]
                                        ;(when (zero? (rem iteration (.showTopicsInterval lda)))
                                          (let [evaluator  (get-evaluator lda)
                                                ll-train   (evaluate-left-to-right evaluator training-corpus :iterations 1000)
                                                ll-test    (evaluate-left-to-right evaluator test-corpus :iterations 1000)]
                                            (println (apply str (interpose ", " [iteration 
                                                                                 (/ (apply + ll-train) training-token-count)
                                                                                 (/ (apply + ll-test) test-token-count)])))))]
    (run-model (make-model :rootname "resources/dmr-synthetic" 
                           :topics 3
                           :iterations 1000 
                           :iteration-trace-fn eval-corpus)
               training-corpus)))


;;------- testing (repl) ----------;;

(comment
  ;(def synth-corpus (dmr-synth-corpus 5000))
  (def dmr (test-dmr malletfn.topicmodel/synth-corpus))
    )
