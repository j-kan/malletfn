(ns malletfn.testdmrtopics
  (:use clojure.test)
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [clojure.contrib.io        :only (pwd)])
  (:use [clojure.java.io           :only (writer)])
  (:require malletfn.dmrtopics)
  (:require malletfn.synth)
  (:import (java.util.logging Logger Level))
  (:import (edu.umass.cs.mallet.users.kan.topics DMRTopicModel))
  (:import (cc.mallet.util MalletLogger))
  (:import (cc.mallet.optimize Optimizable$ByBatchGradient)))

(defn count-features [features alphabet]
    (let [counter (new cc.mallet.types.FeatureCounter alphabet)]
      (doseq [label (iterator-seq (.iterator features))] 
        (.increment counter (.toString label)))
      (.toFeatureVector counter)))

(defn get-token-alphabet [instance]
  (-> instance .getData .getAlphabet))

(defn get-feature-alphabet [instance]
  (-> instance .getTarget .getAlphabet))

(defn get-topic-alphabet [num-topics]
  (let [alphabet (new cc.mallet.types.LabelAlphabet)]
    (doseq [i (range num-topics)] 
      (.lookupIndex alphabet (str "topic" i)))
    alphabet))



(defn dispatch [head & tail] 
  (let [typ (or (:type head) (:class head) (class head))]
    ;(print typ)
    typ))

(defmulti target-feature-vector dispatch)

(defmethod target-feature-vector edu.umass.cs.mallet.users.kan.topics.TopicAssignment
  [ta] (.getTarget (.instance ta)))
  
(defmethod target-feature-vector :topic-assignment
  [ta] (.getTarget (:instance ta)))
  
(defmulti topic-sequence dispatch)

(defmethod topic-sequence edu.umass.cs.mallet.users.kan.topics.TopicAssignment
  [ta] (iterator-seq (.iterator (.topicSequence ta))))
  
(defmethod topic-sequence :topic-assignment
  [ta] (map #(str "topic" %) (:topic-assignment ta)))

(defn- make-dmr-optimizable-aux
  [topic-assignments feature-alphabet topic-alphabet num-batches]  ; regular-prior-variance intercept-prior-variance
  (let [num-features      (inc (.size feature-alphabet))
        num-topics        (.size topic-alphabet)
        param-pipe        (doto (new cc.mallet.pipe.Noop)
                            (.setDataAlphabet feature-alphabet)
                            (.setTargetAlphabet topic-alphabet))
        param-instances   (new cc.mallet.types.InstanceList param-pipe) 
        maxent-classifier (new cc.mallet.classify.MaxEnt 
                            param-pipe 
                            (double-array (* num-features num-topics)))]
    
    (doseq [[i ta] (indexed topic-assignments)]
      (let [target (target-feature-vector ta)]
        (when target
          (.add param-instances (new cc.mallet.types.Instance 
                                  target 
                                  (count-features 
                                    (topic-sequence ta)
                                    topic-alphabet) 
                                  i 
                                  i)))))
    
    (doto (new edu.umass.cs.mallet.users.kan.topics.DMROptimizable 
            param-instances num-batches maxent-classifier)
      (.setRegularGaussianPriorVariance 0.5)
      (.setInterceptGaussianPriorVariance 100.0))))


(defmulti make-dmr-optimizable 
  "construct a DmrOptimizable object given various parameters"
  dispatch)

(defmethod make-dmr-optimizable edu.umass.cs.mallet.users.kan.topics.DMRTopicModel
  [dmr num-batches]
    (let [dmrtopicmodel     (:inferencer dmr)
          topic-assignments (.getData dmrtopicmodel)
          feature-alphabet  (.getTargetAlphabet (.instance (first topic-assignments)))
          topic-alphabet    (.getTopicAlphabet (:inferencer dmr))]
      (make-dmr-optimizable-aux topic-assignments feature-alphabet topic-alphabet num-batches)))
  
(defmethod make-dmr-optimizable :dmr-synth-corpus
  [{corpus            :corpus
    topic-assignments :topic-assignments
    num-topics        :num-topics} 
   num-batches]
  (let [feature-alphabet  (get-feature-alphabet (first corpus))
        topic-alphabet    (get-topic-alphabet num-topics)]
    (make-dmr-optimizable-aux topic-assignments feature-alphabet topic-alphabet num-batches)))
  
(defn print-features [classifier]
  (doseq [strfeats (malletfn.dmrtopics/str-features-sorted classifier)]
    (println strfeats)))


;		public void getBatchValueGradient (double[] buffer, int batchIndex, int[] batchAssignments);
;		public double getBatchValue(int batchIndex, int[] batchAssignments);

(defn wrap-dmr-optimizable [dmro]
  
  (let [instances    (.getInstanceList dmro)
        num-features (inc (-> instances .getDataAlphabet .size))
        num-labels   (-> instances .getTargetAlphabet .size)]
    
    (reify cc.mallet.optimize.Optimizable$ByBatchGradient
    
      (^double getBatchValue [this ^int batchIndex ^ints batchAssignments]
        (print-features (.getClassifier dmro))
        (let [value (.getBatchValue dmro batchIndex batchAssignments)]
          (println "getBatchValue " batchIndex ":" value ":" (map str batchAssignments))
          value))
    
      (^void getBatchValueGradient [this ^doubles buffer ^int batchIndex ^ints batchAssignments]
        (print-features (.getClassifier dmro))
        (.getBatchValueGradient dmro buffer batchIndex batchAssignments)
        (println "getBatchValueGradient " batchIndex ":")
        (dorun (map 
                 (fn [sq] (println (apply str  (map #(format " %8.5f" %) sq))))
                 (partition num-features buffer))))
    
      (^int getNumParameters [this]
        (.getNumParameters dmro))
    
      (^void getParameters [this ^doubles buffer]
        (.getParameters dmro buffer))
    
      (^double getParameter [this ^int index]
        (.getParameter dmro index))
    
      (^void setParameters [this ^doubles params]
        (.setParameters dmro params))
    
      (^void setParameter [this ^int index ^double value]
        (.setParameter dmro index value)))))
      
      
  



(defn do-optimize [[dmr-optimizable optimizer]]
  (comment (doto dmr-optimizable
             (.setRegularGaussianPriorVariance dmr-optimizable 0.5)
             (.setInterceptGaussianPriorVariance dmr-optimizable 100.0)))
  (.optimize optimizer)
  (println )
  (print-features (.getClassifier dmr-optimizable))
  dmr-optimizable)
        
(defn make-lbfgs [dmr]
  (let [dmro (make-dmr-optimizable dmr 1)]
    [dmro (new cc.mallet.optimize.LimitedMemoryBFGS dmro)]))

(defn make-sma [dmr]
  (let [random      (new java.util.Random)
        num-batches 5
        dmro        (make-dmr-optimizable dmr num-batches)
        opt         (new edu.umass.cs.mallet.users.kan.topics.StochasticMetaOptimizer 
                      (wrap-dmr-optimizable dmro) 
                      num-batches 
                      (-> dmro .getInstanceList .size)
                      random)]
    [(doto dmro
       (.setRegularGaussianPriorVariance 0.5)
       (.setInterceptGaussianPriorVariance 100.0))
     (doto opt
       (comment
         (.setInitialStep 0.002)
         (.setMu 0.005)))]))
  

(defn test-sma [corpus]
  (do-optimize (make-sma corpus)))








(comment
  (refer 'clojure.contrib.io :only ['pwd])
  (refer 'clojure.java.io :only ['writer])

  (.setLevel (Logger/getLogger "cc.mallet.optimize.StochasticMetaAscent") Level/WARNING)
  (.setLevel (Logger/getLogger "edu.umass.cs.mallet.users.kan.topics.DMROptimizable") Level/WARNING)
  

  (in-ns 'malletfn.testdmrtopics)
  
  (def dmr-synth-corpus (malletfn.synth/make-dmr-synth-corpus))
  (def sma (make-sma dmr-synth-corpus))
  
  (fun-with-output-log "resources/optimize.log" 
    #(do-optimize sma))
  
  (with-output-log "resources/optimize.log" 
    (do-optimize sma))

  (print-features (.getClassifier (first sma)))

  (w-output-log "doblah.log" 
    (println "blah"))
  
  (macroexpand  
    '(with-output-log "doblah.log" 
       (println "blah")))
  
  (defn fun-with-output-log [logfile fun]
    (with-open [os (writer logfile)]
      (let [save-o *out*
            save-e *err*]
        (set! *out* os)
        (set! *err* os)
        (try (fun)
          (finally
            (set! *out* save-o)
            (set! *err* save-e))))))

  
  (defmacro with-output-log 
    [logfile & body]
    `(let [~'save-o *out*
           ~'save-e *err*
           ~'os (writer ~logfile)]
       (try
         (set! *out* ~'os)
         (set! *err* ~'os)
         (println "###" (str (new java.util.Date)) '~@body)
         (do ~@body)
         (finally
           (println "###" (str (new java.util.Date)) '~@body)
           (.close ~'os)
           (set! *out* ~'save-o)
           (set! *err* ~'save-e)))))


  (defmacro w-output-log 
    [logfile & body]
    `(fun-with-output-log ~logfile
       (fn []
         (println "###" (str (new java.util.Date)) '~@body)
         (do ~@body)
         (println "###" (str (new java.util.Date)) '~@body))))
  

  (def dmro (test-sma dmr-synth-corpus))

  (def num-topics (count malletfn.synth/topics))
  (def corpus-with-topics (malletfn.synth/corpus-instance-list-with-features))
  (def corpus (first corpus-with-topics))
  (def corpus-topic-assignments (second corpus-with-topics))

	;(def corpus (malletfn.synth/corpus-instance-list-with-features))
	(def dmr    (time (malletfn.dmrtopics/run-synth-dmr corpus)))
 
  (print-features (.getRegressionParameters (:inferencer dmr)))
  
  (def lbfgs-pair (make-lbfgs dmr-synth-corpus))
  (def lbfgs-dmro (first lbfgs-pair))
  (def lbfgs      (second lbfgs-pair))
  
  (def dmr-lbfgs-pair (make-lbfgs dmr))
  (.optimize lbfgs)

  (do-optimize dmr-lbfgs-pair)
  (do-optimize lbfgs-pair)
  
  (def sma-pair (make-lbfgs dmr-synth-corpus))
  (do-optimize sma-pair)
  
  (time (do-optimize (make-lbfgs dmr-synth-corpus)))
  (time (do-optimize (make-sma dmr-synth-corpus)))
 
  (time (do-optimize (make-lbfgs dmr)))
  (time (do-optimize (make-sma dmr)))

  (print-features (.getClassifier (first sma-pair)))
  
  ;(import 'edu.umass.cs.mallet.users.kan.topics.StochasticOptimizer)
 
  (def dmr-random (new java.util.Random))
  (.nextInt dmr-random)
  
 
  (def instances (.getInstanceList dmro-sma))
  
  (first instances)
  
  (.getData (first instances))
  (.getTarget (first instances))
  (.getName (first instances))
  (.getSource (first instances))
  
  (malletfn.dmrtopics/dmr-instance-list "{'content' : /\\bexperimental\\b/}")

  (time (def dmr-small (malletfn.dmrtopics/run-dmr-small)))

  
  (time (def lda (malletfn.topicmodel/run-lda)))
  (time (def dmr (malletfn.dmrtopics/run-dmr)))
  
  (malletfn.corpusutil/seq2str (malletfn.dmrtopics/beta-date-features 1999))
 
  (def bach "Johann Sebastian Bach (31 March [O.S. 21 March] 1685, Eisenach Ð 28 July 1750, Leipzig) was better known as a virtuoso organist than as a composer in his day. His sacred music, organ and choral works, and other instrumental music had an enthusiasm and seeming freedom that concealed immense rigor. Bach's use of counterpoint was brilliant and innovative, and the immense complexities of his compositional style -- which often included religious and numerological symbols that seem to fit perfectly together in a profound puzzle of special codes -- still amaze musicians today. Many consider him the greatest composer of all time.\n \n Bach was born in Eisenach in 1685. He was taught to play the violin and harpsichord by his father, Johann Ambrosius, a court trumpeter in the service of the Duke of Eisenach. Young Johann was not yet ten when his father died, leaving him orphaned. He was taken in by his recently married oldest brother, Johann Christoph, who lived in Ohrdruf. Because of his excellent singing voice, Bach attained a position at the Michaelis monastery at LŸneberg in 1700. His voice changed a short while later, but he stayed on as an instrumentalist. After taking a short-lived post in Weimar in 1703 as a violinist, Bach became organist at the Neue Kirche in Arnstadt (1703-1707). His relationship with the church council was tenuous as the young musician often shirked his responsibilities, preferring to practice the organ. One account describes a four-month leave granted Bach, to travel to Lubeck where he would familiarize himself with the music of Dietrich Buxtehude. He returned to Arnstadt long after was expected and much to the dismay of the council. He then briefly served at St. Blasius in MŸhlhausen as organist, beginning in June 1707, and married his cousin, Maria Barbara Bach, that fall. Bach composed his famous Toccata and Fugue in D minor (BWV 565) and his first cantatas while in MŸhlhausen, but quickly outgrew the musical resources of the town. He next took a post for the Duke of Sachsen-Weimar in 1708, serving as court organist and playing in the orchestra, eventually becoming its leader in 1714. He wrote many organ compositions during this period, including his Orgel-BŸchlein. Owing to politics between the Duke and his officials, Bach left Weimar and secured a post in December 1717 as Kapellmeister at Cšthen. In 1720, Bach's wife suddenly died, leaving him with four children (three others had died in infancy). A short while later, he met his second wife, soprano Anna Magdalena Wilcke, whom he married in December 1721. She would bear 13 children, though only five would survive childhood. The six Brandenburg Concertos (BWV 1046-51), among many other secular works, date from his Cšthen years. Bach became Kantor of the Thomas School in Leipzig in May 1723 and held the post until his death. It was in Leipzig that he composed the bulk of his religious and secular cantatas. Bach eventually became dissatisfied with this post, not only because of its meager financial rewards, but also because of onerous duties and inadequate facilities. Thus, he took on other projects, chief among which was the directorship of the city's Collegium Musicum, an ensemble of professional and amateur musicians who gave weekly concerts, in 1729. He also became music director at the Dresden Court in 1736, in the service of Frederick Augustus II; though his duties were vague and apparently few, they allowed him freedom to compose what he wanted. Bach began making trips to Berlin in the 1740s, not least because his son Carl Philipp Emanuel served as a court musician there. In May 1747, the composer was warmly received by King Frederick II of Prussia, for whom he wrote the gloriously abstruse Musical Offering (BWV 1079). Among Bach's last works was his 1749 Mass in B minor. Besieged by diabetes, he died on July 28, 1750.\n \n \n ")
  
  (malletfn.dmrtopics/date-features bach)
  (malletfn.dmrtopics/beta-date-features bach))
