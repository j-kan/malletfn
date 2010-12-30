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
        opt         (new edu.umass.cs.mallet.users.kan.topics.StochasticMetaOptimizer dmro num-batches random)]
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
  (malletfn.dmrtopics/beta-date-features bach)
  )
