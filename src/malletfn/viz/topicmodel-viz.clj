 (ns malletfn.viz.topicmodel-viz
  (:use rosado.processing)
  (:use malletfn.topicmodel)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet))
  (:import (java.io File))
  (:import (cc.mallet.types FeatureSequence FeatureVector InstanceList Alphabet))
  (:import (cc.mallet.pipe.iterator FileIterator))
  (:import (edu.umass.cs.mallet.users.kan.topics ParallelTopicModel)))

;; (def lda (load-lda "rhinoplastfm.ser" 1000 16))
    
;(def input-file "resources/rhinoplastfm.ser")
;(def num-iterations 1000)
;(def num-topics 16)

(def input-file "resources/docs.ser")
(def num-iterations 1000)
(def num-topics 8)

(defn load-lda [] ;;[input-file num-iterations num-topics]
  (let [[basename alpha beta outputdir] (lda-params input-file num-iterations num-topics)
        lda (ParallelTopicModel/read (File. outputdir "lda-model.ser"))]
    
    lda))

(def lda (load-lda))
(def topic-assignments (.getData lda))
(def assign1 (first topic-assignments))

;;(.size topic-assignments)
;;(doc-word-alphabet assign1)
;;(doc-word-features assign1)
;;(doc-word-sequence assign1)
;;(seq (.getAlphabet (doc-topic-sequence assign1)))
;;(seq (doc-topic-features assign1))
;;(doc-content-str assign1)
;;(reduce + 0 (doc-c-topics assign1))
;;(doc-p-topics assign1)

(def twa1 (seq (doc-topic-features assign1)))


(defn frequencies 
  "Returns a map from distinct items in coll to the number of times 
  they appear." 
  [coll] 
  (reduce (fn [counts x] 
              (assoc counts x (inc (get counts x 0)))) 
          {} coll)) 

(def inc-vec    (fn [v i] (assoc v i (inc (nth v i)))))
(defn vec-frequencies 
  [coll num-buckets]
  (reduce (fn [v t] 
            (inc-vec v t)) (vec (int-array num-buckets 0)) coll))

;;(time (dotimes [i 10000] (frequencies twa1)))
;;(time (dotimes [i 10000] (vec-frequencies twa1 16)))



;;------- proce55ing ----------;;


(def drawn       (atom false))
(def start-index (atom 0))
(def page-size   (atom 0))

(def viz-const 
  { :y-line-height 18
    :x-col-width   100
    :x-name-width  300 })

;(def gill      (load-font "GillSans-14.vlw"))
;(def gill-bold (load-font "GillSans-Bold-20.vlw"))

;;(viz-const :y-line-height)
;;(:y-line-height viz-const)


(defn line-xy [w-pane h-pane w-col h-line]
  (for [x (range 0 w-pane w-col) y (range 0 h-pane h-line)]
     [x y]))

;; (line-xy 800 600 200 20)
;; (interleave [1 2 3] [4 5 6 7 8])

;;(for [ i twa1 ]
;;  (do (println "blah")
;;   i))

;;(zipmap [1 2 3] [4 5 6])
;;(map list [1 2 3] [4 5 6])

;;  (binding [*applet* viz-applet]
;;    (draw-doc-topics topic-assignments))

(defn text
  [msg x y]
  (.text *applet* msg (float x) (float y)))



(defn index-to-color [i] 
  (/ (* i 100) 
     num-topics))

(defn viz-topic-width [window-width]
  (/ (- window-width
        (viz-const :x-name-width))
     num-topics))

(defn viz-hue [num-docs]
  (/ (* @start-index 100) num-docs))
                           
(defn zip-with-index [s]
  (map list s (iterate inc 0)))


;;(zip-with-index '(5 4 3 2 1))
;;(binding [*applet* viz-applet] (draw-doc-topics topic-assignments))

(defn draw-doc-text [x y hue doc-name doc-content]
  ;;(println doc-name)
  (fill hue 50 75 80)
  (text doc-name x y) 
    ;;(- (width) 10) (viz-const :y-line-height))
  (fill hue 20 75 20)
  (text doc-content
        (+ x (viz-const :x-name-width)) y)) 
        ;;(- (width) (viz-const :x-name-width)) (viz-const :y-line-height))

        
(defn draw-one-topic [x-topic-width y p i]
  ;;(println p i)
  (stroke (index-to-color i) 75 75 20)
  (fill   (index-to-color i) 75 75 80)
  (text   (str p) 
          (* i x-topic-width) (+ y (viz-const :y-line-height) -2))
  (rect   (* i x-topic-width) y 
          (* p x-topic-width) (- (viz-const :y-line-height) 1))
  p)


(defn draw-doc-topics [x y x-topic-width p-topics]
  ;(println "draw-doc-topics" p-topics)
  (with-translation [(+ x (viz-const :x-name-width)) 2]
    (dorun  (map (partial draw-one-topic x-topic-width y)
                 p-topics 
                 (iterate inc 0)))))

(defn draw-one-doc [tai [x y] num-docs]  
  ;(println "draw-one" tai x y)
  (draw-doc-text x (+ y (viz-const :y-line-height)) (viz-hue num-docs) 
    (doc-name tai) 
    (doc-content-str tai))
  (draw-doc-topics x y (viz-topic-width (width)) (doc-p-topics tai)))


(defn draw-docs [self topic-assigns coords]
  "main draw method"
  (println "====> drawing " @start-index)
  (. self background 0 0 20 100)
  (let [num-docs  (.size topic-assigns)
        num-drawn (count (map draw-one-doc 
                              (drop @start-index topic-assigns) 
                              coords
                              (cycle (list num-docs))))]
   
    (println "<==== drew " num-drawn)))


(def viz-applet
	(proxy [PApplet] []
       (setup []
              (binding [*applet* this]
                (let [margin 400]
                  (size 
                    (- (.screenWidth this) margin) 
                    (- (.screenHeight this) margin))
                  (smooth)
                  (no-stroke)
                  (color-mode HSB 100)
                  (text-font (load-font "resources/GillSans-14.vlw") 14)
                  (fill-float 50 100 80 80)
                  (framerate 10)
                  (swap! drawn false))))
       (draw []
             (if (not @drawn) 
                 (binding [*applet* this]
                   (let [num-drawn (draw-docs this 
                                              topic-assignments 
                                              (line-xy (- (width) 10) (- (height) 20) 
                                                       (- (width) 10) (viz-const :y-line-height)))]
                     (swap! page-size num-drawn)
                     (swap! drawn true)))))
       (keyPressed [evt]
             (binding [*applet* this]
               (swap! start-index (+ @start-index @page-size))))
       (mouseClicked [evt]
             (binding [*applet* this]
               (swap! drawn false)))))


(. viz-applet init)

(def viz-frame (JFrame. input-file))
(doto viz-frame
	(.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
	(.add viz-applet)
	(.pack)
	(.show))

;(do (. viz-applet stop) (. viz-frame dispose) (reset! drawn false))