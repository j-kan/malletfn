(ns mongodb-collection-iterator
  (:import (java.io File))
  (:import (cc.mallet.types Instance))
  (:import (cc.mallet.pipe.iterator FileIterator))
  (:import (com.mongodb DBCollection DBCursor DBObject Mongo MongoException))
  (:import (import com.mongodb.util JSON)))

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
                  (text-font (load-font "GillSans-14.vlw") 14)
                  (fill-float 50 100 80 80)
                  (framerate 10)
                  (reset! drawn false))))
       (draw []
             (if (not @drawn) 
                 (binding [*applet* this]
                   (draw-docs this 
                              topic-assignments 
                              (line-xy (- (width) 10) (- (height) 20) 
                                       (- (width) 10) (viz-const :y-line-height)))
                   (reset! drawn true))))
       (mouseClicked [evt]
             (binding [*applet* this]
               (reset! drawn false)))))

(defn mongodb-collection-iterator [db-name collection-name query fields block]
  )
