(ns malletfn.fileutil
  (:import (java.io File 
             ObjectOutputStream FileOutputStream BufferedOutputStream 
             PrintStream ObjectInputStream FileInputStream)))

(defn basename [filename]
  (first (.split filename "\\.")))

(defn serialize-object [obj file]
  (let [oos (new ObjectOutputStream
              (new BufferedOutputStream
                (new FileOutputStream file)))]
    (try (.writeObject oos obj)
      (finally (.close oos)))))

(defn deserialize-object [file]
  (let [ois (new ObjectInputStream
              (new FileInputStream file))]
    (try (.readObject ois)
      (finally (.close ois)))))

(defn with-file-output-stream [file fun]
  (let [fos (new PrintStream
;              (new BufferedOutputStream
                (new FileOutputStream file))]
    (try (fun fos)
      (finally (.close fos)))))


;;(basename "lda-model.ser")
;;(with-file-output-stream  "blah.blah.txt" (fn [os] (.println os "blah")))
