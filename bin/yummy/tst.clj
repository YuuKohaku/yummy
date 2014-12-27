(ns yummy.tst
  (:use clojure.data)
  (:use clojure.xml))


(clojure.data/diff 
  (clojure.xml/parse "resources/NewFile.xml") 
  (clojure.xml/parse "resources/Copy of NewFile.xml"))