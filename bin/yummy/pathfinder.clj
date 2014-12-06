(ns yummy.pathfinder
    (:use yummy.validation)
    (:require [clojure.string :as str]))

;;waypoints is (split #"/" path)
;;
(defn grep-elems [exp waypoints]
  {:pre [(yummy-tree? exp)]}
  (let [res '(), cur (first waypoints), nxt (rest waypoints)]
    (for [i (exp :content)
          :when (and
                  (not (yummy-atomic? i)) 
                  (or (= (keyword cur) (i :tag)) (= "*" cur))
                  )]
      (if (empty? nxt)
        (cons i res)
        (cons (grep-elems i nxt) res))
      )
    ))

(defn find-elems [exp elem]
  {:pre [yummy-tree? exp()]}
  (let [res [], name (keyword elem)]
    (concat
      (if (= (exp :tag) name) [exp] [])
      ()
      )
    )
  )

(grep-elems {:tag :a :attrs {:key "val"} :content [23 25 {:tag :b :attrs {} :content []} {:tag :b :attrs {} :content [{:tag :c :attrs {:key "val"} :content [2 3]} {:tag :d :attrs {:key "val"} :content [2 3 {:tag :c :attrs {} :content []}]}]}]} (str/split "*/c" #"/"))