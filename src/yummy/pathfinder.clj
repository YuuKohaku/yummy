(ns yummy.pathfinder
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

(defn map-cmp [search attrs]
  {:pre [(map? search), (map? attrs)]}
  (println "------comparing" search attrs)
  (let [df (diff search attrs)]
      (nil? (second df))))

(defn get-attrs [comps]
  (if (empty? (rest comps))
    {} 
    (keywordize-keys (apply hash-map (re-seq #"\w+" (second comps))))))

(defn retrieve [exp el]
  {:pre [(yummy-object? exp)]}
  (println exp el)
(let [comps (str/split el #"@"),
      elem (first comps),
      attrs (get-attrs comps)]
  (cond
    (and 
      (= elem "*")
      (empty? attrs)) (filter 
                        #(yummy-object? %) 
                        (exp :content))
    (and 
      (= elem "*")
      (map? attrs)) (flatten   
                      (concat
                        (filter #(and
                                   (yummy-object? %)
                                   (map-cmp (% :attrs) attrs))
                                (exp :content))
                        (map #(retrieve % elem) (filter #(yummy-object? %) (exp :content)))
    ))
    
    (= elem "..") false
    :else    (flatten   
             (concat
               (filter #(and
                          (yummy-object? %)
                          (= (keyword elem) (% :tag)) 
                          (if (empty? attrs) 
                            true 
                            (map-cmp (% :attrs) attrs)))
            (exp :content))
    (map #(retrieve % elem) (filter #(yummy-object? %) (exp :content)))
    ))))
  )


(defn iterative-search [waypoints exp]
  {:pre [(yummy-object? exp)]}
  (let [comps (str/split (first waypoints) #"@"), 
        cur (first comps),  
        nxt (rest waypoints),
        attrs (get-attrs comps)]
    (println comps cur nxt attrs)
    (println exp)
    (println "-----------")
    (cond
      (and 
        (= cur "*") 
        (empty? attrs)) (if (empty? nxt)
                          (filter #(yummy-object? %) (exp :content))
                          (map (partial iterative-search nxt)
                              (retrieve exp (first nxt))))
       (and 
         (= cur "*") 
         (map? attrs)) (if (empty? nxt)
                          (filter #(and 
                                     (yummy-object? %) 
                                     (map-cmp (% :attrs) attrs)) 
                                  (exp :content))
                          (map (partial iterative-search nxt)
                              (retrieve exp (first waypoints))))
       
      (= cur "..") false
      (and 
        (= (keyword cur) (exp :tag))
        (if (empty? attrs) 
          true 
          (map-cmp (exp :attrs) attrs))) (if (empty? nxt)
                                        (list exp)
                                        (map 
                                          (partial iterative-search nxt) 
                                          (filter #(yummy-object? %) 
                                                  (exp :content))
                                          ))
      :else '()
      )
    ))

(defn get-tag [path exp] 
  (let [waypoints (str/split path #"/")]
  (flatten (iterative-search waypoints exp))
  ))

(get-tag "*" {:tag :a :attrs {:key "val" } :content [23 25 {:tag :c :attrs {} :content [65]} {:tag :b :attrs {} :content [{:tag :c :attrs {:key "val" :k "5"} :content [2 3]} {:tag :d :attrs {:key "val"} :content [2 3 {:tag :c :attrs {} :content [85]}]}]}]})