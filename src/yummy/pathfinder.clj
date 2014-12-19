(ns yummy.pathfinder
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

(defn map-cmp [search attrs]
  {:pre [(map? search), (map? attrs)]}
  (let [df (diff search attrs)]
      (nil? (second df))))

(defn get-attrs [comps]
  (if (empty? (rest comps))
    {} 
    (keywordize-keys 
      (apply hash-map (re-seq #"\w+" (second comps))))))

(defn- retrieve [exp el]
  {:pre [(yummy-object? exp)]}
(let [comps (str/split el #"@"),
      elem (first comps),
      attrs (get-attrs comps)]
  (cond
    (and 
      (= elem "*")
      (empty? attrs)) 
    (filter 
      #(yummy-object? %) 
      (exp :content))
    (and 
      (= elem "*")
      (map? attrs)) (flatten   
                      (concat
                        (if (map-cmp (exp :attrs) attrs) (list exp) '())
                        (map #(retrieve % el) 
                             (filter #(yummy-object? %) 
                                     (exp :content)))
    ))
    
    :else    (flatten   
             (concat
               (if (and 
                     (= (keyword elem) (exp :tag)) 
                     (map-cmp (exp :attrs) attrs))
                 (list exp)
                 '())
               (map #(retrieve % el) (filter #(yummy-object? %) (exp :content)))
    ))))
  )


(defn- iterative-search [waypoints exp]
  {:pre [(yummy-object? exp)]}
  (let [comps (str/split (first waypoints) #"@"), 
        cur (first comps),  
        nxt (rest waypoints),
        attrs (get-attrs comps)]
    (cond
      (and 
        (= cur "*") 
        (empty? attrs)) (if (empty? nxt)
                          (list exp)
                          (map (partial iterative-search nxt)
                              (retrieve exp (first nxt))))
       (and 
         (= cur "*") 
         (map? attrs)) (if (empty? nxt)
                         (concat
                           (if (map-cmp (exp :attrs) attrs) (list exp) '())
                           (map #(retrieve % (first waypoints))
                                (filter #(yummy-object? %) 
                                (exp :content))))
                         (map (partial iterative-search nxt) 
                              (reduce #(concat %1 (filter yummy-object? (%2 :content))) 
                                      '() 
                                      (retrieve exp (first waypoints))))
                         )
       
       (and 
         (= (keyword cur) (exp :tag))
         (map-cmp (exp :attrs) attrs)) 
       (if (empty? nxt)
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
  (let [way (filter #(not (empty? %)) 
                    (reduce #(if (and 
                                 (= %2 "..") 
                                 (not-empty %1)) 
                             (pop %1) 
                             (if (= %2 ".") %1 (conj %1 %2))) 
                          [] 
                          (str/split path #"/")))
        head (first way)
        waypoints (rest way)]
    (if (empty? way)
    '()
    (flatten
    (if (= "$" head)
      (iterative-search waypoints exp)
      (map (partial iterative-search way) 
           (filter yummy-object? (retrieve exp head)))))
  ))
)
