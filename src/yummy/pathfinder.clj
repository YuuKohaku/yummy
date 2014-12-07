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
    (keywordize-keys 
      (apply hash-map (re-seq #"\w+" (second comps))))))

(defn retrieve [exp el]
  {:pre [(yummy-object? exp)]}
  (println "retrieve " exp el)
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
                     (if (empty? attrs)
                       true 
                       (map-cmp (exp :attrs) attrs)))
                 (list exp)
                 '())
               (map #(retrieve % el) (filter #(yummy-object? %) (exp :content)))
    ))))
  )


(defn iterative-search [waypoints exp]
  {:pre [(yummy-object? exp)]}
  (println exp waypoints)
  (let [comps (str/split (first waypoints) #"@"), 
        cur (first comps),  
        nxt (rest waypoints),
        attrs (get-attrs comps)]
    (println "search " comps cur nxt attrs)
 ;;   (println exp)
 ;;   (println "-----------")
    (cond
      (and 
        (= cur "*") 
        (empty? attrs)) (if (empty? nxt)
                          (list exp)
;;                          (filter #(yummy-object? %) (exp :content))
                          (map (partial iterative-search nxt)
                              (retrieve exp (first nxt))))
       (and 
         (= cur "*") 
         (map? attrs)) (if (empty? nxt)
                          (if (map-cmp (exp :attrs) attrs) (list exp) '())
                          (map (partial iterative-search nxt)
                              (retrieve exp (first waypoints))))
       
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
  (let [way (reduce #(if (and 
                           (= %2 "..") 
                           (not-empty %1)) 
                       (pop %1) 
                       (if (= %2 ".") %1 (conj %1 %2))) 
                    [] 
                    (str/split path #"/"))
        head (first way)
        waypoints (rest way)]
    (flatten
    (if (= "$" head)
      (iterative-search waypoints exp)
      (map (partial iterative-search way) 
           (filter yummy-object? (retrieve exp head)))))
  ))

(get-tag "c/./*@[k=5]" {:tag :a 
                        :attrs {:key "val" } 
                        :content [23 
                                  25 
                                  {:tag :c 
                                   :attrs {} 
                                   :content [65 
                                             {:tag :e 
                                              :attrs {:k "5"} 
                                              :content ["branch"   
                                                        {:tag :c 
                                                         :attrs {} 
                                                         :content [58]}]
                                              }] 
                                   } 
                                  {:tag :b 
                                   :attrs {} 
                                   :content [{:tag :c 
                                              :attrs {:key "val" :k "5"} 
                                              :content [2 3]} 
                                             {:tag :d 
                                              :attrs {:key "val"} 
                                              :content [2 
                                                        3 
                                                        {:tag :c 
                                                         :attrs {} 
                                                         :content [85 
                                                                   {:tag :t
                                                                    :attrs {}
                                                                    :content []}]}]}
                                             ]}
                                  ]})