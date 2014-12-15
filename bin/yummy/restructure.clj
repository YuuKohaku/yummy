(ns yummy.restructure    
  (:use yummy.pathfinder)
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

(declare iterative-replace)

(defn restruct [exp wp new-tag]
  {:pre [(yummy-object? exp)]}
;;  (println "restruct " exp wp)
(let [el (first wp),
      nxt (rest wp),
      comps (str/split el #"@"),
      elem (first comps),
      attrs (get-attrs comps)]
  (cond
    (and 
      (= elem "*")
      (empty? attrs)) 
      (merge exp new-tag)
    (and 
      (= elem "*")
      (map? attrs))
    (assoc 
      (if (map-cmp (exp :attrs) attrs) 
        (if (empty? nxt) 
          (assoc new-tag :content (exp :content))
          (iterative-replace wp new-tag exp))
        exp)
      :content
      (reduce #(into %1 [%2]) 
              [] 
              (map #(if (yummy-object? %) 
                      (restruct % wp new-tag) 
                      %) 
                  (exp :content))
              ))
    :else    
    (assoc 
      (if (and 
            (= (keyword elem) (exp :tag)) 
            (if (empty? attrs)
              true 
              (map-cmp (exp :attrs) attrs)))
        (if (empty? nxt) 
          (assoc new-tag :content (exp :content))
          (iterative-replace wp new-tag exp))
        exp)
      :content
      (reduce #(into %1 [%2]) [] (map #(if (yummy-object? %) 
                      (restruct % wp new-tag) 
                      %) 
                   (exp :content))
              ))  
    ))
  )

(defn iterative-replace [waypoints new-tag exp]   
  {:pre [(yummy-object? exp)]}
  (println exp waypoints)
  (let [comps (str/split (first waypoints) #"@"), 
        cur (first comps),  
        nxt (rest waypoints),
        attrs (get-attrs comps)]
 ;;   (println "replace " comps cur nxt attrs)
 ;;   (println exp)
    (println "-----------")
    (cond
      (and 
        (= cur "*") 
        (empty? attrs)) 
      (if (empty? nxt)
        (assoc new-tag :content (exp :content))
        (merge exp (restruct exp nxt new-tag)))
       (and 
         (= cur "*") 
         (map? attrs)) 
       (if (empty? nxt)
         (if (map-cmp (exp :attrs) attrs) 
           (assoc new-tag :content (exp :content))
           exp)
        (merge exp (restruct exp nxt new-tag)))
      (and 
        (= (keyword cur) (exp :tag))
        (if (empty? attrs) 
          true 
          (map-cmp (exp :attrs) attrs))) 
      (if (empty? nxt)
        (assoc new-tag :content (exp :content))
        (assoc exp :content 
               (reduce #(into %1 [(if (yummy-object? %2)
                                    (iterative-replace nxt new-tag %2)
                                    %2)]) 
                       [] 
                       (exp :content))))
      :else exp
      )
    )
  )


(defn set-tag [path exp new-tag] 
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
      (iterative-replace waypoints new-tag exp)
      (map (partial iterative-replace way new-tag) 
           (filter yummy-object? (restruct exp way new-tag)))))
  ))

(iterative-replace (str/split "a/b/c" #"/")
                   {:tag :cr :attrs {} :content ["replaced"]}
                   {:tag :a 
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
                                  ]}
                   )