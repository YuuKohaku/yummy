(ns yummy.restructure    
  (:use yummy.pathfinder)
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

;; restructure is a tool that alters specified tags names and attrs and does not 
;; change the given tree structure. Rules for specifying tags to replace are the
;; same that pathfinder uses.
;; usage for this tool is set-tag [path yummy-tree replacement]. All tags matching for 
;; path will get name and attrs specified in the replacement tag

(declare iterative-replace)

(defn- restruct [exp wp new-tag]
  {:pre [(yummy-object? exp)]}
;;  (println "restruct " exp wp)
(let [el (first wp),
      nxt (rest wp),
      comps (str/split el #"@"),
      elem (first comps),
      attrs (get-attrs comps)]
  (cond
      (= elem ".") (if (map-cmp (exp :attrs) attrs) 
                     (if (empty? nxt) 
                       (assoc new-tag :content (exp :content))
                       (iterative-replace wp new-tag exp))
                     exp)
      
      (= elem "*") (if (map-cmp (exp :attrs) attrs)
                        (if (empty? nxt)
                          (assoc new-tag :content 
                                 (reduce #(into %1 [%2])
                                         []
                                         (map #(if (yummy-object? %)
                                                 (restruct % wp new-tag)
                                                 %)
                                              (exp :content))
                                         ))
                          (iterative-replace wp new-tag (assoc exp :content
                            (reduce #(into %1 [%2])
                                       []
                                       (map #(if (yummy-object? %)
                                               (restruct % wp new-tag)
                                               %)
                                            (exp :content))
                                       ))))
                        (assoc exp :content
                            (reduce #(into %1 [%2])
                                       []
                                       (map #(if (yummy-object? %)
                                               (restruct % wp new-tag)
                                               %)
                                            (exp :content))
                                       )))
      :else (if (and 
                  (= (keyword elem) (exp :tag)) 
                  (map-cmp (exp :attrs) attrs))
              (if (empty? nxt) 
                (assoc new-tag :content  
                       (reduce #(into %1 [%2]) [] (map #(if (yummy-object? %) 
                                                          (restruct % wp new-tag) 
                                                          %) 
                                                       (exp :content))
                        ))
                (iterative-replace wp new-tag exp))
              (assoc exp :content
                     (reduce #(into %1 [%2]) [] (map #(if (yummy-object? %) 
                                                   (restruct % wp new-tag) 
                                                 %) 
                                                (exp :content))
                        ))  
              ))
  ))

(defn- iterative-replace [waypoints new-tag exp]   
  {:pre [(yummy-object? exp)]}
;;  (println exp waypoints)
  (let [comps (str/split (first waypoints) #"@"), 
        cur (first comps),  
        nxt (rest waypoints),
        attrs (get-attrs comps)]
;;    (println "replace " comps cur nxt attrs)
;;    (println exp)
;;    (println "-----------")
    (cond
        (= cur ".") (if (map-cmp (exp :attrs) attrs)
                      (if (empty? nxt)
                        (assoc new-tag :content (exp :content))
                        (assoc exp :content 
                               (reduce #(into %1 [(if (yummy-object? %2)
                                                    (iterative-replace nxt new-tag %2)
                                                      %2)]) 
                                         [] 
                                         (exp :content))
                           ))
                      exp
                      )
   (and
     (= cur "*")
     (empty? attrs))  (if (empty? nxt)
                        (assoc new-tag :content (reduce 
                                                       #(into %1 [(if (yummy-object? %2)
                                                                 (restruct %2 waypoints new-tag)
                                                                 %2)]) 
                                                       [] 
                                                       (exp :content)))
                        (merge exp (restruct exp nxt new-tag)))
   (and
     (= cur "*")
     (not-empty attrs))  (if (map-cmp (exp :attrs) attrs)
                           (if (empty? nxt)
                             (assoc new-tag :content (reduce 
                                                       #(into %1 [(if (yummy-object? %2)
                                                                    (restruct %2 waypoints new-tag)
                                                                    %2)]) 
                                                       [] 
                                                       (exp :content)))
                             (assoc exp :content 
                                    (reduce #(into %1 [(if (yummy-object? %2)
                                                         (iterative-replace nxt new-tag %2)
                                                         %2)]) 
                                            [] 
                                            (exp :content)))
                             )
                           (merge exp (restruct exp waypoints new-tag)))
   (and 
     (= (keyword cur) (exp :tag))
     (map-cmp (exp :attrs) attrs)) 
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
  (let [way (filter #(not (empty? %)) 
                    (reduce #(if (and 
                                 (= %2 "..") 
                                 (not-empty %1)) 
                             (pop %1) 
                             (conj %1 %2)) 
                          [] 
                          (str/split path #"/")))]
    (if (= "$" (first way))
      (iterative-replace (rest way) new-tag exp)
      (restruct exp way new-tag)))
  )
