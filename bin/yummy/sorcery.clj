(ns yummy.sorcery
  (:use yummy.pathfinder)
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

;; sorcery is a tool that alters specified tags content does not 
;; affect tagnames and attrs. Rules for specifying tags to replace are the
;; same that pathfinder uses.
;; usage for this tool is set-content :how [path yummy-tree new-content]. 

(defmulti fill-in (fn [how v val pos] how))

(defmethod fill-in :append [how v val pos]
  {:pre [vector? v]}
  (conj v val))

(defmethod fill-in :prepend [how v val pos]
  {:pre [vector? v]}
  (into [val] v))

(defmethod fill-in :replace [how v val pos]
  {:pre [(and (vector? v) (number? pos) (<= pos (count v)) (pos? pos))]}
  (assoc v pos val))

(defmethod fill-in :insert [how v val pos]
  {:pre [(and (vector? v) (number? pos) (<= pos (count v)) (pos? pos))]}
  (let [c (split-at pos v)]
    (vec (concat (first c) (list val) (second c)) )))

(declare iterative-refill)

(defn- refill [how pos exp wp new-content]
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
                     (assoc exp :content 
                            (fill-in how (exp :content) new-content pos))
                     (iterative-refill how pos wp new-content exp))
                   exp)
    
    (= elem "*") (if (map-cmp (exp :attrs) attrs)
                   (if (empty? nxt)
                     (assoc exp :content 
                            (fill-in how  
                                    (reduce #(into %1 [%2])
                                            []
                                            (map #(if (yummy-object? %)
                                                    (refill how pos % wp new-content)
                                                    %)
                                                 (exp :content))
                                            )
                                    new-content pos)
                            )
                     (iterative-refill how pos wp new-content 
                                       (assoc exp :content
                                              (reduce #(into %1 [%2])
                                                      []
                                                      (map #(if (yummy-object? %)
                                                              (refill how pos % wp new-content)
                                                              %)
                                                           (exp :content))
                                                      ))
                                       ))
                   (assoc exp :content
                          (reduce #(into %1 [%2])
                                  []
                                  (map #(if (yummy-object? %)
                                          (refill how pos % wp new-content)
                                          %)
                                       (exp :content))
                                  )))
    :else (if (and 
                (= (keyword elem) (exp :tag)) 
                (map-cmp (exp :attrs) attrs))
            (if (empty? nxt) 
              (assoc exp :content  
                     (fill-in how (reduce #(into %1 [%2]) 
                                          [] 
                                          (map #(if (yummy-object? %) 
                                                  (refill how pos % wp new-content) 
                                                  %) 
                                               (exp :content)))
                              new-content pos
                              ))
              (iterative-refill how pos wp new-content exp))
            (assoc exp :content
                   (reduce #(into %1 [%2]) 
                           [] 
                           (map #(if (yummy-object? %) 
                                   (refill how pos % wp new-content) 
                                   %) 
                                (exp :content))
                           ))  
            ))
  ))

(defn- iterative-refill [how pos waypoints new-content exp]   
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
                      (assoc exp :content 
                             (fill-in how (exp :content) new-content pos))
                      (assoc exp :content 
                             (reduce #(into %1 [(if (yummy-object? %2)
                                                  (iterative-refill how pos 
                                                                    nxt 
                                                                    new-content 
                                                                    %2)
                                                  %2)]) 
                                     [] 
                                     (exp :content))
                             ))
                    exp
                    )
      (and
        (= cur "*")
        (empty? attrs))  (if (empty? nxt)
                           (assoc exp :content 
                                  (fill-in how 
                                           (reduce 
                                             #(into %1 [(if (yummy-object? %2)
                                                          (refill how pos %2 waypoints new-content)
                                                          %2)]) 
                                             [] 
                                             (exp :content))) 
                                  new-content pos)
                           (merge exp (refill how pos exp nxt new-content)))
      (and
        (= cur "*")
        (not-empty attrs)) 
      (if (map-cmp (exp :attrs) attrs)
        (if (empty? nxt)
          (assoc exp :content 
                 (fill-in how
                          (reduce
                            #(into %1 [(if (yummy-object? %2)
                                         (refill how pos %2 waypoints new-content)
                                         %2)]) 
                            []
                            (exp :content)))
                 new-content pos)
          (assoc exp :content
                 (reduce #(into %1 [(if (yummy-object? %2)
                                      (iterative-refill how pos nxt
                                                        new-content
                                                        %2)
                                      %2)])
                         [] 
                         (exp :content)))
          )
        (merge exp (refill how pos exp waypoints new-content)))
      (and 
        (= (keyword cur) (exp :tag))
        (map-cmp (exp :attrs) attrs)) 
      (if (empty? nxt)
        (assoc exp :content (fill-in how (exp :content) new-content pos))
        (assoc exp :content 
               (reduce #(into %1 [(if (yummy-object? %2)
                                    (iterative-refill how pos nxt new-content %2)
                                    %2)]) 
                       [] 
                       (exp :content))))
      :else exp
      )
    )
  )


(defn set-content [how pos path exp new-content] 
  (let [way (filter #(not (empty? %)) 
                    (reduce #(if (and 
                                   (= %2 "..") 
                                   (not-empty %1)) 
                               (pop %1) 
                               (conj %1 %2)) 
                            [] 
                            (str/split path #"/")))]
    (if (= "$" (first way))
      (iterative-refill how pos (rest way) new-content exp)
      (refill how pos exp way new-content)))
  )
