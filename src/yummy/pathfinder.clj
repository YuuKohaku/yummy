(ns yummy.pathfinder
    (:use yummy.validation)
    (:use clojure.data)
    (:use clojure.walk)
    (:require [clojure.string :as str]))

;; pathfinder provides a single function get-tag to perform search in given yummy tree.
;; get-tag [path-to-element yummy-tree]
;; path may contain some of listed elements: $ .. . @[<attrs list>] *
;; $ - tree root. Path "$/a/b" (absolute path) means that the pathfinder will start the search from the root tag.
;; In this case, pathfinder will return a list of matching tags (all "b" with parent root tag "a"). 
;; Relative path "a/b" or "/a/b" (these are the same case) will return a list of all
;; tags "b" with parent "a".
;; .. - path to parent tag. Path like "a/b/c/../d" cases pathfinder to search tag "a/b/d"
;; . - path to current tag. "a/b/./c" simply equal to "a/b/c"
;; tagname@[key=value] cases pathfinder to check tag attributes and return only name and attrs matching tags
;; * means any tag. Usage for this is not trivial.
;; "a/*" returns all child tags of a (distance = 0)
;; "a/*/c" returns all "c" with distance >=0 from "a"
;; "a/*/*/c" returns all "c" with distance == 1 from "a"
;; "a/*@[key=value]" returns all tags with attrs containing {:key "value"}
;; "a/*@[key=value]/c" returns all "c" that are children of any tags under "a" with attrs containing {:key "value"}

;; true if search attrs are present in given map
;; used in pathfinder and restructure
(defn map-cmp [search attrs]
  {:pre [(map? search), (map? attrs)]}
  (let [df (diff search attrs)]
      (nil? (second df))))

;; parse attrs from parts of tag specifier
;; used in pathfinder and restructure
(defn get-attrs [comps]
  (if (empty? (rest comps))
    {} 
    (keywordize-keys 
      (apply hash-map (re-seq #"\w+" (second comps))))))

;; following two functions differ by search algo and recursively search specified tags
;; Both should never be used outside the pathfinder module. 
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

;; public API function of pathfinder. Does some pre-work and starts the search
;; depending on path type (absolute or relative)
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
