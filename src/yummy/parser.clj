(defmulti doc-start (fn [type] type))

(defmethod doc-start :default [type] 
  (println "Doc starts"))

(defmulti doc-end (fn [type] type))

(defmethod doc-end :default [type] 
  (println "Doc ends"))

(defmulti element-start (fn [type elem attrs] type))

(defmethod element-start :default [type elem attrs]
  (println elem "start, attrs:" attrs))

(defmulti element-end (fn[type elem] type))

(defmethod element-end :default [type elem]
  (println elem "ends"))

(defn parse [type obj & rest]
   {:pre [(map? obj) (or (nil? rest) (= '(false) rest))]}
   (let [tagname (obj :tag)
         content (obj :content)
         attrs (obj :attrs)]
     (if (not rest) (doc-start type))
     (if 
       (not (keyword? tagname)) (do (println "Unknown tag found") false)
     (if 
       (not (map? attrs)) (do (println "No attributes found") false)
     (if
       (not (and (every? keyword? (keys attrs)) 
                 (every? string? (vals attrs)))) (do (println "Wrong attributes") false)
     (if 
       (not (vector? content)) (do (println "Bad content found") false)
     (do 
       (element-start type tagname attrs)
       (if (or (empty? content) 
               (reduce (fn [failure inner-tag] (and failure (parse type inner-tag false)))
                       true
                       (filter map? content)))
         (do
           (element-end type tagname)
           (if (not rest) (doc-end type))
           true)
         false))))))))

(parse "abc" {:tag :b :attrs {} :content []})
(parse "abc" {:tag :a :attrs {:key "val"} :content [23 25 {:tag :b :attrs {} :content []}]})
(parse "abc" {:tag :a :attrs {:key "val"} :content [23 25 {:tag :b :attrs {} :content []}
                                                    {:tag :c :attrs {} :content []}]})

(defmethod doc-start "mine" [type]
  (println "My own doc starts"))

(parse "mine" {:tag :b :attrs {} :content []})

