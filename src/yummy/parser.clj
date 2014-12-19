; method that is called when document starts
; type - parameter that determines which defenition will be used
(defmulti doc-start (fn [type] type))

; default defenition for doc-start method
; just prints "Doc starts"
(defmethod doc-start :default [type] 
  (println "Doc starts"))

; method that is called when document ends
; type - parameter that determines which defenition will be used
(defmulti doc-end (fn [type] type))

; default defenition for doc-end method
; just prints "Doc ends"
(defmethod doc-end :default [type] 
  (println "Doc ends"))

; method that is called when element starts
; type - parameter that determines which defenition will be used
; elem - element that was found
; attrs - element's attributes
(defmulti element-start (fn [type elem attrs] type))

; default defenition for element-start method
; just prints that element started and the list of its attributes
(defmethod element-start :default [type elem attrs]
  (println elem "start, attrs:" attrs))

; method that is called when element ends
; type - parameter that determines which defenition will be used
(defmulti element-end (fn[type elem] type))

; default defenition for element-end method
; just prints that element ended
(defmethod element-end :default [type elem]
  (println elem "ends"))

; method that is called when some failure was found
; type - parameter that determines which defenition will be used
; elem - element that contains error
; failtxt - suggested failure text 
(defmulti element-failure (fn [type elem failtxt] type))

; default defenition for element-failure method
; just prints error text and in which tag it was found
(defmethod element-failure :default [type elem failtxt]
  (println "Failure! Tag" elem "is bad:" failtxt))

; method that parses yummy-object using methods defined above
; stops parsing if failure is found
; type - parameter that determines which handler defenitions will be used
; obj - object that will be parsed
; rest - some additional options (optional)
;        if (false), that doc-start and doc-end methods are not called 
(defn parse [type obj & rest]
   {:pre [(map? obj) (or (nil? rest) (= '(false) rest))]}
   (let [tagname (obj :tag)
         content (obj :content)
         attrs (obj :attrs)]
     (if (not rest) (doc-start type))
     (if 
       (not (keyword? tagname)) (do (element-failure type nil "Unknown tag found") false)
     (if 
       (not (map? attrs)) (do (element-failure type tagname "No attributes found") false)
     (if
       (not (and (every? keyword? (keys attrs))
                 (every? string? (vals attrs)))) (do (element-failure type tagname "Wrong attributes") false)
     (if 
       (not (vector? content)) (do (element-failure type tagname "Bad content found") false)
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

; example of a new doc-start definition
(defmethod doc-start "mine" [type]
  (println "My own doc starts"))

; example of using this definition
(parse "mine" {:tag :b :attrs {} :content []})

