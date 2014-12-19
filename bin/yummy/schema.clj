(ns yummy.schema)

; yummy-schema is yummy-object definition, itself is yummy-object with additional parameters, such as:
; :xs - determines type of yummy-object description
;       There are four possible variants for this parameter:
;    :element - simple yummy-object description, can describe tagname and value type of object
;    :schema - meaningless value (for example, can be used to determine schema's author)
;    :complexType - header for complex description of yummy-object, contanis one of them
;    :sequence - complex description of yummy-object, :content includes array of simple descriptions
;                :name in :attrs describes which description will be chosen (it should be equal tagname)
; :attrs - contains possible tagname and value type descriptions
;    :name - the only possible tagname (optional)
;    :type - array of possible value types, default clojure types are used (optional)
; :content - array, that describes inner-tags in yummy object
;

; example of yummy-schema and yummy-object that it describes:
{:xs :element :attrs {:name :country} :content 
    [{:xs :complexType :content [{:xs :sequence :content [{:xs :element :attrs {:name :cname} :content []}
                                                          {:xs :element :attrs {:name :population} :content []}]}]}]}

{:tag :country :attrs {} :content [{:tag :cname :attrs {} :content []}
                                   {:tag :population :attrs {} :content []}]}

; method that checks if yummy-object matches yummy-schema
; schema - yummy-schema that defines some yummy-object structure
; obj - yummy-object that will be checked
; log - boolean value that determines whether to display the error text in case of difference found (optional)
;       default: true
(defn schema-check [schema obj & log]
  (let [xs (schema :xs)
        xsattrs (schema :attrs)
        xscontent (schema :content)
        tag (if (map? obj) (obj :tag) :arr)
        attrs (if (map? obj) (obj :attrs) {})
        content (if (map? obj) (obj :content) :arr)]
;    (println "xs" xs "xsattrs" xsattrs "xscontent" xscontent "tag" tag "attrs" attrs "content" content)
    (if (some nil? (list xs xscontent tag attrs content))
      (do 
        (if (not (= log '(false))) 
          (println "Failure: can't find necesassary parameter(s)"))
        false)
      (if (= xs :element)
	      (if (and (not (nil? (xsattrs :name))) (not (= (xsattrs :name) tag)))
	        (do
	          (if (not (= log '(false))) (println "Wrong tag found:" tag "when" (xsattrs :name) "expected"))
	          false)
	      (if (and (not (nil? (xsattrs :type))) (not (.contains (xsattrs :type) 
	                                                   (if (> (count (remove map? content)) 1)
	                                                     clojure.lang.PersistentVector
	                                                     (class (first (remove map? content)))))))
	        (do 
	          (if (not (= log '(false)))
             (println "Wrong value type found:" 
	                   (if (> (count (remove map? content)) 1) clojure.lang.PersistentVector (class (first (remove map? content))))
	                   "when" (xsattrs :type) "expected"))
	          false)
        (do
;          (println "fxs" (first xscontent) "obj" (apply vector (filter map? content)) "log" log)
          (if (empty? xscontent)
            (not (some map? content))
            (schema-check (first xscontent) (apply vector (filter map? content)) log)))))
       (if (= xs :complexType)
         (let [xselements ((first xscontent) :content)]
           (if (= ((first xscontent) :xs) :sequence)
             (reduce (fn [failure inner-tag]
                       (do 
;                         (println failure (some #(= (inner-tag :tag) ((% :attrs) :name)) xselements))
	                       (and failure
	                            (if (not (some #(= (inner-tag :tag) ((% :attrs) :name)) xselements))
                               (do
                                (if (not (= log '(false)))
                                  (println "Didn't found appropriate element for" inner-tag))
                                false)
                               true)
	                            (schema-check (some #(if (= (inner-tag :tag) ((% :attrs) :name)) %) xselements) inner-tag))))
                     true
                     obj)
             false)))))))

(schema-check {:xs :element :attrs {:name :c :type [Long]} :content []} {:tag :c :attrs {} :content [12 2]})
(schema-check {:xs :element :attrs {:name :c :type [clojure.lang.PersistentVector]} :content []} {:tag :c :attrs {} :content [12 2]})

(schema-check 
  {:xs :element
   :attrs {:name :d} 
   :content []}
  {:tag :c
  :attrs {} 
  :content []})

(schema-check {:xs :element :attrs {:name :country} :content 
               [{:xs :complexType :content [{:xs :sequence :content [{:xs :element :attrs {:name :cname} :content []}
                                                                     {:xs :element :attrs {:name :population} :content []}]}]}]}
              {:tag :country :attrs {} :content [{:tag :cname :attrs {} :content []}
                                                 {:tag :population :attrs {} :content []}]})


(schema-check {:xs :element :attrs {:name :country} :content 
                     [{:xs :complexType :content [{:xs :sequence :content [{:xs :element :attrs {:name :cname :type [String]} :content []}
                                                                           {:xs :element :attrs {:name :population} :content []}]}]}]}
                    {:tag :country :attrs {} :content [{:tag :cname :attrs {} :content [5]}
                                                       {:tag :population :attrs {} :content []}]})
