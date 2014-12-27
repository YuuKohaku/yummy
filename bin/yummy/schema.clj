(ns yummy.schema
  (:require [clojure.data :refer :all]))

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
        xsattrs (if (map? (schema :attrs)) (schema :attrs) {})
        xscontent (schema :content)
        tag (if (map? obj) (obj :tag) :arr)
        attrs (if (map? obj) (obj :attrs) {})
        content (if (map? obj) (obj :content) :arr)]
    (if (some nil? (list xs xscontent tag attrs content))
      (do 
        (if (not (= log '(false))) 
          (println "Failure: can't find necesassary parameter(s)"))
        false)
      (case xs
        :schema
        (schema-check (first xscontent) obj log)
        :element
	      (if (and (not (nil? (xsattrs :name))) (not (= (xsattrs :name) tag)))
	        (do
	          (if (not (= log '(false))) (println "Wrong tag found:" tag "when" (xsattrs :name) "expected"))
	          false)
         (do
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
         (if (and (map? (xsattrs :tag-attrs))
                  (or
                    (not (reduce (fn [failure attr]
                                   (do 
                                   (and failure 
                                        (if (not (nil? ((xsattrs :tag-attrs) (first attr))))
                                          (((xsattrs :tag-attrs) (first attr)) (second attr))
                                          true))))
                            true
                            attrs))
                    (not (reduce (fn [failure limit]
                                   (do 
                                   (and failure (((xsattrs :tag-attrs) limit) nil))))
                            true
                            (if (nil? (keys attrs))
                              (if (nil? (keys (xsattrs :tag-attrs))) '() (keys (xsattrs :tag-attrs)))
                              (remove (fn [x] (.contains (keys attrs) x)) (keys (xsattrs :tag-attrs))))))))
         (do
	          (if (not (= log '(false))) (println "Wrong tag attribute found in" tag "tag"))
	          false)
         (do
          (if (empty? xscontent)
            (not (some map? content))
            (reduce 
              (fn [failure tagnum]
                (and failure
                     (schema-check (if (= 1 (count xscontent))
                                     (first xscontent)
                                     (nth xscontent tagnum)) (nth (filter map? content) tagnum) log)))
              (or (= 1 (count xscontent)) (= (count xscontent) (count (filter map? content))))
              (range (count (filter map? content)))))))))))
       :complexType
         (let [xselements ((first xscontent) :content)]
           (if (= ((first xscontent) :xs) :sequence)
             (reduce (fn [failure inner-tag]
                       (do 
	                       (and failure
	                            (if (not (some #(= (inner-tag :tag) ((% :attrs) :name)) xselements))
                               (do
                                (if (not (= log '(false)))
                                  (println "Didn't found appropriate element for" inner-tag))
                                false)
                               true)
	                            (schema-check (some #(if (= (inner-tag :tag) ((% :attrs) :name)) %) xselements) inner-tag))))
                     true
                     (if (vector? obj) obj (list obj)))
             false))))))

(schema-check
  {:xs :element
   :attrs {:name :table-of-contents :tag-attrs {:name string?}}
   :content [{:xs :complexType
              :content [{:xs :sequence
                         :content [{:xs :element
                                    :attrs {:name :title :type [String]}
                                    :content []}
                                         
                                   {:xs :element
                                    :attrs {:name :section}
                                    :content [{:xs :element
                                               :attrs {:name :title :type [String] :tag-attrs {:name string?}}
                                               :content []}
                                              {:xs :element
                                               :attrs {:name :list :tag-attrs {:name string?}}
                                               :content [{:xs :complexType
                                                          :content [{:xs :sequence
                                                                     :content [{:xs :element
                                                                                :attrs {:name :li :type [String] 
                                                                                        :tag-attrs {:order 
                                                                                                    (fn [x] 
                                                                                                      (and (string? x)
                                                                                                           (number? (read-string x))))}}
                                                                                 :content []}]}]}]}]}]}]}]}
         
{:tag :table-of-contents
 :attrs {:name "table-of-contents"}
 :content [{:tag :title
					  :attrs {}
					  :content ["Table of contents"]}
					 {:tag :section
					  :attrs {}
					  :content [{:tag :title
					             :attrs {:name "section-title"}
					             :content ["Chapter 1"]}
                      {:tag :list
	                     :attrs {:name "links"}
	                     :content [{:tag :li
					                        :attrs {:order "1"}
					                        :content ["Paragraph"]}
					                       {:tag :li
					                        :attrs {:order "2"}
					                        :content ["Paragraph"]}
					                       {:tag :li
					                        :attrs {:order "3"}
					                        :content ["Paragraph"]}]}]}
           {:tag :section
					  :attrs {}
					  :content [{:tag :title
					             :attrs {:name "section-title"}
					             :content ["Chapter 2"]}
                      {:tag :list
					             :attrs {:name "links"}
					             :content [{:tag :li
                                  :attrs {:order "1"}
					                        :content ["Paragraph"]}
            				             {:tag :li
					                        :attrs {:order "2"}
					                        :content ["Paragraph"]}
					                       {:tag :li
					                        :attrs {:order "3"}
					                        :content ["Paragraph"]}]}]}]})
