(ns yummy.schema)

(defn schema-check [schema obj]
  (let [xs (schema :xs)
        xsattrs (schema :attrs)
        xscontent (schema :content)
        tag (obj :tag)
        attrs (obj :attrs)
        content (obj :content)]
    (if (some nil? (list xs xsattrs xscontent tag attrs content))
      (do 
        (println "Failure: can't find necesassary parameter(s)")
        false)
      (if (and (not (nil? (xsattrs :name))) (not (= (xsattrs :name) tag)))
        (do
          (println "Wrong tag found:" tag "when" (xsattrs :name) "expected")
          false)
      (if (and (not (nil? (xsattrs :type))) (not (.contains (xsattrs :type) 
                                                   (if (> (count (remove map? content)) 1)
                                                     clojure.lang.PersistentVector
                                                     (class (first (remove map? content)))))))
        (do 
          (println "Wrong value type found:" 
                   (if (> (count (remove map? content)) 1) clojure.lang.PersistentVector (class (first (remove map? content))))
                   "when" (xsattrs :type) "expected")
          false)
        true)))))

(schema-check {:xs :element :attrs {:name :c :type [Long]} :content []} {:tag :c :attrs {} :content [12 2]})
(schema-check {:xs :element :attrs {:name :c :type [clojure.lang.PersistentVector]} :content []} {:tag :c :attrs {} :content [12 2]})

(schema-check 
  {:xs :element
   :attrs {:name :d} 
   :content []}
  {:tag :c
   :attrs {} 
   :content []})


