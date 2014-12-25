(ns yummy.html
    (:require 
            [yummy.parser :refer :all]))

(def tab (ref ""))

(defmethod doc-start "html" [type]
  (println "<!DOCTYPE html>"))

(defmethod doc-end "html" [type] 
  (print ""))

(defmethod element-start "html" [type elem attrs value]
  (let [attributes (reduce 
                     (fn [prev attr]
                       (str prev " " (name attr) "=\"" (attrs attr) "\""))
                     ""
                     (reverse (keys attrs)))]
  (println @tab (str "<" (name elem) attributes ">"))
  (dosync
    (ref-set tab (str " " @tab)))
  (if (not (= '() value)) (println @tab (apply str value)))))

(defmethod element-end "html" [type elem]
  (do
    (println (str @tab "</" (name elem) ">"))
    (dosync
      (ref-set tab (apply str (rest @tab))))))

(defn to-html [obj]
  (parse "html" obj))

(yummy.html/to-html {:tag :b :attrs {:name "Vitek" :age "12"} :content [12]})

(yummy.html/to-html {:tag :html
                     :attrs {}
                     :content
                     [{:tag :head
                       :attrs {}
                       :content [{:tag :meta
                                  :attrs {:http-equiv "Content-Type" :content "text/html; charset=utf-8"}
                                  :content []}]}
                      {:tag :body
                       :attrs {}
                       :content
                       [{:tag :p
                         :attrs {}
                         :content [{:tag :b
                                    :attrs {}
                                    :content ["Этот текст будет полужирным"
                                              {:tag :i
                                               :attrs {}
                                               :content ["А этот - еще и курсивным"]}]}]}]}]})
