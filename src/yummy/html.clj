(ns yummy.html
    (:require 
            [yummy.parser :refer :all]
            [clojure.java.io :refer :all]))

(def tab (ref ""))
(def prev-path (ref "$"))

(defn write-xsl [xsl-tag wrtr fullobj cur-elem]
  (let [path ((xsl-tag :attrs) :select)
        tag (if (= (first path) \.) cur-elem (first (yummy.pathfinder/get-tag path fullobj)))
        value (((xsl-tag :attrs) :value) tag)
        ]
  (.write wrtr (str @tab value "\n"))
))

(defn write-tag [tag wrtr fullobj cur-elem]
  (let [attrs (tag :attrs)
        elem (tag :tag)
        content (tag :content)
        str-attrs (reduce 
                     (fn [prev attr]
                       (str prev " " (name attr) "=\"" (attrs attr) "\""))
                     ""
                     (reverse (keys attrs)))]
    (.write wrtr (str @tab "<" (name elem) str-attrs ">\n"))
    (dosync (ref-set tab (str " " @tab)))
    (reduce (fn [prev x]
              (write-xsl x wrtr fullobj cur-elem))
            true
            (filter (fn [x] (nil? (x :tag))) content))
    (reduce (fn [prev x]
              (write-tag x wrtr fullobj cur-elem))
            true
            (remove (fn [x] (nil? (x :tag))) content))
    (dosync (ref-set tab (apply str (rest @tab))))
    (.write wrtr (str @tab  "</" (name elem) ">\n"))
))

(defn into-html [style-table obj filename] 
  (with-open [wrtr (writer filename)]
  
  (defmethod doc-start "into-html" [type]
    (.write wrtr "<html>\n"))

  (defmethod doc-end "into-html" [type]
    (.write wrtr "</html>\n"))
    
  (defmethod element-start "into-html" [type elem attrs value]
    (let [content (style-table :content)
          templ (first (filter (fn [template] (= ((template :attrs) :match) elem)) content))]
      (dosync (ref-set prev-path (str @prev-path "/" (name elem))))
      (if (nil? templ) (println "Very bad, I didn't find template for" elem "tag")
        (let [t-content (templ :content)
              cur-elem {:tag elem :attrs attrs :content value}
              xsl-cont (filter (fn [x] (nil? (x :tag))) t-content)
              tag-cont (remove (fn [x] (nil? (x :tag))) t-content)
              globals (templ :globals)]
          (reduce (fn [prev x]
                    (do 
                    (write-xsl x wrtr obj cur-elem)))
                  true
                  xsl-cont)
          (reduce (fn [prev x]
                    (do 
                    (write-tag x wrtr obj cur-elem)))
                  true
                  tag-cont)
          (reduce (fn [prev x]
                    (let [attrs (x :attrs)
                          str-attrs (reduce 
                                      (fn [prev attr]
                                        (str prev " " (name attr) "=\"" (attrs attr) "\""))
                                      ""
                                      (reverse (keys attrs)))]
                    (.write wrtr (str @tab "<" (name (x :tag)) str-attrs ">\n"))
                    (dosync
                      (ref-set tab (str " " @tab))
                      )
                    ))
                  true
                  globals)
          ))))
  
    (defmethod element-end "into-html" [type elem]
      (let [content (style-table :content)
            templ (first (filter (fn [template] (= ((template :attrs) :match) elem)) content))
            globals (templ :globals)]
        (reduce (fn [prev x]
                  (dosync (ref-set tab (apply str (rest @tab))))
                  (.write wrtr (str @tab "</" (name (x :tag)) ">\n"))
                  )
                true
                (reverse globals))
        (dosync (ref-set prev-path (clojure.string/replace (str @prev-path " ") (str "/" (name elem) " ") "")))
        ))
    (parse "into-html" obj)))

(into-html {:xsl :stylesheet
            :attrs {}
            :content [{:xsl :template
                       :attrs {:match :one}
                       :globals [{:tag :b :attrs {} :content []}]
                       :content [{:tag :a 
                                  :attrs {:something "123"}
                                  :content []}]}
                      {:xsl :template
                       :attrs {:match :two}
                       :globals [{:tag :p :attrs {} :content []}]
                       :content []}
                      {:xsl :template
                       :attrs {:match :three}
                       :globals [{:tag :a :attrs {} :content []}]
                       :content [{:xsl :value-of
                                  :attrs {:select "."
                                          :value (fn[x] ((x :attrs) :secondname))}
                                  :content []}]}
                      ]}
           {:tag :one
            :attrs {:name "Kolyan"}
            :content [{:tag :two
                       :attrs {}
                       :content [{:tag :three
                                  :attrs {:secondname "Lujkov"}
                                  :content []}]}]}
           "txt.txt")


(into-html {:xsl :stylesheet
            :attrs {}
            :content [{:xsl :template
                       :attrs {:match :table-of-contents}
                       :globals [{:tag :body
                                  :attrs {}
                                  :content []}
                                 {:tag :p
                                  :attrs {}
                                  :content []}
                                 ]
                       :content [{:tag :head
                                  :attrs {}
                                  :content [{:tag :title
                                             :attrs {}
                                             :content [{:xsl :value-of
                                                        :attrs {:select "." :value (fn [x] ((x :attrs) :name))}
                                                        :content []}]}]
                                  }
                                 ]
                       }
                      {:xsl :template
                       :attrs {:match :title}
                       :globals []
                       :content [{:tag :font
                                  :attrs {:size "6" :color "blue"}
                                  :content [{:xsl :value-of
                                             :attrs {:select "." :value (fn [x] (first (x :content)))}
                                             :content []}]
                                  }
                                 ]
                      }
                      {:xsl :template
                       :attrs {:match :ctitle}
                       :globals []
                       :content [{:tag :br
                                  :attrs {}
                                  :content []}
                                 {:tag :font
                                  :attrs {:size "5" :color "red" :align "left"}
                                  :content [{:xsl :value-of
                                             :attrs {:select "." :value (fn [x] (first (x :content)))}
                                             :content []}]}]
                       }
                      {:xsl :template
                       :attrs {:match :section}
                       :globals []
                       :content []
                      }
                      {:xsl :template
                       :attrs {:match :list}
                       :globals [{:tag :list
                                 :attrs {}
                                 :content []}]
                       :content []
                       }
                      {:xsl :template
                       :attrs {:match :li}
                       :globals []
                       :content [{:tag :li
                                  :attrs {}
                                  :content [{:xsl :value-of
                                             :attrs {:select "." :value (fn [x] (str ((x :attrs) :order) ". " (first (x :content)) ))}}
                                            ]}]
                       }
                      ]}
           
           
{:tag :table-of-contents
 :attrs {:name "table-of-contents"}
 :content [
           {:tag :title
            :attrs {}
            :content ["Table of contents"]}
           {:tag :section
            :attrs {}
            :content [
                      {:tag :ctitle
                       :attrs {:name "section-title"}
                       :content ["Chapter 1"]
                       }
                      {:tag :list
                       :attrs {:name "links"}
                       :content [
                                 {:tag :li
                                  :attrs {:order "1"}
                                  :content ["Paragraph A"]}
                                 {:tag :li
                                  :attrs {:order "2"}
                                  :content ["Paragraph B"]}
                                 {:tag :li
                                  :attrs {:order "3"}
                                  :content ["Paragraph C"]}
                                 ]
                       } 
                      ]}
           {:tag :section
            :attrs {}
            :content [
                      {:tag :ctitle
                       :attrs {:name "section-title"}
                       :content ["Chapter 2"]
                       }
                      {:tag :list
                       :attrs {:name "links"}
                       :content [
                                 {:tag :li
                                  :attrs {:order "1"}
                                  :content ["Paragraph AA"]}
                                 {:tag :li
                                  :attrs {:order "2"}
                                  :content ["Paragraph BB"]}
                                 {:tag :li
                                  :attrs {:order "3"}
                                  :content ["Paragraph CC"]}
                                 ]
                       } 
                      ]}
           ]
 } 
"new.html")
           
