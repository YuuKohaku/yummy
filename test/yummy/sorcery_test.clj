(ns yummy.sorcery-test
  (:require [clojure.test :refer :all]
            [yummy.sorcery :refer :all]
            [yummy.pathfinder :refer :all]))

(def toc
  {:tag :table-of-contents
   :attrs {:name "table-of-contents"}
   :content [
           {:tag :title
            :attrs {}
            :content ["Table of contents"]}
           {:tag :section
            :attrs {}
            :content [
                      {:tag :title
                       :attrs {:name "section-title"}
                       :content ["Chapter 1"]
                       }
                      {:tag :list
                       :attrs {:name "links"}
                       :content [
                                 {:tag :li
                                  :attrs {:order "1"}
                                  :content ["Paragraph"]}
                                 {:tag :li
                                  :attrs {:order "2"}
                                  :content ["Paragraph"]}
                                 {:tag :li
                                  :attrs {:order "3"}
                                  :content ["Paragraph"]}
                                 ]
                       } 
                      ]}
           {:tag :section
            :attrs {}
            :content [
                      {:tag :title
                       :attrs {:name "section-title"}
                       :content ["Chapter 2"]
                       }
                      {:tag :list
                       :attrs {:name "links"}
                       :content [
                                 {:tag :li
                                  :attrs {:order "1"}
                                  :content ["Paragraph"]}
                                 {:tag :li
                                  :attrs {:order "2"}
                                  :content ["Paragraph"]}
                                 {:tag :li
                                  :attrs {:order "3"}
                                  :content ["Paragraph"]}
                                 ]
                       } 
                      ]}
           ]
 })

(deftest sorcery-test
  (testing "Root test"
           (is (= (set-content :prepend 0 "$/table-of-contents/title" toc "TEST" ) 
                  {:tag :table-of-contents, 
                   :attrs {:name "table-of-contents"}, 
                   :content [
                             {:tag :title, 
                              :attrs {}, 
                              :content ["TEST" "Table of contents"]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 1"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
           (is (= (set-content :append 0 "$/table-of-contents/title" toc "TEST" ) 
                  {:tag :table-of-contents, 
                   :attrs {:name "table-of-contents"}, 
                   :content [
                             {:tag :title, 
                              :attrs {}, 
                              :content ["Table of contents"  "TEST"]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 1"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
           ;;enumerate paragraphs
          (is (=    (assoc toc :content                                  (reduce #(into %1 [%2])
                                         []
                                         (map #(if (yummy-object? %)
                                                 (restruct % wp new-tag)
                                                 %)
                                              (exp :content))
                                         ))

                  {:tag :table-of-contents, 
                   :attrs {:name "table-of-contents"}, 
                   :content [
                             {:tag :title, 
                              :attrs {}, 
                              :content ["Table of contents"]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 1"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["1" "Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["2" "Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["3" "Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["1" "Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["2" "Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["3" "Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
         )
  (testing "Insert-test"
  (is (= (set-content :insert 1 "section" toc "TEST" ) 
                 {:tag :table-of-contents, 
                  :attrs {:name "table-of-contents"}, 
                  :content [
                            {:tag :title, 
                             :attrs {}, 
                             :content ["Table of contents"]} 
                            {:tag :section, 
                             :attrs {}, 
                              :content [
                                        {:content ["Chapter 1"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                "TEST"
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                "TEST"
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
  (is (= (set-content :insert 1 "*@[name=table-of-contents]/./." toc "TEST" ) 
                 {:tag :table-of-contents, 
                  :attrs {:name "table-of-contents"}, 
                  :content [
                            {:tag :title, 
                             :attrs {}, 
                             :content ["Table of contents"]} 
                            {:tag :section, 
                             :attrs {}, 
                              :content [
                                        {:content ["Chapter 1" "TEST"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   "TEST"
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2" "TEST"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   "TEST"
                                                   {:content ["Paragraph"], :attrs {:order "2"}, :tag :li}
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
   (is (= (set-content :replace 1 "*@[name=table-of-contents]/./." toc "TEST" ) 
                 {:tag :table-of-contents, 
                  :attrs {:name "table-of-contents"}, 
                  :content [
                            {:tag :title, 
                             :attrs {}, 
                             :content ["Table of contents"]} 
                            {:tag :section, 
                             :attrs {}, 
                              :content [
                                        {:content ["Chapter 1" "TEST"],
                                         :attrs {:name "section-title"},
                                         :tag :title}
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   "TEST"
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]} 
                             {:tag :section, 
                              :attrs {}, 
                              :content [
                                        {:content ["Chapter 2" "TEST"], 
                                         :attrs {:name "section-title"}, 
                                         :tag :title} 
                                        {:tag :list
                                         :attrs {:name "links"}, 
                                         :content [{:content ["Paragraph"], :attrs {:order "1"}, :tag :li}
                                                   "TEST"
                                                   {:content ["Paragraph"], :attrs {:order "3"}, :tag :li}
                                                   ]}
                                        ]}
                             ]}
                  ))
   )
  )
(run-tests)