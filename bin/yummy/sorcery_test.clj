(ns yummy.sorcery-test
  (:require [clojure.test :refer :all]
            [yummy.sorcery :refer :all]))

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
          (is (= (reduce 
                   #(set-content :prepend 0 (str "li@[order=" %2 "]") %1 (str %2) )
                   toc
                   (range 3)
                   ) 
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
  )
(run-tests)