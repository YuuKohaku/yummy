(ns yummy.restructure-test
  (:require [clojure.test :refer :all]
            [yummy.restructure :refer :all]))
(def table-of-contents
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
(def tst-tag {:tag :a 
              :attrs {:key "val" }
              :content [23
                        25
                        {:tag :c
                         :attrs {}
                         :content [65 
                                   {:tag :e
                                    :attrs {:k "5"}
                                    :content ["branch"
                                              {:tag :c
                                               :attrs {}
                                               :content [58]}]
                                    }]
                         }
                        {:tag :b
                         :attrs {}
                         :content [{:tag :c
                                    :attrs {:key "val" :k "5"}
                                    :content [2 3]}
                                   {:tag :d 
                                    :attrs {:key "val"} 
                                    :content [2
                                              3
                                              {:tag :c
                                               :attrs {}
                                               :content [85
                                                         {:tag :t
                                                          :attrs {}
                                                          :content []}
                                                         ]}
                                              ]}
                                   ]}
                        ]})
(def re-tag {:tag :CHANGED :attrs {:key "CHANGED"} :content []})
;;________________________________________
;;                   d@[key=val]---c---t
;;              b----|
;; a@[key=val]--|    c@[key=val,k=5]
;;              c----e@[k=5]-------c
;;________________________________________
;;позволяет изменять свойства тэгов, не затрагивая структуру дерева
(deftest restructure-test
  (testing "Normal path."
           ;;относительный путь
           (is (= (set-tag "b/c" tst-tag re-tag) 
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}]}]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t,
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           ;;абсолютный путь - корень обозначается как $
           (is (= (set-tag "$/a/b/c" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}]}]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t,
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           ;;частный случай относительного пути, возвращает все  тэги с указанным именем
           (is (= (set-tag "t" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}]}]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :c, 
                                         :attrs {:key "val", :k "5"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :CHANGED, 
                                                                      :attrs {:key "CHANGED"}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           ;;аналогично предыдущему
           (is (= (set-tag "/t" tst-tag re-tag) 
                   {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}]}]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :c, 
                                         :attrs {:key "val", :k "5"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :CHANGED, 
                                                                      :attrs {:key "CHANGED"}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           )
  (testing "Parent-path"
           ;;родительский тэг обозначается ..
           (is (= (set-tag "$/a/b/d/../c" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t,
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           ;;количество подряд идущих ссылок на родителя не ограничено           
           (is (= (set-tag "a/b/d/../../c" tst-tag re-tag) 
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :CHANGED, 
                                    :attrs {:key "CHANGED"}, 
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :c, 
                                         :attrs {:key "val", :k "5"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t,
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})               )       
           ;;но если оно больше, чем число родителей, тэг не изменится           
           (is (= (set-tag "a/b/../../../c" tst-tag re-tag) 
                  tst-tag)
               )       
 
           )
  (testing "Dot-path"
           ;;все с на расстоянии 1 уровней от b
      (is (= (set-tag "b/./c" tst-tag re-tag)
                  {:tag :a, :attrs {:key "val"}, :content [23 25 
                                                           {:tag :c, 
                                                            :attrs {}, 
                                                            :content [65 {:tag :e, 
                                                                          :attrs {:k "5"}, 
                                                                          :content ["branch" {:tag :c, 
                                                                                              :attrs {}, 
                                                                                              :content [58]}
                                                                                    ]}
                                                                      ]} 
                                                           {:tag :b, 
                                                            :attrs {}, 
                                                            :content [{:tag :c
                                                                       :attrs {:key "val" :k "5"}
                                                                       :content [2 3]}
                                                                      {:tag :d, 
                                                                       :attrs {:key "val"}, 
                                                                       :content [2 3 {:tag :CHANGED, 
                                                                                      :attrs {:key "CHANGED"}, 
                                                                                      :content [85 {:tag :t, 
                                                                                                    :attrs {}, 
                                                                                                    :content []}
                                                                                                ]}
                                                                                 ]}
                                                                      ]}
                                                           ]})
               )
           ;;все с на расстоянии 2 уровней от a
      (is (= (set-tag "a/././c" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e,
                                                  :attrs {:k "5"},
                                                  :content ["branch" {:tag :CHANGED,
                                                                      :attrs {:key "CHANGED"},
                                                                      :content [58]}
                                                            ]}
                                              ]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:content [2 3],
                                         :attrs {:key "val", :k "5"}, 
                                         :tag :c} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :CHANGED, 
                                                        :attrs {:key "CHANGED"}, 
                                                        :content [85 {:tag :t, 
                                                                      :attrs {}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]}
                  )
               )             
           ;;все t на расстоянии 3 уровней от а
      (is (= (set-tag "a/./././t" tst-tag re-tag)
                   {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c,
                                    :attrs {},
                                    :content [65 {:tag :e,
                                                  :attrs {:k "5"},
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:content [2 3],
                                         :attrs {:key "val", :k "5"}, 
                                         :tag :c} 
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :c,
                                                        :attrs {}, 
                                                        :content [85 {:tag :CHANGED, 
                                                                      :attrs {:key "CHANGED"}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]}
                  )
          )   
      )
    (testing "Params-path"
           ;;все с, имеющие указанные атрибуты
           (is (= (set-tag "c@[key=val]" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c, 
                                    :attrs {}, 
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]}
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"},
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t, 
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]} 
                  )
           )           
           ;;все тэги, имеющие указанные атрибуты
           (is (= (set-tag "*@[key=val,k=5]" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c, 
                                    :attrs {}, 
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]}
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :d, 
                                         :attrs {:key "val"},
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t, 
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]} 
                  )
               )           

           ;;все дочерние тэги а, имеющие указанные атрибуты
           (is (= (set-tag "a/.@[key=val]" tst-tag re-tag)
                   tst-tag 
                  )
               )                      
       
           ;;дочерние тэги а
            (is (= (set-tag "a/." tst-tag re-tag)
                   {:tag :a, 
                    :attrs {:key "val"}, 
                    :content [23 25 {:tag :CHANGED, 
                                     :attrs {:key "CHANGED"}, 
                                     :content [65 {:tag :e, 
                                                   :attrs {:k "5"}, 
                                                   :content ["branch" {:tag :c, 
                                                                       :attrs {}, 
                                                                       :content [58]}
                                                             ]}
                                               ]}
                              {:tag :CHANGED, 
                               :attrs {:key "CHANGED"},
                               :content [{:content [2 3], 
                                          :attrs {:key "val", :k "5"},
                                          :tag :c}
                                         {:tag :d, 
                                          :attrs {:key "val"},
                                          :content [2 3 {:tag :c,
                                                         :attrs {},
                                                         :content [85 {:tag :t,
                                                                       :attrs {},
                                                                       :content []}
                                                                   ]}
                                                    ]}
                                         ]}
                              ]}
                   )
               )
           ;;дочерние тэги тэга по указанному адресу
            (is (= (set-tag "a/b/." tst-tag re-tag)
                   {:tag :a, 
                    :attrs {:key "val"},
                    :content [23 25 {:tag :c,
                                     :attrs {},
                                     :content [65 {:tag :e, 
                                                   :attrs {:k "5"}, 
                                                   :content ["branch" {:tag :c, 
                                                                       :attrs {}, 
                                                                       :content [58]}
                                                             ]}
                                               ]}
                              {:tag :b,
                               :attrs {},
                               :content [{:tag :CHANGED, 
                                          :attrs {:key "CHANGED"},
                                          :content [2 3]}
                                         {:tag :CHANGED, 
                                          :attrs {:key "CHANGED"},
                                          :content [2 3 {:tag :c, 
                                                         :attrs {},
                                                         :content [85 {:tag :t,
                                                                       :attrs {},
                                                                       :content []}
                                                                   ]}
                                                    ]}
                                         ]}
                              ]}
)
               )            
           ;;дочерние тэги c тэгов ниже b, обладающих указанными атрибутами  
           (is (= (set-tag "b/.@[key=val]/c" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c, 
                                    :attrs {}, 
                                    :content [65 {:tag :e,
                                                  :attrs {:k "5"},
                                                  :content ["branch" {:tag :c,
                                                                      :attrs {},
                                                                      :content [58]}
                                                            ]}
                                              ]} 
                             {:tag :b, 
                              :attrs {},
                              :content [{:content [2 3],
                                         :attrs {:key "val", :k "5"},
                                         :tag :c} 
                                        {:tag :d, 
                                         :attrs {:key "val"},
                                         :content [2 3 {:tag :CHANGED, 
                                                        :attrs {:key "CHANGED"}, 
                                                        :content [85 {:tag :t,
                                                                      :attrs {}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
           )
    (testing "Asterisk-path"
           ;; all tags
           (is (= (set-tag "*" tst-tag re-tag)
                  {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [23 25 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [65 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content ["branch" {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [58]}]}]} {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [{:tag :CHANGED, :attrs {:key "CHANGED"}, :content [2 3]} {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [2 3 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [85 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content []}]}]}]}]})
               )
           ;; all tags
           (is (= (set-tag "$/*" tst-tag re-tag)
                  {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [23 25 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [65 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content ["branch" {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [58]}]}]} {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [{:tag :CHANGED, :attrs {:key "CHANGED"}, :content [2 3]} {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [2 3 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [85 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content []}]}]}]}]})
               )
           ;; root children
           (is (= (set-tag "." tst-tag re-tag)
                  {:tag :CHANGED 
                   :attrs {:key "CHANGED" }
                   :content [23
                             25
                             {:tag :c
                              :attrs {}
                              :content [65 
                                        {:tag :e
                                         :attrs {:k "5"}
                                         :content ["branch"
                                              {:tag :c
                                               :attrs {}
                                               :content [58]}]
                                         }]
                              }
                             {:tag :b
                              :attrs {}
                              :content [{:tag :c
                                         :attrs {:key "val" :k "5"}
                                         :content [2 3]}
                                        {:tag :d 
                                         :attrs {:key "val"} 
                                         :content [2
                                              3
                                              {:tag :c
                                               :attrs {}
                                               :content [85
                                                         {:tag :t
                                                          :attrs {}
                                                          :content []}
                                                         ]}
                                              ]}
                                        ]}
                             ]})
               )
           ;; root children
           (is (= (set-tag "$/." tst-tag re-tag)
                  {:tag :CHANGED 
                   :attrs {:key "CHANGED" }
                   :content [23
                             25
                             {:tag :c
                              :attrs {}
                              :content [65 
                                        {:tag :e
                                         :attrs {:k "5"}
                                         :content ["branch"
                                              {:tag :c
                                               :attrs {}
                                               :content [58]}]
                                         }]
                              }
                             {:tag :b
                              :attrs {}
                              :content [{:tag :c
                                         :attrs {:key "val" :k "5"}
                                         :content [2 3]}
                                        {:tag :d 
                                         :attrs {:key "val"} 
                                         :content [2
                                              3
                                              {:tag :c
                                               :attrs {}
                                               :content [85
                                                         {:tag :t
                                                          :attrs {}
                                                          :content []}
                                                         ]}
                                              ]}
                                        ]}
                             ]})
               )
           ;;все тэги ниже а, имеющие указанные атрибуты
           (is (= (set-tag "a/*@[key=val]" tst-tag re-tag)
                   {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 {:tag :c, 
                                    :attrs {}, 
                                    :content [65 {:tag :e, 
                                                  :attrs {:k "5"}, 
                                                  :content ["branch" {:tag :c, 
                                                                      :attrs {}, 
                                                                      :content [58]}
                                                            ]}
                                              ]}
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED, 
                                         :attrs {:key "CHANGED"}, 
                                         :content [2 3]} 
                                        {:tag :CHANGED,
                                         :attrs {:key "CHANGED"},
                                         :content [2 3 {:tag :c, 
                                                        :attrs {},
                                                        :content [85 {:tag :t, 
                                                                      :attrs {},
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]} 
                  )
               ) 
           ;;все с под a
      (is (= (set-tag "a/*/c" tst-tag re-tag)
                  {:tag :a, :attrs {:key "val"}, :content [23 25 
                                                           {:tag :CHANGED, 
                                                            :attrs {:key "CHANGED"}, 
                                                            :content [65 {:tag :e, 
                                                                          :attrs {:k "5"}, 
                                                                          :content ["branch" {:tag :CHANGED, 
                                                                                              :attrs {:key "CHANGED"}, 
                                                                                              :content [58]}
                                                                                    ]}
                                                                      ]} 
                                                           {:tag :b, 
                                                            :attrs {}, 
                                                            :content [{:tag :CHANGED
                                                                       :attrs {:key "CHANGED"}
                                                                       :content [2 3]}
                                                                      {:tag :d, 
                                                                       :attrs {:key "val"}, 
                                                                       :content [2 3 {:tag :CHANGED, 
                                                                                      :attrs {:key "CHANGED"}, 
                                                                                      :content [85 {:tag :t, 
                                                                                                    :attrs {}, 
                                                                                                    :content []}
                                                                                                ]}
                                                                                 ]}
                                                                      ]}
                                                           ]})
               )           
      (is (= (set-tag "$/a/*@[key=val]/c" tst-tag re-tag)
                  {:tag :a, 
                   :attrs {:key "val"}, 
                   :content [23 25 
                             {:tag :c, 
                              :attrs {}, 
                              :content [65 {:tag :e, 
                                            :attrs {:k "5"}, 
                                            :content ["branch" {:tag :c, 
                                                                :attrs {}, 
                                                                :content [58]}
                                                      ]}
                                        ]} 
                             {:tag :b, 
                              :attrs {}, 
                              :content [{:tag :CHANGED
                                         :attrs {:key "CHANGED"}
                                         :content [2 3]}
                                        {:tag :d, 
                                         :attrs {:key "val"}, 
                                         :content [2 3 {:tag :CHANGED, 
                                                        :attrs {:key "CHANGED"}, 
                                                        :content [85 {:tag :t, 
                                                                      :attrs {}, 
                                                                      :content []}
                                                                  ]}
                                                   ]}
                                        ]}
                             ]})
               )
      ))
(run-tests)
