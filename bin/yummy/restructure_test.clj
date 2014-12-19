(ns yummy.restructure-test
  (:require [clojure.test :refer :all]
            [yummy.restructure :refer :all]))

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
  (testing "Asterisk-path"
           ;;все с на расстоянии >= 0 уровней от b
      (is (= (set-tag "b/*/c" tst-tag re-tag)
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
                                                            :content [{:content [2 3], 
                                                                       :attrs {:key "CHANGED"}, 
                                                                       :tag :CHANGED} 
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
           ;;все с на расстоянии 1 уровней от b
      (is (= (set-tag "b/*/*/c" tst-tag re-tag)
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
                             ]}
                  )
               )             
           ;;все с на расстоянии 2 уровней от а
      (is (= (set-tag "a/*/*/*/c" tst-tag re-tag)
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
           ;;дочерние тэги а
            (is (= (set-tag "a/*" tst-tag re-tag)
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
            (is (= (set-tag "a/b/*" tst-tag re-tag)
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
           ;;дочерние тэги c тэгов ниже а, обладающих указанными атрибутами  
           (is (= (set-tag "a/*@[key=val]/c" tst-tag re-tag)
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
    (testing "Same tag"
           ;; . означает текущий тэг и влияния не имеет. Но поддерживается.
           (is (= (set-tag "a/./c" tst-tag re-tag)
                  {:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :CHANGED, :attrs {:key "CHANGED"}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]})
               )
             ))
(run-tests)
