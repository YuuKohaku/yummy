(ns yummy.pathfinder-test
  (:require [clojure.test :refer :all]
            [yummy.pathfinder :refer :all]))

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

;;________________________________________
;;                   d@[key=val]---c---t
;;              b----|
;; a@[key=val]--|    c@[key=val,k=5]
;;              c----e@[k=5]-------c
;;________________________________________

(deftest pathfinder-test
  (testing "Normal path."
           ;;относительный путь
           (is (= (get-tag "b/c" tst-tag) 
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           ;;абсолютный путь - корень обозначается как $
           (is (= (get-tag "$/a/b/c" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           ;;частный случай относительного пути, возвращает все  тэги с указанным именем
           (is (= (get-tag "t" tst-tag)
                  '({:tag :t, :attrs {}, :content []}))
               )
           ;;аналогично предыдущему
           (is (= (get-tag "/c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           ;;аналогично предыдущему
           (is (= (get-tag "c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                {:tag :c, :attrs {}, :content [58]} 
                {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           )
  (testing "Parent-path"
           ;;родительский тэг обозначается ..
           (is (= (get-tag "$/a/b/d/../c" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           ;;количество подряд идущих ссылок на родителя не ограничено           
           (is (= (get-tag "a/b/d/../../c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]}))
               )       
           ;;но если оно больше, чем число родителей, поиск в неизвестности не производится           
           (is (= (get-tag "a/b/../../../c" tst-tag) 
                  '())
               )       
           ;;частный случай вышесказанного          
           (is (= (get-tag ".." tst-tag) 
                  '())
               )       
           )
  (testing "Dot-path"
           ;;все с на расстоянии 1 уровней от b
           (is (= (get-tag "b/./c" tst-tag)
                  '({:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           ;;все с на расстоянии 2 уровней от a
           (is (= (get-tag "a/././c" tst-tag)
                  '({:tag :c, :attrs {}, :content [58]} 
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )             
           ;;все t на расстоянии 3 уровней от а
           (is (= (get-tag "a/./././t" tst-tag)
                  '({:tag :t, :attrs {}, :content []}))
               )             
           )
    (testing "Params-path"
           ;;все с, имеющие указанные атрибуты
           (is (= (get-tag "c@[key=val]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )           
           ;;все тэги, имеющие указанные атрибуты
           (is (= (get-tag "*@[key=val,k=5]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )           
           ;;все тэги, имеющие указанные атрибуты
           (is (= (get-tag "*@[k=5]" tst-tag)
                  '({:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           ;;все тэги ниже а, имеющие указанные атрибуты
           (is (= (get-tag "a/*@[key=val]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )                      
           ;;дочерние тэги а
            (is (= (get-tag "a/." tst-tag)
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}))
               )
           ;;дочерние тэги тэга по указанному адресу
            (is (= (get-tag "a/b/." tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )            
           ;;дочерние тэги c тэгов ниже а, обладающих указанными атрибутами  
           (is (= (get-tag "a/*@[key=val]/c" tst-tag)
                  '({:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           ;;дочерние тэги c тэгов дочерних тэгов а, обладающих указанными атрибутами  
           (is (= (get-tag "a/.@[key=val]/c" tst-tag)
                  '())
               )

           )
    (testing "Asterisk-path"
           ;; все тэги с такими атрибутами.
           (is (= (get-tag "*@[key=val]" tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )
           ;; все тэги с такими атрибутами.
           (is (= (get-tag "$/*@[key=val]" tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )
           ;; аще ВСЕ
           (is (= (get-tag "$/*" tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]}
                     {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]} 
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]} 
                     {:tag :t, :attrs {}, :content []}))
               )
           ;; аще ВСЕ
           (is (= (get-tag "*" tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]}
                     {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]} 
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]} 
                     {:tag :t, :attrs {}, :content []}))
              )
            ;;все с на любом расстоянии от a
           (is (= (get-tag "a/*/c" tst-tag)
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           ;;дочерние тэги c тэгов тэгов через 1 уровень от а, обладающих указанными атрибутами  
           (is (= (get-tag "a/./.@[key=val]/c" tst-tag)
                  '({:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           ;;...  
           (is (= (get-tag "a/*@[key=val]/./t" tst-tag)
                  '({:tag :t, :attrs {}, :content []}))
               )           
           ;;root tag by default  
           (is (= (get-tag "." tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]}))
               )           
           ;;the same case as previous  
           (is (= (get-tag "$/." tst-tag)
                  '({:tag :a, :attrs {:key "val"}, :content [23 25 {:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}]}))
               )           
           ))
(run-tests)
