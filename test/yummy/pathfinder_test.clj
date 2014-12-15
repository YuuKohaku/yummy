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
           (is (= (get-tag "b/c" tst-tag) 
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           (is (= (get-tag "$/a/b/c" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           (is (= (get-tag "t" tst-tag)
                  '({:tag :t, :attrs {}, :content []}))
               )
           (is (= (get-tag "/c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           (is (= (get-tag "c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :c, :attrs {}, :content [58]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           )
  (testing "Parent-path"
           (is (= (get-tag "$/a/b/d/../c" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )           
           (is (= (get-tag "a/b/d/../../c" tst-tag) 
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]}))
               )       
           )
  (testing "Asterisk-path"
           (is (= (get-tag "b/*/c" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )
           (is (= (get-tag "b/*/*/c" tst-tag)
                  '({:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )             
           (is (= (get-tag "a/*/*/*/c" tst-tag)
                  '({:tag :c, :attrs {}, :content [58]}
                     {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}))
               )             
           )
    (testing "Params-path"
           (is (= (get-tag "c@[key=val]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )           
           (is (= (get-tag "*@[key=val,k=5]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )           
           (is (= (get-tag "*@[k=5]" tst-tag)
                  '({:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]} 
                     {:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}))
               )
           (is (= (get-tag "a/*@[key=val]" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c}
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )                      
            (is (= (get-tag "a/*" tst-tag)
                  '({:tag :c, :attrs {}, :content [65 {:tag :e, :attrs {:k "5"}, :content ["branch" {:tag :c, :attrs {}, :content [58]}]}]} 
                     {:tag :b, :attrs {}, :content [{:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}]}))
               )
            (is (= (get-tag "a/b/*" tst-tag)
                  '({:content [2 3], :attrs {:key "val", :k "5"}, :tag :c} 
                     {:tag :d, :attrs {:key "val"}, :content [2 3 {:tag :c, :attrs {}, :content [85 {:tag :t, :attrs {}, :content []}]}]}))
               )            
           ))
(run-tests)
