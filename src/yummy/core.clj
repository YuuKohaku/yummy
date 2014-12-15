(ns yummy.core
  (:use yummy.validation)
      (:use clojure.walk))

(defn yummy-expr [tagname attrs content]
  {:pre [(and (map? attrs) (vector? content) (every? yummy-valid? (vals attrs)))]}
  {:tag (keyword tagname) :attrs (keywordize-keys attrs) :content content}
    )

(println (yummy-expr "c" {"k" 46} ["sf"]))