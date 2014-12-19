(ns yummy.serialization
  (:use yummy.validation)
  (require [clojure.edn :as edn]))

(defn serialize [exp]
  {:pre [(yummy-valid? exp)]}
  (pr-str exp)
)

(defn deserialize [exp]
  {:post [(yummy-valid? exp)]}
  (binding [*read-eval* false]
    (edn/read-string exp)
   )
)
