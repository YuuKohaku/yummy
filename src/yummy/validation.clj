(ns yummy.validation)

(declare yummy-valid?)

;; Determine if exp is an atomic type
(defn yummy-atomic? [exp]
  (or
    (string? exp)
    (integer? exp)
    (float? exp)
    (true? exp)
    (false? exp)
    (nil? exp)
    )
  )

;; Determine if exp is yummy-object
(defn yummy-object? [exp]
  (and
    (map? exp)
    (keyword? (exp :tag))
    (map? (exp :attrs))
    (vector? (exp :content))
    (every? yummy-valid? (exp :content))
    (every? keyword? (keys (exp :attrs)))
    (every? string? (vals (exp :attrs)))
  )
)

;; Determine if exp is an array of valid expressions
(defn yummy-array?   [exp]
  (and
    (vector? exp)
    (every? yummy-valid? exp)
  )
)

;; Determine if exp is valid yummy expression
(defn yummy-valid? [expr]
  (or
    (yummy-atomic? expr)
    (yummy-array? expr)
    (yummy-object? expr)
  )
)


