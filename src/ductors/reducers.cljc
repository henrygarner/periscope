(ns ductors.reducers
  (:refer-clojure :exclude [assoc concat]))

(def assoc
  (fn
    ([] {})
    ([acc [k v]]
     (clojure.core/assoc acc k v))
    ([acc] acc)))

(def concat
  (fn
    ([] [])
    ([acc x] (clojure.core/concat acc x))
    ([acc] acc)))
