(ns ductors.xforms
  (:refer-clojure :exclude [assoc map keys vals]))

(def map clojure.core/map)

(defn map-keys
  [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc [k v]]
       (rf acc [(f k) v]))
      ([acc] (rf acc)))))

(defn map-vals
  [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc [k v]]
       (rf acc [k (f v)]))
      ([acc] (rf acc)))))

(defn keys
  [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc [k v]]
       (rf acc (f k)))
      ([acc] (rf acc)))))

(defn vals
  [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc [k v]]
       (rf acc (f v)))
      ([acc] (rf acc)))))
