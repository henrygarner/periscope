(ns duxi.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [duxi.ductors :refer [link]]))

(defn T
  "Variadic Thrush combinator"
  [& args]
  (fn [f]
    (apply f args)))

(defn duct
  ([xform rf]
   (T xform rf))
  ([rf]
   (T identity rf)))

(defmonad iter-m
  [m-bind (fn [duct f]
            (fn [f']
              ((f (duct f')) f')))
   m-result (fn [v]
              (fn [f']
                v))])

(defmacro letduct
  "Create a duct by binding ducts together"
  [specs & body]
  `(domonad iter-m ~specs ~@body))

(defn deduce
  ([duct f coll]
   ((duct f) coll))
  ([duct coll]
   (deduce duct identity coll)))

(defn duct->>
  [& ducts]
  (apply comp (clojure.core/map link ducts)))

(defn duct->
  [[f] & ducts]
  (apply comp (clojure.core/map f ducts)))
