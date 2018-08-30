(ns duxi.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [duxi.ductors :refer [make-ductor]]))

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

(defn conditionally
  [f]
  (fn [state x]
    (cond-> x state f)))

(defn deduce
  ([duct f coll]
   ((duct (conditionally f)) true coll))
  ([duct coll]
   (deduce duct identity coll)))

(defn ductor
  [& ducts]
  (apply comp (clojure.core/map make-ductor ducts)))
