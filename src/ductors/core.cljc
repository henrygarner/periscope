(ns ductors.core
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [vals key subseq map filter remove update get assoc] ))

(def vals
  (fn [handler]
    (fn
      ([coll]
       (handler (core/vals coll)))
      ([coll f]
       (persistent!
        (reduce-kv
         (fn [m k v] (assoc! m k (handler v f)))
         (transient (empty coll)) coll))))))

(def all
  (fn [handler]
    (fn
      ([coll]
       (let [xf (core/map handler)]
         (if (seq? coll)
           (sequence xf coll)
           (into (empty coll) xf coll))))
      ([coll f]
       (let [xf (core/map #(handler % f))]
         (if (seq? coll)
           (sequence xf coll)
           (into (empty coll) xf coll)))))))

(defn lens
  [getter setter]
  (fn [handler]
    (fn
      ([state]
       (handler (getter state)))
      ([state f]
       (setter state #(handler % f))))))

(defn in
  ([ks] (in ks nil))
  ([ks default]
   (lens (fn [state] (get-in state ks default))
         (fn [state f] (update-in state ks f)))))

(defn nth
  [n]
  (lens (fn [state]
          (core/nth state n))
        (fn [state f]
          (core/update state n f))))

(def first
  (nth 0))

(def second
  (nth 1))

(def last
  (lens core/last
        (fn [state f]
          (core/update state (dec (count state)) f))))

(defn get
  [lens state]
  ((lens identity) state))

(defn update
  [lens f state]
  ((lens thrush) state f))

(defn assoc
  [st v s]
  (update st (constantly v) s))
