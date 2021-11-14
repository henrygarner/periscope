(ns ductors.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [ductors.ductors :refer [make-ductor]]
            [clojure.core :as core])
  (:refer-clojure :exclude [vals key subseq map filter remove update get assoc] :as core))

(defn- thrush
  [state f]
  (f state))

(defn- conj*
  [handler f]
  (fn
    ([] (conj))
    ([acc] (conj acc))
    ([acc x] (conj acc x))
    ([acc x f] (conj acc (handler x f)))))

(defn- sequence*
  [xf coll handler f]
  (let [rf (xf (conj* handler f))]
    (if (seq? coll)
      (seq (rf (reduce (fn [acc x]
                         (rf acc x f))
                       (rf)
                       coll)))
      (rf (reduce (fn [acc x]
                    (rf acc x f))
                  (rf)
                  coll)))))

(defn duct
  "A duct is a lens over a transducer"
  [xf]
  (fn [handler]
    (fn
      ([coll]
       (let [xf (comp xf (core/map handler))]
         (if (seq? coll)
           (sequence xf coll)
           (into (empty coll) xf coll))))
      ([coll f]
       (sequence* xf coll handler f)))))

(defn filter
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc x]
       (if (pred x)
         (rf acc x)
         acc))
      ([acc x f]
       (if (pred x)
         (rf acc x f)
         (rf acc x)))
      ([acc] (rf acc)))))

(def remove (comp filter complement))

(defn map
  [fx]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc x]
       (rf acc (fx x)))
      ([acc x f]
       (rf acc (fx x) f))
      ([acc] (rf acc)))))

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

(defn get
  [lens state]
  ((lens identity) state))

(defn update
  [lens f state]
  ((lens thrush) state f))

(defn assoc
  [st v s]
  (update st (constantly v) s))
