(ns ductors.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [ductors.ductors :refer [make-ductor]]
            [clojure.core :as core])
  (:refer-clojure :exclude [vals key subseq map filter remove update get assoc] :as core))

(defn- thrush
  [state f]
  (f state))

(defn- conj*
  [handler]
  (fn
    ([] (conj))
    ([acc] (conj acc))
    ([acc x] (conj acc x))
    ([acc x f] (conj acc (handler x f)))))

(defn- transduce*
  [f xf rf coll]
  (let [rf (xf rf)]
    (rf (reduce (fn [acc x]
                  (rf acc x f))
                (rf)
                coll))))

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
       (if (seq? coll)
         (seq (transduce* f xf (conj* handler) coll))
         (transduce* f xf (conj* handler) coll))))))

(defn xform->lens
  [xf f]
  (fn [& args]
    (fn [rf]
      (let [rf' ((apply xf args) rf)]
        (fn
          ([] (rf'))
          ([acc] (rf' acc))
          ([acc x] (rf' acc x))
          ([acc x f']
           (apply f rf acc x f' args)))))))

(def filter
  (xform->lens core/filter
               (fn [rf acc x f pred]
                 (if (pred x)
                   (rf acc x f)
                   (rf acc x)))))

(def map
  (xform->lens core/map
               (fn [rf acc x f fx]
                 (rf acc (fx x) f))))

(def remove (comp filter complement))

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
