(ns periscope.core
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [nth first second last vals key subseq map filter remove update get assoc rest take drop butlast]))

(defn- thrush
  [state f]
  (f state))

(defn- sequence-preserving
  [xform coll]
  (if (seq? coll)
    (sequence xform coll)
    (into (empty coll) xform coll)))

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

(defn scope
  [getter setter]
  (fn [handler]
    (fn
      ([state]
       (handler (getter state)))
      ([state f]
       (setter state #(handler % f))))))

(defn take
  [n]
  (scope (fn [state]
           (if (seq? state)
             (core/take n state)
             (subvec state 0 n)))
         (fn [state f]
           (let [[as bs] (core/split-at n state)]
             (if (seq? state)
               (concat (core/map f as) bs)
               (-> (into (empty state) (core/map f) as)
                   (into bs)))))))

(defn drop
  [n]
  (scope (fn [state]
           (if (seq? state)
             (core/drop n state)
             (subvec state n (count state))))
         (fn [state f]
           (let [[as bs] (core/split-at n state)]
             (if (seq? state)
               (concat as (core/map f bs))
               (-> (into (empty state) as)
                   (into (core/map f) bs)))))))

(defn in
  ([ks] (in ks nil))
  ([ks default]
   (scope (fn [state] (get-in state ks default))
          (fn [state f] (update-in state ks f)))))

(defn nth
  [n]
  (scope (fn [state]
           (core/nth state n))
         (fn [state f]
           (if (seq? state)
             (let [[as [b & bs]] (core/split-at n state)]
               (-> (reduce conj!
                           (-> (reduce conj! (transient []) as)
                               (conj! (f b)))
                           bs)
                   (persistent!)
                   (seq)))
             (core/update state n f)))))

(def first
  (scope core/first
         (fn [state f]
           (if (seq? state)
             (cons (f (core/first state)) (core/rest state))
             (core/update state 0 f)))))

(def second
  (scope core/second
         (fn [state f]
           (if (seq? state)
             (let [nxt (core/next state)]
               (cons (core/first state)
                     (cons (f (core/first nxt))
                           (core/rest nxt))))
             (core/update state 1 f)))))

(def last
  (scope core/last
         (fn [state f]
           (if (seq? state)
             (loop [ret [] s state]
               (if (next s)
                 (recur (conj ret (core/first s)) (next s))
                 (seq (conj ret (f (core/first s))))))
             (core/update state (dec (count state)) f)))))

(def rest
  (scope (fn [state]
           (if (seq? state)
             (core/rest state)
             (into (empty state) (core/rest state))))
         (fn [state f]
           (cond-> (loop [ret (transient [(core/first state)]) s (core/rest state)]
                     (if (seq s)
                       (recur (conj! ret (f (core/first s))) (core/rest s))
                       (persistent! ret)))
             (seq? state)
             (seq)))))

(def butlast
  (scope (fn [state]
           (if (seq? state)
             (core/butlast state)
             (subvec state 0 (dec (count state)))))
         (fn [state f]
           (cond-> (loop [ret (transient []) s state]
                     (if (next s)
                       (recur (conj! ret (f (core/first s))) (next s))
                       (persistent! (conj! ret (core/first s)))))
             (seq? state)
             (seq)))))

(defn get
  [state scope]
  ((scope identity) state))

(defn update
  [state scope f & args]
  (let [f #(apply f % args)]
    ((scope thrush) state f)))

(defn assoc
  [state scope v]
  (update state scope (constantly v)))
