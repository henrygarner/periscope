(ns periscope.core
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [nth first second last vals key subseq map filter remove update get assoc rest take drop butlast constantly]))

(set! clojure.core/*warn-on-reflection* true)

(defn- thrush
  [state f]
  (f state))

(defn- sequence-preserving
  [xform coll]
  (if (seq? coll)
    (sequence xform coll)
    (into (empty coll) xform coll)))

(defprotocol PMapMapper
  (map-vals [this f])
  (map-keys [this f]))

(extend-protocol PMapMapper
  nil
  (map-vals [coll f] nil)
  (map-keys [coll f] nil)
  clojure.lang.PersistentArrayMap
  (map-vals [^clojure.lang.PersistentArrayMap coll f]
    (let [k-it (.keyIterator coll)
          v-it (.valIterator coll)
          array (object-array (* 2 (.count coll)))]
      (loop [i 0]
        (if (.hasNext k-it)
          (let [k (.next k-it)
                v (.next v-it)
                v' (f v)]
            (aset array i k)
            (aset array (inc i) v')
            (recur (+ i 2)))))
      (clojure.lang.PersistentArrayMap. array)))
  (map-keys [^clojure.lang.PersistentArrayMap coll f]
    (let [k-it (.keyIterator coll)
          v-it (.valIterator coll)
          array (object-array (* 2 (.count coll)))]
      (loop [i 0]
        (if (.hasNext k-it)
          (let [k (.next k-it)
                v (.next v-it)
                k' (f k)]
            (aset array i k')
            (aset array (inc i) v)
            (recur (+ i 2)))))
      (clojure.lang.PersistentArrayMap. array)))
  clojure.lang.PersistentHashMap
  (map-vals [^clojure.lang.PersistentHashMap coll f]
    (let [coll' (transient clojure.lang.PersistentHashMap/EMPTY)]
      (-> (reduce-kv (fn [m k v] (assoc! m k (f v))) coll' coll)
          (persistent!))))
  (map-keys [^clojure.lang.PersistentHashMap coll f]
    (let [coll' (transient clojure.lang.PersistentHashMap/EMPTY)]
      (-> (reduce-kv (fn [m k v] (assoc! m (f k) v)) coll' coll)
          (persistent!))))
  Object
  (map-vals [coll f]
    (let [coll' (empty coll)]
      (reduce-kv (fn [m k v] (assoc! m k (f v))) coll' coll)))
  (map-keys [coll f]
    (let [coll' (empty coll)]
      (reduce-kv (fn [m k v] (assoc! m (f k) v)) coll' coll))))

(def vals
  (fn [handler]
    (fn
      ([coll]
       (handler (core/vals coll)))
      ([coll f]
       (map-vals coll #(handler % f))))))

(def keys
  (fn [handler]
    (fn
      ([coll]
       (handler (core/keys coll)))
      ([coll f]
       (map-keys coll #(handler % f))))))

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

(defmacro in
  [ks]
  `(fn* ~'[handler]
        (fn* (~'[state]
              ~(reduce (fn [coll k]
                         (list `core/get coll k))
                       'state
                       ks))
             (~'[state f]
              ~(let [syms (into ['state] (repeatedly (count ks) gensym))
                     symsr (reverse syms)]
                 `(let ~(vec (reduce (fn [coll [[sa sb] k]]
                                       (conj (conj coll sb) (list `core/get sa k)))
                                     []
                                     (core/map vector (partition 2 1 syms) ks)))
                    ~(reduce (fn [coll [s k]]
                               (list `core/assoc s k coll))
                             (list 'handler (core/first symsr) 'f)
                             (core/map vector (core/rest symsr) (reverse ks)))))))))

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

(defn constantly [v]
  (fn
    ([] v)
    ([a] v)
    ([a b] v)
    ([a b c] v)
    ([a b c d] v)
    ([a b c d e] v)
    ([a b c d e f] v)
    ([a b c d e f g] v)
    ([a b c d e f g h] v)
    ([a b c d e f g h i] v)
    ([a b c d e f g h i j] v)
    ([a b c d e f g h i j & ks] v)))

(defn get
  [state scope]
  ((scope identity) state))

(defn update
  ([state scope f]
   ((scope thrush) state f))
  ([state scope f a]
   ((scope thrush) state #(f % a)))
  ([state scope f a b]
   ((scope thrush) state #(f % a b)))
  ([state scope f a b c]
   ((scope thrush) state #(f % a b c)))
  ([state scope f a b c d]
   ((scope thrush) state #(f % a b c d)))
  ([state scope f a b c d e]
   ((scope thrush) state #(f % a b c d e)))
  ([state scope f a b c d e & args]
   ((scope thrush) state #(apply f % a b c d e args))))

(defn assoc
  [state scope v]
  ((scope thrush) state (constantly v)))
