(ns periscope.xforms
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [vals key subseq map filter remove update get assoc take last]))

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

(defn *>>
  "The periscope composition operator"
  [& xfs]
  (fn [handler]
    (let [xf (apply comp xfs)]
      (fn
        ([coll]
         (let [xf (comp xf (core/map handler))]
           (if (seq? coll)
             (sequence xf coll)
             (into (empty coll) xf coll))))
        ([coll f]
         (if (seq? coll)
           (seq (transduce* f xf (conj* handler) coll))
           (transduce* f xf (conj* handler) coll)))))))

(defn xform->pform
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
  (xform->pform core/filter
                (fn [rf acc x f pred]
                  (if (pred x)
                    (rf acc x f)
                    (rf acc x identity)))))

(def map
  (xform->pform core/map
                (fn [rf acc x f fx]
                  (rf acc (fx x) f))))

(def remove (comp filter complement))

(defn take [n]
  (fn [rf]
    (let [nv (volatile! n)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [n @nv
               nn (vswap! nv dec)
               result (if (pos? n)
                        (rf result input)
                        result)]
           (if (not (pos? nn))
             (ensure-reduced result)
             result)))
        ([result input f]
         (let [n @nv
               nn (if (= f identity) n (vswap! nv dec))]
           (if (pos? n)
             (rf result input f)
             (rf result input identity))))))))

(def last
  (fn [rf]
    (let [last (volatile! nil)
          buffer (volatile! [])
          fx (volatile! nil)]
      (fn
        ([] (rf))
        ([acc]
         (let [acc (if @last
                     (if @fx
                       (rf acc @last @fx)
                       (rf acc @last))
                     acc)
               acc (reduce (fn [acc x]
                             (rf acc x identity))
                           acc @buffer)]
           (rf acc)))
        ([acc x]
         (vreset! last x)
         acc)
        ([acc x f]
         (if (not= f identity)
           (let [acc (if @last (rf acc @last identity) acc)
                 acc (reduce (fn [acc x]
                               (rf acc x identity)) acc @buffer)]
             (vreset! buffer [])
             (vreset! last x)
             (vreset! fx f)
             acc)
           (do
             (vswap! buffer conj x)
             acc)))))))
