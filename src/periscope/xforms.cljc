(ns periscope.xforms
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [vals key subseq map filter remove update get assoc take drop last butlast cat]))

(defn- conj*
  [handler]
  (fn
    ([] (conj))
    ([acc] (conj acc))
    ([acc x] (conj acc x))
    ([acc x f] (conj acc (handler x (or f identity))))))

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
                    (rf acc x nil)))))

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
               nn (if f (vswap! nv dec) n)]
           (if (pos? n)
             (rf result input f)
             (rf result input nil))))))))

(defn drop [n]
  (fn [rf]
    (let [nv (volatile! n)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [n @nv]
           (vswap! nv dec)
           (if (pos? n)
             result
             (rf result input))))
        ([result input f]
         (let [n @nv]
           (vswap! nv dec)
           (if (pos? n)
             (rf result input nil)
             (rf result input f))))))))

(defn butlast
  [rf]
  (let [prev-x (volatile! ::none)
        prev-f (volatile! nil)]
    (fn
      ([] (rf))
      ([acc]
       (if @prev-f
         (rf (rf acc @prev-x nil))
         (rf acc)))
      ([acc x]
       (let [result (if (not= @prev-x ::none)
                      (rf acc @prev-x)
                      acc)]
         (vreset! prev-x x)
         result))
      ([acc x f]
       (let [result (if (not= @prev-x ::none)
                      (rf acc @prev-x @prev-f)
                      acc)]
         (vreset! prev-x x)
         (vreset! prev-f f)
         result)))))

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
                             (rf acc x nil))
                           acc @buffer)]
           (rf acc)))
        ([acc x]
         (vreset! last x)
         acc)
        ([acc x f]
         (if f
           (let [acc (if @last (rf acc @last nil) acc)
                 acc (reduce (fn [acc x]
                               (rf acc x nil)) acc @buffer)]
             (vreset! buffer [])
             (vreset! last x)
             (vreset! fx f)
             acc)
           (do
             (vswap! buffer conj x)
             acc)))))))

(defn cat
  [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (reduce rf result input))
    ([result input f]
     (reduce #(rf %1 %2 f) result input))))
