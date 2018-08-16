(ns duxi.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [duxi.rfr :refer [>seq]]
            [kixi.stats.core :as kixi]
            [redux.core :as redux])
  (:refer-clojure :exclude [juxt identity]))

(defprotocol IDuct
  (unduct [this] "Returns the Duct"))

(deftype Duct [thing]
  IDuct
  (unduct [this] thing))

(defn conduct
  "Simples"
  [rfr coll]
  (loop [rfr rfr]
    (let [res (transduce clojure.core/identity (rfr coll) coll)]
      (if (satisfies? IDuct res)
        (recur (unduct res))
        res))))

(defn reductor
  "Transform a reducing function into a reductor.
  Optionally wrap in a transducer."
  ([rf]
   (fn [coll] rf))
  ([xform rf]
   (fn [coll]
     (xform rf))))

(defn xform
  "Wraps a reductor in a transducer"
  [xf rdtr]
  (fn [coll]
    (xf (rdtr coll))))

(defn constant
  "A reducing function which returns a constant value"
  [val]
  (completing
   (fn
     ([] nil)
     ([_ _] (reduced val)))))

(defn identity
  "A reductor which returns a constant value"
  [val]
  (fn [coll] (constant val)))

(defn post-complete
  [rf f]
  (completing rf #(f (rf %))))

(defmonad duct-m
  [m-bind (fn [mv f]
            (fn [coll]
              (post-complete (mv coll)
                             (fn [acc]
                               (Duct. (f acc))))))
   m-result identity])

(defmacro letduct
  [specs & body]
  `(domonad duct-m ~specs ~@body))

(defmacro defduct
  [name docstring specs & body]
  `(def ~name
     (letduct ~specs ~@body)))

(defn duct?
  [duct]
  (satisfies? IDuct duct))

(defn deduct
  [de]
  (cond-> de
    (duct? de) unduct))

(defn juxt
  "A juxt which works for reductors"
  [& rfrs]
  (fn [coll]
    (post-complete (apply redux/juxt (map #(% coll) rfrs))
                   (fn [accs]
                     (let [continue? (some duct? accs)
                           rfrs (mapv (fn [acc]
                                        (if (and continue? (not (duct? acc)))
                                          (identity acc)
                                          (deduct acc)))
                                      accs)]
                       (if continue?
                         (Duct. (apply juxt rfrs))
                         rfrs))))))


;; Example

(def sum
  "A summing reductor"
  (reductor +))

(conduct sum [2 4 4 4 5 5 5 7 9])

;;=> 45

(defduct normalise
  "Returns a normalising duct"
  [[mean sd] (reductor (redux/juxt kixi/mean kixi/standard-deviation))  
   normalised (xform (map #(/ (- % mean) sd)) >seq)]
  normalised)

(conduct normalise [2 4 4 4 5 5 5 7 9])

;;=> [-1.5 -0.5 -0.5 -0.5 0.0 0.0 0.0 1.0 2.0]


(conduct (juxt sum normalise) [2 4 4 4 5 5 5 7 9])

;;=> [45 [-1.5 -0.5 -0.5 -0.5 0.0 0.0 0.0 1.0 2.0]]
