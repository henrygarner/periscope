(ns duxi.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [duxi.rfr :refer [>seq]]
            [kixi.stats.core :as kixi]
            [redux.core :as redux]))

(defprotocol IDuct
  (duct [this] "Returns the Duct"))

(deftype Duct [thing]
  IDuct
  (duct [this] thing))

#_(defn ducts
  "Chains a duct with a sequence of duct generators"
  [duct & ducts]
  (let [rf' (reduce
             (fn [a b]
               (fn [acc]
                 (let [duct (b acc)]
                   (update duct :rf #(completing %
                                                 (fn [acc]
                                                   (a (% acc))))))))
             (reverse ducts))]
    (update duct :rf (fn [rf] (completing rf (fn [acc] (rf' (rf acc))))))))

#_(defrecord Conduct [xform rf])

#_(defn unconduced? [x]
  (instance? Conduct x))

#_(def conduced? (complement unconduced?))

(defn conduct
  "Simples"
  [rfr coll]
  (loop [rfr rfr]
    (let [res (transduce identity (rfr coll) coll)]
      (if (satisfies? IDuct res)
        (recur (duct res))
        res))))

(defn rf
  [rf]
  (fn [coll]
    rf))


(def minmax
  (fn
    ([] [nil nil])
    ([[min max] x]
     (vector (clojure.core/min (or min x) x)
             (clojure.core/max (or max x) x)))
    ([acc] acc)))

(defn normalise
  [[min max]]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc x]
       (rf acc (double
                (/ (- x min)
                   (- max min)))))
      ([acc]
       (rf acc)))))


(defn xform
  "We can use xforms to augment our reducing function recipes"
  [xf rfr]
  (fn [coll]
    (xf (rfr coll))))

(def recipe
  (xform (comp (map inc)
               (filter even?))
         >seq))


(def stateful-recipe
  (xform (take 2) >seq))

(conduct recipe '(1 2 3 4))

(conduct stateful-recipe '(1 2 3 4 5))
(conduct stateful-recipe '(1 2 3 4 5))

(defn connector
  [a b]
  (fn [coll]
    (let [rf (a coll)]
      (completing rf
                  (fn [acc]
                    (Duct. (b (rf acc))))))))

(defn duct
  [& rfrs]
  (reduce connector rfrs))

(def normalise-recipe
  (duct (rf minmax)
        #(xform (normalise %) >seq)))

(conduct normalise-recipe (range 11))

(conduct normalise-recipe (vec (range 11)))

(domonad monad/identity-m
         [a 1
          b 2]
         (+ a b))

(defn post-complete
  [rf f]
  (completing rf #(f (rf %))))

(defmonad duct-m
  [m-bind (fn [mv f]
            (fn [coll]
              (post-complete (mv coll)
                             (fn [acc]
                               (Duct. (f acc))))))
   m-result (fn [v]
              (fn [coll]
                (fn
                  ([] nil)
                  ([_ _] (reduced v))
                  ([acc] acc))))])

(defmacro letduct
  [specs & body]
  `(domonad duct-m ~specs ~@body))


(require '[duxi.core :refer [letduct conduct]]
         '[kixi.stats.core :as kixi]
         '[redux.core :as redux])

(defn normalize [mean sd]
  (fn [x]
    (/ (- x mean) sd)))

(def duct
  "Returns a normalising duct"
  (letduct [;; First calculate the mean and standard deviation of inputs
            [mean sd] (rf (redux/juxt kixi/mean kixi/standard-deviation))
            ;; Then output the normalized inputs to a seq
            res (xform (map (normalize mean sd)) >seq)]
    res))

;; Actually execute the duct:
(conduct duct [2 4 4 4 5 5 5 7 9])
;;=> [-1.5 -0.5 -0.5 -0.5 0.0 0.0 0.0 1.0 2.0]




;;;;;;; API IDEAS 
;; 
;; (defmacro letduct
;;   [specs & body]
;;   ...)
;;
;; (defmacro loopduct
;;   [specs & body]
;;    ...)
;; 
;; (def normaliser
;;   (letduct [min-max (rf minmax)
;;             sd (rf kixi/standard-deviation)
;;             normalized (xform conj (normalise min-max))]
;;      normalized))
;; 
;; (def normalise (conduct normaliser))
;; 
;; (def gradient-descent
;;   (loopduct [coefs [0.0 1 2.0]]
;;     (letduct [new-coefs (rf improve-coefs)]
;;       (if-not (conveged? new-coefs coefs)
;;         (recur new-coefs)
;;         new-coefs))))
;; 
