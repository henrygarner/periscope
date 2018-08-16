(ns duxi.core
  (:require [clojure.algo.monads :as monad :refer [defmonad domonad]]
            [duxi.rfr :refer [>seq]]
            [kixi.stats.core :as kixi]
            [redux.core :as redux])
  (:refer-clojure :exclude [juxt identity transduce]))

(deftype Unfinished [thing]
  clojure.lang.IDeref
  (deref [this] thing))

(defn unfinished?
  "Is there more to do?"
  [x]
  (instance? Unfinished x))

(defn unfinished
  "Mark a return value as a pending computation"
  [x]
  (Unfinished. x))

(defn transduce
  "Like clojure.core/transduce, but will short-cut a reduced? init value"
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
   (cond (reduced? init) (unreduced init)
         :else (clojure.core/transduce xform f init coll))))

(defn conduct
  "Simples"
  [reductor coll]
  (loop [rdtr reductor]
    (let [result (transduce clojure.core/identity (rdtr coll) coll)]
      (if (unfinished? result)
        (recur @result)
        result))))

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
  [xf reductor]
  (fn [coll]
    (xf (reductor coll))))

(defn constant
  "A reducing function which returns a constant value"
  [val]
  (completing
   (fn
     ([] (reduced val)))))

(defn identity
  "A reductor which returns a constant value"
  [val]
  (fn [coll] (constant val)))

(defn post-complete
  "Chain a completing function after rf completion"
  [rf f]
  (completing rf #(f (rf %))))

(defn post-reduce
  "Chain a function accepting the return value of a reductor
  to execute after the reductor has finished."
  [reductor f]
  (fn [coll]
    (post-complete (reductor coll) f)))

(defmonad duct-m
  [m-bind (fn [mv f] (post-reduce mv (comp unfinished f)))
   m-result identity])

(defmacro letduct
  "Create a reductor by chaining reductors together"
  [specs & body]
  `(domonad duct-m ~specs ~@body))

(defmacro defduct
  "Define a reductor by optionally chaining previous reductor steps"
  [name docstring specs & body]
  `(def ~name
     (letduct ~specs ~@body)))

(defn juxt
  "Takes a set of reductors and returns a reductor that is the
  juxtaposition of those reductors. Returns a vector containing the
  result of applying each reductor to the collection (left-to-right).
  (conduct (juxt a b) coll) => [(conduct a coll) (conduct b coll)]"
  [& reductors]
  (fn [coll]
    (post-complete (apply redux/juxt (map #(% coll) reductors))
                   (fn [results]
                     (if (some unfinished? results)
                       (let [reductors (map (fn [result]
                                              (if (unfinished? result)
                                                (deref result)
                                                (identity result)))
                                            results)]
                         (unfinished (apply juxt reductors)))
                       results)))))

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
