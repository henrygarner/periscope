(ns duxi.core)

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

(defn initializing
  [rf]
  (fn [coll]
    rf))

(defn conj
  [coll]
  (fn
    ([] (empty coll))
    ([acc x]
     (clojure.core/conj acc x))
    ([acc]
     (println (type acc))
     (if (list? acc)
       (reverse acc)
       acc))))


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


(defn with-xform
  "We can use xforms to augment our reducing function recipes"
  [rfr xform]
  (fn [coll]
    (xform (rfr coll))))

(def recipe
  (with-xform conj
    (comp (map inc)
          (filter even?))))


(def stateful-recipe
  (with-xform conj (take 2)))

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
  (duct (initializing minmax)
        #(with-xform conj (normalise %))))

(conduct normalise-recipe (range 11))

(conduct normalise-recipe (vec (range 11)))

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
;;   (letduct [min-max (initializing minmax)
;;             sd (initializing kixi/standard-deviation)
;;             normalized (with-xform conj (normalise min-max))]
;;      normalized))
;; 
;; (def normalise (conduct normaliser))
;; 
;; (def gradient-descent
;;   (loopduct [coefs [0.0 1 2.0]]
;;     (letduct [new-coefs (initializing improve-coefs)]
;;       (if-not (conveged? new-coefs coefs)
;;         (recur new-coefs)
;;         new-coefs))))
;; 
