# Duxi

_Experimental_

A Clojure library for specifying and executing Conducts.

## Motivation

Transducers are fantastic. Different reducing contexts can make use of the same core transdcuers, proving their celebrated generality. However, all reducing contexts rely on reducing functions to actually perform useful work.

There's a bitter injustice in the heart of Clojure: `transduce` gives the impression that transducers and reducing functions deserve equal billing. Duxi belives that this is unfair. Transducers perform the work of humble adaptors which reconfigure the behaviour of the noble reducing function.

Not only that, but transducers have an additional unfair advantage: they're collaborative and are initialised with the next transducer in the chain. Compare to the reducing function which is expected to work miracles in isolation.

Duxi's `conduce` restores the reducing function's rightful place centre-stage and relegates transducers to the support act. Duxi fights for what's right: every reducing function is initialised with the collection over which it is reducing. This allows reducing functions some context and offers them much-needed support.

## Example

Considering the following irritation:

```clojure
;; List in, vector out:

(transduce identity conj '(1 2 3 4))

;;=> [1 2 3 4]
```

With duxi, we can define an alternative conj which preserves input and output types:

```clojure
;; List in, list out:

(conduce duxi.rfr/>seq '(1 2 3 4))

;;=> (1 2 3 4)

;; or vector in, vector out:

(conduce duxi.rfr/>seq [1 2 3 4])

;;=> [1 2 3 4]
```

In addition, Duxi defines a `letduct` macro which simplifies the process of specifying iterative algorithms:

```clojure
(require '[duxi.core :refer [letduct conduct xform rf]]
	 '[duxi.rfr :refer [>seq]]
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
```

## License

Copyright © 2018 Henry Garner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
