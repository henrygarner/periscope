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

(conduce duxi.rfr/conj '(1 2 3 4))

;;=> (1 2 3 4)
```

## Usage

Conducts reducing function recipes. `conduce` executes the Conduct. Once executed, a Conduct may return another Conduct. In this way Conducts can specify self-contained recipes for iterative transductions.

For example, binning and normalizing data are difficult or impossible to achieve in a single pass with `transduce`. With `conduce`, however, they can be expressed as simple operations:

```clojure
(conduce (bin 2) [1 4 3 2 5 6])
;;=> (0 1 0 0 1 1)

(conduce normalize [0 5 10])
;;=> (-1.0 0.0 1.0)
  ```

## License

Copyright © 2018 Henry Garner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
