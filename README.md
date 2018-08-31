# Ductors

[![CircleCI](https://circleci.com/gh/henrygarner/ductors/tree/master.svg?style=svg)](https://circleci.com/gh/henrygarner/ductors/tree/master)

_Experimental_

A Clojure library for specifying iterative or recursive transductions. These are called ductors, and are evaluated with `deduce`.

## Motivation

Transducers are fantastic. Different reducing contexts can make use of the same core transdcuers, proving their celebrated generality. However, all reducing contexts rely on reducing functions to actually perform useful work.

There's a bitter injustice in the heart of Clojure: `transduce` gives the impression that transducers and reducing functions deserve equal billing. Ductors belives that this is unfair. Transducers perform the work of humble adaptors which reconfigure the behaviour of the noble reducing function.

Not only that, but transducers have an additional unfair advantage: they're collaborative and are initialised with the next transducer in the chain. Compare to the reducing function which is expected to work miracles in isolation.

Ductors' `deduce` restores the reducing function's rightful place centre-stage and relegates transducers to the support act. Ductors fights for what's right: every reducing function is initialised with the collection over which it is reducing. This allows reducing functions some context and offers them much-needed support.

## License

Copyright © 2018 Henry Garner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
