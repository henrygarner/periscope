# Duxi

A Clojure library for specifying and executing Conducts.

## Usage

Conducts are xform & rf pairs. `conduce` executes the Conduct. Once executed, a Conduct may return another Conduct. In this way Conducts can specify self-contained recipes for iterative transductions.

For example, binning and normalizing data are difficult or impossible to achieve in a single pass with `transduce`. With `conduce`, however, they can be expressed as simple operations:

```clojure
(conduce (bin 2) [1 4 3 2 5 6])
;;=> (0 1 0 0 1 1)

(conduce normalize [0 5 10])
;;=> (-1.0 0.0 1.0)
  ```

## License

Copyright © 2017 Henry Garner

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
