# unify-test

Testing my own unification implementation against
[core.unify](https://github.com/clojure/core.unify). It loosely follows the
abstract first-order unification algorithm outlined by Robinson
([Robinson 1965](http://aitopics.org/sites/default/files/classic/Machine%20Intelligence%206/MI6-Ch4-Robinson.pdf)).
Most of the unit tests included in core.unify have been included here.

## Results

Id          nCalls  Min      Max    MAD     Mean     Time%  Time
mine        1,000   159.0μs  3.0ms  63.0μs  207.0μs  58     207.0ms
core.unify  1,000    99.0μs  3.0ms  30.0μs  121.0μs  34     121.0ms

## License

Copyright © 2014 Jeb Beich

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
