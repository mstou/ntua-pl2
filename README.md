# Programming Languages II, ECE NTUA

This repository contains my solutions to the Programming Languages II
[course assignments](https://courses.softlab.ntua.gr/pl2/2019b/).
A brief summary of each exercise follows.

* `vm`: Implemented an __indirectly threaded virtual
machine__ for `Befunge-93` (a BrainFuck like language).

* `gc`: Implemented a __garbage collector__ for an extension
of `Befunge-93` that also supports linked lists. We
implemented the __Mark & Sweep__ algorithm on top of
the virtual machine we built for the previous exercise

* `haskell-dp`: Solved an algorithmic problem using
__Dynamic Programming in Haskell__ both in a pure and an impure way.

* `quickcheck`: __Property based testing__ in Haskell
using QuickCheck. We implemented a method to produce
random trees and wrote some properties to test BFS, DFS
and other graph algorithms.

* `type-inference`: Implemented __Type Inference for
Lambda-Calculus__ with simple types, using the [Algorithm W](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W).

* `parallel-hs`: Experimented with __Parallelism and Concurrency in Haskell__ using the Par Monad and MVars

* `typesys`: Defined a __Type System__ for a simple
stack machine language

* `densem`: Implemented the __Denotational Semantics__
for the stack machine language defined in the previous
exercise

* `scripting`: Experimented with scripting languages.
We implemented a client and plays an online game and a
server that hosts the game.

* `ax-sem`: Experimented with __Axiomatic Semantics and
Program Verification__. We proved the correctness
of a given algorithm using the __Frama-C__ library.
