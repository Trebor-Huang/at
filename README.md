Effective Algebraic Topology
==
This is a fork of [AT](https://github.com/mvr/at), a Haskell rewrite of
[Kenzo](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/), a
collection of algorithms for 'effective algebraic topology'.
See the links for more details and great references.

Playing around with this code is the only chance I have of
understanding it!

Examples
--------
See the `examples/` folder.

Plan
----

- Tweak stuff and see what happens (get faster?)
  - [X] I changed the free abelian group implementation to use `Data.Map.Lazy`, and it did get a bit faster on my computer.
- Write a series of accessible articles about it (where do I put it?)
- Dependent types?
- Take on some unfinished plans of the original repo
  - [ ] inverse of the Hurewicz map
- Try to modularize stuff
  - [X] stop exposing `Combination` internals

References
----------

Will contain some good references not included in the original repo.
