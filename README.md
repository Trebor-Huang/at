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
  - [X] Changed the representation of degeneracies.
  - [X] Changed the free abelian group implementation to use `Data.Map.Lazy`, and it did get a bit faster on my computer.
  - [ ] Faster smith normal form
- Write a series of accessible articles about it (where do I put it?)
- Dependent types?
- Take on some unfinished plans of the original repo
- Try to modularize stuff
  - [X] stop exposing `Combination` internals

References
----------

- [Discrete Vector Fields and Fundamental Algebraic Topology](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Vector-Fields.pdf). Book covering a lot of effective homology stuff, in particular DVFs.
- [Francis Sergeraert's Talks](https://www-fourier.univ-grenoble-alpes.fr/~sergerar/Talks/).
- [Constructive Homological Algebra and Applications](https://arxiv.org/abs/1208.3816).
