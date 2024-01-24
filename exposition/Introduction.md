# What is this? How does it work?

This is a series of articles aiming to explain effective algebraic topology. It will assume some functional programming experience, and basic mathematics (fundamentals of linear algebra, the definition of groups, etc). Other than that, I'll try to explain everything as I go.

## What is algebraic topology?

You have no doubt heard of topology as the part of mathematics that deals with general shapes of objects. A mug and a doughnut, that sort of stuff. Now algebraic topology tries to assign different algebraic structures to geometric objects, such as vector spaces and groups, so that by calculating the algebraic structures, we can uncover topological properties of the objects.

Some of the most important structures are homology and homotopy groups. These two, in intricately connected but complementary ways, measure how many "holes" there are in a space. They will both tell you that the figure "8" has two holes, and we shall see exactly how in this series. A lot of other topological attributes, for example the Euler characteristics and connectedness, can be seen as an incomplete reflection of homology and homotopy.

## What is effective algebraic topology?

Here, "effective" is short for "effectively computable". For example, you can ask the program to compute $H_n (\mathbb{RP}^3)$, and it will immediately tell you that it is $\mathbb Z, \mathbb Z/2 \mathbb Z, 0, \mathbb Z$. Homotopy groups are slightly difficult, but we can still manage to reach interesting results such as $\pi_4 (\mathbb S^3) = \mathbb Z / 2 \mathbb Z$ and $\pi_6 (\mathbb S^3) = \mathbb Z / 12 \mathbb Z$.

All is not rosy, though. There is a famous undecidable problem called [the word problem](https://en.wikipedia.org/wiki/Word_problem_(mathematics)). This directly blocks the possibility of algorithmically determining whether a space is simply connected. Broadly speaking, this barrier applies when we are in the non-commutative realm. So as long as we deal with commutative groups, the problem is gone. Therefore a lot of theorems and algorithms will have some preconditions that specifically excludes this case. Fortunately, the homotopy groups except in dimension 1, and all the homology groups are commutative, so we can still do a lot.

## Roadmap

- [**Homology groups**](Homology%20Groups.md): The fundamentals of homology and how we can compute a few homology groups by hand.
- **The Smith decomposition**: The concrete algorithmic details of Smith decomposition. This section is completely independent and doesn't concern algebraic topology at all. You can skip it since Smith decomposition is available in a lot of libraries.
- **Simplicial sets and chain complexes**: The framework of representing spaces in computers and computing homology groups. We construct a few spaces and see 
- **Chain reductions and perturbations**: How to tame infinite simplicial sets and produce something computable. This technique also reduces the amount of work dramatically sometimes.
- **Discrete vector fields**:
- **The bar construction**:
- **Product and twisted product**:
- **Loop spaces**:
