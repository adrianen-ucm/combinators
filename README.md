# Combinators

A way for [defining combinators](./src/Comb.hs) like [this](./src/SKI.hs) and some simple utilities for searching among them.

This is a GHCi example session used to solve the [Exercise 5](https://github.com/adrianen-ucm/formal-methods-exercises/blob/main/untyped-lambda-calculus/untyped-lambda-calculus.pdf):

```haskell
λ> :set -XTypeApplications
λ> import Equation
λ> import Search
λ> import BM

λ> take 1 $ search @BM 100 $ \e -> e :=: App e e
[(M) (((B) (M)) (M))]
```
