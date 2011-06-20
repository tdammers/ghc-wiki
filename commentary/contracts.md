# Contracts for Haskell


## Involved


- Simon Peyton-Jones
- Dimitrios Vytiniotis
- Koen Claessen
- Charles-Pierre Astolfi

## Overview



Contracts, just as types, give a specification of the arguments and return values of a function. For example we can give to head the following contract:


```wiki
head ::: {x | not (null x)} -> Ok
```


Where Ok means that the result of head is not an error/exception as long as the argument isn't.



Any Haskell boolean expression can be used in a contract, for example


```wiki
fac ::: a:Ok -> {x | x >= a}
```


is a contract that means that for every a which is an actual integer (not an error), then fac a \>= a



We can also use a higher-order contracts:


```wiki
map ::: ({x | x >= 0} -> {x | x > 0}) -> {xs | not (null xs)} -> Ok
```


This contract means that if we apply map to a non-empty list with a function that takes a non-negative integer and returns an positive integer then map returns a list of values without errors.



For a formal introduction, one can read \[1\].


## The plan



Verifying that a function satisfies a given contract is obviously undecidable, but that does not mean that we can't prove anything interesting. Our plan is to translate Haskell programs to first-order logic (with equality) and then use Koen's automated theorem prover to check contract satisfaction. Given that first-order logic is only semi-decidable, the theorem prover can (and in fact does) hang when fed with contracts that are in contradiction with the function definition.


## Current status



The current status is described in \[3\] and some code and examples can be found in \[2\]. Note that given it's just a prototype the input syntax is slightly different from Haskell. In the end, we should get a ghc extension for contracts.


## Questions


- Do we need cfness predicate anymore? It was important in the POPL paper but is still relevant?
- UNR should be renamed to a less confusing name.
- Hoare logic vs liquid types
- Semantics & domain theory to prove the correctness of the translation
- Unfolding for proving contracts on recursive functions

## References



\[1\] : [
http://research.microsoft.com/en-us/um/people/simonpj/papers/verify/index.htm](http://research.microsoft.com/en-us/um/people/simonpj/papers/verify/index.htm) 

\[2\] : [
https://github.com/cpa/haskellcontracts](https://github.com/cpa/haskellcontracts) and [
https://github.com/cpa/haskellcontracts-examples](https://github.com/cpa/haskellcontracts-examples) 

\[3\] : [
https://github.com/cpa/haskellcontracts/blob/master/draft2.pdf](https://github.com/cpa/haskellcontracts/blob/master/draft2.pdf)


