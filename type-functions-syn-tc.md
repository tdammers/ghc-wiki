# Type Checking with Indexed Type Synonyms


>
>
> **This is OLD and OUT OF DATE material.**
>
>


GHC has now F<sub>C</sub> as its typed intermediate language.
In a next step, we wish to add type functions to
GHC's source language.  Type functions in combination
with type annotations and GADTs allow us to type check
some interesting programs.


```wiki
data Zero
data Succ n
data List a n where
  Nil  :: List a Zero
  Cons :: a -> List a m -> List a (Succ m)

type family Add :: * -> * -> *
type instance Add Zero     y = y
type instance Add (Succ x) y = Succ (Add x y)

append :: List a l -> List a m -> List a (Add l m)
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)
```


However, type checking with type functions is challenging.


- [The challenge](type-functions-syn-tc/challenge)
- [A first (naive) attempt](type-functions-syn-tc/naive)
- [A second attempt](type-functions-syn-tc/second)
- [Type equations in GHC](type-functions-syn-tc/ghc)
- [Plan MS](type-functions-syn-tc/plan-ms)
- [Plan MS revised](type-functions-syn-tc/plan-ms-revised)
- [Plan MS revised again](type-functions-syn-tc/plan-ms-revised2)
- [Brief comparison](type-functions-syn-tc/comparison)
- [CHR-style simplification for GHC](type-functions-syn-tc/ghc-chr)
- [Examples](type-functions-syn-tc/ghc-chr-examples)
