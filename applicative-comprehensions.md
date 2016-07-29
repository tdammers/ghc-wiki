
Applicative Comprehensions



This is a proposal to add support to GHC for desugaring certain instances of comprehensions into Applicative expression where possible. This feature is related to both [ApplicativeDo](applicative-do) and [MonadComprehensions](monad-comprehensions).


## Summary



The `ApplicativeComprehensions` language extension would cause GHC to attempt to desugar comprehensions using a similar approach to `MonadComprehensions`, but in a way that only results in code with an `Applicative` constraint.



Any comprehension that includes only generator expressions where the bindings defined by a generator expression are not used in any subsequent generator expression can be desugared using only operations from the `Applicative` typeclass, so an expression like


```wiki
[ expr_0 | pat_1 <- expr_1
         , pat_2 <- expr_2
         , ...
         , pat_n <- expr_n
         ]
```


can be desugared to


```wiki
(\ pat_1 pat_2 ... pat_n ->
     pure expr_0) <$> expr_1
                  <*> expr_2
                  <*> ...
                  <*> expr_n
```

## Motivation



As explained in the *Motivation* section for [ApplicativeDo](applicative-do), some kinds of `Applicative` code can be obscure or difficult to read. The example given there is


```wiki
(\x y z -> x*y + y*z + z*x) <$> expr1 <*> expr2 <*> expr3
```


This code, when translated into an equivalent `Applicative` comprehension, becomes


```wiki
[ x*y + y*z + z*x | x <- expr1, y <- expr2, z <- expr3 ]
```


which has fewer operators and clearer structure, but still bears a strong resemblance to the desugared code, especially when compared with the equivalent `ApplicativeDo` expression.



In addition to being terse and clear in their own right, `ApplicativeComprehensions` have some small advantages over the `ApplicativeDo` extension: the `ApplicativeDo` transformation as implemented in GHC 8 is an exclusively syntactic translation, which can lead to some unexpected corner cases. For example, the following example code typechecks in GHC 8 with `LANGUAGE ApplicativeDo`:


```wiki
-- this typechecks with ApplicativeDo
incrA :: Applicative f => f Int -> f Int
incrA nA = do
  n <- nA
  return (n + 1)
```


However, the eta-expanded version of the same code \_does not\_ typecheck, because the `ApplicativeDo` transformation only happens if the `do`-notation block ends with a literal invocation of `return` or `pure`:


```wiki
-- this does NOT typecheck with ApplicativeDo
incrA' :: Applicative f => f Int -> f Int
incrA' nA = do
  n <- nA
  (\ x -> return x) (n + 1)
```


The corresponding problem is impossible to express with `ApplicativeComprehensions`, because a call to `pure` or `return` is *always* implicitly applied to the return value of the comprehension. The comprehension-based equivalent to `incrA` is:


```wiki
incrA'' :: Applicative f => f Int -> f Int
incrA'' nA = [ n + 1 | n <- nA ]
```

## Some Standing Questions



It's possible to desugar `ApplicativeComprehensions`-compatible expressions that contain guards into expressions with an `Alternative` constraint, e.g. by translating


```wiki
[ expr_0 | pat_1 <- expr_1
         , pat_2 <- expr_2
         , guard_expr
         ]
```


into


```wiki
(\ pat_1 pat_2 () -> expr_0) <$> expr_1
                             <*> expr_2
                             <*> guard guard_expr
```


but this would be subject to the same restrictions as before: the guard expression would not be able to reference the variables from any of the other pattern bindings. This doesn't seem as useful as `ApplicativeComprehensions` in general, but it might be worthwhile to include.



If a given instance of `do`-notation doesn't satisfy the criteria for `ApplicativeDo`, then it implicitly is given a `Monad` constraint instead of an `Applicative` constraint. Fallback for `ApplicativeComprehensions` is not as clear, in part because it might be determined by whether a given source file also has `MonadComprehensions` enabled or not, or whether `MonadComprehensions` implies `ApplicativeComprehensions` or vice versa.


