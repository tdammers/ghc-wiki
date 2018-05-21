


# Deferring compilation type errors to runtime



This page describes the `-fdefer-type-errors` flag that was added in GHC 7.6.1. Ticket [\#5624](http://gitlabghc.nibbler/ghc/ghc/issues/5624) tracks this feature request.



For more information, see the associated [
Equality Proofs and Deferred Type Errors](https://www.microsoft.com/en-us/research/publication/equality-proofs-and-deferred-type-errors-a-compiler-pearl/) (ICFP'12).


## Tickets



Use Keyword = `DeferredErrors` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#11197](http://gitlabghc.nibbler/ghc/ghc/issues/11197)</th>
<td>Overeager deferred type errors</td></tr>
<tr><th>[\#14370](http://gitlabghc.nibbler/ghc/ghc/issues/14370)</th>
<td>improve documentation of -fdefer-typed-holes for naked expressions in ghci</td></tr>
<tr><th>[\#14722](http://gitlabghc.nibbler/ghc/ghc/issues/14722)</th>
<td>Error message points to wrong location</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#14325](http://gitlabghc.nibbler/ghc/ghc/issues/14325)</th>
<td>Erroneous program emits no errors</td></tr></table>



## Overview



While developing, sometimes it is desirable to allow compilation to succeed even
if there are type errors in the code. Consider the following case:


```wiki
module Main where

a :: Int
a = 'a'

main = print "b"
```


Even though `a` is ill-typed, it is not used in the end, so if all that we're
interested in is `main` it is handy to be able to ignore the problems in `a`.



Since we treat type equalities as evidence, this is relatively simple. Whenever
we run into a type mismatch in `TcUnify`, we would normally just emit an error. But it
is always safe to defer the mismatch to the main constraint solver. If we do
that, `a` will get transformed into


```wiki
$co :: Int ~# Char
$co = ...

a :: Int
a = 'a' `cast` $co
```


The constraint solver would realize that `co` is an insoluble constraint, and
emit an error. But we can also replace the right-hand side
of `co` with a runtime error call. This allows the program
to compile, and it will run fine unless we evaluate `a`. Since coercions are
unboxed they will be eagerly evaluated if used, so laziness will not "get on
the way".


## Example



Here's an example of all sorts of errors that can be deferred:


```wiki
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

t5624 :: IO ()
t5624 = putStr "Hello World" >> putStr ','

a :: Int
a = 'p'

data B = B
b :: B -> Bool
b x = x == x

data C a where
  C1 :: C Int
  C2 :: Bool -> C Bool

c :: C Int -> Bool
c (C2 x) = True

d :: a -> a
d = 1

e = 'p'
f = e 'q'

h :: a -> (Char,Char) 
h x = (x,'c')

data T a where 
  K  :: a -> T a

i a = seq (not (K a)) ()

class MyClass a where myOp :: a -> String

j = myOp 23 -- Two errors, should not combine them

k :: (Int ~ Bool) => Int -> Bool
k x = x

l :: IO ()
l = putChar >> putChar 'p'


main :: IO ()
main = print "No errors!"
```


This module compiles and runs!


## Implementation details



The first step is to make sure that `TcUnify` does not ever fail with a type
error; instead, we call `uType_defer` to defer the unification to the constraint
solver.



In the constraint solver, we call `reportUnsolved` with any remaining unsolved
constraints. At this stage, if we are not deferring errors, we simply make the
errors for each unsolved constraint and throw them. If we are deferring errors,
we make the errors for each constraint and then set the evidence binder of each
unsolved constraint to be a runtime error that emits the type error message we
just created. This is done by `reportTidyWanteds` and `deferCt` in
`typecheck/TcErrors`. Note that `reportTidyWanteds` has some shuffling to do,
because when we are not deferring errors we group certain errors, or we don't
report all the errors. But when we are deferring we really want to have
(at least) one error for every coercion.


### Error messages



For simplicity, we defer errors from `TcUnify` to the constraint solver even
if `-fdefer-type-errors` is not on; in that case, we will simply fail in the
constraint solver, rather than directly in the unifier.



This means that some type error messages change, even without `-fdefer-type-errors`.
In particular, many tests from the testsuite need to have their output adapted.


## Kind errors



Currently we cannot defer kind errors because we do not create coercions for
kind equalities. This might change in the near future, though, in which case
we could use the same strategy to defer kind errors to runtime.


