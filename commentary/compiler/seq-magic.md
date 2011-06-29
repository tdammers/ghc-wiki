# Seq magic



The innocent-looking `seq` operator causes all manner of mayhem in GHC. This page summarises the issues.  See also discussion in Trac [\#5129](http://gitlabghc.nibbler/ghc/ghc/issues/5129), [\#5262](http://gitlabghc.nibbler/ghc/ghc/issues/5262)


## The baseline position



Our initial story was that `(seq e1 e2)` meant precisely


```wiki
   case e1 of { _ -> e2 }
```


Indeed this was `seq`'s inlining.  This translation validates some important rules


```wiki
 * `seq` is strict in both its arguments
 * (e1 `seq` e2) e3            ===>   e1 `seq` (e2 e3)
 * case (e1 `seq` e2) of alts  ===>   e1 `seq` (case e2 of alts)
 * value `seq` e               ===>   e
```


But this approach has problems; see `Note [Deguaring seq]` in `DsUtils`.


### Problem 1 (Trac [\#1031](http://gitlabghc.nibbler/ghc/ghc/issues/1031))



Consider


```wiki
   f x y = x `seq` (y `seq` (# x,y #))
```


The `[CoreSyn let/app invariant]` (see `CoreSyn`) means that, other things being equal, because 
the argument to the outer `seq` has an unlifted type, we'll use call-by-value thus:


```wiki
   f x y = case (y `seq` (# x,y #)) of v -> x `seq` v
```


But that is bad for two reasons: 


- we now evaluate `y` before `x`, and 
- we can't bind `v` to an unboxed pair


Seq is very, very special!  Treating it as a two-argument function, strict in
both arguments, doesn't work. We "fixed" this by treating `seq` as a language
construct, desugared by the desugarer, rather than as a function that may (or
may not) be inlined by the simplifier.  So the above term is desugared to:


```wiki
        case x of _ -> case y of _ -> (# x,y #)
```

### Problem 2 (Trac [\#2273](http://gitlabghc.nibbler/ghc/ghc/issues/2273))



Consider


```wiki
   let chp = case b of { True -> fst x; False -> 0 }
   in chp `seq` ...chp...
```


Here the `seq` is designed to plug the space leak of retaining `(snd x)`
for too long.



If we rely on the ordinary inlining of `seq`, we'll get


```wiki
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of _ { I# -> ...chp... }
```


But since `chp` is cheap, and the case is an alluring contet, we'll
inline `chp` into the case scrutinee.  Now there is only one use of `chp`,
so we'll inline a second copy.  Alas, we've now ruined the purpose of
the seq, by re-introducing the space leak:


```wiki
    case (case b of {True -> fst x; False -> 0}) of
      I# _ -> ...case b of {True -> fst x; False -> 0}...
```


We can try to avoid doing this by ensuring that the binder-swap in the
case happens, so we get his at an early stage:


```wiki
   case chp of chp2 { I# -> ...chp2... }
```


But this is fragile.  The real culprit is the source program.  Perhaps we
should have said explicitly


```wiki
   let !chp2 = chp in ...chp2...
```


But that's painful.  So the desugarer does a little hack to make `seq`
more robust: a saturated application of `seq` is turned **directly** into
the case expression, thus:


```wiki
   x  `seq` e2 ==> case x of x -> e2    -- Note shadowing!
   e1 `seq` e2 ==> case x of _ -> e2
```


So we desugar our example to:


```wiki
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of chp { I# -> ...chp... }
```


And now all is well.



Be careful not to desugar


```wiki
   True `seq` e  ==> case True of True { ... }
```


which stupidly tries to bind the datacon 'True'. This is easily avoided.



The whole thing is a hack though; if you define `mySeq=seq`, the hack
won't work on `mySeq`.  


### Problem 3 (Trac [\#5262](http://gitlabghc.nibbler/ghc/ghc/issues/5262))



Consider


```wiki
  f x = x `seq` (\y.y)
```


With the above desugaring we get


```wiki
  f x = case x of x { _ -> \y.y }
```


and now ete expansion gives


```wiki
  f x y = case x of x { _ -> y }
```


Now suppose that we have


```wiki
       f (length xs) `seq` 3
```


Plainly `(length xs)` should be evaluated... but it isn't because `f` has arity 2.
(Without -O this doesn't happen.)


### Problem 4: seq in the IO monad



See the extensive discussion in Trac [\#5129](http://gitlabghc.nibbler/ghc/ghc/issues/5129).


### Problem 5: the need for special rules



Roman found situations where he had


```wiki
      case (f n) of _ -> e
```


where he knew that `f` (which was strict in `n`) would terminate if n did.
Notice that the result of `(f n)` is discarded. So it makes sense to
transform to


```wiki
      case n of _ -> e
```


Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:


```wiki
  RULE "f/seq" forall n e.  seq (f n) e = seq n e
```


You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to `seq` and looks for
a RULE.  (This is done in `Simplify.rebuildCase`.)  As usual, the
correctness of the rule is up to you.



To make this work, we need to be careful that `seq` is **not** desguared
into a case expression on the LHS of a rule.



To increase applicability of these user-defined rules, we also
have the following built-in rule for `seq` 


```wiki
  seq (x |> co) y = seq x y
```


This eliminates unnecessary casts and also allows other seq rules to
match more often.  Notably,     


```wiki
   seq (f x |> co) y  -->  seq (f x) y
```


and now a user-defined rule for `seq` may fire.


# A better way



Here's our new plan. 


- Introduce a new primop `seq# :: a -> State# s -> (# a, State# s #)` (see [be5441799b7d94646dcd4bfea15407883537eaaa](/trac/ghc/changeset/be5441799b7d94646dcd4bfea15407883537eaaa/ghc))
- Implement `seq#` by turning it into the obvious eval in the backend.  In fact, since the return convention for `(# State# s, a #)` is exactly the same as for `a`, we can implement `seq# s a` by `a` (even when it appears as a case scrutinee).
- Define `evaluate` thus

  ```wiki
    evaluate :: a -> IO a
    evaluate x = IO $ \s -> seq# x s
  ```


That fixes problem 4.



We could go on and desugar `seq` thus:


```wiki
   x  `seq` e2 ==> case seq# x RW of (# x, _ #) -> e2    -- Note shadowing!
   e1 `seq` e2 ==> case seq# x RW of (# _, _ #) -> e2
```


and if we consider `seq#` to be expensive, then we won't eta-expand around it, and that would fix problem 3.



However, there is a concern that this might lead to performance regressions in examples like this:


```wiki
f :: Int -> Int -> IO Int
f x y | x `seq` False = undefined
f x 3 = do
  ... some IO monad code here ...
```


so `f` turns into


```wiki
f = \x . \y . case seq# x RW of (# _, x #) -> case y of 3 -> \s . some IO monad code
```


and we won't get to eta-expand the `\s` as we would normally do (this is pretty important for getting good performance from IO and ST monad code).



Arguably `f` should be rewritten with a bang pattern, and we should treat bang patterns as the eta-expandable seq and translate them directly into `case`, not `seq#`.  But this would be a subtle difference between `seq` and bang patterns.



Furthermore, we already have `pseq`, which is supposed to be a "strictly ordered seq", that is it preserves evaluation order.  So perhaps `pseq` should be the one that more accurately implements the programmer's intentions, leaving `seq` as it currently is.



We are currently pondering what to do here.


