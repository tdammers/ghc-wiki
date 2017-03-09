
See the [root page for exceptions](exceptions)


## Fixing demand analysis for exceptions



There are a couple different problems we have to deal with.


1. [\#13330](http://gitlabghc.nibbler/ghc/ghc/issues/13330) was caused by an ugly and somewhat broken hack trying to analyze `catch#` as stricter than it really is. It would be very nice if the *good ideas* that went into that ugly hack could be extracted and repaired to produce a more aggressive analysis that's still correct.

1. [\#13380](http://gitlabghc.nibbler/ghc/ghc/issues/13380) reveals something of a disagreement about how we should view the result of `raiseIO#` (used to implement `throwIO`). Simon Marlow and David Feuer feel pretty strongly that `throwIO` should be viewed as producing an entirely deterministic, well-behaved `IO` action, and that the exception resulting from it should never be mixed up with an imprecise exception. Reid Barton and Simon Peyton Jones seem to wonder if that precision is worth the potential performance cost.


Assuming that I (David F.) and Simon M. win this debate, the key problem here is that we analyze `raiseIO# e s` as `ThrowsExn`, the same way we analyze something that either diverges or throws an imprecise exception. Assuming we change this, we want to take some care to recover dead code elimination that the current analysis allows. In particular, given


```
case raiseIO# e s of
  (# s', a #) -> EXPR
```


we surely want to consider `EXPR` to be dead code, even though we don't want to consider `raiseIO# e s` to be precisely bottom.


### Important conventions below



In the rest of this page, I will squash all `Exception` types down to `SomeException`, to avoid all the conversion mess. So instead of `Exception e => ...`, I will simply assume that `e` is `SomeException`.



Furthermore, for the sake of readability, I uniformly substitute `Either a b` in place of `(# a | b #)`.



By a **precise** exception, I mean an exception produced by `raiseIO#` (the primop version of `throwIO`).



By an **imprecise** exception, I basically mean an exception produced by `throw` (as described in [
A Semantics for Imprecise Exceptions](https://www.microsoft.com/en-us/research/publication/a-semantics-for-imprecise-exceptions/)).


### Semantics of precise exceptions



I (David Feuer) believe that precise exceptions should implement the following model.


```
newtype IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, Either SomeException a #)
instance Monad IO where
  return a = IO $ \s -> (# s, Right a #)
  m >>= f = IO $ \s -> case unIO m s of
    (# s', Left e #) -> (# s', Left e #)
    (# s', Right a #) -> unIO (f a) s'

throwIO :: SomeException -> IO a
throwIO e = IO $ \s -> (# s, Left e #)

-- The name 'catchIO' is, sadly, taken by a less interesting function already
catchThrowIO :: IO a -> (SomeException -> IO a) -> IO a
catchThrowIO m f = IO $ \s ->
  case unIO m s of
    (# s', Left e #) -> unIO (f e) s'
    good -> good
```


Notes


- I believe we likely should expose an actual *catchThrowIO* function. Since it doesn't catch imprecise exceptions, it can be treated much more aggressively. For example, `catchThrowIO (putStrLn x) (\_ -> print 2)` can safely be analyzed as strict in `x`, whereas the equivalent expression using `catch` cannot.

- With the above semantics it is clear that this function ([\#13380](http://gitlabghc.nibbler/ghc/ghc/issues/13380) comment:4) whoudl be lazy in `y`:

  ```wiki
  f :: Int -> Int -> IO Int
  f x y | x>0       = throwIO (userError "What")
        | y>0       = return 1
        | otherwise = return 2
  ```

- See `Note [IO hack in the demand analyser]` in `DmdAnal`.  This note would make much more sense with the above semantics for the IO monad.


The "I/O hack" in the demand analyzer actually does something very important that the note doesn't mention. The note begins


>
>
> There's a hack here for I/O operations.  Consider
>
>
> >
> >
> > `case foo x s of { (# s, r #) -> y }`
> >
> >
>
>
> Is this strict in `y`?  Normally yes, but what if `foo` is an I/O
> operation that simply terminates the program (not in an erroneous way)?
>
>


In fact, we have to worry about this under *all interesting* conditions. For example, consider


```
case unIO (putStrLn "About to run y") s of
  (# s', r #) -> y
```


Is this strict in `y`? No! `y` could turn out to be undefined; if we force it early then we'll never see the message. So I think we can really only consider this strict in `y` in the very special case where the `IO` action is `pure x`.


- Consider`throwIO exn >>= BIG`.  Just inlining shows us that we can discard `BIG`.  Currently (GHC 8) inlining turns this into `case throwIO# exn sn of (# s#, r #) -> BIG r`, which allows us to discard `BIG` because `throwIO#` is treated as diverging.  But [\#13380](http://gitlabghc.nibbler/ghc/ghc/issues/13380), comment:4 suggests that it should not.

### `catch#` strictness



How strict can `catch# m f s` be? See `Note [Exceptions and strictness]` in `Demand`.  The `ExnStr` business is pretty horrible.



Making `catch#` strict made a significant perf difference in libraries: see comment:4 of [\#10712](http://gitlabghc.nibbler/ghc/ghc/issues/10712).   Maybe indeed adding `catchThrowIO` as David suggests above, making it strict, and using it in the libraries in place of `catch` , would be the way to go.



We know several things:


1. If `m s` diverges (without throwing an exception), then `catch# m f s` diverges.

1. If `m s` certainly executes successfully, then `catch# m f s = m s`.

1. If `m s` is strict in some value `x`, and `x` certainly does not throw an exception (i.e., it either evaluates successfully to WHNF or diverges), then it is safe to consider `catch# m f s` strict in `x`.

1. If `m s` and `f e s` are both strict in some value `x`, then it is safe to consider `catch# m f s` strict in `x`.

1. If `m s` certainly throws an exception (either imprecise or precise) that it does not itself catch, then `f` is certainly called.

### `catchRetry#`



`catchRetry#` is used to implement `orElse` for `STM`. I *believe* that it functions (from the perspective of demand analysis) very much like the hypothetical `catchThrowIO`, and that we can probably treat them similarly.


## Concrete ideas



I think, first, that we should draw a clear line between imprecise exceptions, generally produced by `raise#`, and precise exceptions, produced by `raiseIO`.



Operationally, we need `raise#` and `raiseIO#` to set some flag to allow `catchRaiseIO#` to see which exceptions it should handle.



I believe we want `unsafePerformIO` and `unsafeInterleaveIO` to convert precise exceptions into imprecise ones. That is, they should effectively catch any precise exceptions and rethrow them as imprecise ones. Perhaps we can do this in `runRW#`. Currently, `unsafeInterleaveIO` doesn't *use* `runRW#`, but I think we can and probably should change that.



I strongly suspect there is something to be gained by treating expressions using `catch#` or `catchRaiseIO#` specially in the demand analyzer, but I don't know enough to say just how. I suspect the "result domain" does need to be expanded from the classical one, but in a slightly different direction than what we have now; we want to be able to express that certain things certainly will or certainly won't throw imprecise or precise exceptions.



We seem to take some advantage of `has_side_effects` to avoid applying the I/O demand analysis hack too broadly, but perhaps we could do a better job by propagating side effect information as we do demand information. I don't know.


