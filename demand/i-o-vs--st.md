# `IO` vs. `ST` in demand analysis



`IO` and strict `ST` look fairly similar if you squint some: both of them
are single-threaded monads offering access to mutable references and arrays.



Unfortunately, that similarity breaks down almost completely when it comes to
demand analysis. The demand analysis needs for `IO` are explored in
[Exceptions/PreciseExceptions](exceptions/precise-exceptions).



Whereas `forever m >> n` for `IO` should be considered lazy in `n`, in `ST` it is
hyperstrict in `n`. If we hit bottom (failing to calculate the final result pair),
*that's it*. The entire `runST` computation will be bottom. We've
been mutating all sorts of references and arrays, but the moment we hit bottom,
all of those references and arrays vanish in a puff of smoke; there are no lasting
side effects. Although GHC runs `ST` computations with `State# RealWorld`, this seems a bit wrong;
we really want to run them with `State# FakeWorld`! The whole I/O demand hack
is unnecessary in the context of `ST`, and we can be as aggressive as we like (ignoring
such horrors as `unsafeSTtoIO` and `unsafeIOtoST`).


### Implementation



I (David) believe that we need, at a minimum, a new magical function
`runFW#` (run in a fake world) that takes an `ST` computation rather
than an `IO` one (*unless* the floating problem with inlining `runRW#`
has been resolved by our avoiding floating operations that have side
effects. Has anyone checked?). One option would be to actually add a `FakeWorld`
type and a `fakeWorld#` primitive to use it. This option is explored
in [
phab:D3375](https://phabricator.haskell.org/D3375). One surprising point: the `case` on the result needs to be moved
inside the `runFW#` argument. That is, unlike


```
unsafePerformIO (IO m) = case runRW# m of (# _, a #) -> a
```


we need


```
runST (ST m) = runFW# (\s -> case m s of (# _, a #) -> a)
```


This allows demand analysis to see that we actually use the second component.
It seems that the demand signature we give to `runRW#` is insufficient for that.



Another possible option Simon hinted at would be to give `runFW#` a
rank-2 type like `runST`. This would delay instantiation of the state
type to `RealWorld` until after demand analysis, presumably preventing
the hack from tripping on it. This option avoids needing a `FakeWorld`
type and a `fakeWorld#` primitive.


