
A couple issues have arisen with `COMPLETE` pragmas for patterns.


### Type issues



[\#14135](http://gitlabghc.nibbler/ghc/ghc/issues/14135) involves a panic when types don't match up correctly. Simon's suggestion in
[ticket:14135\#comment:8](http://gitlabghc.nibbler/ghc/ghc/issues/14135) seems to make a lot of sense:


>
>
> filter the candidate COMPLETE sets, to choose only those all of whose
> constructors match the type of the scrutinee.
>
>


Why does that make sense, and not just ignoring patterns that don't match? Because
what remains is likely not complete! Imagine


```
LZ :: (Ord a, Num a) => () => a -> a
Z :: (Eq a, Num a) => () => a -> a
GZ :: (Ord a, Num a) => () => a -> a
{-# COMPLETE LZ, Z, GZ #-}
```


for splitting numbers by whether they're negative, zero, or positive.
If a type isn't `Ord`, then filtering out the matching patterns from
this set would give no patterns; but that doesn't mean the type is
uninhabited.


### Overlap issues



Pattern matches on *individual constructors* can't overlap. This is *not* the
case for general pattern synonyms. This is highlighted by [\#14253](http://gitlabghc.nibbler/ghc/ghc/issues/14253). In particular, the
fact that `P1` and `P2` form a `COMPLETE` set does *not* mean that other patterns
are necessarily redundant. The simplest rule that will do something fairly sensible
is this: only consider patterns redundant as a result of a `COMPLETE` pragma if they
come after *all* the patterns listed in that pragma.



If I have


```
data Foo = Bar | Baz
pattern Quux :: Foo
pattern Quuux :: Foo
{-# COMPLETE Quux, Quuux #-}
```


Then


```
f Quux = ...
f Quuux = ...
f Bar = ...
```


should complain that `Bar` is redundant, but


```
f Quux = ...
f Bar = ...
f Quuux = ...
```


should pass, because there is no `Quuux` before `Bar`.



A fancier approach would be to offer a pragma that allows users to specify
a partial ordering of patterns by overlap. This would let GHC catch more
redundant matches, but may not be worth the complexity.


