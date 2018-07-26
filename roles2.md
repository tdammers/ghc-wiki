# Update to Roles



It has become (somewhat) clear that the Roles mechanism as implemented in GHC 7.8 is insufficient. (See [examples](roles2#xamples) below.) This page is dedicated to creating a new design for roles that might fix the problems, continuing the discussion started in [\#9123](http://gitlabghc.nibbler/ghc/ghc/issues/9123).



See also the [main roles wiki page](roles) and these related tickets: [\#9117](http://gitlabghc.nibbler/ghc/ghc/issues/9117), [\#9118](http://gitlabghc.nibbler/ghc/ghc/issues/9118), [\#9123](http://gitlabghc.nibbler/ghc/ghc/issues/9123), and [\#9131](http://gitlabghc.nibbler/ghc/ghc/issues/9131).


## Problem examples



We have three known examples of where current roles are failing us.


### Adding `join` to `Monad`



As part of the [
Applicative-Monad Proposal](http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal), we wish to add `join` to `Monad`, thus:


```wiki
class Applicative m => Monad m where
  ...
  join :: forall a. m (m a) -> m a
```


This is all well and good, and would work with GeneralizedNewtypeDeriving (GND) most of the time. But, consider this:


```wiki
newtype T m a = T (m a)
  deriving (Functor, Applicative, Monad)

readRef :: MonadIO m => IORef a -> T m a
readRef = T . liftIO . readIORef
```


The designer of `T` will export it abstractly, allowing only the reading of `IORef`s in the `T` monad but not other operations.



Sadly, the use of GND fails here, with `join` in the `Monad` class. Here is the relevant generated code:


```wiki
instance Monad m => Monad (T m) where
  ...
  join = coerce (join :: m (m a) -> m a) :: forall a. T m (T m a) -> T m a
```


Thus, GHC must derive `Coercible (m (m a) -> m a) (T m (T m a) -> T m a)`. This requirement reduces (in part) to `Coercible (m (m a)) (T m (T m a))`. We try to solve this by applying a newtype-unwrapping instance, `Coercible x (m a) => Coercible x (T m a)`. Then, we must solve `Coercible (m (m a)) (m (T m a))`. And here, we are stuck. We do know `Coercible (m a) (T m a)`, but we don't know the role of `m`'s parameter, and must assume (for safety) that it could be nominal. Thus, we can't solve for `Coercible (m (m a)) (m (T m a))`.



This problem would occur if `join` were in `Monad` and a programmer used GND on any monad transformer. This is a common enough idiom to make us want to fix the situation.


### The `MVector` class



A redesign of the `vector` package is underway, introducing the `Vector` class, among other changes. Here is the offending method:


```wiki
class (...) => Vector v a where
  basicUnsafeIndexM :: Monad m => v a -> Int -> m a
  ...
```


Here, `a` is the type of the thing stored in the vector, and it is natural to want to coerce a vector of `Int`s to a vector of `Age`s. But, GND would not work here, for very similar reasons to the case above -- we won't be able to coerce `m Int` to `m Age`, because we don't know enough about `m`.


### The `acme-schoenfinkel` package



This next example is the one known (not updated) case of type-safe code that existed before GHC 7.8 that does not work with GHC 7.8's roles. The package `acme-schoenfinkel-0.1.1` package (by Ertugrul Söylemez) defines


```wiki
    WrappedSchoenfinkel {
      unwrapSchoenfinkel :: cat a b
    }
    deriving (Alternative, Applicative, Arrow, ArrowApply,
              ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero,
              Category, Functor)
```


GND fails on `ArrowApply`:


```wiki
class Arrow a => ArrowApply (a :: * -> * -> *) where
  app :: forall b c. a (a b c, b) c
```


The problem here echoes the `join` problem quite closely.


## The best known solution



Edward Kmett initially described an approach in an [
email](http://www.haskell.org/pipermail/ghc-devs/2014-May/004974.html), roughly as follows (names subject to bikeshedding, as always):



Currently, `Data.Type.Coercion` makes this definition:


```wiki
data Coercion a b where
  Coercion :: Coercible a b => Coercion a b
```


Building on that, define a class `Rep` like this:


```wiki
class Rep (f :: k1 -> k2) where
  co :: Coercible a b => Coercion (f a) (f b)
```


The idea is that a type is in the `Rep` class if its *next* parameter is representational. Thus, we would have instances `Rep Maybe`, `Rep []`, `Rep Either`, `Rep (Either a)`, etc. We would *not* have `Rep G`, where `G` is a GADT.



Using this, we can define GND over `join` thus (continuing the [example](roles2#) above):


```wiki
instance (Rep m, Monad m) => Monad (T m) where
  ...
  join   = case co :: Coercion (m (m a)) (m (T m a)) of
             Coercion -> coerce (join :: m (m a) -> m a)
           :: forall a. T m (T m a) -> T m a
```


This compiles without difficulty.



Of course, we need to bake this reasoning into the compiler and the existing `Coercible` solver, essentially with a `Coercible` "instance" like


```wiki
instance (Rep f, Coercible a b) => Coercible (f a) (f b)
```


We also would want automatic generation of instances of `Rep`, not unlike the generation of instances for `Coercible`.


### Integrating `Rep` into the solver



[comment:19:ticket:9123](http://gitlabghc.nibbler/ghc/ghc/issues/9123) shows an intricate solution to a `Coercible` constraint. This solution would be hard to find algorithmically. Thus, even though the definition for `Rep` above is sufficient, we propose the following implication


```wiki
(Rep f, Rep g, Coercible f g, Coercible a b) => Coercible (f a) (g b)
```


as the only way that the solver will dispatch `Rep` constraints. Internally, only one of `Rep f` and `Rep g` will be used, but if we require only one of these constraints, then the solver will be weirdly incomplete. (For example, the solver might know `Rep f, Coercible f g` and need `Rep g`. How would it know to look at `f`?)



This implication is derivable from `(Rep f, Coercible a b) => Coercible (f a) (f b)` (the instance above) as follows (using `~` for `Coercible`):


```wiki
Rep f, a ~ b             f ~ g
------------ Rep       --------- App
f a ~ f b              f b ~ g b
---------------------------------- Trans
f a ~ g b
```


Using the enhanced rule (as suggested originally by Edward Kmett) will thus lead to an easier solver algorithm, even though it may permit fewer coercions.



Writing the solver still presents challenges. There are many "overlapping instances" floating around -- in other words, the solver may choose to go down a dead-end path when a perfectly good path exists. In particular, we must be wary of unification variables that might become more informative. It seems that simplifying a wanted `Coercible` constraint that has unification variables present is a bad idea. However, this problem (of increased information) does not exist with *skolems*, for which the solver will *not* get more information. Because all solutions are equivalent, we do not care if a skolem variable is instantiated with a type that could yield a different solution. (Contrast to OverlappingInstances, which cares very much about skolems.) We just care that the solver works independent of when unification variables are solved. This may or may not be related to the parameter to GHC's `SkolemTv` constructor. (RAE thinks the ideas are close, but the implementations won't be.)



**Conjecture:** normalizing newtypes (with respect to in-scope constructors) first, before anything else, is a good starting point. Indeed, this idea has been adopted en route to solving [\#9117](http://gitlabghc.nibbler/ghc/ghc/issues/9117).


### Open user-facing design questions


1. How far should instance generation go? For example, for the type

```wiki
newtype ReaderT r m a = ReaderT (r -> m a)
```

>
>
> the instance
>
>

```wiki
instance Rep m => Rep (ReaderT r m)
```

>
>
> can be written. Should this be inferred? I (Richard) can imagine a beefed up role inference algorithm which could figure this out. But, perhaps there exist harder cases that would not be inferrable.
>
>

1. Should users be able to write instances for `Rep` by hand? They cannot do so for `Coercible`.

1. What should the method in `Rep` be? `Coercion a b -> Coercion (f a) (f b)`, `Coercible a b => Coercion (f a) (f b)`, and `Coercible a b => Coercible (f a) (f b)` (definable internally) are all contenders.

1. Should we do something analogous for phantom roles?

1. (Comment from Dominique Devriese)
  Should `Functor f` have `Rep f` as a parent constraint?  After all, if `Coercible a b` then we can already convert from `f a` to `f b` using `fmap coerce` and the parametricity of `fmap` implies that it can't function in a type-specific way, so I suspect that, together with the functor identity law, this somehow implies that `fmap coerce` must necessarily function the same as a low-level coerce.

### Open implementation questions


1. Currently, all coercions (including representational ones) are unboxed (and thus take up exactly 0 bits) in a running program. (We ignore `-fdefer-type-errors` here.) But, Core has no way of expressing *functions* in the coercion language, and the `co` method above essentially desugars to a coercion function. Either we have to add functions to the language of coercions, or we have to keep the coercions generated by `Rep` instances boxed at runtime, taking of the space of a pointer and potentially an unevaluated thunk.

1. The `ReaderT` example defined `ReaderT` as a newtype. The `Rep` instance shown is indeed writable by hand, right now. But, if `ReaderT` were defined as a *data* type, the `Rep` instance would be impossible to write, as there are no newtype-unwrapping instances. It seems a new form of axiom would be necessary to implement this trick for data types. This axiom would have to be produced at the data type definition, much like how newtype axioms are produced with newtype definitions.

1. The `Coercible` solver is getting somewhat involved already ([\#9117](http://gitlabghc.nibbler/ghc/ghc/issues/9117), [\#9131](http://gitlabghc.nibbler/ghc/ghc/issues/9131)). Can this be incorporated cleanly? We surely hope that the solver is sound with respect to the definition of representational coercions in Core. How complete is it? How will this affect completeness? In other words, will adding this extension necessarily mean that there are more types that are provably representationally equal but which GHC is unable to find this proof?

### Other issues


1. There is a weird asymmetry here. If we know `(Rep f, Coercible f g, Coercible a b)`, we can prove `Coercible (f a) (g b)`. We do this by proving `Coercible (f a) (f b)` and then `Coercible (f b) (g b)` and using transitivity. But, note that we do *not* know `Rep g`! Furthermore, `(Rep f, Coercible f g)` do *not* imply `Rep g` in the presence of role annotations:

```wiki
newtype MyMaybe a = Mk (Maybe a)
type role MyMaybe nominal
```

>
>
> Here, we have `Rep Maybe` and `Coercible Maybe MyMaybe` but not `Rep MyMaybe`. This is all very strange. Of course, we *could* define an instance `Rep MyMaybe`, despite the role annotation, by using the newtype-unwrapping instance. But, what does this mean if the author wants to export `MyMaybe` abstractly?
>
>

1. Consider the `StateT` newtype:

```wiki
newtype StateT s m a = StateT (s -> m (a, s))
```

>
>
> Its roles are `nominal representational nominal`. But, if we have `Rep m`, then the roles could all be representational. For the `a` parameter, this is just like `ReaderT`. But, we are stuck with the `s` parameter, simply because the `s` parameter comes *before* `m` in the parameter list. There's no way to assert something about `m` when describing a property of `s`.
>
>

## Other possible designs


1. The design from the [
  POPL'11 paper](http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf). This design incorporates roles into kinds. It solves the exact problems here, but at great cost: because roles are attached to kinds, we have to choose a types roles in the wrong place. For example, consider the `Monad` class. Should the parameter `m` have kind `*/R -> *`, requiring all monads to take representational arguments, or should it have type `*/N ->*`, disallowing GND if `join` is in the `Monad` class? We're stuck with a different set of problems. And, there is the pervasiveness of this change, which is why we didn't implement it in the first place.

  - (comment from Dominique Devriese) Note that `Monad` implies `Functor`, so by the argument about `Functor f` implying `Rep f` above, I would argue that `m` should have kind `*/R -> *`.

1. (This is just Richard thinking out loud. It may be gibberish.) What if we generalize roles to be parameterized? To make the definitions well-formed, roles would be attached directly to type constructors (not the parameters), but be a mapping from 1-indexed natural numbers to roles. As an example, `ReaderT`'s role would be `[1 |-> R, 2 |-> R, 3 |-> ((2.1 ~ R) => R; N)]`. The first two entries just say that parameters `r` and `m` have representational roles. The last entry (`3 |-> ((2.1 ~ R) => R; N)`) says that, if `m`'s first parameter (that is, parameter `2.1`, where the `.` is some sort of indexing operator -- not a decimal point!) is representational, then so is `a`; otherwise, `a` is nominal. This defaulting behavior does *not* cause coherence problems, as long as the roles are listed in order from phantom to nominal -- if GHC can't prove a more permissive role, a more restrictive one is assumed.

>
>
> Under this scenario, `StateT`'s role would be `[1 |-> (2.1 ~ R => R; N), 2 |-> R, 3 |-> (2.1 ~ R => R; N)]`.
>
>

>
>
> To implement this, we would probably need role *evidence* sloshing around, not unlike coercions. This evidence would be consumed by appropriately beefed up coercion forms (particularly, the TyConAppCo case). It would be produced by role *axioms* at every data- and newtype definition.
>
>

>
>
> This design seems something like a middle road between the flexibility and modularity (that is, roles and kinds are distinct) that we have now and the completeness offered by the POPL'11 solution.
>
>

