# Support for generic programming






GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in paper [
A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf). This page sketches the specifics of the implementation; we assume you have read the paper. The [
HaskellWiki page](http://www.haskell.org/haskellwiki/Generics) gives a more general overview.



This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). What we describe until the "Kind polymorphic overhaul" section is implemented and released in GHC 7.2.1.


## Status



Use **Keyword** = `Generics` to ensure that a ticket ends up on this auto-generated list



Open Tickets:

<table><tr><th>[\#5642](http://gitlabghc.nibbler/ghc/ghc/issues/5642)</th>
<td>Deriving Generic of a big type takes a long time and lots of space</td></tr>
<tr><th>[\#7492](http://gitlabghc.nibbler/ghc/ghc/issues/7492)</th>
<td>Generic1 deriving: Can we replace Rec1 f with f :.: Par1?</td></tr>
<tr><th>[\#8516](http://gitlabghc.nibbler/ghc/ghc/issues/8516)</th>
<td>Add (-\>) representation and the Invariant class to GHC.Generics</td></tr>
<tr><th>[\#8560](http://gitlabghc.nibbler/ghc/ghc/issues/8560)</th>
<td>Derive some generic representation for GADTs</td></tr>
<tr><th>[\#10068](http://gitlabghc.nibbler/ghc/ghc/issues/10068)</th>
<td>Make the runtime reflection API for names, modules, locations more systematic</td></tr>
<tr><th>[\#10087](http://gitlabghc.nibbler/ghc/ghc/issues/10087)</th>
<td>DefaultSignatures: error message mentions internal name</td></tr>
<tr><th>[\#10514](http://gitlabghc.nibbler/ghc/ghc/issues/10514)</th>
<td>Generic for existential types</td></tr>
<tr><th>[\#11068](http://gitlabghc.nibbler/ghc/ghc/issues/11068)</th>
<td>Make Generic/Generic1 methods inlinable</td></tr>
<tr><th>[\#12731](http://gitlabghc.nibbler/ghc/ghc/issues/12731)</th>
<td>Generic type class has type family; leads to big dep\_finsts</td></tr>
<tr><th>[\#13065](http://gitlabghc.nibbler/ghc/ghc/issues/13065)</th>
<td>Prohibit user-defined Generic and Generic1 instances</td></tr>
<tr><th>[\#15969](http://gitlabghc.nibbler/ghc/ghc/issues/15969)</th>
<td>Generic1 deriving should use more coercions</td></tr></table>




Closed Tickets:

<table><tr><th>[\#2824](http://gitlabghc.nibbler/ghc/ghc/issues/2824)</th>
<td>Duplicate symbols generated when Generics flag and syb-with-class "derive" used simultaneously</td></tr>
<tr><th>[\#3391](http://gitlabghc.nibbler/ghc/ghc/issues/3391)</th>
<td>Generics compilation failure in combination with Data.Accessor deriving</td></tr>
<tr><th>[\#5220](http://gitlabghc.nibbler/ghc/ghc/issues/5220)</th>
<td>GHC internal error when missing -XFlexibleContexts with generics</td></tr>
<tr><th>[\#5227](http://gitlabghc.nibbler/ghc/ghc/issues/5227)</th>
<td>Large space usage when deriving Generic</td></tr>
<tr><th>[\#5462](http://gitlabghc.nibbler/ghc/ghc/issues/5462)</th>
<td>Deriving clause for arbitrary classes</td></tr>
<tr><th>[\#5464](http://gitlabghc.nibbler/ghc/ghc/issues/5464)</th>
<td>Change the way the extra bindings for the generic representation are generated</td></tr>
<tr><th>[\#5884](http://gitlabghc.nibbler/ghc/ghc/issues/5884)</th>
<td>GHC panics while trying to derive a Generic instance for Complex a</td></tr>
<tr><th>[\#5936](http://gitlabghc.nibbler/ghc/ghc/issues/5936)</th>
<td>Support for data families in generics</td></tr>
<tr><th>[\#5939](http://gitlabghc.nibbler/ghc/ghc/issues/5939)</th>
<td>Standalone deriving Generic on type with instantiated arguments</td></tr>
<tr><th>[\#7035](http://gitlabghc.nibbler/ghc/ghc/issues/7035)</th>
<td>\`deriving instance Generic (HsExpr Id)\` raises panic</td></tr>
<tr><th>[\#7255](http://gitlabghc.nibbler/ghc/ghc/issues/7255)</th>
<td>Wrong suggestion when deriving Generic on an instantiated type</td></tr>
<tr><th>[\#7263](http://gitlabghc.nibbler/ghc/ghc/issues/7263)</th>
<td>Add derived Show instances to GHC.Generics</td></tr>
<tr><th>[\#7346](http://gitlabghc.nibbler/ghc/ghc/issues/7346)</th>
<td>Allow the use of \`deriving\` for GHC generics</td></tr>
<tr><th>[\#7422](http://gitlabghc.nibbler/ghc/ghc/issues/7422)</th>
<td>GHC panics while trying to derive Generic for GADT with kind-lifted phantom parameter</td></tr>
<tr><th>[\#7444](http://gitlabghc.nibbler/ghc/ghc/issues/7444)</th>
<td>Update documentation regarding derivability of Generic1</td></tr>
<tr><th>[\#7459](http://gitlabghc.nibbler/ghc/ghc/issues/7459)</th>
<td>deriving Generic does not work with TypeLits</td></tr>
<tr><th>[\#7487](http://gitlabghc.nibbler/ghc/ghc/issues/7487)</th>
<td>Deriving Generic1 for a type containing Either</td></tr>
<tr><th>[\#7631](http://gitlabghc.nibbler/ghc/ghc/issues/7631)</th>
<td>Allow to differentiate between newtypes and datatypes when using Generics</td></tr>
<tr><th>[\#7878](http://gitlabghc.nibbler/ghc/ghc/issues/7878)</th>
<td>Panic when using DerivingGeneric with hs-boot</td></tr>
<tr><th>[\#8416](http://gitlabghc.nibbler/ghc/ghc/issues/8416)</th>
<td>GHC.Generics needs more documentation</td></tr>
<tr><th>[\#8468](http://gitlabghc.nibbler/ghc/ghc/issues/8468)</th>
<td>ghc panic in deriving Generic1 on Array\#</td></tr>
<tr><th>[\#8479](http://gitlabghc.nibbler/ghc/ghc/issues/8479)</th>
<td>Generic1 crashes with tyConStupidTheta when using associated type families</td></tr>
<tr><th>[\#8797](http://gitlabghc.nibbler/ghc/ghc/issues/8797)</th>
<td>Generics instances for monoid and applicative newtypes</td></tr>
<tr><th>[\#8929](http://gitlabghc.nibbler/ghc/ghc/issues/8929)</th>
<td>Deriving Generics broken</td></tr>
<tr><th>[\#9043](http://gitlabghc.nibbler/ghc/ghc/issues/9043)</th>
<td>Add missing type class instances for data types in GHC.Generics</td></tr>
<tr><th>[\#9453](http://gitlabghc.nibbler/ghc/ghc/issues/9453)</th>
<td>The example for GHC Generics is kinda broken</td></tr>
<tr><th>[\#9523](http://gitlabghc.nibbler/ghc/ghc/issues/9523)</th>
<td>Typo in GHC Generics documentation</td></tr>
<tr><th>[\#9526](http://gitlabghc.nibbler/ghc/ghc/issues/9526)</th>
<td>Add missing Generic instances in base</td></tr>
<tr><th>[\#9527](http://gitlabghc.nibbler/ghc/ghc/issues/9527)</th>
<td>Add Generic instances for Language.Haskell.TH</td></tr>
<tr><th>[\#9563](http://gitlabghc.nibbler/ghc/ghc/issues/9563)</th>
<td>Support for deriving Generic1 for data families</td></tr>
<tr><th>[\#9630](http://gitlabghc.nibbler/ghc/ghc/issues/9630)</th>
<td>compile-time performance regression (probably due to Generics)</td></tr>
<tr><th>[\#9766](http://gitlabghc.nibbler/ghc/ghc/issues/9766)</th>
<td>Use TypeLits in the meta-data encoding of GHC.Generics</td></tr>
<tr><th>[\#9821](http://gitlabghc.nibbler/ghc/ghc/issues/9821)</th>
<td>DeriveAnyClass support for higher-kinded classes + some more comments</td></tr>
<tr><th>[\#9968](http://gitlabghc.nibbler/ghc/ghc/issues/9968)</th>
<td>DeriveAnyClass fails on multi-parameter type classes</td></tr>
<tr><th>[\#10030](http://gitlabghc.nibbler/ghc/ghc/issues/10030)</th>
<td>packageName for GHC.Generics.Datatype</td></tr>
<tr><th>[\#10361](http://gitlabghc.nibbler/ghc/ghc/issues/10361)</th>
<td>DeriveAnyClass does not fill in associated type defaults</td></tr>
<tr><th>[\#10487](http://gitlabghc.nibbler/ghc/ghc/issues/10487)</th>
<td>DeriveGeneric breaks when the same data name is used in different modules</td></tr>
<tr><th>[\#10512](http://gitlabghc.nibbler/ghc/ghc/issues/10512)</th>
<td>Generic instances missing for Int32, Word64 etc.</td></tr>
<tr><th>[\#10513](http://gitlabghc.nibbler/ghc/ghc/issues/10513)</th>
<td>ghc 7.6.3 Compiler panic with Generics</td></tr>
<tr><th>[\#10598](http://gitlabghc.nibbler/ghc/ghc/issues/10598)</th>
<td>DeriveAnyClass and GND don't work well together</td></tr>
<tr><th>[\#10604](http://gitlabghc.nibbler/ghc/ghc/issues/10604)</th>
<td>Make Generic1 kind polymorphic</td></tr>
<tr><th>[\#10669](http://gitlabghc.nibbler/ghc/ghc/issues/10669)</th>
<td>Missing Generic instances for base types.</td></tr>
<tr><th>[\#10716](http://gitlabghc.nibbler/ghc/ghc/issues/10716)</th>
<td>Metadata in GHC.Generic should give access to strictness annotation</td></tr>
<tr><th>[\#10775](http://gitlabghc.nibbler/ghc/ghc/issues/10775)</th>
<td>Enable PolyKinds in GHC.Generics</td></tr>
<tr><th>[\#10852](http://gitlabghc.nibbler/ghc/ghc/issues/10852)</th>
<td>ghc 7.8.4 on arm - panic: Simplifier ticks exhausted</td></tr>
<tr><th>[\#10868](http://gitlabghc.nibbler/ghc/ghc/issues/10868)</th>
<td>Make GHC generics capable of handling unboxed types</td></tr>
<tr><th>[\#11292](http://gitlabghc.nibbler/ghc/ghc/issues/11292)</th>
<td>Generics unboxed types: TEST=GEq1 WAY=optasm is failing</td></tr>
<tr><th>[\#11357](http://gitlabghc.nibbler/ghc/ghc/issues/11357)</th>
<td>Regression when deriving Generic1 on poly-kinded data family</td></tr>
<tr><th>[\#11358](http://gitlabghc.nibbler/ghc/ghc/issues/11358)</th>
<td>GHC generics has differing conFixity behavior between 7.10.3 and 8.1</td></tr>
<tr><th>[\#11415](http://gitlabghc.nibbler/ghc/ghc/issues/11415)</th>
<td>pandoc-types fails to build on 4 GB machine</td></tr>
<tr><th>[\#11703](http://gitlabghc.nibbler/ghc/ghc/issues/11703)</th>
<td>Segmentation fault/internal error: evacuate: strange closure type 15 with GHC.Generics</td></tr>
<tr><th>[\#11732](http://gitlabghc.nibbler/ghc/ghc/issues/11732)</th>
<td>Deriving Generic1 interacts poorly with TypeInType</td></tr>
<tr><th>[\#11775](http://gitlabghc.nibbler/ghc/ghc/issues/11775)</th>
<td>singleton classes in ghc.generics are defined but not exported</td></tr>
<tr><th>[\#11991](http://gitlabghc.nibbler/ghc/ghc/issues/11991)</th>
<td>Generics deriving is quadratic</td></tr>
<tr><th>[\#12144](http://gitlabghc.nibbler/ghc/ghc/issues/12144)</th>
<td>GHC panic when using DeriveAnyClass with functor-like class and datatype with a type variable in a contravariant position</td></tr>
<tr><th>[\#12367](http://gitlabghc.nibbler/ghc/ghc/issues/12367)</th>
<td>Commit adding instances to GHC.Generics regression compiler performance</td></tr>
<tr><th>[\#12423](http://gitlabghc.nibbler/ghc/ghc/issues/12423)</th>
<td>Panic with DeriveAnyClass</td></tr>
<tr><th>[\#12594](http://gitlabghc.nibbler/ghc/ghc/issues/12594)</th>
<td>DeriveAnyClass fails to derive some classes</td></tr>
<tr><th>[\#13059](http://gitlabghc.nibbler/ghc/ghc/issues/13059)</th>
<td>High memory usage during compilation</td></tr>
<tr><th>[\#13206](http://gitlabghc.nibbler/ghc/ghc/issues/13206)</th>
<td>Error in GHC.Generics documentation</td></tr>
<tr><th>[\#13272](http://gitlabghc.nibbler/ghc/ghc/issues/13272)</th>
<td>DeriveAnyClass regression involving a rigid type variable</td></tr>
<tr><th>[\#13314](http://gitlabghc.nibbler/ghc/ghc/issues/13314)</th>
<td>StandaloneDeriving and DeriveAnyClass don't work together</td></tr>
<tr><th>[\#13919](http://gitlabghc.nibbler/ghc/ghc/issues/13919)</th>
<td>Incorrect unused top binding warning when interacting with GHC.Generics</td></tr></table>



## Main components


- `TcDeriv.tcDeriving` now allows deriving `Generic` instances.

- The representation types and core functionality of the library live on `GHC.Generics` (on the `ghc-prim` package).

- Many names have been added as known in `prelude/PrelNames`

- Most of the code generation is handled by `types/Generics`

## Things that have been removed


- All of the [generic classes stuff](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). In particular, the following have been removed:

  - `hasGenerics` field from `TyCon`;
  - `HsNumTy` constructor from `HsType`;
  - `TypePat` constructor from `Pat`.

- The `-XGenerics` flag is now deprecated.

## What already works


- `Generic` and `Generic1` instances can be derived when `-XDeriveGeneric` is enabled.

- The `default` keyword can used for generic default method signatures when `-XDefaultSignatures` is enabled.

- Generic defaults are properly instantiated when giving an instance without defining the generic default method.

- Base types like `[]`, `Maybe`, tuples, come with Generic instances.

## Testing


- Tests are available under the `generics` directory of the testsuite.

# Differences from original paper



GHC generics was originally based off of the paper [
A Generic Deriving Mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf) (referred to henceforth as GDMH), but GHC generics has diverged from the original presentation in the paper since then. Here are some of the differences:


- What GDMH called `Representable0` and `Representable1` are now called `Generic` and `Generic1`
- GDMH has a type `Par0` for denoting occurrences of the last type parameter of a datatype in a `Generic` instance. GHC generics, however, does not use `Par0` anymore in derived instances (after all, you can have `Generic` instances for datatypes without type parameters!), and the `Par0` type has been marked as deprecated.
- GDMH uses auxiliary datatypes to encode metadata about datatypes, constructors, and selectors. In GHC 8.0, these auxiliary datatypes were scrapped in favor of a [type-level encoding of metadata](commentary/compiler/generic-deriving#).
- GHC generics has much more metadata than what was originally presented in GDMH (for example, `Selector` now encodes [strictness information](commentary/compiler/generic-deriving#strictness))
- GHC generics has an extra representation type (`URec`) for [certain unlifted types](commentary/compiler/generic-deriving#handling-unlifted-types)
- GHC generics alters the algorithm proposed in GDMH for implementing `to(1)`/`from(1)` for [performance reasons](commentary/compiler/generic-deriving#compilation-performance-tricks)

# Kind polymorphic overhaul



With the new `-XPolyKinds` functionality we can make the support for generic programming better typed. The basic idea is to define the universe codes (`M1`, `:+:`, etc.) as constructors of a datatype. Promotion then lifts these constructors to types, which we can use as before, only that now we have them all classified under a new kind. The overhaul of the main module is explained below; for easier comparison with the current approach, names are kept the same whenever possible.


## Generic representation universe



`m` is the only real parameter here. `f` and `x` are there because we
can't write kinds directly, since `Universe` is also a datatype (even if
we're only interested in its promoted version). So we pass `f` and `x`
only to set them to `* -> *` and `*`, respectively, in `Interprt`.
`m` is different: it stands for the kind of metadata representation types,
and we really want to be polymorphic over that, since each user datatype
will introduce a new metadata kind.


```wiki
data Universe f x m = 
  -- Void (used for datatypes without constructors)
    VV
    
  -- Unit
  | UU
  
  -- The parameter
  | PAR
  
  -- Recursion into a type of kind * -> *
  | REC f
  
  -- Constants (either other parameters or recursion into types of kind *)
  | KK Constant x
  
  -- Metadata
  | MM MetaData m (Universe f x m)
  
  -- Sum, product, composition
  | Universe f x m :++: Universe f x m
  | Universe f x m :**: Universe f x m
  | f :..: Universe f x m
  -- Note that we always compose a concrete type on the left (like []) with
  -- a generic representation on the right

infixr 5 :++:
infixr 6 :**:
infixr 6 :*:
infixr 7 :..:

-- Some shortcuts
data MetaData = CC | DD | SS
data Constant = PP | RR

data ConstantV (c :: Constant) where
  P :: ConstantV PP
  R :: ConstantV RR
  
data MetaDataV (m :: MetaData) where
  C :: MetaDataV CC
  D :: MetaDataV DD
  S :: MetaDataV SS
```

## Universe interpretation



As promised, we set `f` to `* -> *` and `x` to `*`.
Unfortunately we don't have [explicit kind variable annotations](ghc-kinds#)
yet, so we cannot leave `m` polymorphic! So this code doesn't compile:


```wiki
data Interprt :: Universe (* -> *) * m -> * -> * where

  -- No interpretation for VV, as it shouldn't map to any value
  
  -- Unit
  U1     :: Interprt UU p
  
  -- The parameter
  Par1   :: p -> Interprt PAR p
  
  -- Recursion into a type of kind * -> *
  Rec1   :: r p -> Interprt (REC r) p
  
  -- Constants
  K1     :: x -> Interprt (KK c x) p
  -- Constants shortcuts
  Par0   :: x -> Interprt (KK PP x) p
  Rec0   :: x -> Interprt (KK RR x) p
  
  -- Metadata
  M1     :: Interprt x p -> Interprt (MM m c x) p
  -- Metadata shortcuts
  D1     :: Interprt x p -> Interprt (MM DD c x) p
  C1     :: Interprt x p -> Interprt (MM CC c x) p
  S1     :: Interprt x p -> Interprt (MM SS c x) p
  
  -- Sum, product, and composition
  L1     :: Interprt a r -> Interprt (a :++: b) r
  R1     :: Interprt b r -> Interprt (a :++: b) r
  (:*:)  :: Interprt a r -> Interprt b r -> Interprt (a :**: b) r
  Comp1  :: f (Interprt g r) -> Interprt (f :..: g) r
```

### Names



As an aside, note that we have to come up with names like `UU` and `KK` for the `Universe`
even though we really just wanted to use `U1` and `K1`, like before. Then we would have
a type and a constructor with the same name, but that's ok. However, `Universe` defines
both a type (with constructors) and a kind (with types). So if we were to use `U1` in the
`Universe` constructors, then we could no longer use that name in the `Interprt`
constructors. It's a bit annoying, because we are never really interested in the type
`Universe` and its constructors: we're only interested in its promoted variant.
This is a slight annoyance of automatic promotion: when you define a "singleton type"
(like our GADT `Interprt` for `Universe`) you cannot reuse the constructor names.


## Metadata representation


```wiki
data Proxy d = Proxy -- kind polymorphic

-- Meta data classes
class Datatype d where -- kind polymorphic
  -- The name of the datatype, fully qualified
  datatypeName :: Proxy d -> String
```


There's more of these, but they don't add any new concerns.


## Conversion between user datatypes and generic representation



We now get a more precise kind for `Rep`:


```wiki
-- Representable types of kind *
class Generic a where
  type Rep a :: Universe (* -> *) * m
  from :: a -> Interprt (Rep a) x
  to   :: Interprt (Rep a) x -> a
  
-- Representable types of kind * -> *
class Generic1 (f :: * -> *) where
  type Rep1 f :: Universe (* -> *) * m
  from1  :: f a -> Interprt (Rep1 f) a
  to1    :: Interprt (Rep1 f) a -> f a
```


  


## Example generic function: `fmap` (kind `* -> *`)



User-visible class, exported:


```wiki
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: (Generic1 f, GFunctor (Rep1 f)) => (a -> b) -> f a -> f b
  fmap f = to1 . gfmap f . from1  
```


Defined by the generic programmer, not exported:


```wiki
class GFunctor (f :: Universe (* -> *) * m) where
  gfmap :: (a -> b) -> Interprt f a -> Interprt f b
  
instance GFunctor UU where
  gfmap _ U1 = U1
  
instance GFunctor PAR where
  gfmap f (Par1 a) = Par1 (f a)

instance GFunctor (KK i c) where
  gfmap _ (K1 a) = K1 a

instance (Functor f) => GFunctor (REC f) where
  gfmap f (Rec1 a) = Rec1 (fmap f a)

instance (GFunctor f) => GFunctor (MM m c f) where
  gfmap f (M1 a) = M1 (gfmap f a)

instance (GFunctor f, GFunctor g) => GFunctor (f :++: g) where
  gfmap f (L1 a) = L1 (gfmap f a)
  gfmap f (R1 a) = R1 (gfmap f a)

instance (GFunctor f, GFunctor g) => GFunctor (f :**: g) where
  gfmap f (a :*: b) = gfmap f a :*: gfmap f b

instance (Functor f, GFunctor g) => GFunctor (f :..: g) where
  gfmap f (Comp1 x) = Comp1 (fmap (gfmap f) x)
```


Note that previously `Functor` and `GFunctor` had exactly the same types.
Now we can make clear what the difference between them is.
  


## Example generic function: `show` (kind `*`, uses metadata)



User-visible class, exported:


```wiki
class Show (a :: *) where
  show :: a -> String
  default show :: (Generic a, GShow (Rep a)) => a -> String
  show = gshow . from
```


  
Defined by the generic programmer, not exported:


```wiki
class GShow (f :: Universe (* -> *) * m) where
  gshow :: Interprt f x -> String
  
instance GShow UU where
  gshow U1 = ""
  
instance (P.Show c) => GShow (KK i c) where
  gshow (K1 a) = P.show a
  
instance (Datatype c, GShow f) => GShow (MM DD c f) where
  gshow (M1 x) = datatypeName (Proxy :: Proxy c) ++ " " ++ gshow x
```


The other cases do not add any further complexity.
  
  


## Example datatype encoding: lists (derived by the compiler)


```wiki
instance Generic [a] where
  type Rep [a] = MM DD DList 
                   (MM CC DList_Nil UU :++: 
                    MM CC DList_Cons (KK PP a :**: KK RR [a]))

  from [] = D1 (L1 (C1 U1))
  from (h:t) = D1 (R1 (C1 (Par0 h :*: Rec0 t)))
  to (D1 (L1 (C1 U1))) = []
  to (D1 (R1 (C1 (Par0 h :*: Rec0 t)))) = h:t
  
-- Metadata
data List_Meta = DList | DList_Nil | DList_Cons
```


Note that we use only one datatype; more correct would be to use 3, one for
`DList`, another for the constructors, and yet another for the selectors
(or maybe even n datatypes for the selectors, one for each constructor?)
But we don't do that because `Universe` is polymorphic only over `m`, so
a single metadata representation type. If we want a more fine-grained
distinction then we would need more parameters in `Universe`, and also to
split the `MM` case.


```wiki
instance Datatype DList where datatypeName _ = "[]"
```


  


### Digression



Even better would be to index the metadata representation types over
the type they refer to. Something like:


```wiki
  data family MetaTypes a -- kind polymorphic
  data instance MetaTypes [] = DList | DList_Nil | DList_Cons
```


But now we are basically asking for promotion of data families, since we want
to use promoted `DList`. Also, the case for `MM` in `Universe` would then
be something like:


```wiki
  | MM MetaData (MetaTypes m) (Universe f x m)
```


But I'm not entirely sure about this.


## GHC 8.0 and later


### Type-level metadata encoding



Because what we've described so far is rather backwards-incompatible, we wanted to at least try to improve the encoding of metadata, which was currently rather clunky prior to GHC 8.0 (giving rise to lots of empty, compiler-generated datatypes and respective instances). We can accomplished that by changing `M1` to keep the meta-information *at the type level*:


```wiki
newtype M1 i (c :: Meta) f p = M1 { unM1 :: f p }

data Meta = MetaData Symbol Symbol  Bool
          | MetaCons Symbol FixityI Bool
          | MetaSel  Symbol SourceUnpackedness SourceStrictness DecidedStrictness

data Fixity  = Prefix  | Infix  Associativity Int
data FixityI = PrefixI | InfixI Associativity Nat

data Associativity = LeftAssociative
                   | RightAssociative
                   | NotAssociative

data SourceUnpackedness = NoSourceUnpackedness
                        | SourceNoUnpack
                        | SourceUnpack

data SourceStrictness = NoSourceStrictness
                      | SourceLazy
                      | SourceStrict

data DecidedStrictness = DecidedLazy
                       | DecidedStrict
                       | DecidedUnpack
```


Why did we need to add `FixityI`? Because `Fixity` does not promote. Yet, we wanted to expose `Fixity` to the user, not `FixityI`. Note that the meta-data classes remained mostly unchanged (aside from some enhancements to [
Datatype](https://ghc.haskell.org/trac/ghc/ticket/10030) and [
Selector](https://ghc.haskell.org/trac/ghc/ticket/10716)):


```wiki
class Datatype d where
  datatypeName :: t d (f :: * -> *) a -> [Char]
  moduleName   :: t d (f :: * -> *) a -> [Char]
  packageName  :: t d (f :: * -> *) a -> [Char]
  isNewtype    :: t d (f :: * -> *) a -> Bool

class Constructor c where
  conName     :: t c (f :: * -> *) a -> [Char]
  conFixity   :: t c (f :: * -> *) a -> Fixity
  conIsRecord :: t c (f :: * -> *) a -> Bool

class Selector s where
  selName               :: t s (f :: * -> *) a -> [Char]
  selSourceUnpackedness :: t s (f :: * -> *) a -> SourceUnpackedness
  selSourceStrictness   :: t s (f :: * -> *) a -> SourceStrictness
  selDecidedStrictness  :: t s (f :: * -> *) a -> DecidedStrictness
```


But now, using the magic of singletons, we give *one single instance* for each of these classes, instead of having to instantiate them each time a user derives `Generic`:


```wiki
instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, SingI nt)
    => Datatype ('MetaData n m p nt) where
  datatypeName _ = symbolVal (Proxy :: Proxy n)
  moduleName   _ = symbolVal (Proxy :: Proxy m)
  packageName  _ = symbolVal (Proxy :: Proxy p)
  isNewtype    _ = fromSing  (sing  :: Sing nt)

instance (KnownSymbol n, SingI f, SingI r)
    => Constructor ('MetaCons n f r) where
  conName     _ = symbolVal (Proxy :: Proxy n)
  conFixity   _ = fromSing  (sing  :: Sing f)
  conIsRecord _ = fromSing  (sing  :: Sing r)

instance (SingI mn, SingI su, SingI ss, SingI ds)
    => Selector ('MetaSel mn su ss ds) where
  selName               _ = fromMaybe "" (fromSing (sing :: Sing mn))
  selSourceUnpackedness _ = fromSing (sing :: Sing su)
  selSourceStrictness   _ = fromSing (sing :: Sing ss)
  selDecidedStrictness  _ = fromSing (sing :: Sing ds)
```


Naturally, we require singletons for `Bool`, `Maybe`, `FixityI`, `Associativity`, `SourceUnpackedness`, `SourceStrictness`, and `DecidedStrictness`, but that is one time boilerplate code, and is not visible for the user. (In particular, this is where we encode that the demotion of (the kind) `FixityI` is (the type) `Fixity`.)



I believe this change is almost fully backwards-compatible, and lets us simplify the code for `deriving Generic` in GHC. Furthermore, I suspect it will be useful to writers of generic functions, who can now match at the type-level on things such as whether a constructor is a record or not.



I say "almost fully backwards-compatible" because handwritten `Generic` instances might break with this change. But we've never recommended doing this, and I think users who do this are more than aware that they shouldn't rely on it working across different versions of GHC.


#### Example



Before GHC 8.0, the following declaration:


```
data Example = Example deriving Generic1
```


Would have generated all of this:


```
instance Generic Example where
  type Rep Example = D1 D1Example (C1 C1_0Example (S1 NoSelector U1))
  ...

data D1Example
data C1_0Example

instance Datatype D1Example where
  datatypeName _ = "Example"
  moduleName   _ = "Module"
  isNewtype    _ = False

instance Constructor C1_0Example where
  conName     _ = "Example"
  conFixity   _ = Prefix
  conIsRecord _ = False
```


But on GHC 8.0 and later, this is all that is generated (assuming it was compiled with no strictness optimizations):


```
instance Generic Example where
  type Rep Example =
    D1 ('MetaData "Example" "Module" "package" 'False)
      (C1 ('MetaCons "Example" 'PrefixI 'False)
        (S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
          U1))
  ...
```


Not bad!


### Strictness



The `Selector` class now looks like this:


```
class Selector s where
  selName               :: t s (f :: * -> *) a -> [Char]
  selSourceUnpackedness :: t s (f :: * -> *) a -> SourceUnpackedness
  selSourceStrictness   :: t s (f :: * -> *) a -> SourceStrictness
  selDecidedStrictness  :: t s (f :: * -> *) a -> DecidedStrictness

data SourceUnpackedness = NoSourceUnpackedness
                        | SourceNoUnpack
                        | SourceUnpack

data SourceStrictness = NoSourceStrictness
                      | SourceLazy
                      | SourceStrict

data DecidedStrictness = DecidedLazy
                       | DecidedStrict
                       | DecidedUnpack
```


This design draws much inspiration from the way Template Haskell handles strictness as of GHC 8.0 (see [
here](https://ghc.haskell.org/trac/ghc/ticket/10697) for what motivated the change). We make a distinction between the *source* strictness annotations and the strictness GHC actually *decides* during compilation. To illustrate the difference, consider the following data type:


```
data T = T {-# UNPACK #-} !Int
                          !Int
                           Int
  deriving Generic
```


If we were to encode the source unpackedness and strictness of each of `T`'s fields, they were be `SourceUnpack`/`SourceStrict`, `NoSourceUnpackedness`/`SourceStrict`, and `NoSourceUnpackedness`/`NoSourceStrictness`, no matter what. Source unpackedness/strictness is a purely syntactic property.



The strictness that the user writes, however, may be different from the strictness that GHC decides during compilation. For instance, if we were to compile `T` with no optimizations, the decided strictness of each field would be `DecidedStrict`, `DecidedStrict`, and `DecidedLazy`. If we enabled `-O2`, however, they would be `DecidedUnpack`, `DecidedStrict`, and `DecidedLazy`.



Things become even more interesting when `-XStrict` and `-O2` are enabled. Then the strictness that GHC would decided is `DecidedUnpack`, `DecidedStrict`, and `DecidedStrict`. And if you enable `-XStrict`, `-O2`, *and* `-funbox-strict-fields`, then the decided strictness is `DecidedUnpack`, `DecidedUnpack`, and `DecidedUnpack`.



The variety of possible `DecidedStrictness` combinations demonstrates that strictness is more just annotation—it's also a dynamic property of the compiled code. Both facets of strictness can be useful to a generic programmer, so we include both in the above design. That way, you get more bang for your buck.



One key difference between the way Template Haskell handles strictness and the way GHC generics would handle strictness is that in the former, source and decided strictness are decoupled, whereas the latter lumps them all into `MetaSel`. This is a result of Template Haskell allowing programmers to splice in TH ASTs directly into Haskell source code. Imagine if you wanted to splice in a datatype that had not been declared yet (i.e., one GHC had not compiled). You'd probably write something like this


```
$([d| ... Bang NoSourceUnpackedness NoSourceStrictness ??? ... |])
```


and become stuck. What do you put in place of `???`? After all, you'd need a *decided* strictness, but there's no way to know what GHC had decided yet—the datatype hasn't been compiled!



GHC generics does not suffer from this problem, since you can only reify metadata with generics, not splice metadata back into code. Therefore, we can bundle both source and decided strictness with `MetaSel` without issue.


# Using standard deriving for generic functions



Currently, users can instantiate a generic function to their datatypes by doing the following:


1. Attaching a `deriving Generic` clause to their datatype declaration:

  ```wiki
  data MyDatatype = .... deriving Generic
  ```
1. Giving an empty instance:

  ```wiki
  instance GBinary MyDatatype
  ```


However, it would be more natural and concise to simply be able to write:


```wiki
data MyDatatype = .... deriving (Generic, GBinary)
```


Ultimately, this might even allow us to replace the generated code by classes such as `Eq`, `Ord`, etc. with plain generic code.



The ticket for this request is [\#5462](http://gitlabghc.nibbler/ghc/ghc/issues/5462). We refer to this new feature as `DeriveAnyClass`.


## Which classes can be derived?



How do we figure out which classes will now be allowed as part of a deriving clause? We don't; we allow any class! Although the feature makes more sense for classes whose [\`MINIMAL\` set](http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#minimal-pragma) is empty, it'll work for any other class too. It simply generates an instance without any method definitions.


## Interaction with `GeneralizedNewtypeDeriving`



[GeneralizedNewtypeDeriving](http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#newtype-deriving) (GND) already allows many non-standard classes to be derived, but:


1. It means something different from ordinary deriving. Instead of generating new code for the instance, GHC simply uses the instance for the type inside the newtype.
1. As such, this only works when deriving newtypes, and
1. The classes `Read`, `Show`, `Typeable`, `Data`, and `Generic` even with GND on, give rise to standard instances. That is, in the following deriving clause

  ```wiki
  newtype MyInt = MyInt Int deriving (Eq, Num, Show)
  ```


`Eq` and `Num` operations will simply be lifted through the newtype coercion, but `show (MyInt 3)` will still print `MyInt 3`, not just `3`.



GND interacts with `DeriveAnyClass` because both are about generating instances for arbitrary classes, but they do it in different ways. There are multiple ways to handle this.


#### 1. Disallow `DeriveAnyClass` for newtypes



In this case we simply do not allow `GeneralizedDeriving` for newtypes. It's simple to implement, but it's a bit limiting, in that if you happen to be using newtypes for which you want generic instances, you have to go back to manually writing down the empty instances.


#### 2. Try both approaches, fail if there's ambiguity



In many cases it will be clear whether the user wants GND or the new `DeriveAnyClass`:


1. If GND is on, and `DeriveAnyClass` is off, then we do GND.
1. Conversely, if GND is off, and `DeriveAnyClass` is on, then we do the latter.
1. If both are on, we pick the `DeriveAnyClass` approach.


The problem with this approach is that in situation 3 users are left in a bad situation if they actually wanted GND; they'll have to manually write a `Coercible` instance, which is not nice.


#### 3. Let class authors specify how their classes should be derived



Right now we already treat some classes specially for GND: `Read`, `Show`, `Typeable`, `Data`, and `Generic` are derivable, but not via GND. We could make it a property of the class to say "do not use GND" in deriving clauses. In that case, we know that we can use the `DeriveAnyClass` strategy for newtypes instead.



Maybe we don't even need to make this user-controllable; a handful of built-in classes may suffice.


#### 4. Let users annotate their deriving clauses



We could even let users specify which of the two types of deriving they want:


```
newtype MyInt = MyInt Int
  deriving newtype  GBinary
  deriving anyclass GShow
```


Here, we use the keywords `newtype` and `anyclass` to specify that the `GBinary` instance should be created using GND (so that we do not have the unnecessary extra tag in the binary encoding), but `GShow` should use the "generic" instance (so that we still print `"MyInt"`). See the [deriving strategies page](commentary/compiler/deriving-strategies) for information.


## Standard vs. Standalone deriving



There's no reason to restrict this new deriving form to be attached to a datatype. Users should be free to write the following, if they wish:


```wiki
deriving instance (GBinary a) => GBinary (Maybe a)
```


... even though the following would do exactly the same with 9 characters less:


```wiki
instance (GBinary a) => GBinary (Maybe a)
```

## Figuring out the context



The only non-trivial part of actually generating the instances from a deriving clause is that now we have to figure out the context. Previously the user would have written


```wiki
data MyMaybe a ... deriving Generic
instance (GBinary a) => GBinary (MyMaybe a)
```


Now they'll simply write


```wiki
data MyMaybe a ... deriving (Generic, GBinary)
```


So it's now up to us to find out that the instance needs a `GBinary a =>` head.



For this we propose using the already existing strategy in GHC. Consider the following case:


```wiki
class GBinary  (a :: *)      where ...
class GFunctor (f :: * -> *) where ...

data MyMaybe a ... deriving (Generic, GBinary, GFunctor)
```


The instance head for the `GBinary (MyMaybe a)` instance will be guessed in the same way as if we were deriving an `Eq` instance. The instance head for the `GFunctor MyMaybe` instance will be guessed in the same way as if we were deriving a `Functor` instance.



The context might end up not being exactly what the user intended. For example, the class being derived might not require instances for each of the type arguments of the datatype. We *could* try to figure this out in a clever way from the definition of the class being derived, but this is very hard in general. So, in this case, we just leave the user to specify the context manually via standalone deriving (or, well, just giving an empty instance with a context, as they had to do before).


# Handling unlifted types



A weakness of generics (prior to GHC 8.0) was that it wasn't expressive enough to represent types of kind `#`. Part of the reason was due to `Rec0` expecting a type of kind `*`, not kind `#`.



Having a generic `#` representation would be desirable to mimic GHC's built-in support for certain unlifted types when deriving `Eq`, `Ord`, and `Show`:


```
data IntHash = IntHash Int# deriving (Eq, Ord, Show)

-- Wouldn't have been allowed!
deriving instance Generic IntHash
```

## Attempt 1: re-use `Rec0`



One especially simple approach to this problem would be to mark occurrences of `Int#` with `Rec0 Int`, `Char#` with `Rec0 Char`, etc. After all, `data Int = I# Int#`, so it would be simple to wrap up an occurrence of `Int#` with `I#`.



This approach suffers from a number of issues:


- Ambiguity. When we encounter `Rec0 Int`, does it refer to `Int` or `Int#`? Without giving `K1` a different tag, there'd be no way to distinguish them.
- Legacy issues. Because of the ambiguity of the representation, previously written generic code (which was not aware of unlifted types) will start to work with data types that have unlifted arguments. This may not be desirable. For example, a programmer may not want their class to support datatypes with unlifted arguments at all.

## Attempt 2: introduce new data types



To avoid reusing `Int`, `Char`, etc., we could introduce new data types such as `data UInt = UInt Int#`, `data UChar = UChar Char#`, etc. to represent occurrences of `Int#`, `Char#`, etc. in generic code. This would avoid any ambiguity issues, because we now have datatypes with the sole purpose of boxing unlifted types *in generic code*.



This approach is still lacking, however, because `UInt`, `UChar`, etc. aren't really representation types. To illustrate what that means, consider a simple generic program:


```
class GShow f where
    gshow :: f a -> String

...

instance Show c => GShow (K1 c) where
    gshow (K1 x) = gshow x

...
data Example = Example Int# deriving Generic
instance Show Example where show = gshow . from
```


When GHC creates the `Generic Example` instance, `UInt` appears as a type parameter of `K1` in `Example`'s representation type. This means that one can't use `GShow` to define how `UInt`s should be shown. Rather, one must use `Show` directly! This is a bit awkward, since we'd like to provide a `Show UInt` instance from `GHC.Generics` directly, but that would enforce how all generic programmers must show `UInt`.


## Attempt 3: separate unlifted representation types



Instead of using `Rec0` at all, let's create standalone representation types to mark occurrences of unlifted types:


```
data UInt  (p :: *) = UInt  Int#
data UChar (p :: *) = UChar Char#
...
```


This solves the problems with the previous two approaches, since (1) unlifted types are now completely separate cases from `Rec0`, and (2) we can properly use a `GShow UInt` instance (as an example).



There's a thorny issue that arises with this method: how can we define generic code that behaves the same on *every* unlifted data type? For example, what if a programmer wanted the string `"<unlifted>"` to show whenever any unlifted data type is encountered? She could write this:


```
instance GShow UInt where
    gshow _ = gshowUnlifted

instance GShow UChar where
    gshow _ = gshowUnlifted

...

gshowUnlifted :: String
gshowUnlifted = "<unlifted>"
```


But then she'd be writing `gshow _ = gshowUnlifted` for every single unlifted data type under consideration. That's more boilerplate than any reasonable generic programmer should have to deal with!


## Attempt 4: use a GADT



We could reformulate `UInt`, `UAddr`, etc. into a single GADT:


```
data URec :: * -> * -> * where
    UInt  :: Int#  -> URec Int  p
    UChar :: Char# -> URec Char p
    ...
```


Now the `GShow` example above can be dramatically simplified:


```
instance GShow (URec a) where
    gshow _ = "<unlifted>"
```


From a conceptual standpoint, however, a GADT doesn't quite capture the notion of what `URec` is trying to represent. A GADT implies a closed set of things that can be exhaustively pattern matched, but we certainly aren't encoding every unlifted data type in existence into `URec`! On the contrary, we deliberately only choose a handful of unlifted types to achieve parity with GHC's built-in deriving. Imagine if we were to support more unlifted types in the future—code which pattern-matched on a `URec` value would no longer be exhaustive!


## Current design: data family instances



To convey the property that `URec` is "open", we use a data family:


```
data family URec a p
data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#   }
data instance URec Char     p = UChar   { uChar#   :: Char#   }
data instance URec Double   p = UDouble { uDouble# :: Double# }
data instance URec Int      p = UInt    { uInt#    :: Int#    }
data instance URec Float    p = UFloat  { uFloat#  :: Float#  }
data instance URec Word     p = UWord   { uWord#   :: Word#   }

type UAddr   = URec (Ptr ())
type UChar   = URec Char
type UDouble = URec Double
type UFloat  = URec Float
type UInt    = URec Int
type UWord   = URec Word
```


With this approach, it is still possible to write an all-encompassing unlifted type case:


```
instance GShow (URec a) where
    gshow _ = "<unlifted>"
```


Or, if a programmer desires, she can write type-specific cases:


```
instance GShow UInt where
    gshow (UInt i) = show (I# i)

instance GShow UChar where
    gshow (UChar c) = show (C# c)

...
```


One downside is that data families are conceptually more complicated than any of the tricks we've used in previous approaches, which may add some cognitive overhead for generic programmers. What is clever about this design, however, is that if a programmer wishes, he can simply use the provided type synonyms and pretend that the `URec` family doesn't exist!


## Example



The following declaration:


```
data IntHash = IntHash Int# deriving Generic
```


yields:


```
instance Generic IntHash where
  type Rep IntHash =
    D1 ('MetaData "IntHash" "Module" "package" 'False)
      (C1 ('MetaCons "IntHash" 'Prefix 'False)
        (S1 'MetaNoSel UInt))
```

# Compilation performance tricks



Unfortunately, deriving `Generic` has been known to incur large compilation times. It is suspected that deriving `Generic` has both nonlinear behavior as well as a large constant overhead (see [\#5642](http://gitlabghc.nibbler/ghc/ghc/issues/5642) for further discussion). This section discusses some of the tricks GHC developers have used to make deriving `Generic` faster.


## Factoring out `M1`



If you have a datatype like:


```
data Letter = A | B | C | D
```


then a naïve `Generic` instance for `Letter` would be:


```
instance Generic Letter where
  type Rep Letter = D1 (MetaData ...) ...

  to (M1 (L1 (L1 (M1 U1)))) = A
  to (M1 (L1 (R1 (M1 U1)))) = B
  to (M1 (R1 (L1 (M1 U1)))) = C
  to (M1 (R1 (R1 (M1 U1)))) = D

  from A = M1 (L1 (L1 (M1 U1)))
  from B = M1 (L1 (R1 (M1 U1)))
  from C = M1 (R1 (L1 (M1 U1)))
  from D = M1 (R1 (R1 (M1 U1)))
```


Notice that in every LHS pattern-match of the `to` definition, and in every RHS expression in the `from` definition, the topmost constructor is `M1`. This corresponds to the datatype-specific metadata (the `D1` in the `Rep Letter` instance). But this is wasteful from a typechecking perspective, since this definition requires GHC to typecheck an application of `M1` in every single case, leading to an O(n) increase in the number of coercions the typechecker has to solve, which in turn increases allocations and degrades compilation speed.



Luckily, since the topmost `M1` has the exact same type across every case, we can factor it out reduce the typechecker's burden:


```
instance Generic Letter where
  type Rep Letter = D1 (MetaData ...) ...

  to (M1 x) = case x of
    L1 (L1 (M1 U1)) -> A
    L1 (R1 (M1 U1)) -> B
    R1 (L1 (M1 U1)) -> C
    R1 (R1 (M1 U1)) -> D

  from x = M1 (case x of
    A -> L1 (L1 (M1 U1))
    B -> L1 (R1 (M1 U1))
    C -> R1 (L1 (M1 U1))
    D -> R1 (R1 (M1 U1)))
```


A simple change, but one that pays off, since it turns an O(n) amount of coercions to an O(1) amount.


