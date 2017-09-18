

This wiki page is by Nicolas Frisby (nfrisby).


# Overview



I've been recently developing a `coxswain` library that defines row types and implements row unification.



My motivations are as follows.


- I think the Haskell community is hungry for row types, especially records and variants.
- I'm pretty familiar with the Core simplifier, but not the GHC type checker. The OutsideIn JFP is a wonder (!), but it's just too much for me to internalize without getting my hands dirty. So this plugin seemed like a wonderful way to learn.
- I think the plugin API is important, and I wanted to get a sense of its current state and capabilities, both for my knowledge and as feedback to its owners.


This wiki page outlines the basic design and intended use of the library, use cases I envision, a rough bibliography, my plugin architecture, my notes about the type checker plugin API, and my open questions.



(**DISCLAIMER** In the following text, I make lots of claims about what the plugin does and doesn't do. But it's very much a work in progress (remember, I'm using it to learn). I don't have anything that's anywhere close to approaching a proof of its correctness.)



The plugin code is on GitHub, BSD3. [
https://github.com/nfrisby/coxswain](https://github.com/nfrisby/coxswain)


# Plugin Debug Options



The plugin understands a couple debugging options, if you want to inspect what it's doing to constraints.


- `-fplugin-opt=Coxswain:summarize` This will dump out the "before-and-after" constraints of each time GHC gives the plugin a "task" (e.g. solve the Wanted constraints in some definition's RHS).
- `-fplugin-opt=Coxswain:trace` This will dump out the constraints and the contradictory or solved/new constraints that the plugin finds every time GHC invokes the plugin (i.e. multiple times per "task").
- `-fplugin-opt=Coxswain:logfile=<file>` This will write that output to the specified file instead of to the default of `stderr`.

# Demonstration


## Comparison to the Nice Elm Overview



To get a basic sense of what the library feels like, the following code block imitates the well-written and beginner-friendly [
documentation page about records in Elm](http://elm-lang.org/docs/records#what-is-a-record-). It imports the `Data.Sculls.Symbol` module from the `sculls` package, which is my current draft of record types on top of the `coxswain` row types.



(Please forgive my punny names. Naming is one of the most fun parts for me. If this were to gain enough momentum and "official" adoption, I would suggest renaming it to something plain, like `row-types` and the `RowTypes` module and `record-types` and the `Data.Record` e.g. module.)


```wiki
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=Coxswain #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}

module Elm where

import Data.Sculls.Symbol
import GHC.TypeLits (type (+))
import Text.Read (readMaybe)

----- What is a Record?

frag1 = mkR .* #x .= 3 .* #y .= 4

frag2 = mkR .* #title .= "Steppenwolf" .* #author .= "Hesse" .* #pages .= 237

----- Access

point2D = mkR
  .* #x .= 0
  .* #y .= 0

point3D = mkR
  .* #x .= 3
  .* #y .= 4
  .* #z .= 12

-- PEOPLE

bill = mkR
  .* #name .= "Gates"
  .* #age .= 57

steve = mkR
  .* #name .= "Jobs"
  .* #age .= 56

larry = mkR
  .* #name .= "Page"
  .* #age .= 39

people =
  [ bill
  , steve
  , larry
  ]

----- Access

frag3 = (
    point3D `dot` #z
  ,
    bill `dot` #name
  ,
    (`dot` #name) bill
  ,
    map (`dot` #age) people
  )

frag4 = (
    point2D `dot` #x
  ,
    point3D `dot` #x
  ,
    (mkR .* #x .= 4) `dot` #x
  )

----- Pattern Matching   -- TODO: record patterns

frag5 !r =
  sqrt (x^2 + y^2)
  where
  x = r `dot` #x
  y = r `dot` #y

frag6 !r =
  age < 50
  where
  age = r `dot` #age

-----  Updating R Iecords

frag7 = (
    point2D ./* #y .= 1
  ,
    point3D ./* #x .= 0 ./* #y .= 0
  ,
    steve ./* #name .= "Wozniak"
  )

rawInput = mkR
  .* #name .= "Tom"
  .* #country .= "Finland"
  .* #age .= "34"
  .* #height .= "1.9"

prettify person = person
  ./* #age .= toInt (person `dot` #age)
  ./* #height .= toFloat (person `dot` #height)
  where
  toInt :: String -> Maybe Int
  toInt = readMaybe
  toFloat :: String -> Maybe Float
  toFloat = readMaybe

input =
  prettify rawInput

----- R Iecord Types

origin :: R I (Row0 .& "x" .= Float .& "y" .= Float)
origin = mkR
  .* #x .= 0
  .* #y .= 0

type Point = Row0
  .& "x" .= Float
  .& "y" .= Float

hypotenuse :: R I Point -> Float
hypotenuse !p =
  sqrt (x^2 + y^2)
  where
  x = p `dot` #x
  y = p `dot` #y

type Positioned a = a
  .& "x" .= Float
  .& "y" .= Float

type Named a = a
  .& "name" .= String

type Moving a = a
  .& "velocity" .= Float
  .& "angle" .= Float

lady :: R I (Named (Row0 .& "age" .= Int))
lady = mkR
  .* #name .= "Lois Lane"
  .* #age .= 31

dude :: R I (Named (Moving (Positioned Row0)))
dude = mkR
 .* #x .= 0
 .* #y .= 0
 .* #name .= "Clark Kent"
 .* #velocity .= 42
 .* #angle .= 30  -- degrees

getName ::
     (
       Lacks a "name"   -- Necessary difference compared to Elm. Comparable to KnownNat/KnownSymbol/etc.
     ,
       Short (NumCols a)   -- Merely consequence of particular record implemention.
     )
  => R I (Named a) -> String
getName !r = r `dot` #name

names :: [String]
names =
  [ getName dude, getName lady ]

getPos ::
     (
       Lacks a "y" , Lacks a "x"   -- Necessary difference compared to Elm. Comparable to KnownNat/KnownSymbol/etc.
     ,
       Short (NumCols a + 1)   -- Merely consequence of particular record implemention.
     )
  => R I (Positioned a) -> (Float,Float)
getPos !r = (x,y)
  where
  x = r `dot` #x
  y = r `dot` #y

positions :: [(Float,Float)]
positions =
  [ getPos origin, getPos dude ]

----- BEYOND THE ELM TUTORIAL

vars :: [V I (Row0 .& "x" .= Int .& "y" .= Char .& "z" .= Bool)]
vars = [inj #z True,inj #x 7,inj #y 'h',inj #z False,inj #y 'i',inj #x 3]

pvars = vpartition vars
```


At the GHCi prompt, those definitions result in the following.


```
*Main> vars
[<z True>,<x 7>,<y 'h'>,<z False>,<y 'i'>,<x 3>]
*Main> pvars
{(x=[7,3]) (y="hi") (z=[True,False])}
```


The `vpartition` function is not a primitive! The following is its definition inside `sculls`.


```

-- | Partition a list of variants into in list-shaped record of the
-- same row.
vpartition :: forall (p :: Row kl *) f. (Foldable f,Short (NumCols p),Short (NumCols p - 1)) => f (V I p) -> R (F []) p
vpartition = foldr cons (rpure (F []))
  where
  cons v !acc = velim (f /$\ rhas) v
    where
    f :: forall (l :: kl) t. (HasCol p :->: I :->: C (R (F []) p)) l t
    f = A $ \HasCol -> A $ \x -> C $ runIdentity $ rlens (Identity . g x) acc

  g :: forall (l :: kl) t. I l t -> F [] l t -> F [] l t
  g (I x) (F xs) = F (x:xs)
```


Unpacking that definition is a helpful overview of the internals of the `sculls` library. We'll return to do that after a tour of the basic structures of `coxswain` and `sculls`.


## The Row Type Basics



(I've since generalized `Row` to be polykinded in the column type as well as the column name. But I haven't yet updated this section.)



We'll start with the core row type declarations from the `coxswain` library.


```

-- | Column kind.
data Col (k :: Type)

infix 3 .=
-- | Column constructor.
type family (l :: k) .= (t :: Type) = (col :: Col k) | col -> l t where

-- | Row kind.
data Row (k :: Type)

-- | The empty row.
type family Row0 :: Row k where

infixl 2 .&
-- | Row extension.
type family (p :: Row k) .& (col :: Col k) = (q :: Row k) where
```


Those declarations are closed empty type families; the type checker plugin is what gives them semantics. So they're just syntax: `Row0 .& "a" .= Int .& "b" .= Bool` is a row type with two columns: an `Int` named `a` and a `Bool` named `b`. The only row type formers are the empty row and row extension. (Future version might include row union, row intersection, etc.)



The whole point of rows is that column order doesn't matter. So `Row0 .& "a" .= Int .& "b" .= Bool` should be equal (really actually equal, as in `~`) to `Row0 .& "b" .= Bool .& "a" .= Int`. That's what the type checker plugin achieves, by implementing a well-established approach called *row unification*, which is pretty simple. The engineering effort was to realize it as a plugin to GHC's OutsideIn formalism.



Row unification determines what is in a row, but we usually also need to know what is **not** in the row. This is the role of the `Lacks` constraint. `Lacks p l` means that `l` is not the name of a column in `p`. The cleverest bit about `Lacks` constraints is how to evidence them: it's the position in the **sorted** columns of the concrete row at which `l` *would be* inserted to preserve the sorting. So, presuming intuitive order, `Lacks (Row0 .& "b" .= Int) "a"` would be 0, since `a` would be inserted at the front. And `Lacks (Row0 .& "a" .= Int) "b"` would be 1, since `b` would be the second column.


```
-- | Evidence of a 'Lacks' constraint.
newtype RowInsertionOffset (p :: Row k) (l :: k) = MkRowInsertionOffset Word16

-- | The lacks predicate.
class Lacks (p :: Row k) (l :: k) where rowInsertionOffset_ :: RowInsertionOffset p l

-- | Returns the index in the normal form of @p@ at which @l@ would be inserted.
rowInsertionOffset :: forall p (l :: k). Lacks p l => Proxy# p -> Proxy# l -> Word16
```


There's an inductive specification: `Lacks Row0 l` is 0, `Lacks (p .& l1 .= t) l2` is `Lacks p l2` if `l2 < l1` and it's `Lacks p l2` plus one if `l1 > l2` -- it's a contradiction if `l1 ~ l2`. This is the logic the plugin implements.



That's all of what's covered in most presentations of row types and row unification. But we need more in order to be a full participant in Haskell! For example, you can't use the row types syntax in type class instances, since they're type families! Row types are structural, not nominal, and type families and type classes can only scrutinize nominal types. My solution to that is to have the plugin provide a couple useful interpretations of a row type.


```
infixl 2 `NExt`
-- | A normalized row.
--
-- The following invariant is intended, and it is ensure by the 'Normalize' type family.
--
-- INVARIANT: The columns are sorted ascending with respect to an arbitrary total order on the column names.
data NRow k =
    NRow0  -- ^ The empty row.
  |
    NExt (NRow k) k Type   -- ^ A row extension.

-- | Normalize a row.
type family Normalize (p :: Row k) = (q :: NRow k) | q -> p where

-- | The number of columns in a row.
type family NumCols (p :: Row k) :: Nat where
```


The plugin makes `Normalize` and `NumCols` do what you'd expect. The key is that we can index type classes and such by `Normalize p` and `NumCols p` in order to overload based on the *observable* structure of the row.



There's one last piece to mention. In Haskell, we're unaccustomed to "negative information" like the `Lacks` constraint. What we typically see is positive information, like a "Has" constraint. With this classic approach, positive information is equality information, which row unification lets us use without concern for ordering. (If order does matter, then we don't need rows, we can just use type-level lists of columns (like `NRow`) and GHC's normal unification will suffice. This would be like Python's "named tuples".)



Clearly our rows do *have* columns, so we should be able to write "Has" constraints, right? Yes, and there are two definitions of "Has" in terms of `Lacks` and `~` (at kind `Row`).


```
-- | @HasRestriction p q l t@ evidences that @p@ is @q@ extended with @l '.=' t@.
data HasRestriction :: Row k -> Row k -> k -> * -> * where
  HasRestriction :: Lacks q l => HasRestriction (q .& l .= t) q l t

-- | @HasCol p l t@ evidences that there exists a @q@ such that @p@ is @q@ extended with @l '.=' t@.
data HasCol :: Row k -> k -> * -> * where
  HasCol :: Lacks q l => HasCol (q .& l .= t) l t
```


`HasRestriction` could be defined as a type class alias: `(Lacks q l,p ~ q .& l .= t) => HasRestriction p q l t`, but `HasCol` cannot, because it would require an existential for `q` on the equality constraint, which GHC currently does not support.



That's everything in the current `coxswain` package! Those are the exposed declarations, and the type checker plugin makes row unification and the `Normalize` and `NumCols` type families do what they're supposed to.


## The Record and Variant Basics



The `sculls` package defines types for records and variants. They both have two indices: the row type and the structure of each field.


```
newtype R (f :: k -> * -> *) (p :: Row k) = MkR ...   -- The omission is essentially a named tuple determined by Normalized p, with each component structured by f. (Notably, that's what most existing Haskell record libraries use as records, full stop.)

data V (f :: k -> * -> *) (p :: Row k) = MkV !Any !Word16   -- Yes, I use unsafeCoerce here. Sorry.
```


There are several useful ways to structure the fields.


- `I l t` is `t` -- it's the "identity" field structure.
- `F f l t` is `f t` -- it wraps the column type in a functor.
- `(f :->: g) l t` is `f l t -> g l t` -- it's a function between field structures.
- `C a l t` is `a` -- a constant field structure.
- `D c l t` is a dictionary for `c l t`, for when we want field's value to depend on the column types.


You can write your own, of course; it's just a type of kind `k -> * -> *`. (We could overload on the kind of `t` too, but I just haven't yet and don't yet have a concrete motivating use case.)



In fact, `HasCol p` has that kind, and we'll see below that it can be very useful as a field.


## Back to `vpartition`



Now let's revisit `vpartition` to see those pieces in action with some primitive functions on records and variants.


```

-- | Partition a list of variants into in list-shaped record of the
-- same row.
vpartition :: forall (p :: Row kl *) f. (Foldable f,Short (NumCols p),Short (NumCols p - 1)) => f (V I p) -> R (F []) p
vpartition = foldr cons (rpure (F []))
  where
  cons v !acc = velim (f /$\ rhas) v
    where
    f :: forall (l :: kl) t. (HasCol p :->: I :->: C (R (F []) p)) l t
    f = A $ \HasCol -> A $ \x -> C $ runIdentity $ rlens (Identity . g x) acc

  g :: forall (l :: kl) t. I l t -> F [] l t -> F [] l t
  g (I x) (F xs) = F (x:xs)
```

- The `Short (NumCols p)` constraint is an implementation detail of my current choice for the internal representation of the `R` type. Feel free to think of it as `KnownNat (NumCols p)`.
- `R f p` is a record for the row `p` where each field `l .= t` inhabits `f l t`.
- `I l t` is a newtype around `t`, so an `I`-structured record is what we normally think of when we hear "record".
- `V f p` is a variant, which is a sum of the columns instead of `R`'s product of columns.
- The `F f l t` field type is a newtype around `f t`, so `R (F []) p` is a record where every field is a list of the column type.
- `rpure` is a primitive that creates a record where every field has the given polymorphic value.
- `elimV` is a primitive identified by Gaster and Jones (1996) that takes a record of functions with a fixed codomain and a variant and applies the corresponding record to the variant's payload; i.e. it's a case expression where the record is the alternatives and the variant is the discriminant.
- `rmap` applies a record of functions to a record of arguments. The record of functions must be built with the `:->:` field type; like `I` and `F`, `f :->: g` is a newtype around `f l t -> g l t.`
- `rHasCol` builds a record where each field is evidence of its own existence! Here the field structure (like `I` and `F`) is `HasCol p l t`, which is a GADT evidencing that the row `p` has a column `l .= t`.
- The `C` field structure just holds a constant, irrespective of the column label and type.
- `rlens` is a lens into a field of a record; the expression above with the `Identity` `Applicative` achieves `over` from the `lens` library.

## Performance?



\[Thank you, Ara Adkins. I forgot my section about performance until your email.\]



As usual, the objective is for the library to have minimal run-time overhead. With hundreds of `Lacks` constraints floating around, that means -- again, as usual -- that inlining and specialization are key. Things look promising at this stage.



For example, the `vpartition` example from above (repeating here)


```

vars :: [V I (Row0 .& "x" .= Int .& "y" .= Char .& "z" .= Bool)]
vars = [inj #z True,inj #x 7,inj #y 'h',inj #z False,inj #y 'i',inj #x 3]

pvars :: R (F []) (Row0 .& "x" .= Int .& "y" .= Char .& "z" .= Bool)
pvars = vpartition vars
```


-- **at a concrete row type** -- essentially compiles down to the following Core at `-O1`, which is pretty good.


```

Rec {
-- RHS size: {terms: 48, types: 197, coercions: 7,299, joins: 0/0}
main_go
main_go
  = \ ds_a9PI eta_B1 ->
      case ds_a9PI of {
        [] -> eta_B1;
        : y_a9PN ys_a9PO ->
          case y_a9PN of { MkV flt_a9Lz dt3_a9LA ->
          case dt3_a9LA of {
            __DEFAULT ->
              case eta_B1 `cast` <Co:797> of { V3 a0_a9N8 a1_a9N9 a2_a9Na ->
              main_go
                ys_a9PO
                ((V3
                    a0_a9N8
                    a1_a9N9
                    ((: flt_a9Lz (a2_a9Na `cast` <Co:65>)) `cast` <Co:59>))
                 `cast` <Co:1512>)
              };
            0## ->
              case eta_B1 `cast` <Co:797> of { V3 a0_a9N8 a1_a9N9 a2_a9Na ->
              main_go
                ys_a9PO
                ((V3
                    ((: flt_a9Lz (a0_a9N8 `cast` <Co:65>)) `cast` <Co:59>)
                    a1_a9N9
                    a2_a9Na)
                 `cast` <Co:1512>)
              };
            1## ->
              case eta_B1 `cast` <Co:797> of { V3 a0_a9N8 a1_a9N9 a2_a9Na ->
              main_go
                ys_a9PO
                ((V3
                    a0_a9N8
                    ((: flt_a9Lz (a1_a9N9 `cast` <Co:65>)) `cast` <Co:59>)
                    a2_a9Na)
                 `cast` <Co:1512>)
              }
          }
          }
      }
end Rec }
```


The `V3` constructor is part of something I haven't talked about on this page yet: my representation strategy for the `R` type. That representation is a "short vector", which is a tuple of strict `Any`s that I coerce into and out of according to the column type. It's a data family indexed by `NumCols p`, and the `Short (NumCols p)` dictionaries provide operations on it. There are plenty of other options, and which one is right will depend on the situation. But, in my experience, short vectors (up to \~16 or so?) seem to optimize better as algebraic data types than as serialized byte arrays. That's definitely something to revisit eventually, with some hard measurements.


## And steak knives!



There are several more primitives. Most notably, records have both `Applicative`-like and `Traversable`-like properties, since they're products.


```

-- | Record analog of 'pure'.
rpure :: Short (NumCols p) => (forall (l :: k) t. f l t) -> R f p
-- | Record analog of 'fmap'.
rmap :: Short (NumCols p) => (forall (l :: k) t. (a :->: b) l t) -> R a p -> R b p
-- | Record analog of '<*>'.
rsplat :: Short (NumCols p) => R (a :->: b) p -> R a p -> R b p

-- | Combine all the fields' values. Beware that the fields are combined in an arbitrary order!
rfold :: (Short (NumCols p),Monoid m) => R (C m) p -> m
-- | Traverse all the fields. Beware that the fields are visited in an arbitrary order!
rtraverse :: (Applicative g,Short (NumCols p)) => (forall (l :: k) t. (f :->: F g :.: h) l t) -> R f p -> g (R h p)
```


Also, rows are finite, so we can require that a constraint holds for each column.


```

-- | Record of evidence that each column exists in the record's row.
rHasCol :: Short (NumCols p) => R (HasCol p) p
-- | A record of dictionaries for the binary predicate @c@ on column name and column type.
rdictCol :: RAll c p => Proxy# (c :: k -> * -> Constraint) -> R (D c) p
```


And records are `Show`able, `Monoid`s, `Generic`, etc, etc.



Variants exhibit the dual properties, for the most part.



Together, records and variants have a few useful interactions.


```

-- | Eliminate a variant with a functional record. (Gaster and Jones 1996)
velim :: Short (NumCols p) => R (f :->: C a) p -> V f p -> a

-- | Eliminate a record with a functional variant. (Gaster and Jones 1996)
relimr :: Short (NumCols p) => V (f :->: C a) p -> R f p -> a

-- | Convert each field to a variant of the same row.
rvariants :: Short (NumCols p) => R f p -> R (C (V f p)) p

-- | Partition a list of variants into in list-shaped record of the same row.
vpartition :: Short (NumCols p) => [V I p] -> R (F []) p
```

## Some Light Core Snorkeling



Simon Peyton Jones gave the following snippet and asks "What does Core look like?  I think your answer is “No change to Core, but there are lots of unsafe coerces littered around”.  But even then I’m not sure.  Somehow in the ??? \[below\] I have to update field n of a tuple x.  How do I do that?".


```

f :: Lacks r "f" => V (Row (r .& ("f" .= Int))) -> V (Row (r .& ("f" .= Int)))
f n x = ???
```


If I understand the question correctly, the source would look be as follows.


```

upd :: I "f" Int -> I "f" Int
upd (I x) = I (x + 1)

f ::
    ( Lacks r "f"
    , Short (NumCols r) )
  => R I (r .& "f" .= Int)
  -> R I (r .& "f" .= Int)
f = over rlens upd

-- Or (f r = r ./ #f .* #f .= (1 + r `dot` #f)), but that Core is worse.
```


I suspect the `Short` constraint is what Simon was smelling. This class was mentioned a bit in the previous section about performance. Here is what the `Short` class looks like, to offer some more insights:


```

-- | A short, homogenous, and strict tuple.
data family SV (n :: Nat) :: * -> *

type Fin (n :: Nat) = Word16

-- | Predicate for supported record sizes.
class (Applicative (SV n),Traversable (SV n)) => Short (n :: Nat) where
  select :: SV (n + 1) a -> Fin (n + 1) -> a
  lensSV :: Functor f => (a -> f a) -> SV (n + 1) a -> Fin (n + 1) -> f (SV (n + 1) a)
  extend :: SV n a -> a -> Fin (n + 1) -> SV (n + 1) a
  restrict :: SV (n + 1) a -> Fin (n + 1) -> (a,SV n a)
  indices :: SV n (Fin n)
```


The `SV` data family provides the tuples of `Any` that I currently use to represent records. For example, the `rlens` record primitive is defined by simply coercing the `lensSV` method. I generate `SV` and `Short` instances via Template Haskell. The `SV` instance for `2` and its `lensSV` method definition are as follows.


```

data instance SV 3 a = V3 !a !a !a
  deriving (Foldable,Functor,Show,Traversable)

instance Short 2 where
  ...
  {-# INLINE lensSV #-}
  lensSV f (V3 a b c) = \case
    0 -> (\x -> V3 x b c) <$> f a
    1 -> (\x -> V3 a x c) <$> f b
    _ -> (\x -> V3 a b x) <$> f c
```


Thus, the `-O1 -dsuppress-all` core for `f` is as follows.


```

-- RHS size: {terms: 8, types: 6, coercions: 4, joins: 0/0}
f2
f2
  = \ x_a5UG ->
      case x_a5UG `cast` <Co:4> of { I# x1_a6ax -> I# (+# x1_a6ax 1#) }

-- RHS size: {terms: 10, types: 53, coercions: 175, joins: 0/0}
f1
f1
  = \ @ r_a5QC $dLacks_a5QE $dShort_a5QF eta_B1 ->
      lensSV
        ($dShort_a5QF `cast` <Co:25>)
        $fFunctorIdentity
        (f2 `cast` <Co:41>)
        (eta_B1 `cast` <Co:96>)
        ($dLacks_a5QE `cast` <Co:13>)

-- RHS size: {terms: 1, types: 0, coercions: 115, joins: 0/0}
f
f = f1 `cast` <Co:115>
```


That reveals the necessary dictionary passing for row polymorphic functions. If we specialize the row variable `r`, we get something much closer to the ideal Core.


```

fmono ::
     R I (Row0 .& "a" .= a .& "b" .= b .& "c" .= c .& "f" .= Int)
  -> R I (Row0 .& "a" .= a .& "b" .= b .& "c" .= c .& "f" .= Int)
fmono = f

--- That optimizes to:

-- RHS size: {terms: 13, types: 9, coercions: 98, joins: 0/0}
fmono2
fmono2
  = \ @ c_a5Rb @ b_a5Ra @ a_a5R9 ->
      case (\ @ kt_X6bA -> W16# 0##) `cast` <Co:98> of { W16# x#_a6b2 ->
      W16# (narrow16Word# (plusWord# x#_a6b2 3##))
      }

-- RHS size: {terms: 60, types: 89, coercions: 1,925, joins: 0/0}
fmono1
fmono1
  = \ @ c_a5Rb @ b_a5Ra @ a_a5R9 eta_B1 ->
      case eta_B1 `cast` <Co:405> of
      { V4 a0_a6ch a1_a6ci a2_a6cj a3_a6ck ->
      case a3_a6ck `cast` <Co:45> of wild1_s6fH { I# x_s6fI ->
      case a2_a6cj `cast` <Co:45> of wild2_s6fK { I# x1_s6fL ->
      case a1_a6ci `cast` <Co:45> of wild3_s6fN { I# x2_s6fO ->
      case a0_a6ch `cast` <Co:45> of wild4_s6fQ { I# x3_s6fR ->
      case fmono2 of { W16# x4_a6cK ->
      case x4_a6cK of {
        __DEFAULT ->
          (V4
             (wild4_s6fQ `cast` <Co:48>)
             (wild3_s6fN `cast` <Co:48>)
             (wild2_s6fK `cast` <Co:48>)
             ((I# (+# x_s6fI 1#)) `cast` <Co:46>))
          `cast` <Co:145>;
        0## ->
          (V4
             ((I# (+# x3_s6fR 1#)) `cast` <Co:46>)
             (wild3_s6fN `cast` <Co:48>)
             (wild2_s6fK `cast` <Co:48>)
             (wild1_s6fH `cast` <Co:48>))
          `cast` <Co:145>;
        1## ->
          (V4
             (wild4_s6fQ `cast` <Co:48>)
             ((I# (+# x2_s6fO 1#)) `cast` <Co:46>)
             (wild2_s6fK `cast` <Co:48>)
             (wild1_s6fH `cast` <Co:48>))
          `cast` <Co:145>;
        2## ->
          (V4
             (wild4_s6fQ `cast` <Co:48>)
             (wild3_s6fN `cast` <Co:48>)
             ((I# (+# x1_s6fL 1#)) `cast` <Co:46>)
             (wild1_s6fH `cast` <Co:48>))
          `cast` <Co:145>
      }}}}}}}

-- RHS size: {terms: 4, types: 9, coercions: 268, joins: 0/0}
fmono
fmono = (\ @ a_a5R9 @ b_a5Ra @ c_a5Rb -> fmono1) `cast` <Co:268>
```


I wasn't expecting that worrying number of coercions or that `fmono2` declaration. Adding those to the list. Thanks, Simon!


# Future Directions


## Other Row Relations



For example, `SameLabels p q` would not unify the column types. And `WiderThan p q` would require that `p` is a structural subtype (width
subtyping) of `q`: i.e. `p` has every column that `q` has.



For these two examples at least, the relevant unification could be
realized via actual equalities between adjustments of `p` and `q`. For
`SameLabels`, the adjustment is to replace all column types in at least
one of `p` and `q` with fresh unification vars. For `WiderThan`, it's to
replace the occurrence of `Row0` in the narrower row (`p`) with a fresh
unification var. Note however that this approach does not provide
actionable evidence! For example, `WiderThan p q` should support a
function `upcast :: WiderThan p q => R f p -> R f q`. The necessary
evidence for that seems to be an offset into `p` for each column of `q`.


## Other Row Operators



Some users might also like to intersect or union their rows. And if we
have polykinded column types, then these operators could either unify
the colliding column types or they could pair them up.



These operators could currently be handled via type classes on
Normalized rows, but that would be noisier in the type system and less
well-behaved, I think -- e.g. `Lacks (p `Union` q) l` should imply `Lacks p l`.


## Set Types



By "set type", I mean a row type where there is no column types. Is
there a proper term for this? Whatever it is, it's not useless,
e.g. consider homogenous record/variant types, type-indexed
records/variants, permissions/privileges trackings, restricted/indexed
monads, etc!



In fact, I think such type-indexed records/variants might even be more
useful **in Haskell** than the "full" ones because they're less
structural/more nominal and so more conducive to type class
overloading etc.



These can encoded within the current implementation (just ignore the
column type... they'll all be `Any` eventually) --- and particularly
cleanly if we had polykinded column types inhabiting a void kind ---
but it'll always be cleaner to have direct support.


## Front-End Plugins



Would a front-end plugin be able to automate Lacks constraints from
manifest row extensions? This would be nicer, but it would also
introduce some "invisible" dictionary arguments, and that makes me
nervous about transparency.



Also, maybe it could achieve eta-expansion for any errant `Col` vars?


# Use Cases for Rows, Records, Variants


## The Expression Problem



Row polymorphism is a pretty great fit for modularity and extensibility.


## Named Arguments



Once a function has even three arguments, it gets pretty confusing to
identify each of the arguments at a call site (especially a few months
later!). `f x y z` would become `f (mkR .* #foo .= x .* #bar .= y .* #baz .= z)`.



For example, from O'Sullivan's `math-functions` package, the [
\`newtonRaphson\` function](http://hackage.haskell.org/package/math-functions-0.2.1.0/docs/Numeric-RootFinding.html#v:newtonRaphson) has the following signature.


```

-- | Solve equation using Newton-Raphson iterations.
--
-- This method require both initial guess and bounds for root. If
-- Newton step takes us out of bounds on root function reverts to
-- bisection.
newtonRaphson
  :: Double
     -- ^ Required precision
  -> (Double,Double,Double)
  -- ^ (lower bound, initial guess, upper bound). Iterations will no
  -- go outside of the interval
  -> (Double -> (Double,Double))
  -- ^ Function to finds roots. It returns pair of function value and
  -- its derivative
  -> Root Double

```


Using a record for the parameters might result in the following signature.


```

newtonRaphson ::
     R I (Row0
     .& "precision" .= Double
     .& "lowerbound" .= Double
     .& "guess0" .= Double
     .& "upperbound" .= Double
     )
     -- ^ Required precision. Iterations will no go outside of the interval.
  -> (Double -> (Double,Double))
  -- ^ Function to finds roots. It returns pair of function value and
  -- its derivative
  -> Root Double

```


Someday, maybe Haddock would allow per-column comments.


## VoR Generics



Especially if we had intersection / union / width subtyping, etc --- I
think (type-indexed?) Variants of (type-indexed?) Records would be an
excellent representation type, generalizing the sum-of-products
representation.



Along these same lines, I suspect it might be a useful programming
practice for some data types to simply be originally implemented as a
type-indexed sum where each summand is a nominal record type (i.e. a
proper Haskell data type or a newtype wrapped record type).


## Command-Line Options Parser



I suspect a Variant of Records would be an excellent center piece of
the interface to a command-line option parsing library, building upon
Gonzalez's [
http://hackage.haskell.org/package/options](http://hackage.haskell.org/package/options).


## YAML/JSON/CSV/etc Format



These popular "human-readable" interchange/configuration formats all
seem to focus on structural subtyping in one way or another.



Might be great, combined with VoR Generics


## Modular/Extensible Reader Monad Environment



This one is a classic. Pretty self-explanatory, I think.


## Modular/Extensible Free "Effects" Monad



A type-indexed sum seems like a great fit for the "which effects"
index on a free monad.


## Row Type for Array Dimensions



I've been pondering using row types as the shape of an
multi-dimensional array: each dimension would have a strongly-typed
index instead of just an ordinal. Something along the lines of:
`matmul :: (Dim x,Dim n,Dim m,Num a) => proxy x -> M (Row0 .& m .& x) a -> M (Row0 .& n .& x) a -> M (n .& m) a`. The most obvious possible
drawback I've thought of here is that the arbitrary ordering of
columns means the in-memory representation (e.g. row-col vs col-row)
is unpredictable -- therefore a "named tuple" (which doesn't require
rows at all) might be better here.


# Rough Bibilography



Gaster and Jones 1996 - [
http://web.cecs.pdx.edu/\~mpj/pubs/polyrec.html](http://web.cecs.pdx.edu/~mpj/pubs/polyrec.html) - The fundamentals of row unification and lacks predicates.



Leijen 2004 - [
https://www.microsoft.com/en-us/research/publication/first-class-labels-for-extensible-rows/](https://www.microsoft.com/en-us/research/publication/first-class-labels-for-extensible-rows/) - Lots of inspiring use-cases for rows, records, and variants.



Vytiniotis et all 2011 - [
https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/](https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/) - Glorious motivation and details of GHC's OutsideIn(X) type checking algorithm. Unfortunately, spares on some details. For example: what is a Derived constraint and where exactly do they come from‽



Gundry 2015 - [
http://adam.gundry.co.uk/pub/typechecker-plugins/](http://adam.gundry.co.uk/pub/typechecker-plugins/) and [
https://github.com/adamgundry/uom-plugin/](https://github.com/adamgundry/uom-plugin/) - The only published type checker plugin I'm aware of. Very helpful.



Gundry et al 2015 - [
https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker](https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker) - The most documentation I could find about the type checker plugin API :(. It's helpful but terse; we'll need more going forward.



Baaij 2016 - [
http://christiaanb.github.io/posts/type-checker-plugin/](http://christiaanb.github.io/posts/type-checker-plugin/) - A very nice post that overviews the type checker plugin API. Fact: this post made me actually start this project; thanks Christiaan.


# Plugin Architecture



This is the key concept for my plugin's architecture.


```wiki
    Do Not Re-implement Substitution and Unification!
    
    GHC is really good and careful at unification and rewriting, better than I am (especially among Given constraints) -- so let it do it.
```


This also makes it possible to interact with other plugins (notably the GHCs type-equality solver to which I just referred!).



Now the interesting question was: How to correctly compel GHC into doing that? Gundry's paper was the most helpful, but the presentation of the interactions between GHC and the plugins was still too far removed from the actual implementation, specifically the split between simplification of given constraints and solving of wanted constraints and the explicit `\theta` substitution versus the implicit substitution in GHC.



I seem to have successfully compelled GHC to create my substitution for me, but I'm sadly still not sure that I'm not violating any crucial invariants. Primarily because I don't know what they all are! They're hard to find, and sometimes the listed invariants are perfectly fine to violate (e.g. flatness of `Xi` types in "new" `Ct`s in `TcPluginOk`; this is referenced as "Only canonical constraints that are actually in the inert set carry all the guarantees" in the source `Note [zonkCt behaviour]`). Also, they're often out-of-date, either wrong or mysteriously referring to source Notes that no longer exist.



Obviously, the plugin needs to simplify `Ct`s according to the row unification, `Lacks`, `Normalize`, and `NumCols` semantic rules. (The two type families are interesting because they don't "float to the top" like other constraints --- I have to seek them out within all `Cts` and rewrite the whole `Ct`. I essentially use SYB for that in my current implementation.)



But, in addition to the obvious simplifications (actually, sort of before and after doing those), my plugin uses the following approach in order to achieve row unification.


1. Immediately upon receiving Givens, thaw `Row` skolem vars, i.e. replace them by unification vars with the same kind and level. This enables certain Given-Given interactions, specifically "unification" via the GHC type equality solver rules EQSAME, EQDIFF, etc. especially involving flatten skolems.


This allows GHC's type equality solver to "learn" the equivalences between `Row` "skolem" vars for me.


1. Importantly, when renaming the skolems, I also create some fake Given constraints `Ren p_sk p_tau` where `p_tau` is the temporary replacement of `p_sk`. This is a robust way for me to later recover the `Row` var equalities that GHC's solver learned for me.

1. When finished simplifying givens (i.e. if I would be returning `TcPluginOk _ []`), replace any remaining thawed variables (I dissatisfyingly filter based on a `fsLit "$coxswain"`) by original skolems if possible or fresh skolems if not. Such fresh skolems correspond to row restrictions (i.e. removing one or more columns) of one or more original skolems.


The motivation here is that having unification variables in the Givens while simplifying Wanteds is A Bad Thing (e.g. loss of confluence). Also note that you cannot solve/create Givens when simplifying Wanteds, so this step has to happen when simplifying Givens.



I was worried that creating fresh skolems and leaving them in the Givens would be problematic, but it hasn't caused any trouble so far.



Note that I leave the `Ren` constraints, for the sake of the next step.


1. When solving Wanted constraints, first recover the learning substitution from the `Ren` constraints and apply it to the Wanted constraints.


Other than some care to handle nested Givens (i.e. when a signature with Givens is inside the scope of a signature with its own Givens), that's the whole exercise. 



It's most notable to me that I can apparently leave those fresh restriction skolems around, even within the Wanteds. It hasn't caused any trouble at all yet, though I suspect it might lead to confusing error messages. If need be, I can replace them with a closed empty type family for record restrictions, just for the sake of error messages. (The ultimate user should never use record restrictions, because they apparently lose principal types.)


# Plugin API Notes



These are things I lost hours learning, because I couldn't find any *obviously up-to-date* documentation.


## Do not zonk givens



`zonkCt` is not expected to work on givens. (Note: Gundry's `uom` plugin does apply `zonkCt` to givens.) In particular, zonking does
not see the latest expansion for the flatten skolems. It seems I have to do
the unflattening manually.



(I'm currently wondering if the "out-of-date cached expansion" I'm seeing is because I'm creating ill-formed variables in the Givens. However, there is this quote "NB: we do not expect to see any CFunEqCans, because zonkCt is only called on unflattened constraints." in the source `Note [zonkCt behaviour]`.)


## Preserve `CFunEqCan`



Never mutate/discard `CFunEqCan`s! Flatten skolems may *cache* their
own image, but the part of GHC that matches givens and wanteds doesn't
work without the flatten skolems' `CFunEqCan`. Also, the caches are not neccesarily up-to-date (see above about zonking givens).


## Preserve cc\_pend\_sc



Do not needlessly churn `CDictCan` constraints; needlessly toggling the `cc_pend_sc` field back to `True` leads to infinite loops.


## Inefficient `Nat` dictionaries



Evidence bindings for `KnownNat` are essentially `Integer`s, which GHC does a
surprisingly terrible job optimizing (e.g. Core dive `print (natVal(Proxy :: Proxy 3))` sometime...). So I've replace the use of `EvNum` with explicit
invocation of dictionaries for building `Word16`s bit by bit. GHC does a
great job with those. I also made the classes nullary: the `Nat`
parameter doesn't matter since these classes exist solely so that the
`DFunId` can be invoked -- there's no actual instance resolution for
these.


## What are the ways GHC invokes my plugin?



I wrote a plugin that merely pretty-prints its inputs and is a no-op
otherwise. I use this to inspect the ways in which GHC invokes
plugins.


### "Backwards" Invocations during Inference



For example, with GHC 8.2.1, the following module


```
{-# OPTIONS_GHC -fplugin=Dumper #-}

module Test (f) where

f x = [x] == [x] && null (show x)
```


results in the following output, which shows that for this module the
plugin first "solves Wanteds" (without any Givens, because there's no
signature, GADT patterns, etc) and then "simplifies givens" --- but
the "Givens" are just the unsolved Wanteds! This is somewhat backwards
compared to what we usually consider: here we're simplifying "Givens"
*after* solving Wanteds.


```
========== 0 ==========
given
derived
wanted
  [WD] $dEq_a2nz {1}:: Eq a0_a2nr[tau:2] (CDictCan)
  [WD] $dShow_a2ns {0}:: Show a0_a2nr[tau:2] (CDictCan)
========== 1 ==========
given
  [G] $dShow_a2DE {0}:: Show a_a2DD[sk:2] (CDictCan)
  [G] $dEq_a2DF {0}:: Eq a_a2DD[sk:2] (CDictCan)
derived
wanted
========== 2 ==========
given
  [G] $dShow_a2Dx {0}:: Show a_a2nr[sk:2] (CDictCan)
  [G] $dEq_a2Dy {0}:: Eq a_a2nr[sk:2] (CDictCan)
derived
wanted
```


I don't know why it appears to simplify those Givens twice.



If we add the signature `f :: (Eq a,Show a) => a -> Bool` and
recompile, then we only see the two Givens simplification invocations.


### Nested Declarations



For the following, we see lots of invocations.


```

f x = bar () where
  bar () = [x] == [x] && null (show x)

----- Dumps:

========== 0 ==========
given
derived
wanted
  [WD] $dEq_a2nL {1}:: Eq (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan)
  [WD] $dShow_a2q3 {0}:: Show (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan)
  [WD] hole{a2n3} {0}:: (p0_a1Tl[tau:2] :: RuntimeRep) ~# ('LiftedRep :: RuntimeRep) (CNonCanonical)
========== 1 ==========
given
derived
wanted
  [WD] $dEq_a2DQ {1}:: Eq (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan(psc))
  [WD] $dShow_a2DU {0}:: Show (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan(psc))
  [WD] hole{a2n3} {0}:: (p0_a1Tl[tau:2] :: RuntimeRep) ~# ('LiftedRep :: RuntimeRep) (CNonCanonical)
========== 2 ==========
given
derived
wanted
  [WD] $dEq_a2DV {1}:: Eq (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan)
  [WD] $dShow_a2DW {0}:: Show (p1_a1Tm[tau:2] |> (TYPE {a2n3})_N) (CDictCan)
  [WD] hole{a2n3} {0}:: (p0_a1Tl[tau:2] :: RuntimeRep) ~# ('LiftedRep :: RuntimeRep) (CNonCanonical)
========== 3 ==========
given
derived
wanted
  [WD] $dEq_a2DX {1}:: Eq p0_a1Tm[tau:2] (CDictCan(psc))
  [WD] $dShow_a2DY {0}:: Show p0_a1Tm[tau:2] (CDictCan(psc))
========== 4 ==========
given
derived
wanted
  [WD] $dEq_a2DX {1}:: Eq p0_a1Tm[tau:2] (CDictCan)
  [WD] $dShow_a2DY {0}:: Show p0_a1Tm[tau:2] (CDictCan)
========== 5 ==========
given
  [G] $dShow_a2E3 {0}:: Show p_a2E2[sk:2] (CDictCan)
  [G] $dEq_a2E4 {0}:: Eq p_a2E2[sk:2] (CDictCan)
  [G] $dShow_a2E3 {0}:: Show p_a2E2[sk:2] (CDictCan)
  [G] $dEq_a2E4 {0}:: Eq p_a2E2[sk:2] (CDictCan)
derived
wanted
========== 6 ==========
given
  [G] $dShow_a2DZ {0}:: Show p_a1Tm[sk:2] (CDictCan)
  [G] $dEq_a2E0 {0}:: Eq p_a1Tm[sk:2] (CDictCan)
  [G] $dShow_a2DZ {0}:: Show p_a1Tm[sk:2] (CDictCan)
  [G] $dEq_a2E0 {0}:: Eq p_a1Tm[sk:2] (CDictCan)
derived
wanted
```


I was expecting invocations 3 and one for simplifying its unsolved
Wanteds as Givens. I don't know levity polymorphism well enough to
grok invocations 0-2. I don't know why 4 duplicates 3. And I don't
know what's going on with 5 and 6: why they have redundant givens and
why 5 has different variable uniques. (I'm assuming they're the
"backwards inference" invocations.)



Adding a signature here makes a huge difference.


```

f :: (Eq a,Show a,Enum a) => a -> Bool
f x = bar () where
  bar () = [x] == [x] && null (show x)

----- Dumps:

========== 0 ==========
given
  [G] $dEq_a2Ml {0}:: Eq a_a2Mk[sk:2] (CDictCan)
  [G] $dShow_a2Mm {0}:: Show a_a2Mk[sk:2] (CDictCan)
  [G] $dEnum_a2Mn {0}:: Enum a_a2Mk[sk:2] (CDictCan)
derived
wanted
========== 1 ==========
given
derived
wanted
  [WD] $dEq_a2Nd {1}:: Eq a_a2Mv[sk:2] (CDictCan)
  [WD] $dShow_a2N7 {0}:: Show a_a2Mv[sk:2] (CDictCan)
========== 2 ==========
given
  [G] $dEq_a2Mx {0}:: Eq a_a2Mv[sk:2] (CDictCan)
  [G] $dShow_a2My {0}:: Show a_a2Mv[sk:2] (CDictCan)
  [G] $dEnum_a2Mz {0}:: Enum a_a2Mv[sk:2] (CDictCan)
derived
wanted
```


We see that only invocation 1 solves Wanteds. What if we split up the
method invocations?


```
f :: (Eq a,Show a,Enum a) => a -> Bool
f x = bar () && baz () where
  bar () = [x] == [x]
  baz () = null (show x)

----- Dumps:

========== 0 ==========
given
  [G] $dEq_a2Mm {0}:: Eq a_a2Ml[sk:2] (CDictCan)
  [G] $dShow_a2Mn {0}:: Show a_a2Ml[sk:2] (CDictCan)
  [G] $dEnum_a2Mo {0}:: Enum a_a2Ml[sk:2] (CDictCan)
derived
wanted
========== 1 ==========
given
derived
wanted
  [WD] $dShow_a2N5 {0}:: Show a_a2Mw[sk:2] (CDictCan)
========== 2 ==========
given
derived
wanted
  [WD] $dEq_a2Np {1}:: Eq a_a2Mw[sk:2] (CDictCan)
========== 3 ==========
given
  [G] $dEq_a2My {0}:: Eq a_a2Mw[sk:2] (CDictCan)
  [G] $dShow_a2Mz {0}:: Show a_a2Mw[sk:2] (CDictCan)
  [G] $dEnum_a2MA {0}:: Enum a_a2Mw[sk:2] (CDictCan)
derived
wanted
```


We see two separate solve-Wanteds invocations.



I think that the fact that we see one solve-Wanteds invocation per
nested declaration (both here and in the previous example) reflects
the fact that GHC (as discussed in the OutsideIn JFP paper) does not
generalize the nested declarations (i.e. making them polymorphic
functions that accept dictionary arguments, which the RHS of `f` would
then need to fabricate and supply; instead, those dictionaries'
bindings are floating all the way up to `f`, which its signature
happens to provide directly).


### Ambiguity Checks



When we're using type families, it becomes possible to write
signatures that are ambiguous, because the type variables are only
arguments to type families.



I'm struggling to come up with a good example here, so this one is
indeed ambiguous.


```
type family F a

f :: Eq (F a) => F a -> Bool
f x = [x] == [x]

--- Dumps:

========== 0 ==========
given
  [G] $dEq_a1Ui {0}:: Eq fsk0_a1Uo[fsk] (CDictCan)
  [G] cobox_a1Up {0}:: (F a_a1Uh[sk:2] :: *) ~# (fsk0_a1Uo[fsk] :: *) (CFunEqCan)
zonk given
  [G] $dEq_a1Ui {0}:: Eq (F a_a1Uh[sk:2]) (CDictCan)
derived
wanted
========== 1 ==========
given
  [G] $dEq_a1Ui {0}:: Eq fsk0_a1Uo[fsk] (CDictCan)
  [G] cobox_a1Up {0}:: (F a_a1Uh[sk:2] :: *) ~# (fsk0_a1Uo[fsk] :: *) (CFunEqCan)
derived
wanted
  [W] $dEq_a1Uu {0}:: Eq (F a0_a1Uj[tau:2]) (CDictCan)
  [WD] hole{a1Ut} {2}:: (F a0_a1Uj[tau:2] :: *) ~# (F a_a1Uh[sk:2] :: *) (CNonCanonical)

Test.hs:10:6: error:
    * Couldn't match type `F a0' with `F a'
      Expected type: F a -> Bool
        Actual type: F a0 -> Bool
      NB: `F' is a type function, and may not be injective
      The type variable `a0' is ambiguous
    * In the ambiguity check for `f'
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature: f :: Eq (F a) => F a -> Bool
   |
10 | f :: Eq (F a) => F a -> Bool
   |      ^^^^^^^^^^^^^^^^^^^^^^^
```


The `[WD] hole` Wanted in invocation 1 is GHC's way of determining whether the signature is ambiguous or not.



With the `coxswain` plugin, for example, ambiguity constraints like these arise and must be solved via the plugin's extra knowledge about equivalencies involving the `.&` and `.=` families.


### Conclusions



I unfortunately feel too adrift to draw any conclusions beyond "look
at these interesting invocations; be wary of assuming how exactly your
solver is invoked and in what order"!


# Outstanding Questions



These are questions I still don't know the answer to. They keep me up at night.


- When/where exactly do Derived constraints arise? I'm not recognizing them in the OutsideIn paper.

- Is there a robust way to maintain solver state across **related** invocations? I'm currently essentially serializing my state (phase and substitution) as spurious constraints. It's seemingly robust but seems wrong. If GHC maintained some kind of "thread of solving" state tokens I could use those to robustly store and recover my custom solver state in response to GHC's invocations of `tcPluginSolve ` separately without having to pollute the constraints.

- GHC has various kinds of variable and skolem (e.g. signature skolem) that I'm not recognizing in the OutsideIn paper. Is there a comprehensive discussion of them all?

- What would Core need (I'm hoping just `EvTerm`, actually) to remain statically typed instead of all these coercions the plugin and the `sculls` library (i.e. records and variants on top of rows) assert?
