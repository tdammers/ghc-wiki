# System FC: equality constraints and coercions



For many years, GHC's intermediate language was essentially:


- System Fw, plus
- algebraic data types (including existentials)


But that is inadequate to describe GADTs and associated types.  So in 2006 we extended GHC to support System FC, which adds


- equality constraints and coercions


You can find a full description of FC in the paper [
https://ghc.haskell.org/trac/ghc/wiki/ReadingList\#TypeEqualities](https://ghc.haskell.org/trac/ghc/wiki/ReadingList#TypeEqualities); note that GHC uses the system described in post-publication Appendix C, not the system in the main body of the paper.  The notes that follow sketch the implementation of FC in GHC, but without duplicating the contents of the paper.



A coercion `c`, is a type-level term, with a kind of the
form `T1 :=: T2`. (`c :: T1 :=: T2`) is a proof that a term of type `T1`
can be coerced to type `T2`. 
Coercions are classified by a new sort of kind (with the form 
`T1 :=: T2`).  Most of the coercion construction and manipulation functions
are found in the `Coercion` module, [compiler/types/Coercion.hs](/trac/ghc/browser/ghc/compiler/types/Coercion.hs).



Coercions appear in Core in the form of `Cast` expressions:
if `t :: T1` and `c :: T1:=:T2`, then `(t `cast` c) :: T2`. 
See [Commentary/Compiler/CoreSynType](commentary/compiler/core-syn-type).


## Coercions and Coercion Kinds



The syntax of coercions extends the syntax of types (and the type
`Coercion` is just a synonym for `Type`).  By representing coercion
evidence on the type level, we can take advantage of the existing
erasure mechanism and keep non-termination out of coercion proofs
(which is necessary to keep the system sound).  The syntax of
coercions and types also overlaps a lot.  A normal type is evidence
for the reflexive coercion, i.e.,


```wiki
Int :: Int :=: Int
```


Coercion variables are
used to abstract over evidence of type equality, as in


```wiki
(/\c::(a :=: Bool). \x::a. if (x `cast` c) then 0 else 1) :: (a :=: Bool) => a -> Int
```


There are also coercion constants that are introduced by the compiler
to implement some source language features (newtypes for now,
associated types soon and probably more in the future).  Coercion
constants are represented as `TyCon`s made with the constructor
`CoercionTyCon`. 



Coercions are type level terms and can have normal type constructors applied
to them.  The action of type constructors on coercions is much like in
a logical relation.  So if `c1 :: T1 :=: T2` then


```wiki
[c1] :: [T1] :=: [T2]
```


and if `c2 :: S1 :=: S2` then


```wiki
c1 -> c2 :: (T1 -> S1 :=: T2 -> S2)
```


The sharing of syntax means that a normal type can be looked at as
either a type or as coercion evidence, so we use two different kinding
relations, one to find type-kinds (implemented in Type as \`typeKind ::
Type -\> Kind\`) and one to find coercion-kinds (implemented in Coercion as
`coercionKind :: Coercion -> Kind`).



Coercion variables are distinguished from type variables, and
non-coercion type variables (just like any normal type) can be used as
the reflexive coercion, while coercion variables have a particular
coercion kind which need not be reflexive.  


## GADTs



The internal representation of GADTs is as regular algebraic datatypes that carry coercion evidence as arguments.  A declaration like


```wiki
data T a b where
  T1 :: a -> b -> T [a] (a,b)
```


would result in a data constructor with type


```wiki
T1 :: forall a b. forall a1 b1. (a :=: [a1], b :=: (a1, b1)) => a1 -> b1 -> T a b
```


This means that (unlike in the previous intermediate language) all data constructor return types have the form `T a1 ... an` where
`a1` through `an` are the parameters of the datatype.  



However, we also generate wrappers for GADT data constructors which have the expected user-defined type, in this case


```wiki
$wT1 = /\a b. \x y. T1 [a] (a,b) a b [a] (a,b) x y
```


Where the 4th and 5th arguments given to `T1` are the reflexive coercions


```wiki
[a]   :: [a] :=: [a]
(a,b) :: (a,b) :=: (a,b)
```


 


## Representation of coercion assumptions



In most of the compiler, as in the FC paper, coercions are abstracted
using `ForAllTy cv ty` where `cv` is a coercion variable, with a kind of
the form `PredTy (EqPred T1 T2)`.  However, during type inference it is
convenient to treat such coercion qualifiers in the same way other
class membership or implicit parameter qualifiers are treated.  So
functions like `tcSplitForAllTy` and `tcSplitPhiTy` and `tcSplitSigmaTy`,
treat `ForAllTy cv ty` as if it were `FunTy (PredTy (EqPred T1 T2)) ty`
(where `PredTy (EqPred T1 T2)` is the kind of `cv`).  Also, several of the `dataCon`XXX functions treat coercion members of the data constructor
as if they were dictionary predicates (i.e. they return the `PredTy (EqPred T1 T2)` with the theta).


## Newtypes are coerced types



The implementation of newtypes has changed to include explicit type coercions in the place of the previously used ad-hoc mechanism.  
For a newtype declared by


```wiki
newtype T a = MkT (a -> a)
```


the `NewTyCon` for `T` will contain n`t_co = CoT` where:


```wiki
CoT t : (T t :=: t -> t)
```


This `TyCon` is a `CoercionTyCon`, so it does not have a kind on its
own; it basically has its own typing rule for the fully-applied
version.  If the newtype `T` has k type variables, then `CoT` has arity at
most k.  In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had


```wiki
newtype S a = MkT [a]
```


then we would generate the arity 0 coercion `CoS : S :=: []`.  The
primary reason we do this is to make newtype deriving cleaner.  If the coercion
cannot be reduced in this fashion, then it has the same arity as the tycon.



In the paper we'd write


```wiki
axiom CoT : (forall t. T t) :=: (forall t. [t])
```


and then when we used `CoT` at a particular type, `s`, we'd say


```wiki
CoT @ s
```


which encodes as `(TyConApp instCoercionTyCon [TyConApp CoT [], s])`



But in GHC we instead make `CoT` into a new piece of type syntax
(like `instCoercionTyCon`, `symCoercionTyCon` etc), which must always
be saturated, but which encodes as


```wiki
TyConApp CoT [s]
```


In the vocabulary of the paper it's as if we had axiom declarations
like


```wiki
axiom CoT t :  T t :=: [t]
```


The newtype coercion is used to wrap and unwrap newtypes whenever the constructor or case is used in the Haskell source code.



Such coercions are always used when the newtype is recursive and are optional for non-recursive newtypes.  Whether or not they are used can be easily changed by altering the function mkNewTyConRhs in iface/BuildTyCl.lhs.


## Roles



Roles specify what nature of equality a coercion is proving. See [Roles](roles) and [RolesImplementation](roles-implementation).


## Simplification


- exprIsConApp\_maybe

- simplExpr
