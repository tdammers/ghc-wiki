# Trees that Grow Guidance



[
The Trees that Grow (TTG) idiom](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf) can be used to provide different forms of extensions and variations on an AST. Since April 2018, the [HsSyn](implementing-trees-that-grow/hs-syn) AST inside GHC supports the TTG idiom. This page provides a set of guiding principles for GHC developers on how to understand and use the TTG idiom in [HsSyn](implementing-trees-that-grow/hs-syn).


## Context and Scope



The new [HsSyn](implementing-trees-that-grow/hs-syn) AST supporting the TTG idiom (from now on referred to as TTG [HsSyn](implementing-trees-that-grow/hs-syn)) is designed to subsume five different representations of Haskell syntax:


- AST GhcPs: the AST used in GHC's parsing phase
- AST GhcRn: the AST used in GHC's renaming phase
- AST GhcTc: the AST used in GHC's typechecking phase
- AST TH:    the AST used in Template Haskell
- AST HSE:   the AST used in an external tool such as Haskell-Src-Exts


By "subsume" we mean that it should be possible to instantiate TTG [HsSyn](implementing-trees-that-grow/hs-syn) to serve all five use-cases.



The subsumption of above five ASTs is done by providing instances for the extension type families.
For instance, the AST for GHC's parsing, renaming, and typechecking are defined by providing instances of the extension type families using accordingly the indices `GhcPs`, `GhcRn`, and `GhcTc`.
[
Here](https://github.com/ghc/ghc/blob/master/compiler/hsSyn/HsExpr.hs#L737-L835) is the actual code providing such instances for the `HsExpr` datatype of expressions in the TTG [HsSyn](implementing-trees-that-grow/hs-syn).


## General pattern for TTG



In general, a TTG-idiom data type has


- A type parameter, called the *phase descriptor*, that indexes which particular instantiation is required
- One *extension field* in each data constructor, whose type is given by a type family.  By giving phase-specific instances to this type family, we can add phase-specific information to the constructor.
- One unary *extension constructor* for each data type, whose argument type is given by a type family. By giving phase-specific instances to this type family, we can add extra phase-specific constructors to the type.


For example:


```
data Exp x
  = Var (XVar x) (IdP x)
  | Lam (XLam x) (IdP x) (Exp x)
  | App (XApp x) (Exp x) (Exp x)
  | New (XNew x)

type family XVar x
type family XLam x
type family XApp x
type family XNew x
```


Here the phase descriptor is `x`.  The first field of each constructor (of type `XVar x` etc) are the extension fields.  The data constructor `XNew` is the extension constructor.



All fields of the data constructors except the first (extension) field are called *payload fields*.  They are present in every instantiation of the data type.


## Guiding Principles



The design of TTG [HsSyn](implementing-trees-that-grow/hs-syn) follows these principles:


1. The base TTG [HsSyn](implementing-trees-that-grow/hs-syn) should have all the constructors common across all five ASTs (the *common data constructors*). These constructors should have, as payload fields, all the fields common across all five ASTs.

1. Note, however, that the type of a payload field of a constructor may vary with phase.  For example, in `Lam` above, the first payload field has type `Id x`, and that may vary with phase:

  ```
  type family IdP x
  type instance IdP GhcPs = RdrName
  type instance IdP GhcRn = Name
  type instance IdP GhcTc = Id
  ```

  But it is still a payload field, because every instantiation of `Exp` has a lambda with a binder; albeit the type of that binder field varies.  This happens in [HsSyn](implementing-trees-that-grow/hs-syn): for example, the type of the common (payload) field of the common constructor `HsVar`of `HsExpr x` is `IdP x` where `IdP` is a type family and `x` the phase descriptor.

1. The non-payload (i.e. phase-specific) fields of a data constructor are grouped together and introduced via the extension field.  Similarly the phase-specific data constructors are introduced using the extension constructor.

1. The instantiation of TTG [HsSyn](implementing-trees-that-grow/hs-syn), for a particular phase, should result in a tree that has no redundant fields and constructors.

>
>
> For example, the `HsExpr GhsPs` expressions of AST GhcPs should not have the constructor `HsUnboundVar` of the post-renaming phases, or its `HsMultiIf` constructor should also not have an unused field (of the type `Type`) to store the related type produced in the typechecking phase.
>
>

>
>
> As a result, the instantiated TTG [HsSyn](implementing-trees-that-grow/hs-syn) should not depend on the code from the other phases. Hence, the base (uninstantiated) TTG [HsSyn](implementing-trees-that-grow/hs-syn) should not depend on any GHC/TH/HSE-specific code.
>
>

>
>
> For example, if `HsExpr GhsPs` expressions of AST GhcPs had the constructor `HsUnboundVar` then it had to depend on the code defining `UnboundVar` (a field of `HsUnboundVar`) in the renaming phase, or if its constructor `MultiIf` had a field of type `Type` then it had to depend on the code defining `Type` in the typechecking phase.
>
>

## Example



Consider the following three simple datatypes `ExpPs`, `ExpRn`, and `ExpTc` representing correspondingly expressions in a parsing, renaming and typechecking phase:


```
module Parsing where

-- the definition of RdrName
-- ...

data ExpPs
  = Var RdrName
  | Lam RdrName ExpPs
  | App ExpPs   ExpPs
```

```
module Renaming where

-- the definition of `Name` and `UnboundVar`
-- ...

data ExpRn
  = Var Name
  | Lam Name  ExpRn
  | App ExpRn ExpRn
  | UVar UnboundVar
```

```
module Typechecking where

-- the definition of `Id`, `UnboundVar`, and `Type`
-- ...

data ExpTc
  = Var  Id
  | Lam  Id   ExpTc
  | App  Type ExpTc ExpTc
  | UVar UnboundVar
```


Based on the TTG idiom, we will have a base declaration such as the following.


```
{-# LANGUAGE TypeFamilies , EmptyCase #-}
module AST where

data Exp x
  = Var (XVar x) (XId x)
  | Abs (XAbs x) (XId x) (Exp x)
  | App (XApp x) (Exp x) (Exp x)
  | New (XNew x)

type family XVar x
type family XAbs x
type family XApp x
type family XNew x

type family XId  x

data NoExt = NoExt -- no field extension

data NoNewCon      -- no constructor extension

noNewCon :: NoNewCon -> a
noNewCon x = case x of {}
```


and the following three instantiations:


```
{-# LANGUAGE TypeFamilies #-}
module Parsing where

import AST

data RdrName -- = ...

data Ps

type ExpPs = Exp Ps

type instance XVar Ps = NoExt
type instance XAbs Ps = NoExt
type instance XApp Ps = NoExt
type instance XNew Ps = NoNewCon

type instance XId  Ps = RdrName
```

```
{-# LANGUAGE TypeFamilies #-}
module Renaming where

import AST

data Name       -- = ...
data UnboundVar -- = ...

data Rn

type ExpRn = Exp Rn

type instance XVar Rn = NoExt
type instance XAbs Rn = NoExt
type instance XApp Rn = NoExt
type instance XNew Rn = UnboundVar

type instance XId  Rn = Name
```

```
{-# LANGUAGE TypeFamilies #-}
module Typechecking where

import AST

data Id         -- = ...
data UnboundVar -- = ...
data Type       -- = ...

data Tc

type ExpTc = Exp Tc

type instance XVar Tc = NoExt
type instance XAbs Tc = NoExt
type instance XApp Tc = Type
type instance XNew Tc = UnboundVar

type instance XId  Tc = Id
```


Note that we define a specific pair of datatypes to mark and handle empty extension fields and constructors.


