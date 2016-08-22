
This page is to track the discussions on


- [\#11348](http://gitlabghc.nibbler/ghc/ghc/issues/11348)
- [\#12088](http://gitlabghc.nibbler/ghc/ghc/issues/12088)
- [\#12239](http://gitlabghc.nibbler/ghc/ghc/issues/12239)


The essential problem is that type-checking declarations can depend on `type instance`s so we must be careful to process `type instance`s in the correct order. When necessary interleaving the instances with other declarations.



The motivating example is


```
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}

import Data.Kind
import Data.Proxy

type family TrivialFamily t :: Type
type instance TrivialFamily (t :: Type) = Bool

data R where
    R :: Proxy Bool -> R

type ProblemType t = 'R ('Proxy :: Proxy (TrivialFamily t))
```


Which was rejected with


```wiki
error:
    • Expected kind ‘Proxy Bool’,
        but ‘'Proxy’ has kind ‘Proxy (TrivialFamily t)’
    • In the first argument of ‘R’, namely
        ‘(Proxy :: Proxy (TrivialFamily t))’
      In the type ‘R (Proxy :: Proxy (TrivialFamily t))’
      In the type declaration for ‘ProblemType’
```


GHC was typechecking `ProblemType` before the instance for `TrivialFamily` was processed.


# Original Solution



Alex Vieth partially solved the solution for simple cases where `type` declaractions depend on `type instances` as in the example. The solution was
to \*eagerly\* process type family instances as soon as it was possible to do so. The solution is inadequate due to the method used to compute dependencies for type instances. 



The algorithm is:


1. Compute the SCCs for `TyClDecls` as we do now in HEAD.
1. For all of the `InstDecls`, associate it with its `FreeVars`, intersected with the set of `Name`s of `TyClDecls` that we just analysed.
1. Fold the list of SCCs, at each step extracting the set of `InstDecls` for which its FreeVars is empty, and then eliminating all of the Names found in that SCC. That set of `InstDecls`, if non-empty, comes before the current component in the output list.


Implicit in step 2 is an algorithm to compute the free variables of each `type instance` declaration.



It was implemented as follows:



For a `type instance` declaration for a type family `T`, the free variables of the declaration are


1. The type constructor `T`.
1. For a free variables `v` in the LHS patterns and RHS. Either the parent of `v` if one exists or else `v` itself.


For example, considering


```
data Fin :: N -> Type where                                                     
   FZ :: Fin (S n)                                                               
   FS :: Fin n -> Fin (S n)  

data T

type instance F T FZ = Int
```


We compute the free variables to be `{ F, T, Fin }`. Then, Alex's algorithm ensures that as soon as `F` `T` and `Fin` are declared, the instance declaration is added to environment. This ensures that if later declarations depend on `F`, they can make use of this instance.



Here is one example which this algorithm fixes.


```
{-# LANGUAGE TypeInType, TypeFamilies #-}
module Kinds where

import GHC.Types

type family G (a :: Type) :: Type
type instance G Int = Bool

type family F (a :: Type) :: G a
type instance F Int = True
```


The algorithm ensures that `F` depends on `G` so that `G` is processed first. As soon as `G` is processed we can process `type instance G Int = Bool` and as such `type instance F Int = True` succeeds. 


# The Problem



However, this is insufficient as there are other (hidden) dependencies for type instance declarations. We also perform \*kind checking\* of 
each argument to the instance declaration, as a result, we should also declare these dependencies to ensure that all instances which could affect this kind checking are processed first.



Here is a simple example to demonstrate the problem:


```
{-# LANGUAGE TypeInType, GADTs, TypeFamilies #-}

import Data.Kind (Type)

data N = Z | S N

data Fin :: N -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

type family FieldCount (t :: Type) :: N

type family FieldType (t :: Type) (i :: Fin (FieldCount t)) :: Type

data T

type instance FieldCount T = S (S (S Z))

type instance FieldType T FZ = Int
```


The kind of the second argument of `FieldType` depends on `FieldCount`. However, the originally proposed algorithm does not ensure that we process all instances for `FieldCount` before `FieldType` as the computed dependencies of `type instance FieldTYpe T FZ = Int` are `{ FieldType, T, Fin }`. 


# The Solution



[\#12088](http://gitlabghc.nibbler/ghc/ghc/issues/12088) is a lengthy discussion about possible solutions to solve this problem.



This is incomplete and I don't understand. Please help me fill in the details..!


# What about data instances?



The original problem was elucidated by Alex's attempt to allow the promotion of `data instances`. However, data instances are mostly a red herring as Simon points out, `type instances` are what give rise to nominal equalties. It
is just that `data instance`s are one place where we can use the extra nominal equalities. They can also be used in other `type instance`s, `type` declarations etc. 


>
>
> The only instances we must have in hand to do kind-checking are type instances, because they give rise to implicit nominal type/kind equalities (e.g. F Int \~ Bool) that the kind checker must solve.
>
>
>
> But data instances are different: they do not give rise to implicit nominal type/kind equalities. So in that way they behave more like data type declarations. Indeed, I think that the only way that type/class declaration T can depend on a data instance declaration D is if T mentions one of D's promoted data constructors. This will be sorted out by the ordinary SCC analysis.
>
>

