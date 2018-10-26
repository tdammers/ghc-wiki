# Handling of Source Locations in Trees that Grow


- Relevant ticket: [\#15495](http://gitlabghc.nibbler/ghc/ghc/issues/15495)

## Problem



The current design of [
TTG HsSyn AST](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance) in GHC stores source locations for terms of a datatype `Exp` in a separate wrapper datatype `LExp` which is mutually recursive with `Exp` such that every recursive reference to `Exp` is done **indirectly**, via a reference to the wrapper datatype `LExp` (see the example code below). We refer to this style of storing source locations as the ping-pong style.



Besides the indirection and the resulting complications of the ping-pong style, there are two key problems with it: 


1. It bakes-in the source locations in the base TTG AST, forcing all instances to store source locations, even if they don't need them.
  For example, TH AST does not carry source locations. 

1. It results in a form of conceptual redundancy: source locations are tree decorations and they belong in the extension points.
  (see [
  TTG Guidance](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance))

## Solutions



The key solution is to move source locations to the extension points, remove the indirection (e.g., the wrapper datatype `LExp`) altogether, and update the related code (e.g., functions over `Exp`) accordingly. There are a couple of ways to implement such a solution:
 


1. We put the source locations in the new constructor extension, similar in spirit to the current `Located`.
1. We put the source locations in the new field extensions and use a typeclass to set/get the locations.


In the implementation, we have settled on the solution A, mainly as it avoids the clutter: handling of source locations is done once per data type rather than once in every constructor.
A list of the pros and cons, a sample code demonstrating the problem and the two solutions follows.



There are also two related design choices (rather orthogonal design to the problem of where to store the locations):


- The old wrapper `Located a` with the constructor `L :: SrcSpan -> a -> Located a` can no longer be used to wrap syntactic entities (expressions, patterns, etc) with locations, what should be done instead?
  For example, before, in the ping-pong style,  for some expression `e :: HsExpr p` and `span1, span2 :: SrcSpan` we had 

  ```
  L span1 (HsPar noExt (L span2 e)) :: Located (HsExpr p)
  ```

  or at the same time, for some `p :: Pat p` and `span1 , span2 :: SrcSpan` we had 

  ```
  L span1 (ParPat noExt (L span2 p)) :: Located (Pat p)
  ```

  and we could have a function like

  ```
  sL1 :: Located a -> b -> Located b
  sL1 (L sp _) = L sp
  ```

  Notice how `L` in the ping-pong style above is used to generically wrap both expressions and patterns with source locations. 
  Such a generic use of `L` in the ping-pong style is possible as we hard-coded `Located` into the definition of the trees, that we specifically want to avoid such hardcodings in the trees.
  For example, before, in the ping-pong style, we had

  ```
  data HsExpr p = ... | HsPar (XPar p) (Located (HsExpr p)) | ...
  ```

  and 

  ```
  data Pat p = ... | ParPat (XParPat p) (Located (Pat p)) | ...
  ```

  In the TTG style (both solutions A and B), we won't have such a generic data constructor `L`, as`Located` won't be baked into the definition of trees.
  For example, we will have

  ```
  data HsExpr p = ... | HsPar (XPar p) (HsExpr p) | ...
  ```

  and 

  ```
  data Pat p = ... | ParPat (XParPat p) (Pat p) | ...
  ```

  and to retain the genericity offered by baking-in `Located` (e.g., to be able to write generic functions like `sL1`, that are many), we need to resort to overloading either by directly
  using methods of a setter/getter typeclass, that we refer to as `HasSrcSpan`, or a pattern synonym to simulate `L` using the setter/getter methods.
  For example, we will have a typeclass

  ```
  type family SrcSpanLess a
  class HasSrcSpan a where
    composeSrcSpan   :: (SrcSpanLess a , SrcSpan) -> a
    decomposeSrcSpan :: a -> (SrcSpanLess a , SrcSpan)
    {- laws:
       composeSrcSpan . decomposeSrcSpan = id
       decomposeSrcSpan . composeSrcSpan = id
    -}  
  ```

  (or, 

  ```
  type family SrcSpaned a
  class HasSrcSpan a where
    composeSrcSpan   :: (a , SrcSpan) -> SrcSpaned a
    decomposeSrcSpan :: SrcSpaned a -> (a , SrcSpan)
    {- laws:
       composeSrcSpan . decomposeSrcSpan = id
       decomposeSrcSpan . composeSrcSpan = id
    -}  
  ```

  )
  and possibly a pattern synonym

  ```
  pattern LL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
  pattern LL s m <- (decomposeSrcSpan -> (m , s))
    where
          LL s m =  composeSrcSpan (m , s)
  ```

>
>
> so by providing instances for `HasSrcSpan` (by either Solution A or Solution B),
>
>


 


>
>
> for some expression `e :: HsExpr (GhcPass p)` and `span1, span2 :: SrcSpan`, we will have
>
>

```
LL span1 (HsPar noExt (LL span2 e)) :: HsExpr (GhcPass p)
```

>
>
> or at the same time, for some `p :: Pat (GhcPass p)` and `span1 , span2 :: SrcSpan` we had 
>
>

```
LL span1 (ParPat noExt (LL span2 p)) :: Pat (GhcPass p)
```

>
>
> and we could have a function like
>
>

```
sL1 :: (HasSrcSpan a , HasSrcSpan b) => a -> SrcSpanLess b -> b
sL1 (LL sp _) = LL sp
```


 


- Although we assume typefamily instances are nested (to help with resolving constraint solving), we may, or may not, assume that these extension typefamily instances for GHC-specific decorations are closed.

>
>
> For example, instead of a list of open type family instances
>
>

```
type instance XApp (GHC p) = XAppGHC p
type family   XAppGHC (p :: Phase)
type instance XAppGHC Ps = ()
type instance XAppGHC Rn = ()
type instance XAppGHC Tc = Type
```

>
>
> we can have
>
>

```
type instance XApp (GHC p) = XAppGHC p
type family   XAppGHC (p :: Phase) where
              XAppGHC Ps = ()
              XAppGHC Rn = ()
              XAppGHC Tc = Type
```

>
>
> The closed type family solution is elegant and solves some of the constraint solving problems in place (see the commented section in type class instance of solution B). However, the closed typed family solution couples together the code from different passes of the compiler, e.g., the definition of a parser with the type `parseExp :: String -> M (HsExpr (Ghc Ps))` (for some parsing monad `M`) refers to the closed type family `XAppGHC` which refers to the definition `Type` that is not relevant to the parsing phase. We want the parser and other machineries within GHC front-end (e.g., the pretty-printer) to not to be GHC-specific (e.g., depending on `Type`, or even core via `Tickish`!).
>
>

## Pros & Cons


### Solution A: the source locations in the new constructor extension



Pros:


- It makes it easy to omit locations altogether (see the notes about "Generated" code).
  This is a Good Thing.
- It makes it easy to store fewer locations (e.g. one location for `(f x y z)`, 
  rather than one for `(f x y z)`, one for `(f x y)`, and one for `(f x)`).
- It's easy to add the current location to the monad

>
>
> `f (XNew loc e) = setSrcSpan loc $ f e`
>
>

>
>
> Simple, elegant!
>
>


Cons:


- At the binding site of a variable we know that we \*always\* have a location, and we can put that in its Name.  
  If locations were more optional, that would not be so true.

### Solution B: the source locations in the new field extensions



Pros:


- TODO 


Cons:


- An instance of `HasSpan` should be defined per datatype which requires a large pattern matching over datatype
- Handling of the source locations should be done once per constructor
- When constructing/generating terms the first field of the constructors should explicitly mention the source location
  (see the `par` function in the Solution A's code, where the first field of `Par` should have a `SrcSpan`, even though a dummy one.)

## An example to illustrate



To explain the design choices, we use a simple language of expressions.
Here are the base definitions in [TTG style](implementing-trees-that-grow/trees-that-grow-guidance):


```
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
module TTG where

-- ----------------------------------------------
-- AST Base
-- ----------------------------------------------
data Exp x
  = Var (XVar x) (XId x)
  | Abs (XAbs x) (XId x) (Exp x)
  | App (XApp x) (Exp x) (Exp x)
  | Par (XPar x) (Exp x)
  | New (XNew x) -- The extension constructor

type family XVar x
type family XAbs x
type family XApp x
type family XPar x
type family XNew x
type family XId  x
```


with some basic GHC-specific types defined as


```
{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies , DataKinds, EmptyDataDeriving, EmptyCase #-}
module BasicGHCTypes where

-- ----------------------------------------------
-- GHC-Specific Declarations
-- ----------------------------------------------
data Phase = Ps | Rn | Tc
data GHC (p :: Phase)

data NoExt = NoExt
data NoNewCon

noNewCon :: NoNewCon -> a
noNewCon x = case x of {}

data RdrName    -- = the definition of RdrName
data Name       -- = the definition of Name
data Id         -- = the definition of Id
data Type       -- = the definition of SrcSpan
data UnboundVar -- = the definition of UnboundVar
data SrcSpan    -- = the definition of SrcSpan
  deriving Eq

data Located a = L SrcSpan a

noSrcSpan :: SrcSpan
noSrcSpan = ... -- an empty SrcSpan

type family   XAppGHC (p :: Phase)
type instance XAppGHC Ps = NoExt
type instance XAppGHC Rn = NoExt
type instance XAppGHC Tc = Type

type family   XNewGHC (p :: Phase)
type instance XNewGHC Ps = NoNewCon
type instance XNewGHC Rn = UnboundVar
type instance XNewGHC Tc = UnboundVar

type family   XIdGHC  (p :: Phase)
type instance XIdGHC  Ps = RdrName
type instance XIdGHC  Rn = Name
type instance XIdGHC  Tc = Id

-- NB: if GHC later wants to add extension fields to (say)
-- XAbs, we can just redefine XAbs (GHC p) to be more like
-- the XApp case
```


Notice that the payload of the `Var` constructor is of type `XId x`. For
GHC, `x` will be instantiated to `GHC p`, and `XId` has a `type instance` that
delegates to `XIdGHC p`.  The latter can be defined by a nice *closed* type
family.


### Ping-pong style



Here is a representation of lambda expressions in the ping-pong style.
Unfortunately, this forces us to redefine the base TTG data type,
forcing it into ping-pong style, which is why we don't like it for the reasons mentioned above.


```
{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}

module Original where

import BasicGHCTypes

-- ----------------------------------------------
-- AST Base
-- ----------------------------------------------
type LExp x = Located (Exp x)

data Exp x -- Notice the alternation between LExp and Exp
  = Var (XVar x) (XId x)
  | Abs (XAbs x) (XId x)  (LExp x)
  | App (XApp x) (LExp x) (LExp x)
  | Par (XPar x) (LExp x)
  | New (XNew x) -- The extension constructor

type family XVar x
type family XAbs x
type family XApp x
type family XPar x
type family XNew x
type family XId  x

-- ----------------------------------------------
-- GHC-Specific Decorations
-- ----------------------------------------------
type instance XVar (GHC _)  = NoExt
type instance XAbs (GHC _)  = NoExt
type instance XApp (GHC p)  = XAppGHC p
type instance XPar (GHC _)  = NoExt
type instance XNew (GHC p)  = XNewGHC p
type instance XId  (GHC p)  = XIdGHC  p

-- ----------------------------------------------
-- Example Function
-- ----------------------------------------------
par :: LExp (GHC x) -> LExp (GHC x)
par l@(L sp (App{})) = L sp (Par NoExt l)
par l                = l
```

### The SrcSpan Accessor Typeclass



Here is a complete definition of the `HasSrcSpan` typeclass mentioned earlier:


```
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies, PatternSynonyms, ViewPatterns #-}
module HasSrcSpan where

import BasicGHCTypes

type family SrcSpanLess a
class HasSrcSpan a where
  composeSrcSpan   :: (SrcSpanLess a , SrcSpan) -> a
  decomposeSrcSpan :: a -> (SrcSpanLess a , SrcSpan)
  {- laws (isomorphic relation):
     composeSrcSpan . decomposeSrcSpan = id
     decomposeSrcSpan . composeSrcSpan = id
  -}


unSrcSpan :: HasSrcSpan a => a -> SrcSpanLess a
unSrcSpan = fst . decomposeSrcSpan

getSrcSpan :: HasSrcSpan a => a -> SrcSpan
getSrcSpan = snd . decomposeSrcSpan

setSrcSpan :: HasSrcSpan a => a -> SrcSpan -> a
setSrcSpan e sp = composeSrcSpan (unSrcSpan e , sp)

type instance SrcSpanLess (Located a) = a
instance HasSrcSpan (Located a) where
  composeSrcSpan   (e , sp) = L sp e
  decomposeSrcSpan (L sp e) = (e , sp)

type instance SrcSpanLess SrcSpan = SrcSpan
instance HasSrcSpan SrcSpan where
  composeSrcSpan   (_ , sp) = sp
  decomposeSrcSpan sp       = (sp , sp)

type instance SrcSpanLess NoNewCon = NoNewCon
instance HasSrcSpan NoNewCon where
  composeSrcSpan   (n , _) = noNewCon n
  decomposeSrcSpan n       = noNewCon n


pattern LL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
pattern LL s m <- (decomposeSrcSpan -> (m , s))
  where
        LL s m =  composeSrcSpan (m , s)
```

### Solution A - Example Code



In the code below, as compared to the ping-pong style above, we have the following key changes:


- `LExp` is replaced with `Exp`
- a new constructor extension is introduced to wrap `Exp` with a `SrcSpan` 
- a pattern synonym `LL` is introduced using the new constructor

```
{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors
                -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, PatternSynonyms, DataKinds, FlexibleInstances #-}
module SolutionA where

import BasicGHCTypes

import TTG
import HasSrcSpan

-- ----------------------------------------------
-- GHC-Specific Decorations
-- ----------------------------------------------
type instance XVar (GHC _)  = NoExt
type instance XAbs (GHC _)  = NoExt
type instance XApp (GHC p)  = XAppGHC p
type instance XPar (GHC _)  = NoExt
type instance XNew (GHC p)  = Either (Located (Exp (GHC p)))
                                     (XNewGHC p)
type instance XId  (GHC p)  = XIdGHC  p

-- ----------------------------------------------
-- HasSrcSpan Instance
-- ----------------------------------------------

type instance SrcSpanLess (Exp (GHC p)) = Exp (GHC p)
instance HasSrcSpan (Exp (GHC p)) where
  composeSrcSpan (m , sp) = if noSrcSpan == sp
                            then m
                            else New (Left (L sp m))
  decomposeSrcSpan (New (Left (L sp m))) = (m , sp)
  decomposeSrcSpan m                     = (m , noSrcSpan)

-- ----------------------------------------------
-- Example Function
-- ----------------------------------------------
par :: Exp (GHC p) -> Exp (GHC p)
par l@(LL sp (App{})) = LL sp (Par NoExt l)
par l                 = l
```

### Solution B - Example Code



In the code below, as compared to the ping-pong style above, we have the following key changes:


- `LExp` is replaced with `Exp`
- field extensions are set to have a `SrcSpan` paired (via `Located`)
  with a closed type family specialised for GHC phases
- a setter/getter function pair is introduced by a typeclass
- a pattern synonym `LL` is introduced using the setter/getter function pair

```
{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors
                -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, PatternSynonyms, DataKinds, FlexibleInstances #-}
module SolutionB where

import BasicGHCTypes
import TTG
import HasSrcSpan

-- ----------------------------------------------
-- GHC-Specific Decorations
-- ----------------------------------------------
type instance XVar (GHC _)  = Located NoExt
type instance XAbs (GHC _)  = Located NoExt
type instance XApp (GHC p)  = Located (XAppGHC p)
type instance XPar (GHC _)  = Located NoExt
type instance XNew (GHC p)  = Located (XNewGHC p)
type instance XId  (GHC p)  = XIdGHC p

-- ----------------------------------------------
-- HasSrcSpan Instance
-- ----------------------------------------------

type instance SrcSpanLess (Exp (GHC p)) = Exp (GHC p)
instance HasSrcSpan (Exp (GHC p)) where
{- or,
type ForallX (p :: * -> Constraint) x
  = ( p (XVar x) , p (XAbs x) , p (XApp x) , p (XPar x)
    , p (XNew x) )

instance ForallX HasSrcSpan x => HasSrcSpan (Exp x) where
-}
  composeSrcSpan (Var ex x   , sp) = Var (setSrcSpan ex sp) x
  composeSrcSpan (Abs ex x n , sp) = Abs (setSrcSpan ex sp) x n
  composeSrcSpan (App ex l m , sp) = App (setSrcSpan ex sp) l m
  composeSrcSpan (Par ex m   , sp) = Par (setSrcSpan ex sp) m
  composeSrcSpan (New ex     , sp) = New (setSrcSpan ex sp)

  decomposeSrcSpan m@(Var ex _)    = (m , getSrcSpan ex)
  decomposeSrcSpan m@(Abs ex _ _)  = (m , getSrcSpan ex)
  decomposeSrcSpan m@(App ex _ _)  = (m , getSrcSpan ex)
  decomposeSrcSpan m@(Par ex _)    = (m , getSrcSpan ex)
  decomposeSrcSpan m@(New ex)      = (m , getSrcSpan ex)

-- ----------------------------------------------
-- Example Function
-- ----------------------------------------------
par :: Exp (GHC p) -> Exp (GHC p)
par l@(LL sp (App{})) = Par (L sp NoExt) l
{- or,
                      = LL sp (Par (L noSrcSpan NoExt) l)
-}
par l                 = l
```

## Extra Notes



Here are some extra notes:


- We also currently have sections of AST without source locations, such as those generated when converting TH AST to hsSyn AST, or for GHC derived code.
  We can perhaps deal with these by either defining an additional pass, so

```
data Pass = Parsed | Renamed | Typechecked | Generated
  deriving (Data)
```

>
>
> or by making the extra information status dependent on an additional parameter, so
>
>

```
data GhcPass (l :: Location) (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance (Typeable l,Typeable c) => Data (GhcPass l c)

data Pass = Parsed | Renamed | Typechecked
  deriving (Data)

data Location = Located | UnLocated
```

>
>
> Thanks to Zubin Duggal for bringing the unlocated problem up on IRC.
>
>

- The setter/getter functions can be generalised to set/get anything:

  ```
  type family Without b a
  class Has b a where
    compose   :: (Without b a , b) -> a
    decompose :: a -> (Without b a , b)
    {- laws (isomorphic relation):
       compose . decompose = id
       decompose . compose = id
    -}
  ```

- The API Annotations are similar to the `SrcSpan`, in that they are additional decorations, and also currently appear wherever there is a `SrcSpan`.
  The API Annotations can be accommodated via a straightforward extension of the type class approach, by defining

  ```
    data Extra = Extra SrcSpan [(SrcSpan,AnnKeywordId)]

    type HasExtra a = Has Extra a

    getSpan :: HasExtra a => a -> SrcSpan
    getSpan = ...

    setSpan :: HasExtra a => a -> SrcSpan -> a
    setSpan = ...
   
    getApiAnns :: HasExtra a => a -> [(SrcSpan,AnnKeywordId)]
    getApiAnns = ...
    
    setApiAnns :: HasExtra a => a -> [(SrcSpan,AnnKeywordId)] -> a
    setApiAnns = ...
  ```


   


