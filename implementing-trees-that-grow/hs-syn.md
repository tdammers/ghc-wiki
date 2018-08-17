#
The new [HsSyn](implementing-trees-that-grow/hs-syn) based on Trees That Grow (TTG)



This page gives an overview of the new design of `HsSyn`, based on the ideas in [
Trees That Grow](https://www.microsoft.com/en-us/research/publication/trees-that-grow/) --- call it "TTG `HsSyn`".



The overall objectives of TTG `HsSyn` are:


- For the base `HsSyn` data type to be a library (say `hs-syn`) independent of GHC,
  supported with some generic functions (e.g. a pretty printer).

- This `hs-syn` library is intended to be suitable to replace

  - `Language.Haskell.TH`, the Template Haskell data type, which is always lagging `HsSyn`.
  - The data types in `haskell-src-exts`


In the shorter term, though, this page documents the design of both the base data type, *and* its instantiation for GHC itself.


## The base data type



The base data type looks like this:


```
data HsExpr x
  = HsVar (XVar x) (XId x)
  | HsApp (XApp x) (HsExpr x) (HsExpr x)
  | NewExpr (XNewExpr x) -- The extension constructor

type family XId x
type family XVar x
type family XApp x
tyep family XNewExpr x
```


To make this overview readable we use only a tiny data type with two constructors,
for variables (`HsVar`) and applications (`HsApp`).



Notice:


- The type parameter `x` is the *index* of the data type; different clients (GHC, TH, haskell-src-exts) will instantiate it differently.

- The first field of each base constructor is the *extension field*, e.g. of type `XApp x` for the `HsApp` constructor.  The type family `XApp x` specifies the type of the extension field for `HsApp` for the index `x`.

- The `NewExpr` constructor is called the *extension constructor* of the type, and always comes last.  Clients can use this to extend the data type with new data constructors by giving a suitable type instance for `XNewExpr`.

- The second field of `HsVar` has type `XId x`, and allows the client to specify the type of variable occurrences.  In GHC, for example, variables can be `RdrName`, `Name`, or `Id`, depending on the compilation phase.

## GHC's instantiation of the base type



GHC, as one client of the base TTG `HsSyn` types, instantiates them as follows.


```
data Located a = L SrcSpan a   -- This data type has been in GHC for ages

data GhcPhase = Ps | Rn | Tc
data GHC (p :: GhcPhase)

data HsExpr x
  = HsVar (XVar x) (XId x)
  | HsApp (XApp x) (HsExpr x) (HsExpr x)
  | NewExpr (XNewExpr x) -- The extension constructor

data NoExt = NoExt

type instance XVar (GHC _)     = NoExt              -- Note 1
type instance XApp (GHC _)     = NoExt              -- Note 1
type instance XId  (GHC p)     = Located (GhcId p)  -- Note 2
type instance XNewExpr (GHC p) = Located (HsExpr p) -- Note 3

type family GhcId (p :: Phase) where
  GhcId Ps = RdrName
  GhcId Rn = Name
  GhcId Tc = Id
```


This code expresses the following choices


- The TTG index used by GHC is of form `GHC p` for some GHC phase `p :: GhcPhase`.

- The GHC phase can be `Ps` (output of the parser), `Rn` (output of the renamer), or `Tc` (output of the type checker)

- Note 1: No GHC phase uses an extension field for `HsVar` or `HsApp`; hence `type instance XVar (GHC _) = NoExt`

- Note 2: The type instance for `XId (GHC p)` says that all GHC phases expect a source location on each variable occurrence; but the type of that occurrence depends on the GHC phase, as expressed by the type family `GhcId`.

- The type family `GhcId` can be a closed family, since `GhcPhase` is a data type with just three constructors.

- Note 3: GHC uses the extension constructor `NewExpr` to allow a location to be added at any point in the syntax tree.  (Unlike the old ping/pong style, a location is not required at every node.)  This is a significant change.  Other alternatives are discussed in [ImplementingTreesThatGrow/HandlingSourceLocations](implementing-trees-that-grow/handling-source-locations).

## To Do


- Add description of `HasSrcSpan`.
