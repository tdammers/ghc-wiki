# The Marvellous Module Structure of GHC


- **See also: [Proposal for hierarchical module structure](module-dependencies/hierarchical)**

- **NOTE:** Possibly outdated.


GHC is built out of about 493 Haskell modules. It can be quite tricky to figure out what the module dependency graph looks like. It can be important, too, because loops in the module dependency graph need to be broken carefully using .hi-boot interface files.



This section of the commentary documents the subtlest part of the module dependency graph, namely the part near the bottom.


- The list is given in compilation order: that is, module near the top are more primitive, and are compiled earlier.
- Each module is listed together with its most critical dependencies in parentheses; that is, the dependencies that prevent it being compiled earlier.
- Modules in the same bullet don't depend on each other.
- Loops are documented by a dependency such as "loop Type.Type". This means tha the module imports Type.Type, but module Type has not yet been compiled, so the import comes from Type.hi-boot. 

## Compilation order is as follows:


- First comes a layer of modules that have few interdependencies, and which implement very basic data types:

  - Util
  - OccName
  - Pretty
  - Outputable
  - StringBuffer
  - ListSetOps
  - Maybes
  - etc 

- Now comes the main subtle layer, involving types, classes, type constructors identifiers, expressions, rules, and their operations.

  - Name, PrimRep
  - PrelNames
  - Var (Name, loop IdInfo.IdInfo, loop Type.Type, loop Type.Kind)
  - VarEnv, VarSet, ThinAir
  - Class (loop TyCon.TyCon, loop Type.Type)
  - TyCon (loop Type.Type, loop DataCon.DataCon, loop Generics.GenInfo)
  - TypeRep (loop DataCon.DataCon, loop Subst.substTyWith)
  - Type (loop PprType.pprType, loop Subst.substTyWith)
  - FieldLabel (Type), TysPrim (Type)
  - Literal (TysPrim, PprType), DataCon (loop PprType, loop Subst.substTyWith, FieldLabel.FieldLabel)
  - TysWiredIn (loop MkId.mkDataConIds)
  - TcType ( lots of TysWiredIn stuff)
  - PprType ( lots of TcType stuff )
  - PrimOp (PprType, TysWiredIn)
  - CoreSyn \[does not import Id\]
  - IdInfo (CoreSyn.Unfolding, CoreSyn.CoreRules)
  - Id (lots from IdInfo)
  - CoreFVs, PprCore
  - CoreUtils (PprCore.pprCoreExpr, CoreFVs.exprFreeVars, CoreSyn.isEvaldUnfolding CoreSyn.maybeUnfoldingTemplate)
  - CoreLint ( CoreUtils ), OccurAnal (CoreUtils.exprIsTrivial), CoreTidy (CoreUtils.exprArity )
  - CoreUnfold (OccurAnal.occurAnalyseGlobalExpr)
  - Subst (CoreUnfold.Unfolding, CoreFVs), Generics (CoreUnfold.mkTopUnfolding), Rules (CoreUnfold.Unfolding, PprCore.pprTidyIdRules)
  - MkId (CoreUnfold.mkUnfolding, Subst, Rules.addRule)
  - PrelInfo (MkId), HscTypes ( Rules.RuleBase ) 

- That is the end of the infrastructure. Now we get the main layer of modules that perform useful work.

  - CoreTidy (HscTypes.PersistentCompilerState) 

## Typechecker stuff


- TcType
- TcEvidence( TcType )
- TcMType( TcEvidence )
- TcUnify( TcMType )
- TcSMonad( TcMType )
- TcSimplify( TcSMonad )
- TcValidity( TcSimplify.simplifyTop, TcUnify.tcSubType )
- TcHsType( TcValidity.checkValidType, TcValidity.checkValidInstance )

## HsSyn stuff


- HsPat.hs-boot
- HsExpr.hs-boot (loop HsPat.LPat)
- HsTypes (loop HsExpr.HsSplice)
- HsBinds (HsTypes.LHsType, loop HsPat.LPat, HsExpr.pprFunBind and others) HsLit (HsTypes.SyntaxName)
- HsPat (HsBinds, HsLit) HsDecls (HsBinds)
- HsExpr (HsDecls, HsPat) 

## Library stuff: base package


- GHC.Base
- Data.Tuple (GHC.Base), GHC.Ptr (GHC.Base)
- GHC.Enum (Data.Tuple)
- GHC.Show (GHC.Enum)
- GHC.Num (GHC.Show)
- GHC.ST (GHC.Num), GHC.Real (GHC.Num)
- GHC.Arr (GHC.ST) GHC.STRef (GHC.ST)
- GHC.!IOBase (GHC.Arr)
- Data.Bits (GHC.Real)
- Data.HashTable (Data.Bits, Control.Monad)
- Data.Typeable (GHC.IOBase, Data.HashTable)
- GHC.Weak (Data.Typeable, GHC.IOBase) 

## High-level Dependency Graph



Dark red edges indicate that only one module in one group depends on a module in the other group. Dark green means 11 or more dependencies. Arrows point from the importing module to the imported module.



[](/trac/ghc/attachment/wiki/Commentary/ModuleStructure/dep5.png)



See also attachment [](/trac/ghc/attachment/wiki/Commentary/ModuleStructure/deps.pdf) showing module dependencies without the most common groups (basicTypes, utils, coreSyn, main, types, prelude) as of March 2018.


