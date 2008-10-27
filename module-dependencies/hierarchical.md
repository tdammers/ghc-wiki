# Proposal for Hierarchical Module Structure for GHC


```wiki
what to do with C stuff?

principles (so far) (correct/mark violations):
  - try to be consistent (i.e., either always use Decl or always use Declaration)
  - "Types" is a collection of types to be used in the compiler
  - "Type" is related to working with (a representation) of a Haskell type

------------------------------------------------------------------------------------

what to do with C stuff?

principles (so far) (correct/mark violations):
  - don't use too short names
  - try to be consistent (i.e., either always use Decl or always use Declaration)
  - "Types" is a collection of types to be used in the compiler
  - "Type" is related to working with (a representation) of a Haskell type


main/
  GHC.hs                        GHC -- could use some further splitting up
  				GHC.Make  -- parts of GHC.hs (depanal, etc.)
  Config.hs			GHC.Config
  Constants.lhs                 GHC.Config.Constants
  ParsePkgConf.y                GHC.Config.Packages.Parse
  PackageConfig.hs              GHC.Config.Package[s]
  StaticFlags.hs                GHC.[Config.]Flags.Static
  StaticFlagParser.hs           GHC.[Config.]Flags.Static.Parse
  DynFlags.hs                   GHC.[Config.]Flags.Dynamic
  DriverMkDepend.hs             GHC.[Driver.Depend/MkDepend] -- it's a 
  				 or: GHC.MakeDepend  -- since it's a GHC API client
  DriverPhases.hs               GHC.Driver.Phase[s] -- ?
  DriverPipeline.hs		GHC.Driver.Pipeline  -- ?
  HeaderInfo.hs			GHC.Driver.HeaderInfo -- (used by DriverPipeline, GHC)
  				 or: GHC.Parse.HeaderInfo
  HscMain.lhs			GHC.Compiler
  HscStats.lhs			GHC.Compiler.Stats -- only used by HscMain
  CodeOutput.lhs		GHC.Compiler.CodeOutput -- ?
  CmdLineParser.hs		GHC.CmdLine.Parse
  Finder.lhs                    GHC.Finder
  HscTypes.lhs/boot		GHC.Types    -- very hard to split up
  InteractiveEval.hs/boot     	GHC.Interactive
  Packages.lhs/boot		GHC.Package[s] -- ?
  PprTyThing.hs			GHC.Interactive.PprTyThing -- ? (used by InteractiveUI, Debugger)
  SysTools.lhs			GHC.[Driver.]SysTools -- used by GHC, DriverPipeline, DriverMkDepend
  TidyPgm.lhs			GHC.Core.Tidy.Program -- ?
  BreakArray.hs                 GHC.Utils.BreakArray
  ErrUtils.lhs/boot             GHC.Utils.Error -- depends on StaticFlags, DynFlags, SrcLoc, 

ghci/
  ByteCodeAsm.lhs		GHC.ByteCode.Assemble[r]
  ByteCodeFFI.lhs		GHC.ByteCode.FFI
  ByteCodeGen.lhs		GHC.ByteCode.Generate
  ByteCodeInstr.lhs		GHC.ByteCode.Instruction[s]
  ByteCodeItbls.lhs		GHC.ByteCode.InfoTable[s]
  ByteCodeLink.lhs/boot		GHC.[ByteCode.Link/Link.ByteCode] -- difference to GHC.Interactive.Link?
  Debugger.hs			GHC.Interactive.Debugger
  GhciMonad.hs			GHCi.Monad
  GhciTags.hs			GHCi.Tags
  InteractiveUI.hs		GHCi.UI   -- possibly further split-up
  LibFFI.hsc			GHCi.ByteCode.LibFFI -- this is a libffi binding
  Linker.lhs			GHC.Interactive.Link -- or Linker?
  ObjLink.lhs			GHC.Interactive.Link.ObjCode
  RtClosureInspect.hs		GHC.[ByteCode].ClosureInspect
  keepCAFsForGHCi.c

utils/
  Bag.lhs			GHC.Utils.Bag
  Binary.hs			GHC.Utils.Binary / GHC.Data.Binary?
  BufWrite.hs			GHC.Utils.[Handle.WriteBuffered/BufWrite] 
  Digraph.lhs			GHC.Utils.[DiGraph/Graph.Directed]
  Encoding.hs			GHC.Utils.[Char.]Encod[e/ing]
  Exception.hs			GHC.[Utils.]Exception
  FastBool.lhs			GHC.Utils.FastBool
  FastFunctions.lhs		GHC.Utils.Unsafe -- it's really a bunch of unsafe stuff
  FastMutInt.lhs		GHC.Utils.FastMutableInt -- used only inside utils/
  FastString.lhs		GHC.Utils.FastString
  FastTypes.lhs			GHC.Utils.FastTypes -- or UnboxedTypes?
  Fingerprint.hsc		GHC.Utils.Fingerprint
  FiniteMap.lhs			GHC.Utils.FiniteMap
  GraphBase.hs			GHC.Utils.Graph.Base
  GraphColor.hs			GHC.Utils.Graph.Colo[u]r
  GraphOps.hs			GHC.Utils.Graph.Operations
  GraphPpr.hs			GHC.Utils.Graph.PrettyPrint
  IOEnv.hs			GHC.[Utils.]Monad.IOEnv
  Interval.hs			GHC.Utils.Interval
  LazyUniqFM.lhs		GHC.Utils.UniqFM.Lazy
  ListSetOps.lhs		GHC.Utils.List.SetOps
  Maybes.lhs			GHC.Utils.Maybe
  MonadUtils.hs			GHC.[Utils.]Monad[.Utils]
  OrdList.lhs			GHC.Utils.OrdList
  Outputable.lhs		GHC.Utils.Outputable
  Panic.lhs			GHC.Utils.Panic
  Pretty.lhs			GHC.Utils.Pretty[Print]
  State.hs			GHC.[Utils.]Monad.State
  StringBuffer.lhs		GHC.Utils.StringBuffer
  Unicode.hs			GHC.Utils.Unicode
  UniqFM.lhs			GHC.Utils.FiniteMap.Unique
  UniqSet.lhs			GHC.Utils.Set.Unique
  Util.lhs			GHC.Utils -- re-export other things?
  md5.c
  md5.h

-- very unsure about these
basicTypes/
  BasicTypes.lhs		GHC.Types.BasicTypes -- maybe splitting might be useful
  DataCon.lhs/boot		GHC.Types.DataCon
  Demand.lhs			GHC.Types.Demand.Old
  Id.lhs			GHC.Types.Id
  IdInfo.lhs/boot		GHC.Types.Id.Info
  MkId.lhs/boot			GHC.Types.Id.Make
  Var.lhs			GHC.Types.Var
  VarEnv.lhs			GHC.Types.Var.Env
  VarSet.lhs			GHC.Types.Var.Set
  OccName.lhs/boot		GHC.Types.OccName
  RdrName.lhs			GHC.Types.RdrName
  Name.lhs/boot			GHC.Types.Name
  NameEnv.lhs			GHC.Types.Name.Env
  NameSet.lhs			GHC.Types.Name.Set
  Literal.lhs			GHC.Types.Literal
  Module.lhs/boot		GHC.Types.Module
  NewDemand.lhs			GHC.Types.Demand
  SrcLoc.lhs			GHC.Types.SrcLoc
  Unique.lhs			GHC.Types.Unique 
                                 -- if we can make it independent of GHC it
				 -- could go into GHC.Utils
  UniqSupply.lhs		GHC.Types.Unique.Supply

types/
  Class.lhs			GHC.Types.Class
  Coercion.lhs			GHC.Types.Type.Coercion -- ?
  FamInstEnv.lhs		GHC.Types.FamInstEnv -- ?
  FunDeps.lhs			GHC.Types.FunDeps
  Generics.lhs			GHC.Types.Generics
  InstEnv.lhs			GHC.Types.InstEnv -- ?
  TyCon.lhs/boot		GHC.Types.TyCon
  Type.lhs			GHC.Types.Type
  TypeRep.lhs/boot		GHC.Types.Type.Rep
  Unify.lhs			GHC.Types.Type.Unify -- ?

hsSyn/
  Convert.lhs			GHC.Syntax.[TH/TemplateHaskell].Convert
  HsBinds.lhs			GHC.Syntax.Bind[er] -- what about sigs?
  HsDecls.lhs			GHC.Syntax.Declaration
  HsDoc.hs			GHC.Syntax.Documentation
  HsExpr.lhs/boot		GHC.Syntax.Expression
  HsImpExp.lhs			GHC.Syntax.ImportExport
  HsLit.lhs			GHC.Syntax.Literal
  HsPat.lhs/boot		GHC.Syntax.Pattern
  HsSyn.lhs			GHC.Syntax
  HsTypes.lhs			GHC.Syntax.Type
  HsUtils.lhs			GHC.Syntax.Utils

parser/
  Ctype.lhs			GHC.[Haskell.Parser.]Utils.ClassifyChar
  HaddockLex.hs-boot
  HaddockLex.x			Haddock.Lex  -- can this clash with haddock itself?
  HaddockParse.y		Haddock.Parse -- ditto
  HaddockUtils.hs		Haddock.Parse.Utils -- ditto
  Lexer.x			GHC.Parse.Lex
  Parser.y[.pp]			GHC.Parse.Parse
  RdrHsSyn.lhs			GHC.Parse.Syntax / GHC.Parse.Utils -- ?
  LexCore.hs			GHC.Core.Lex
  ParserCore.y			GHC.Core.Parse
  ParserCoreUtils.hs		GHC.Core.Parse.Utils
  cutils.c
  cutils.h
  hschooks.c

typecheck/
  FamInst.lhs			GHC.Typecheck.FamInst
  Inst.lhs			GHC.Typecheck.Instance
  TcArrows.lhs			GHC.Typecheck.Arrows
  TcBinds.lhs			GHC.Typecheck.Binders
  TcClassDcl.lhs		GHC.Typecheck.ClassDecl
  TcDefaults.lhs		GHC.Typecheck.Defaults
  TcDeriv.lhs			GHC.Typecheck.Deriving
  TcEnv.lhs/boot		GHC.Typecheck.Env
  TcGenDeriv.lhs		GHC.Typecheck.Deriving.Generate
  TcExpr.lhs/boot		GHC.Typecheck.Syntax.Expr
  TcForeign.lhs			GHC.Typecheck.Syntax.Foreign
  TcHsSyn.lhs			GHC.Typecheck.Syntax -- ?
  TcHsType.lhs			GHC.Typecheck.Syntax.[Mono]Type -- ?
  TcMatches.lhs/boot		GHC.Typecheck.Syntax.Matches
  TcPat.lhs			GHC.Typecheck.Syntax.Pattern
  TcInstDcls.lhs		GHC.Typecheck.InstDecl
  TcRnDriver.lhs		GHC.Typecheck[Rename].[Driver/Module]
  TcRnMonad.lhs			GHC.Typecheck[Rename].Monad
  TcMType.lhs			GHC.Typecheck.Monad.TypeOps
  TcRnTypes.lhs/boot		GHC.Typecheck.Types
  TcRules.lhs			GHC.Typecheck.Rules
  TcSimplify.lhs		-- what does this do?
  TcSplice.lhs/boot		GHC.Typecheck.Splice
  TcTyClsDecls.lhs		GHC.Typecheck.TypeAndClassDeclaration
  TcTyDecls.lhs			GHC.Typecheck.TypeDeclaration
  TcTyFuns.lhs			GHC.Typecheck.TypeFunction
  TcType.lhs/boot		GHC.Typecheck.Type
  TcUnify.lhs/boot		GHC.Typecheck.Unify -- difference to Unify?

rename/
  RnBinds.lhs			GHC.Rename.Bind
  RnEnv.lhs			GHC.Rename.Env
  RnExpr.lhs/boot		GHC.Rename.Expr
  RnHsDoc.hs			GHC.Rename.Docs
  RnHsSyn.lhs			GHC.Rename.Syntax
  RnNames.lhs			GHC.Rename.[Name/ImportExport]
  RnPat.lhs			GHC.Rename.Pattern
  RnSource.lhs			GHC.Rename.Main
  RnTypes.lhs			GHC.Rename.Type -- module description wrong
  rename.tex

coreSyn/
   <new>			GHC.Core -- re-exports CoreSyn and utils
  CoreFVs.lhs			GHC.Core.FreeVars
  CoreLint.lhs			GHC.Core.Lint
  CorePrep.lhs			GHC.Core.Prepare -- ?
  CoreSubst.lhs			GHC.Core.Subst
  CoreSyn.lhs			GHC.Core.[Syntax/Types]
  CoreTidy.lhs			GHC.Core.Tidy
  CoreUnfold.lhs		GHC.Core.Unfold
  CoreUtils.lhs			GHC.Core.Utils
  MkCore.lhs			GHC.Core.Make
  PprCore.lhs			GHC.Core.Pretty[Print]
  ExternalCore.lhs		GHC.ExternalCore  -- no imports
  MkExternalCore.lhs		GHC.ExternalCore.Make -- local import only CoreSyn
  PprExternalCore.lhs		GHC.ExternalCore.Pretty[Print]

deSugar/
  Coverage.lhs			GHC.Desugar.[Program]Coverage
  Desugar.lhs			GHC.Desugar
  DsArrows.lhs			GHC.Desugar.Arrows
  DsBinds.lhs			GHC.Desugar.Bind[er]s
  DsCCall.lhs			GHC.Desugar.Foreign.Call
  DsExpr.lhs/boot		GHC.Desugar.Expression
  DsForeign.lhs			GHC.Desugar.Foreign.Declaration]
  DsGRHSs.lhs			GHC.Desugar.GuardedRHS
  DsListComp.lhs		GHC.Desugar.ListComp[rehension]
  DsMeta.hs			GHC.Desugar.Meta -- ? what is this?
  DsMonad.lhs			GHC.Desugar.Monad
  DsUtils.lhs			GHC.Desugar.Utils
  Match.lhs/boot		GHC.Desugar.Match -- ?
  Check.lhs			GHC.Desugar.Match.Check
  MatchCon.lhs			GHC.Desugar.Match.Constructor
  MatchLit.lhs			GHC.Desugar.Match.Literal[Pattern] -- also used by DsExpr, DsMeta

iface/
  BinIface.hs			GHC.Interface.Binary
  BuildTyCl.lhs			GHC.Interface.BuildTypeAndClass
  IfaceEnv.lhs			GHC.Interface.Environment
  IfaceSyn.lhs			GHC.Interface.[Syntax/Types]
  IfaceType.lhs			GHC.Interface.[Type/Types]
  LoadIface.lhs			GHC.Interface.Load
  MkIface.lhs			GHC.Interface.Make
  TcIface.lhs/boot		GHC.Interface.Typecheck / GHC.Typecheck.Interface 
  				-- maybe both (i.e. re-export)?  seems to import only one Tc* module

simplCore/
  SimplCore.lhs			GHC.Core.Optimise
  SimplEnv.lhs			GHC.Core.[Simplify.]Env
  SimplMonad.lhs		GHC.Core.[Simplify.]Monad
  SimplUtils.lhs		GHC.Core.[Simplify.]Utils
  Simplify.lhs			GHC.Core.Simplify
  CSE.lhs			GHC.Core.[CSE/CommonSubexprElim]
  SAT.lhs			GHC.Core.Simplify.[StaticArgTrans/StaticArgumentTransformation]
  FloatIn.lhs			GHC.Core.FloatIn
  FloatOut.lhs			GHC.Core.FloatOut
  SetLevels.lhs			GHC.Core.FloatOut.SetLevels
  LiberateCase.lhs		GHC.Core.LiberateCase
  OccurAnal.lhs			GHC.Core.OccAnalysis -- used by SimplCore, CoreUnfold, DsBinds, Rules
  simplifier.tib		

stranal/
  DmdAnal.lhs			GHC.Core.DemandAnalysis
  SaAbsInt.lhs			GHC.Core.DemandAnalysis.AbstractInterp
  SaLib.lhs			GHC.Core.StrictAnalysis.Types -- old strictness stuff
  StrictAnal.lhs		GHC.Core.StrictAnalysis -- old
  WorkWrap.lhs			GHC.Core.Work[er]Wrap[per]
  WwLib.lhs			GHC.Core.Work[er]Wrap[per].Utils

cprAnalysis/
  CprAnalyse.lhs		-- part of old strictness anal.

-- this could go below GHC.Core. or become a separate top-level thingy
vectorise/
  VectBuiltIn.hs		-- only used by VectMonad
  VectCore.hs			GHC.Core.Vectorise -- .Core/Main/Pass/Step ?
  VectMonad.hs			GHC.Core.Vectorise.Monad
  VectType.hs			GHC.Core.Vectorise.Type
  VectUtils.hs			GHC.Core.Vectorise.Utils
  Vectorise.hs			GHC.Core.[Simplify.]Vectorise -- ? (called only by SimplCore)

specialise/
  Rules.lhs			GHC.Core.Rules -- maybe extract GHC.Types.Rules for syntax etc.
  SpecConstr.lhs		GHC.Core.Simplify.Spec[ialise]Constr[uctor] --only used by SimplCore
  Specialise.lhs		GHC.Core.Simplify.Specialise -- only used by SimplCore

stgSyn/
  CoreToStg.lhs			GHC.Core.ToSTG / GHC.STG.FromCore / GHC.CoreToSTG
  StgLint.lhs			GHC.STG.Lint
  StgSyn.lhs			GHC.STG

simplStg/
  SRT.lhs			GHC.STG.StaticRefTable
  SimplStg.lhs			GHC.STG.Optimise
  StgStats.lhs			GHC.STG.[Optimise.]Stats

profiling/
  CostCentre.lhs		GHC.Types.CostCentre
  NOTES				-- merge into haddocks somewhere? (where?)
  SCCfinal.lhs			GHC.STG.Simplify.SCCFinal / GHC.STG.Profiling

-- pretty messy stuff
prelude/
  ForeignCall.lhs		GHC.Types.ForeignCall
  PrelInfo.lhs			-- put into other files
  PrelNames.lhs			GHC.Builtin.Names
  PrelRules.lhs			GHC.Builtin.Rules
  PrimOp.lhs			GHC.Builtin.Prim.PrimOps
  TysPrim.lhs			GHC.Builtin.Prim.Types
  TysWiredIn.lhs		GHC.Builtin.Types / GHC.Builtin.Prim.Rules ?
   				  -- this module contains wired-in *knowledge* about types, really
  primops.txt[.pp]		-- put where?


------------------------------------------------------------------------------
-- probably changed by John Dias' patches

-- General Idea, put it under GHC.Cmm (with GHC. prefix because it
-- at least uses some utils, 'Name', 'StaticFlags', etc.)
cmm/
...

-- GHC.CodeGen.*
codeGen/

-- GHC.NativeGen.* or GHC.CodeGen.Native.*
nativeGen/
```