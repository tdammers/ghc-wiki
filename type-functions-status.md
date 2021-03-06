
[TypeFunctions](type-functions)/Status


# The new solver infrastructure: Dimitrios and Simon



This section collects notes about the new constraint solver (Dec 2009) that we are about to put into GHC.


- Don't forget: instance declarations should not mention a type family in the head, just as type instances should not:

  ```wiki
  type instance F (G x) = x    -- Illegal if G is a type function
  instance C (G x) where ...   -- Likewise should be illegal
  ```

---


# Type Families: Implementation Status



**Open bugs related to type families**


- Declarations involving families:

  - [\#3714](http://gitlabghc.nibbler/ghc/ghc/issues/3714): scoping error for associated types
  - If a type variable occurs only in arguments to type synonym families in a signature, GHC ought to reject the signature as ambiguous.  (If the variable is mentioned in an argument to a type class in the context, we cannot reject it though, as the class may contain a TF or FD that constrains the variable.)
  - [\#2435](http://gitlabghc.nibbler/ghc/ghc/issues/2435) (Inconsistency in handling qualification of names of class methods and associated types in instance declarations)
  - Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)  If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)

- Constraint simplification:

  - [\#3787](http://gitlabghc.nibbler/ghc/ghc/issues/3787): ill-typed program made by typechecker
  - [\#3651](http://gitlabghc.nibbler/ghc/ghc/issues/3651): unreachable branches not discovered
  - [\#3584](http://gitlabghc.nibbler/ghc/ghc/issues/3584): premature commitment to an instance declaration
  - [\#3554](http://gitlabghc.nibbler/ghc/ghc/issues/3554): assertion failure
  - [\#3500](http://gitlabghc.nibbler/ghc/ghc/issues/3500): type functions and recursive dictionaries — need to require UndecidableInstances
  - [\#3484](http://gitlabghc.nibbler/ghc/ghc/issues/3484): type checker diverges (involves higher rank types)
  - [\#3460](http://gitlabghc.nibbler/ghc/ghc/issues/3460): mixed equalities and type classes
  - [\#3208](http://gitlabghc.nibbler/ghc/ghc/issues/3208) (another problem with recursive groups that containing signatures with equalities over TFs)
  - [\#3297](http://gitlabghc.nibbler/ghc/ghc/issues/3297): Rank-n types: In `TcTyFuns.flattenType`, we need to pull out type families below foralls -maybe pull out if possible, but definitely improve the error message
  - Implicit parameters: In `TcTyFuns`, we need to normalise IP constraints, too (in `normDict` and `substDict`).
  - [\#3330](http://gitlabghc.nibbler/ghc/ghc/issues/3330): typechecker goes into a loop
  - [\#2664](http://gitlabghc.nibbler/ghc/ghc/issues/2664) (typechecker diverges; actually, it's after Tc9, probably during zonking)
  - [\#2102](http://gitlabghc.nibbler/ghc/ghc/issues/2102) & [\#2715](http://gitlabghc.nibbler/ghc/ghc/issues/2715) (superclass equalities)

    - To fix superclass equalities (specifically getting the coercion evidence), we could introduce a kind of typelet just for evidence.  In fact, re-use `HsBind.VarBind` and make its right-hand side a specially data structure describing evidence construction, instead of being a general `HsExpr`.  That evidence construction generation can have a case for extracting superclass constraints.  The desugarer than has to generate the case expression bringing the equality in scope from that.
    - What about filtering the `EqInst`s in `TcSimplify.addSCs`.  We need them, don't we?  But they give rise to `Var`s, not `Id`s, and we haven't got selectors.

- GADT:

  - `gadt/lazypatok` needs to be fixed, but are irrefutable patterns really ok, see [
    http://okmij.org/ftp/Haskell/GADT-problem.hs](http://okmij.org/ftp/Haskell/GADT-problem.hs)?
  - It'd be nice if the error message of `tcfail167` would include "Inaccessible case alternative: Can't match types `Char' and `Float'" again.  We could achieve in `TcPat.tcConPat` by having a look at `eq_preds` in the GADT case.  If the `eq_preds` are obviously unsatisfiable (due to a clash of data type constructors), then we have an inaccessible case alternative.
  - Rigidity:

    - Shiuld test `GADT7` really succeed?  or is it ok to fail.
    - Remove the dodgy rigidity test that is in `tcConPat` right now.
    - implement the proposal where we infer a rigidity flag for case scutinees and pass that down when type checking the patterns.
    - We infer the rigidity flag for the case scrutinee by generalising its type and checking whether that has any foralls at the top.  It's rigid if it has no foralls.
    - As usual, if a pattern has a GADT constructor (ie, any constraints in the data constructor signature), the scutinee must be rigid,
    - We  need to know of types whether they are rigid (not only whether they contain unification variables).  We can achieve that by a flag in the environment that indicates whether the computation of that type involved non-rigid type variables.

- Misc:

  - [\#3064](http://gitlabghc.nibbler/ghc/ghc/issues/3064): exponentially slow compile times
  - [\#3169](http://gitlabghc.nibbler/ghc/ghc/issues/3169) & [\#2360](http://gitlabghc.nibbler/ghc/ghc/issues/2360) (improve occurs-check error message in two instances)
  - [\#2721](http://gitlabghc.nibbler/ghc/ghc/issues/2721) (generalised newtype deriving for classes with associated types) Tom S has a use-case for this
  - `TcPat` and `TcUnify` (and maybe other modules) still have calls to the unification engine that ignore the returned coercion!!
  - Test `Simple17` (corelint error as a dict binding, used to specialise a call to a local function, floats out too far)
  - Improve error messages for loopy equalities: TF tests `Simple13` & `SkolemOccursLoop`
  - [\#1897](http://gitlabghc.nibbler/ghc/ghc/issues/1897): If you infer a type for a function, then should check the function against that sigature, to check that if the user gave that signature, then typechecking would again succeed.  See this thread [
    http://www.haskell.org/pipermail/haskell-cafe/2008-April/041385.html](http://www.haskell.org/pipermail/haskell-cafe/2008-April/041385.html).
  - [\#1769](http://gitlabghc.nibbler/ghc/ghc/issues/1769) (deriving typeable for data families)
  - When a `type instance` changes (in an orphan modules), currently clients are not properly recompiled at least by `--make`.
  - When we raise a mismatch error in `TcSimplify` for unresolvable equalities, we effectively tidy the two non-matching types twice.  Add a comment to highlight this and say why it is ok (i.e., they are never grouped together with `groupErrs` or similar).
  - [\#2296](http://gitlabghc.nibbler/ghc/ghc/issues/2296): error message involving fundep gives unhelpful location.  I want to remember to come back to this one when we have the new type-family simplification stuff in place.
  - Fix export list problem (ie, export of data constructors introduced by orphan data instances):

    - Change `HscTypes.IfaceExport` to use `Name` instead of `OccName`.
    - Then, there is also no need for the grouping of the identifiers by module anymore (but sort it to avoid spurious iface changes dur to re-ordering when re-compiling).
    - We still need to have the name parent map, though.
    - See email for example.
  - [\#2436](http://gitlabghc.nibbler/ghc/ghc/issues/2436) (Bad warning on export)
  - Eliminate code duplication between `tcTyClDecl1` and `tcFamInstDecl1`.  The code for vanilla data/newtype declarations and the code for data/newtype instances has many commonalities.


**Additional feature:**


- [\#3699](http://gitlabghc.nibbler/ghc/ghc/issues/3699): wildcards in type function LHSs
- [\#3490](http://gitlabghc.nibbler/ghc/ghc/issues/3490): relax restrictions on superclass contexts
- [\#2101](http://gitlabghc.nibbler/ghc/ghc/issues/2101)
- Total families
- Test `DerivingNewType`
- Implementing FDs by TFs:

  - Step 1: Replace the existing improvement machinery for FDs by code that generates explicit equalities from the two FD rules.  Then, all improvement is by normalisation of equalities, which hopefully allows us to simplify `TcSimplify.reduceContext`.  (Apply this change when integrating the simplification of equalities and dictionaries.)
  - Step 2: Desugar FDs into TFs and superclass equalities.
- [\#3005](http://gitlabghc.nibbler/ghc/ghc/issues/3005) & ghci command to print normalised type and add [
  http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799](http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799) as a test to the testsuite.
- Most general signatures for record selectors:

  ```wiki
  type family F a
  data T a b = MkT1 { fa :: F a, fb :: b }
  upd t x = t { fb = x }
  ```

  What is the most general type of `upd`?  It's

  ```wiki
  upd :: (F a ~ F d) => T a b -> c -> T d c
  ```

  However, we currently insist on the less general

  ```wiki
  upd :: T a b -> c -> T a c
  ```

  It seems a bit complicated to come up with the most general type.  The relevant code is in `TcExpr.tcExpr` in STEP 4 of the `RecordUpd` case.

---



 


## Parsing and Renaming



Todo (low-level): None.



Todo (high-level):


1. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)


Done:


- Parsing and renaming of kind signatures (toplevel and in classes).
- Parsing and renaming of indexed type declarations (toplevel and in classes).
- Using new syntax with `family` and `instance` on top level.
- Added `-findexed-types` switch.
- Allowing `type` tag in export lists to list associated types in the sub-binder list of an import/export item for a class.
- Import/export lists: ATs can be listed as subnames of classes and the data constructors of instances of a data family are subnames of that family.
- Parsing and renaming of equational constraints in contexts.

## Type Checking



Todo (low-level):


- Allow data family GADT instances.
- Deriving `Typeable` for data families ([\#1769](http://gitlabghc.nibbler/ghc/ghc/issues/1769))
- If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)


Todo (high-level): 


1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type check functional dependencies as type functions.


Done: 


- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types, including the generation of representation tycons.
- Wrapper generation and type checking of pattern matching for indexed data and newtypes.
- Consistency checking for family instances.
- Enforce syntactic constraints on type instances needed to ensure the termination of constraint entailment checking.
- Equality constraint normalisation and coercion term generation.
- GADT type checking implemented with equality and implication constraints.

## Desugaring



Todo (low-level): None.



Todo (high-level): None.



Done:


- Representation of family kind signatures as `TyCon.TyCon`s.
- Extension of `Class.Class` by associated `TyCon`s.
- Extension of `TyCon.TyCon` with a reference to the parent `TyCon` for data instances.
- Extension of `DataCon.DataCon` with instance types for constructors belonging to data instances.
- Extension of `TyCon.TyCon` such that the parent of a data instance is paired with a coercion identifying family instance and representation type.
- For indexed data types, the datacon wrapper uses data instance coercion and pattern matching casts the scrutinee via an `ExprCoFn` in a `CoPat`.
- Import and exporting.
- Generation and plumbing through of rough matches.
- Equational constraints in contexts.
