# The GHC Commentary: Checking Types



Probably the most important phase in the frontend is the type checker, which is located at [compiler/typecheck/](/trac/ghc/browser/ghc/compiler/typecheck/). GHC type checks programs in their original Haskell form before the desugarer converts them into Core code. This complicates the type checker as it has to handle the much more verbose Haskell AST, but it improves error messages, as those message are based on the same structure that the user sees.



GHC defines the abstract syntax of Haskell programs in [compiler/hsSyn/HsSyn.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.hs) using a structure that abstracts over the concrete representation of bound occurences of identifiers and patterns. The module [compiler/typecheck/TcHsSyn.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcHsSyn.hs) defines a number of helper function required by the type checker. Note that the type [compiler/typecheck/TcRnTypes.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcRnTypes.hs).`TcId` used to represent identifiers in some signatures during type checking is, in fact, nothing but a synonym for a [plain Id](commentary/compiler/entity-types#type-variables-and-term-variables).



It is also noteworthy, that the representations of types changes during type checking from `HsType` to `TypeRep.Type`. The latter is a [hybrid type](commentary/compiler/type-type) representation that is used to type Core, but still contains sufficient information to recover source types. In particular, the type checker maintains and compares types in their `Type` form.


## The Overall Flow of Things


- `TcRnDriver` is the top level.  It calls

  - `TcTyClsDecls`: type and class declaration
  - `TcInstDcls`: instance declarations
  - `TcBinds`: value bindings

    - `TcExpr`: expressions
    - `TcMatches`: lambda, case, list comprehensions
    - `TcPat`: patterns
  - `TcForeign`: FFI declarations
  - `TcRules`: rewrite rules
  - `TcHsTypes`: kind-checking type signatures
  - `TcValidity`: a second pass that walks over things like types or type constructors, checking a number of extra side conditions.

- The constraint solver consists of:

  - `TcSimplify`: top level of the constraint solver
  - `TcCanonical`: canonicalising constraints
  - `TcInteract`: solving constraints where they interact with each other
  - `TcTypeNats`: solving natural-number constraints
  - `TcSMonad`: the monad of the constraint solver (built on top of the main typechecker monad)
  - `TcEvidence`: the data types used for evidence (mostly pure)
  - `TcUnify`: solves unification constraints "on the fly"; if it can't, it generates a constraint for the constraint solver to deal with later
  - `TcErrors`: generates good error messages from the residual, unsolved constraints.

>
>
> The best place reading for the constraint solver is the paper [
> Modular type inference with local assumptions](http://www.haskell.org/haskellwiki/Simonpj/Talk:OutsideIn)
>
>

- Underlying infrastructure:

  - `TcRnTypes`: a big collection of the types used during type checking
  - [TcRnMonad](commentary/compiler/tc-rn-monad): the main typechecker monad
  - `TcType`: pure functions over types, used by the type checker


   


### Entry Points Into the Type Checker



The interface of the type checker (and [renamer](commentary/compiler/renamer)) to the rest of the compiler is provided by [compiler/typecheck/TcRnDriver.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcRnDriver.hs). Entire modules are processed by calling `tcRnModule` and GHCi uses `tcRnStmt`, `tcRnExpr`, and `tcRnType` to typecheck statements and expressions, and to kind check types, respectively. Moreover, `tcTopSrcDecls` is used by Template Haskell - more specifically by `TcSplice.tc_bracket` - to type check the contents of declaration brackets.


### Renaming and Type Checking a Module



The functions `tcRnModule` and `tcRnModuleTcRnM` control the complete static analysis of a Haskell module. They set up the combined renamer and type checker monad, resolve all import statements, take care of hi-boot files, initiate the actual renaming and type checking process, and finally, wrap off by processing the export list.



The actual type checking and renaming process is initiated via `TcRnDriver.tcRnSrcDecls`, which uses a helper called `tc_rn_src_decls` to implement the iterative renaming and type checking process required by [
Template Haskell](http://darcs.haskell.org/ghc/docs/comm/exts/th.html) (TODO Point at new commentary equivalent). After it invokes `tc_rn_src_decls`, it simplifies type constraints and zonking (see below regarding the later).



The function `tc_rn_src_decls` partitions static analysis of a whole module into multiple rounds, where the initial round is followed by an additional one for each toplevel splice. It collects all declarations up to the next splice into an `HsDecl.HsGroup`. To rename and type check that declaration group it calls `TcRnDriver.rnTopSrcDecls` and `TcRnDriver.tcTopSrcDecls`. Afterwards, it executes the splice (if there are any left) and proceeds to the next group, which includes the declarations produced by the splice.



The renamer, apart from renaming, computes the global type checking environment, of type `TcRnTypes.TcGblEnv`, which is stored in the [type checking monad](commentary/compiler/tc-rn-monad) before type checking commences.


## Type Checking a Declaration Group



The type checking of a declaration group, performed by `tcTopSrcDecls` and its helper function `tcTyClsInstDecls`, starts by processing of the type and class declarations of the current module, using the function `TcTyClsDecls.tcTyAndClassDecls`. This is followed by a first round over instance declarations using `TcInstDcls.tcInstDecls1`, which in particular generates all additional bindings due to the deriving process. Then come foreign import declarations (`TcForeign.tcForeignImports`) and default declarations (`TcDefaults.tcDefaults`).



Now, finally, toplevel value declarations (including derived ones) are type checked using `TcBinds.tcTopBinds`. Afterwards, `TcInstDcls.tcInstDecls2` traverses instances for the second time. Type checking concludes with processing foreign exports (`TcForeign.tcForeignExports`) and rewrite rules (`TcRules.tcRules`). Finally, the global environment is extended with the new bindings.


## Type checking Type and Class Declarations



Type and class declarations are type checked in a couple of phases that contain recursive dependencies - aka *knots*. The first knot encompasses almost the whole type checking of these declarations and forms the main piece of `TcTyClsDecls.tcTyAndClassDecls`.



Inside this big knot, the first main operation is kind checking, which again involves a knot. It is implemented by `kcTyClDecls`, which performs kind checking of potentially recursively-dependent type and class declarations using kind variables for initially unknown kinds. During processing the individual declarations some of these variables will be instantiated depending on the context; the rest gets by default kind \* (during *zonking* of the kind signatures). Type synonyms are treated specially in this process, because they can have an unboxed type, but they cannot be recursive. Hence, their kinds are inferred in dependency order. Moreover, in contrast to class declarations and other type declarations, synonyms are not entered into the global environment as a global `TyThing`. (`TypeRep.TyThing` is a sum type that combines the various flavours of typish entities, such that they can be stuck into type environments and similar.)


## More Details


### Types Variables and Zonking



During type checking type variables are represented by mutable variables - cf. the [
variable story](http://darcs.haskell.org/ghc/docs/comm/the-beast/vars.html#TyVar) (TODO Point at new commentary equivalent). Consequently, unification can instantiate type variables by updating those mutable variables. This process of instantiation is (for reasons that elude me) called [
zonking](http://dictionary.reference.com/browse/zonk) in GHC's sources. The zonking routines for the various forms of Haskell constructs are responsible for most of the code in the module [compiler/typecheck/TcHsSyn.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcHsSyn.hs), whereas the routines that actually operate on mutable types are defined in [compiler/typecheck/TcMType.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcMType.hs); this includes the zonking of type variables and type terms, routines to create mutable structures and update them as well as routines that check constraints, such as that type variables in function signatures have not been instantiated during type checking. The actual type unification routine is `uTys` in the module [compiler/typecheck/TcUnify.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcUnify.hs).



All type variables that may be instantiated (those in signatures may not), but haven't been instantiated during type checking, are zonked to `()`, so that after type checking all mutable variables have been eliminated.


### Type Representation



The representation of types is fixed in the module [TypeRep](/trac/ghc/browser/ghc/compiler/types/TypeRep.lhs) (TODO Update to the latest information) and exported as the data type `Type`. Read the comments in the `TypeRep` module!  A couple of points:


- Type synonym applications are represented as a `TyConApp` with a `TyCon` that contains the expansion.  The expansion is done on-demand by `Type.coreView`.  Unexpanded type synonyms are useful for generating comprehensible error messages.

- The `PredTy` constructor wraps a type constraint argument (dictionary, implicit parameter, or equality).  They are expanded on-demand by `coreView`.


As explained in [compiler/typecheck/TcType.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcType.hs), GHC supports rank-N types, but during type inference maintains the restriction that type variables cannot be instantiated to quantified types (i.e., the type system is predicative).  However the type system of Core is fully impredicative.


### Type Checking Environment



During type checking, GHC maintains a *type environment* whose type definitions are fixed in the module [compiler/typecheck/TcRnTypes.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcRnTypes.hs) with the operations defined in [compiler/typecheck/TcEnv.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcEnv.hs). Among other things, the environment contains all imported and local instances as well as a list of *global* entities (imported and local types and classes together with imported identifiers) and *local* entities (locally defined identifiers). This environment is threaded through the [type checking monad](commentary/compiler/tc-rn-monad).


### Expressions



Expressions are type checked by [compiler/typecheck/TcExpr.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcExpr.hs).



Usage occurences of identifiers are processed by the function tcId whose main purpose is to [instantiate overloaded identifiers](commentary/compiler/type-checker#handling-of-dictionaries-and-method-instances). It essentially calls `TcInst.instOverloadedFun` once for each universally quantified set of type constraints. It should be noted that overloaded identifiers are replaced by new names that are first defined in the LIE (Local Instance Environment?) and later promoted into top-level bindings.


### Handling of Dictionaries and Method Instances



GHC implements overloading using so-called *dictionaries*. A dictionary is a tuple of functions -- one function for each method in the class of which the dictionary implements an instance. During type checking, GHC replaces each type constraint of a function with one additional argument. At runtime, the extended function gets passed a matching class dictionary by way of these additional arguments. Whenever the function needs to call a method of such a class, it simply extracts it from the dictionary.



This sounds simple enough; however, the actual implementation is a bit more tricky as it wants to keep track of all the instances at which overloaded functions are used in a module. This information is useful to optimise the code. The implementation is the module [compiler/typecheck/Inst.hs](/trac/ghc/browser/ghc/compiler/typecheck/Inst.hs).



The function `instOverloadedFun` is invoked for each overloaded usage occurrence of an identifier, where overloaded means that the type of the identifier contains a non-trivial type constraint. It proceeds in two steps: (1) Allocation of a method instance (`newMethodWithGivenTy`) and (2) instantiation of functional dependencies. The former implies allocating a new unique identifier, which replaces the original (overloaded) identifier at the currently type-checked usage occurrence.



The new identifier (after being threaded through the LIE) eventually will be bound by a top-level binding whose rhs contains a partial application of the original overloaded identifier. This papp applies the overloaded function to the dictionaries needed for the current instance. In GHC lingo, this is called a *method*. Before becoming a top-level binding, the method is first represented as a value of type Inst.Inst, which makes it easy to fold multiple instances of the same identifier at the same types into one global definition. (And probably other things, too, which I haven't investigated yet.)



**Note:** As of 13 January 2001 (wrt. to the code in the CVS HEAD), the above mechanism interferes badly with RULES pragmas defined over overloaded functions. During instantiation, a new name is created for an overloaded function partially applied to the dictionaries needed in a usage position of that function. As the rewrite rule, however, mentions the original overloaded name, it won't fire anymore -- unless later phases remove the intermediate definition again. The latest CVS version of GHC has an option '-fno-method-sharing', which avoids sharing instantiation stubs. This is usually/often/sometimes sufficient to make the rules fire again.


