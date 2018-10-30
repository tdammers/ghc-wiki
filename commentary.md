# The GHC Commentary



This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.



For the beginners there is [a short getting started guide](newcomers).



For the dedicated, there are [videos of Simon and Simon giving an overview of GHC](about-videos), at the 2006 [GHC Hackathon](hackathon).



Also check out the [GHC Reading List](reading-list), which gives lots of background reading that will help you understand the actual implementation.  Here's [
another reading list](http://www.stephendiehl.com/posts/essential_compilers.html) from Stephen Diehl.



Stephen also has a helpful series of blog posts about GHC internals


- [ Dive into GHC: pipeline](http://www.stephendiehl.com/posts/ghc_01.html)
- [
  Dive into GHC: intermediate forms](http://www.stephendiehl.com/posts/ghc_02.html)
- [
  Dive into GHC: targeting Core](http://www.stephendiehl.com/posts/ghc_03.html)

## Editing the Commentary



Please feel free to add material to the rest of the wiki: don't worry too much about accuracy (in due course someone will edit your contribution). When unsure though please indicate this and its best to ask on the GHC mailing list so you can correct the commentary. Please give some thought to where in the commentary your contribution belongs. GHC has an older commentary (non wiki based) that read like a single coherent narrative, made sure to define terms before using them, and introduced concepts in the order which made them easiest to understand.  Please do try to preserve those properties in this wiki commentary. If you're unsure or in a hurry, consider creating a wiki page outside the commentary and linking to it from the commentary (or the "contributed documentation" section below).



Try to link to source files as much as possible by using this macro: `[[GhcFile(compiler/Makefile)]]`. Also try to add appropriate links to other parts of the commentary.


## Contents


- [Getting Started](commentary/getting-started)

  - [Source Tree Roadmap](commentary/source-tree)
  - [Module Structure](commentary/module-structure)
  - [Coding Style](commentary/coding-style)
  - [Abbreviations in GHC](commentary/abbreviations)
  - [Platforms and their Naming Convention](commentary/platform-naming)

- [The Compiler](commentary/compiler)

- [The Libraries on which GHC depends](commentary/libraries)

  - [The Integer libraries (\`integer-gmp\` and \`integer-simple\`)](commentary/libraries/integer)

- [The Runtime System (RTS)](commentary/rts)

  - [RTS Coding Conventions](commentary/rts/conventions)
  - [The Haskell Execution Model](commentary/rts/haskell-execution)
  - [The memory layout of heap and stack objects](commentary/rts/storage)


 


- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Profiling](commentary/profiling)
  - [Wired-in and known-key things](commentary/compiler/wired-in)
  - [Primitive Operations (PrimOps)](commentary/prim-ops)
  - [The Package System](commentary/packages)


 


- [The User Manual](commentary/user-manual) (formatting guidelines etc)

## Contributed Documentation



The above commentary covers the source code of GHC. For material that doesn't concern this topic (such as proposals, work-in-progress and status reports) or that don't fit into the existing structure, you will find them below. Feel free to add new material here but please categorise it correctly.


- General Notes on the GHC compiler

  - Ningning Xie's [
    literature review](https://github.com/xnning/GHC-Core-Literature-Review/blob/master/doc/doc.pdf) of various contributions to GHC's type system 
  - Edward Yang's blog post about [
    the entire complilation pipeline for \`factorial\`](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/)
  - [New Prim Ops](adding-new-primitive-operations): How to add new primitive operations to GHC Haskell.
  - [Replacing GMP](replacing-gmp-notes): Notes from an effort to replace GMP with another Bignum library.
  - [External Core](external-core): Describes the process of bringing External Core up to speed. Once finished, this will simply describe what External Core is, and how it works. 
  - [
    The Scrap your boilerplate homepage](http://sourceforge.net/apps/mediawiki/developers/index.php?title=ScrapYourBoilerplate).
  - [Optimisation Ordering](commentary/compiler/opt-ordering) Describe the ordering and interaction of optimisation passes (Old).
  - [
    GHC Illustrated](https://github.com/takenobu-hs/haskell-ghc-illustrated) (follow the PDF link), a very insightful tutorial on GHC's internals.
  - [
    Ollie Charles's 24 days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html), and [
    Lennart Augstsson's commentary](http://augustss.blogspot.com/2014/12/a-commentary-on-24-days-of-ghc.html)

    - [
      Welcome](https://ocharles.org.uk/blog/posts/2014-12-01-24-days-of-ghc-extensions.html)
    - [
      Static Pointers](https://ocharles.org.uk/blog/guest-posts/2014-12-23-static-pointers.html)
    - [
      Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)
    - [ Arrows](https://ocharles.org.uk/blog/guest-posts/2014-12-21-arrows.html)
    - [
      Scoped Type Variables](https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html)
    - [
      Existential Quantification](https://ocharles.org.uk/blog/guest-posts/2014-12-19-existential-quantification.html)
    - [
      Rank N Types](https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html)
    - [
      Overloaded Strings](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html)
    - [
      DeriveGeneric](https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html)
    - [
      Deriving](https://ocharles.org.uk/blog/guest-posts/2014-12-15-deriving.html)
    - [
      Functional Dependencies](https://ocharles.org.uk/blog/posts/2014-12-14-functional-dependencies.html)
    - [
      Multi-parameter Type Classes](https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html)
    - [
      Type Families](https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html)
    - [
      Implicit Parameters](https://ocharles.org.uk/blog/posts/2014-12-11-implicit-params.html)
    - [
      Nullary Type Classes](https://ocharles.org.uk/blog/posts/2014-12-10-nullary-type-classes.html)
    - [
      Recursive Do](https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html)
    - [
      Type Operators](https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html)
    - [
      List Comprehensions](https://ocharles.org.uk/blog/guest-posts/2014-12-07-list-comprehensions.html)
    - [
      Rebindable Syntax](https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html)
    - [
      Bang Patterns](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html)
    - [
      Record Wildcards](https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html)
    - [
      Pattern Synonyms](https://ocharles.org.uk/blog/posts/2014-12-03-pattern-synonyms.html)
    - [
      View Patterns](https://ocharles.org.uk/blog/posts/2014-12-02-view-patterns.html)
    - [ Thanks](https://ocharles.org.uk/blog/posts/2014-12-24-conclusion.html)
  - [Commentary/Rts/CompilerWays](commentary/rts/compiler-ways): Compiler *ways* in GHC, what, how, and where

- Notes on implemented GHC features:

  - [
    Evaluation order and state tokens](https://www.fpcomplete.com/tutorial-preview/4431/z0KpB0ai2R): notes written by Michael Snoyberg in response to [\#9390](http://gitlabghc.nibbler/ghc/ghc/issues/9390).
  - [Notes on fusion](foldr-build-notes) (eg foldr/build)
  - [Overloaded list syntax](overloaded-lists) allows you to use list notation for things other than lists.
  - [Kind polymorphism and data type promotion](ghc-kinds)
  - [A kind for class constraints. Implemented as ConstraintKinds](kind-fact)
  - [LLVM back end](commentary/compiler/backends/llvm)
  - [Support for generic programming](commentary/compiler/generic-deriving)
  - [Notes about Template Haskell](template-haskell)
  - [Rewrite Rules](rewrite-rules): Notes about the implementation of RULEs in GHC
  - [Monad Comprehensions](monad-comprehensions): Translation rules and some implementation details 
  - [Haddock](haddock-comments): Some notes about how the Haddock comment support is implemented.  
  - [Intermediate Types](intermediate-types): Notes about the type system of GHC's new intermediate language (in the HEAD since ICFP'06)  
  - [Type families/type functions](type-functions): Notes concerning the implementation of type families, associated types, and equality constraints as well as the extension of the type checker with a contraint solver for equality constraints.
  - [Magic to do with \`seq\` and friends](commentary/compiler/seq-magic)
  - [Compiler plug-ins](new-plugins)
  - [memcpy/memmove/memset optimizations](memcpy-optimizations)  
  - [Backend Ideas](back-end-notes): Some ideas and notes about the back end.
  - [Notes about the new code generator](commentary/compiler/new-code-gen)
  - [A record of improvements made to the performance of the Hoopl library for dataflow optimisation](commentary/compiler/hoopl-performance)
  - [DPH](data-parallel): Notes about the implementation of Data Parallel Haskell
  - [Safe Haskell](safe-haskell): The design of the GHC Safe Haskell extension
  - [SQL-Like Comprehensions](sql-like-comprehensions): Notes on SPJs "Comprehensive Comprehensions" (TransformComprehensions)
  - [Deferring compilation type errors to runtime (\`-fdefer-type-errors\`)](defer-errors-to-runtime)
  - [Demand analyser](commentary/compiler/demand) Notes on the meanings, worker-wrapper splitting of demand signatures and relevant components of the compiler
  - [Closed type families](new-axioms)
  - [OneShot](one-shot) The magic `oneShot` function.
  - [Deriving Functor, Foldable, and Traversable](commentary/compiler/derive-functor)

- Notes on proposed or in progress (but out of tree) GHC compiler features:

  - [Making Haskell strict](language-strict)
  - [Improving pattern-match overlap and exhaustiveness checks](pattern-match-check)
  - [Source-locations on HsSyn](ghc-ast-annotations)
  - [How GHC inter-operates with Cabal](cabal-dependency) and [Backpack](backpack)
  - [StaticValues](static-values) and ticket [\#7015](http://gitlabghc.nibbler/ghc/ghc/issues/7015)
  - [Partial type signatures](partial-type-signatures) and its ticket [\#9478](http://gitlabghc.nibbler/ghc/ghc/issues/9478)
  - [Late lambda-lifting](late-lam-lift), and its ticket [\#9476](http://gitlabghc.nibbler/ghc/ghc/issues/9476)
  - [Roles in Haskell](roles)
  - [Dependent types in Haskell](dependent-haskell)
  - [Nested CPR analysis](nested-cpr)
  - [Giving Template Haskell full access to annotations](template-haskell/annotations)
  - [Checking consistency of functional dependencies](fun-deps)
  - [Allowing multiple instances of the same package to be installed](commentary/g-so-c-multiple-instances), each instance having different dependencies
  - [Contracts in Haskell](commentary/contracts)
  - [Agda-style holes in terms](holes) which supports writing partial programs.
  - [Records](records)
  - [ Cloud Haskell](http://haskell.org/haskellwiki/GHC/CouldAndHPCHaskell)
  - [A modular package language for Haskell](package-language) Scott Kilpatrick and Derek Dreyer are designing a new “package language” for Haskell in the style of the ML module system.
  - [Pattern synonyms](pattern-synonyms)
  - [Type level naturals](type-nats) and [type level reasoning](type-level-reasoning).
  - [The solve for type-level naturals](commentary/compiler/type-nat-solver)
  - [Polymorphic Dynamic](polymorphic-dynamic): Notes on adding ad-hoc polymorphic dynamic types
  - [Proposal to allow classes to give default implementations for their superclasses](default-superclass-instances)
  - [Cmm: Implementing Exception Handling](commentary/cmm-exceptions): Implementing exception handling for primitive operations in Cmm
  - [Cmm: Using Kinds to implement calling conventions](commentary/cmm-kinds): Pointers to explanations of what a `CmmKind` is and why you might find one useful.
  - [SIMD](simd): Notes on adding SIMD instructions and primOps to GHC
  - [Explicit Call Stack](explicit-call-stack): Notes about maintaining an explicit call stack, to support error attribution and profiling.
  - [Objective-C FFI](objective-c): Haskell FFI support for Objective-C
  - [C Blocks](block-objects): Haskell FFI support for block objects (closures) in C
  - [Syntax for explicit type application and scoped type variables](explicit-type-application)
  - [Syntax for defining kinds that do not arise from datatype promotion](ghc-kinds/kinds-without-data)
  - [Lambda-Case](lambdas-vs-pattern-matching): Syntax for full (with branching) pattern matching on arguments of lambda abstractions
  - [Typeable](typeable): Making Typeable more expressive
  - [SafeRoles](safe-roles): Roles & Safe Haskell

- Library issues

  - [Splitting up the base package](split-base)

- GHCi Debugger

  - [Commentary/GHCi](commentary/gh-ci): Notes on the implementation details for the support of breakpoints in GHCi.
  - [GHCi Debugger](ghci-debugger): Notes on the implementation details of the GHCi debugger's feature for inspecting values.

- The Runtime System

  - [A new lightweight concurrency substrate for GHC](lightweight-concurrency)
  - [Garbage Collector](garbage-collector-notes): Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
  - [GMP Memory Managment](gmp-memory-management): Describes how the garbage collector cooperates with GMP for Integer.
  - [SemiTagging](semi-tagging): Describes how the semi-tagging optimisation will be implemented.
  - [PAPI](papi): Measurement of program performance using CPU events (cache misses, branch mis-predictions).

- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Haskell Program Coverage](commentary/hpc): How HPC works

- [Parallel Haskell Variants](gp-h-eden): All aspects of the GpH and Eden parallel variants of GHC.


 


## Old Documentation



Here are some useful, but somewhat-out-of-date resources:


- [
  The old GHC Commentary](http://darcs.haskell.org/ghc/docs/comm/): Information on the internals of GHC, in various states of up-to-dateness.  We are keen to move this stuff out of its current location and onto this Wiki.  If anyone is willing to help do that, even for just a part in which you are interested, we would be delighted.  There is a [page](commentary/migrating-old-commentary) which tracks the progress of migrating information from the old commentary to this wiki.
- [GHC Papers](ghc-papers): Papers and pointers to other documents that relate to the inner workings of GHC.
