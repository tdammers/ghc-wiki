


# Packages in GHC



This page summarises our current proposal for packages in GHC. (See also [an extended proposal](commentary/packages/package-namespaces-proposal) to make namespaces first-class. The two proposals are mutually exclusive.)


## The problem



A vexed question in the current design of Haskell is the issue of whether a single program can contain two modules with the same name.  In Haskell 98 that is absolutely ruled out.
As a result, packages are fundamentally non-modular: to avoid collisions *every* module in *every* package written by *anyone* must have different module names.  That's like saying that every function must have different local variables, and is a serious loss of modularity.



GHC 6.6 makes a significant step forward by lifting this restriction.  However it leaves an open question, which is what this page is about.


## Assumptions



Before we start, note that we take for granted the following


- **Each package has a globally-unique name**, organised by some social process.  This assumption is deeply built into Cabal, and lots of things would need to change if it wasn't met.

- **Module names describe *purpose* (what it's for, e.g. `Data.Bits`), whereas package names describe *provenance* (where it comes from, e.g. `"gtkhs"`)**.  We should not mix these two up, and that is a good reason for not combining package and module names into a single grand name.  One quite frequently wants to globally change provenance but not purpose (e.g. compile my program with a new version of package "foo"), without running through all the source files to change the import statements.

- **New: a module name must be unique within its package (only)**.   That is, a single program can use two modules with the same module name, provided they come from different packages.  This is new in GHC 6.6.  


For all this to work, GHC must incorporate the package name (and version) into the names of entities the package defines.  That means that when compiling a module M you must say what package it is part of:


```wiki
  ghc -c -package-name P1 C.hs
```


Then C.o will contain symbols like "`P1.A.B.C.f`" etc.  In effect, the "original name" of a function `f` in module `M` of package `P` is `<P,M,f>`.


## The open question



The remaining question is this: **When you say `import A.B.C`, from what package does A.B.C come?**.  Three alternatives are under consideration:


- Plan A (GHC's current story)
- Plan B: grafting.  An enhancement of plan A; see [Frederik Eaton's proposal](commentary/packages/package-mounting-proposal)
- Plan C: optionally specify the package in the import.  An alternative to (B), described in a [separate page](commentary/packages/package-imports-proposal).

---


## Plan A: GHC's current story



GHC already has a fairly elaborate scheme (perhaps too elaborate; [documentation here](http://www.haskell.org/ghc/dist/current/docs/users_guide/packages.html)) for deciding what package you mean when you say "import A.B.C":


- For a start, it only looks in *installed* packages.  
- Even for installed packages, the package may or may not be *exposed* by default (reasoning: you may want old versions of package X to be installed, but not in scope by default).  
- Then, you can use the `-hide-package` flag to hide an otherwise-exposed package, and the `-package` flag to expose an otherwise-hidden package.


So, you can expose package P1 when compiling module M (say), and expose P2 when compiling module N by manipulating these flags.  Then M and N could both import module A.B.C, which would come from P1 and P2 respectively. But:


- What if you wanted to import A.B.C from P1 and A.B.C from P2 into the *same* module?
- What if you want to only replace *parts* of P1 (e.g., you want to use an updated version of a module in `base`)?
- Compiling different modules with different flags in a way that affects the *semantics* (rather than, say, the optimisation level) seems undesirable.
- To support `--make` in this situation we'd need to allow `-package` flags in the per-module `OPTIONS` pragmas, which isn't currently supported.  (`ghc --make` already gathers those options together for the link step.)  *This is not yet implemented, but it is close to being implemented.*


If we did implement the "`-package` in `OPTIONS` pragma" fix, then is is not clear how pressing the need is for anything more.  It's still impossible to import M from P1, and M from P2, into the same module.  But how often will that happen?


---


## Plan B: package mounting



This proposal is described by a [separate page](commentary/packages/package-mounting-proposal).


---


## Plan C: mention the package in the import



This proposal is described by a [separate page](commentary/packages/package-imports-proposal).


