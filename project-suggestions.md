# Suggestions for projects related to GHC



Here are some suggestions for projects related to GHC that could be undertaken by an intern or undergraduate project student.  There are also lots of ideas in


- GHC's task list (Ticket query: status: !closed, type: task, order: priority)
- GHC's feature request list (Ticket query: status: !closed,
  type: feature+request, order: priority)

---


## Projects that should be within reach of a good undergraduate


- **Implement overlap and exhaustiveness checking for pattern matching**.  GHC's current overlap and exhaustiveness checker is old and inadequate.  Furthermore, it takes no account of GADTs and type families. See [\#595](http://gitlabghc.nibbler/ghc/ghc/issues/595) and [\#2395](http://gitlabghc.nibbler/ghc/ghc/issues/2395).  There's an excellent selection of background material:

  - [
    Warnings for pattern matching](http://pauillac.inria.fr/~maranget/papers/warn/warn.pdf) by Luc Maranget (JFP 17(3), 2007)
  - [
    Focusing on pattern matching](http://www.cs.cmu.edu/~neelk/pattern-popl09.pdf) by Neelakantan Krishnaswami (POPL 2009)
  - [
    Compiling pattern matching to good decision trees](http://pauillac.inria.fr/~maranget/papers/ml05e-maranget.pdf) by Luc Maranget, ML Workshop 2008

- **Improve parallel profiling tools**.  Starting with [
  ThreadScope](http://research.microsoft.com/en-us/projects/threadscope/), incorporate performance-counter events, visualise more runtime events, include source-code information in the profile.

- **Implement some low-level C-- optimisations**.  During 2011 we expect to have the new C-- code generation route in place, and that will open up new opportunities for doing classic compiler-course optimisations on the imperative C-- code.  There is more than routine stuff here, because we can use our [
  generic dataflow framework](http://research.microsoft.com/~simonpj/papers/c--) to do the heavy lifting.  Here are some [particular ideas for optimisations](back-end-notes) we'd like to implement.

---


## More ambitious or less-well-defined projects (PhD students / Interns)


### Programming environment and tools


- Maintaining an explicit call stack [ExplicitCallStack](explicit-call-stack)

### Turning GHC into a platform



Projects aimed at making GHC into a user-extensible plug-in platform, and less of a monolithic compiler.


- **Allow much finer and more modular control over the way in which rewrite rules and inlining directives are ordered**.  See this [
  email thread](http://www.haskell.org/pipermail/haskell-cafe/2008-January/038196.html)


  


### Types


- **Allow unboxed tuples as function arguments**.   Currently unboxed tuples are second class; fixing this would be a nice simplification.

- **Extend kinds beyond \* and k1-\>k2**.  With GADTs etc we clearly want to have kinds like `Nat`, so that advanced hackery at the type level can be done in a typed language; currently it's all effectively untyped.  A neat approach would be to re-use any data type declaration as a kind declaration.

- **Extensible constraint domains**.  Andrew Kennedy shows how to incorporate [
  dimensional analysis](http://research.microsoft.com/~akenn/units/index.html) into an ML-like type system.  Maybe we could do an extensible version of this, so that it wasn't restricted to dimensions.  Integer arithmetic is another obvious domain.  

### Runtime system


- Incremental or concurrent GC, for reducing pause-times.  Perhaps via implementing mark-region GC in the old generation.

### Packages


- A package API tool.

- fixed package ABIs for binary-upgradable packages.

---


# Just Hacking



Projects for people who want a decent-sized hacking project, with less research content.


## Compiler


- <table><tr><th>[\#602](http://gitlabghc.nibbler/ghc/ghc/issues/602)</th>
  <td>Warning Suppression</td></tr></table>


- Whole-program dead-code detection (with `--make`).
- Whole-program overloading elimination (with `--make`).
- Evolve a better ordering for the optimisation passes using [
  Acovea](http://www.coyotegulch.com/products/acovea/).

## Build system


- <table><tr><th>[\#989](http://gitlabghc.nibbler/ghc/ghc/issues/989)</th>
  <td>Build GHC on Windows using Microsoft toolchain</td></tr></table>


## Tools


- Update/improve [
  Visual Haskell](http://www.haskell.org/visualhaskell) to use the (free) [
  Visual Studio Shell](http://msdn2.microsoft.com/en-us/vsx2008/products/bb933751.aspx).
