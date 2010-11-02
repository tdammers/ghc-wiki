# GHC Status October 2010



GHC is humming along.  We are currently deep into the release cycle for GHC 7.0.  We have finally bumped the major version number, because GHC 7.0 has quite a bit of new stuff


- As long promised, Simon PJ and Dimitrios have spent a good chunk of the summer doing a **complete rewrite of the constraint solver in the type inference engine**.  Because of GHC's myriad type-system extensions, especially GADTs and type families, the old engine had begun to resemble the final stages of a game of Jenga.  It was a delicately-balanced pile of blocks that lived in constant danger of complete collapse, and had become extremely different to modify (or even to understand).  The new inference engine is much more modular and robust; it is described in detail in our paper [
  http://haskell.org/haskellwiki/Simonpj/Talk:OutsideIn OutsideIn](http://haskell.org/haskellwiki/Simonpj/Talk:OutsideIn OutsideIn).  A blog post describes some consequential changes to let generalisation [
  http://hackage.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7 LetGen](http://hackage.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7 LetGen).

>
>
> As a result we have closed dozens of open type inference bugs, especially related to GADTs and type families.
>
>

- There is a new, **robust implementation of INLINE pragmas**, that behaves much more intuitively. GHC now captures the original RHS of an INLINE function, and keeps it more-or-less pristine, ready to inline at call sites.  Separately, the original RHS is optimised in the usual way.  Suppose you say

  ```wiki
  {-# INLINE f #-}
  f x = ...blah...

  g1 y = f y + 1
  g2 ys = map f ys
  ```

  Here, `f` will be inlined into `g1` as you'd expect, but obviously not into `g2` (since it's not applied to anything).  However `f`'s right hand side will be optimised (separately from the copy retained for inlining) so that the call from `g2` runs optimised code.

>
>
> There's a raft of other small changes to the optimisation pipeline too.  The net effect can be dramatic: Bryan O'Sullivan reports some five-fold (!) improvements in his text-equality functions, and concludes "The difference between 6.12 and 7 is so dramatic, there's a strong temptation for me to say 'wait for 7!' to people who report weaker than desired performance." [
> http://www.serpentine.com/blog/2010/10/19/a-brief-tale-of-faster-equality/ Bryan](http://www.serpentine.com/blog/2010/10/19/a-brief-tale-of-faster-equality/ Bryan)
>
>

- David Terei implemented a new **back end for GHC using LLVM**. In certain situations using the LLVM backend can give fairly substantial performance improvements to your code, particularly if you're using the Vector libraries, DPH or making heavy use of fusion. In the general case it should give as good performance or slightly better than GHC's native code generator and C backend. You can use it through the '-fllvm' compiler flag. More details of the backend can be found in David's and Manuel Chakravarty's Haskell Symposium paper [
  http://www.cse.unsw.edu.au/\~davidt/downloads/ghc-llvm-hs10.pdf Llvm](http://www.cse.unsw.edu.au/~davidt/downloads/ghc-llvm-hs10.pdf Llvm).

- Bryan O’Sullivan and Johan Tibell and implemented a new, **highly-concurrent I/O manager**. GHC now supports over a hundred thousand open I/O connections. The new I/O manager defines a separate backend per operating system, using the most efficient system calls for that particular operating system (e.g. `epoll` on Linux.) This means that GHC can now be used to implement servers that make use of e.g. HTTP long polling, where the server needs to handle a large number of open idle connections.

- In joint work with Phil Trinder and his colleagues at Herriot Watt, Simon M designed implemented a new **parallel strategies library**, described in their 2010 Haskell Symposium paper [
  http://www.haskell.org/\~simonmar/papers/strategies.pdf Seq](http://www.haskell.org/~simonmar/papers/strategies.pdf Seq).

- As reported in the previous status update, **the runtime system has undergone substantial changes** to the implementation of lazy evaluation in parallel, particularly in the way that threads block and wake up again.  Certain benchmarks show significant improvements, and some cases of wildly unpredictable behaviour when using large numbers of threads are now much more consistent.

- The **API for asynchronous exceptions** has had a redesign.  Previously the combinators `block` and `unblock` were used to prevent asynchronous exceptions from striking during critical sections, but these had some serious disadvantages, particularly a lack of modularity where a library function could unblock asynchronous exceptions despite a prevailing `block`.  The new API closes this loophole, and also changes the terminology: preventing asynchronous exceptions is now called "masking", and the new combinator is `mask`.  See the documentation for the new API in `Control.Exception` for more details.


We are fortunate to have a growing team of people willing to roll up their
sleeves and help us with GHC.  Amongst those who have got involved recently are:


- Daniel Fischer, who worked on improving the performance of the numeric libraries
- Milan Straka, for great work improving the performance of the widely-used containers package [
  http://research.microsoft.com/\~simonpj/papers/containers/containers.pdf Containers](http://research.microsoft.com/~simonpj/papers/containers/containers.pdf Containers)
- Greg Wright is leading a strike team to make GHC work better on Macs, and has fixed the RTS linker so that GHCi will now work in 64-bit mode on OS X.
- Evan Laforge who has taken on some of the long-standing issues with the Mac installer
- Sam Anklesaria implemented full import syntax for GHCi, and rebindable syntax for conditionals
- PHO, who improved the OS X support
- Sergei Trofimovich, who has fixed GHC on some less common Linux platforms
- Marco Túlio Gontijo e Silva, who has been working on the RTS
- Matthias Kilian, who has been working on \*BSD support
- Dave Peixotto, who has improved the PAPI support
- Edward Z. Yang, who has implemented interruptible FFI calls
- Reiner Pope, who added view patterns to Template Haskell
- Gabor Pali, who added thread affinity support for FreeBSD
- Bas van Dijk has been improving the exceptions API


At GHC HQ we are having way too much fun; if you wait for us to
do something you have to wait a long time.  So don't wait; join in!  


## Language developments, especially types



GHC continues to act as an incubator for interesting new language developments.
Here's a selection that we know about.


- Pedro Magalhaes is implementing the **derivable type classes** mechanism described in his 2010 Haskell Symposium paper [
  http://www.dreixel.net/research/pdf/gdmh\_nocolor.pdf Derivable](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf Derivable).  I plan for this to replace GHC's current derivable-type-class mechanism, which has a poor power-to-weight ratio and is little used.

- Stephanie Weirich and Steve Zdancewic had a great sabbatical year at Cambridge.  One of the things we worked on, with Brent Yorgey who came as an intern, was to close the embarrassing hole in the type system concerning **newtype deriving** (see Trac bug [\#1496](http://gitlabghc.nibbler/ghc/ghc/issues/1496)).  I have delayed fixing until I could figure out a Decent Solution, but now we know; see our 2011 POPL paper [
  http://www.cis.upenn.edu/\~sweirich/newtypes.pdf Newtype](http://www.cis.upenn.edu/~sweirich/newtypes.pdf Newtype).  Brent is working on some infrastructal changes to GHC's Core language, and then we'll be ready to tackle the main issue.

- Next after that is a mechanism for **promoting types to become kinds**, and data constructors to become types, so that you can do *typed* functional programming at the type level.  Conor McBride's SHE prototype is the inspiration here [
  http://personal.cis.strath.ac.uk/\~conor/pub/she/ SHE](http://personal.cis.strath.ac.uk/~conor/pub/she/ SHE).  Currently it is, embarrassingly, essentially untyped.  

- **Template Haskell** seems to be increasingly widely used.  Simon PJ has written a proposal for a raft of improvements, which we plan to implement in the new year [
  http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal TemplateHaskell](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal TemplateHaskell).

- Iavor Diatchki plans to add **numeric types**, so that you can have a type like `Bus 8`, and do simple arithmetic at the type level.  You can encode this stuff, but it's easier to use and more powerful to do it directly.

- David Mazieres at Stanford wants to implement **Safe Haskell**, a flag for GHC that will guarantee that your program does not use `unsafePerformIO`, foreign calls, RULES, and other stuff stuff.  


7.0 also has support for the Haskell 2010 standard, and the libraries that it specifies.


## Packages and the runtime system


- Simon Marlow is working on a new garbage collector that is designed to improve scaling of parallel programs beyond small numbers of cores, by allowing each processor core to collect its own local heap independently of the other cores.  Some encouraging preliminary results were reported in a [
  blog post](http://hackage.haskell.org/trac/ghc/blog/2010/9#new-gc-preview).  Work on this continues; the complexity of the system and the number of interacting design choices means that achieving an implementation that works well in a broad variety of situations is proving to be quite a challenge.

- The "new back end" is still under construction.  This is a rewrite of the part of GHC that turns STG syntax into C--, i.e. the bit between the Core optimisation passes and the native code generator.  The rewrite is based on [
  http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/dfopt.pdf Hoopl](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/dfopt.pdf Hoopl), a data-flow optimisation framework.  Ultimately this rewrite should enable better code generation.  The new code generator is already in GHC, but turned off by default; you get it with the flag `-fuse-new-codegen`.  Don't expect to get better code with this flag yet!

## The Parallel Haskell Project



Microsoft Research is funding a 2-year project to develop the real-world use of parallel Haskell. The project has recently kicked off with four industrial partners, with consulting and engineering support from Well-Typed. Each organisation is working on its own particular project making use of parallel Haskell. The overall goal is to demonstrate successful serious use of parallel Haskell, and along the way to apply engineering effort to any problems with the tools that the organisations might run into.



We will shortly be announcing more details about the partner organisations and their projects. For the most part the projects are scientific and focus on single-node SMP systems, though one of the partners is working on network servers and another partner is very interested in clusters. In collaboration with Bernie Pope, the first tangible results from the project will be a new MPI binding, which will appear on hackage shortly.



Progress on the project will be reported to the community. Since there are now multiple groups in the community that are working on parallelism, the plan is to establish a parallel Haskell website and mailing list to provide visibility into the various efforts and to encourage collaboration.


## Data Parallel Haskell



Since the last report, we have continued to improve support for nested parallel divide-and-conquer algorithms.  We started with [
http://darcs.haskell.org/packages/dph/dph-examples/spectral/QuickHull/dph/QuickHullVect.hs QuickHull](http://darcs.haskell.org/packages/dph/dph-examples/spectral/QuickHull/dph/QuickHullVect.hs QuickHull) and are now working on an implementation of the [
http://darcs.haskell.org/packages/dph/dph-examples/real/BarnesHut/Solver/NestedBH/Solver.hs Barnes-Hut](http://darcs.haskell.org/packages/dph/dph-examples/real/BarnesHut/Solver/NestedBH/Solver.hs Barnes-Hut) *n*-body algorithm.  The latter is not only significantly more complex, but also requires the vectorisation of recursive tree data-structures, going well beyond the capabilities of conventional parallel-array languages.  In time for the stable branch of GHC 7.0, we replaced the old, per-core sequential array infrastructure (which was part of the sub-package `dph-prim-seq`) by  the [
http://hackage.haskell.org/package/vector vector package](http://hackage.haskell.org/package/vector vector package) — vector started its life as a next-generation spin off of `dph-prim-seq`, but now enjoys significant popularity independent of DPH. 



The new handling of INLINE pragmas as well as other changes to the Simplifier improved the stability of DPH optimisations (and in particular, array stream fusion) substantially.  However, the current candidate for GHC 7.0.1 still contains some performance regressions that affect the DPH and [
http://hackage.haskell.org/package/repa Repa](http://hackage.haskell.org/package/repa Repa) libraries and to avoid holding up the 7.0.1 release, we decided to push fixing these regressions to GHC 7.0.2.  More precisely, we are planning a release of DPH and Repa that is suitable for use with GHC 7.0 for the end of the year, to coincide with the release of GHC 7.0.2.  From GHC 7.0 onwards, the library component of DPH will be shipped separately from GHC itself and will be available to download and install from Hackage as for other libraries.



To catch DPH performance regressions more quickly in the future, Ben Lippmeier implemented a performance regression testsuite that we run nightly on the HEAD.  The results can be enjoyed on the GHC developer mailing list.



Sadly, Roman Leshchinskiy has given up his full-time engagement with DPH to advance the use of Haskell in the financial industry.  We are looking forward to collaborating remotely with him.


## Installers



The GHC installers have also received some attention for this release.



The Windows installer includes a much more up-to-date copy of the MinGW system, which in particular fixes a couple of issues on Windows 7. Thanks to Claus Reinke, the installer also allows more control over the registry associations etc.



Meanwhile, the Mac OS X installer has received some attention from Evan Laforge. Most notably, it is now possible to install different versions of GHC side-by-side.


## Bibliography


- \[Bryan\] "A brief tale of faster equality", Bryan O'Sullivan blog post, Oct 2010, [
  http://www.serpentine.com/blog/2010/10/19/a-brief-tale-of-faster-equality/](http://www.serpentine.com/blog/2010/10/19/a-brief-tale-of-faster-equality/).

- \[Containers\] "The performance of the Haskell containers package", Straka, Haskell Symposium 2010, [
  http://research.microsoft.com/\~simonpj/papers/containers/containers.pdf](http://research.microsoft.com/~simonpj/papers/containers/containers.pdf)

- \[Derivable\] "A generic deriving mechanism for Haskell", Magalhães, Dijkstra, Jeuring and Löh, Haskell Symposium 2010, [
  http://www.dreixel.net/research/pdf/gdmh\_nocolor.pdf](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf).

- \[LetGen\] "Let generalisation in GHC 7.0", Peyton Jones, blog post Sept 2010, [
  http://hackage.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7](http://hackage.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7)

- \[Newtype\] "Generative Type Abstraction and Type-level Computation", Weirich, Zdancewic, Vytiniotis, and Peyton Jones, POPL 2010, [
  http://www.cis.upenn.edu/\~sweirich/newtypes.pdf](http://www.cis.upenn.edu/~sweirich/newtypes.pdf)

- \[Llvm\] "An LLVM Backend for GHC", Terei and Chakravarty, Haskell Symposium 2010, [
  http://www.cse.unsw.edu.au/\~davidt/downloads/ghc-llvm-hs10.pdf](http://www.cse.unsw.edu.au/~davidt/downloads/ghc-llvm-hs10.pdf)

- \[Seq\] "Seq no more", Marlow, Maier, Trinder, Loidl, and Aswad, Haskell Symposium 2010, [
  http://www.haskell.org/\~simonmar/papers/strategies.pdf](http://www.haskell.org/~simonmar/papers/strategies.pdf)

- \[SHE\] The Strathclyde Haskell Enhancement, Conor McBride, 2010, [
  http://personal.cis.strath.ac.uk/\~conor/pub/she/](http://personal.cis.strath.ac.uk/~conor/pub/she/)

- \[TemplateHaskell\] New directions for Template Haskell, Peyton Jones, blog post October 2010, [
  http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)

- \[Hoopl\] Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation, [
  http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/dfopt.pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/dfopt.pdf) 
