# GHC Status May 2011



GHC is still busy as ever. The GHC 7.0 branch has come and gone, and now that the branch has been closed we have finally made the long-planned switch from darcs to git. Meanwhile, we are busily working towards the 7.2 branch, and hope to make the 7.2.1 release in June. Some of the forthcoming highlights are:


- In the autumn, Dimitrios and Simon PJ implemented a completely
  new constraint solver for the type checker; we also complete an
  epic JFP paper describing how it works [
  http://research.microsoft.com/\~simonpj/papers/constraints/ OutsideIn](http://research.microsoft.com/~simonpj/papers/constraints/ OutsideIn).  The new
  solver is far more tractable and maintainable than the old type
  checker, and has fixed many outstanding problems.  We are still
  shaking out the last bugs, and we have some nifty ideas for improving
  performance.  Based on this new foundation, we are planning to
  develop the type system further, notably by adding a richer kind
  system along the lines of Conor McBride's SHE system [
  http://personal.cis.strath.ac.uk/\~conor/pub/she/ SHE](http://personal.cis.strath.ac.uk/~conor/pub/she/ SHE).

- GHC's intermediate language (which we call "Core") is a simple,
  explicitly-typed lambda in the style of System F.  Core is far,
  far simpler than Haskell (`CoreExpr` has only eight data
  constructors), so GHC can type-check Core very fast and reliably.
  In theory, such a typecheck is redundant (since the original
  Haskell program was typechecked), but in practice, typechecking
  Core is a very powerful internal consistency check on GHC itself:
  many compiler bugs generate type-incorrect Core. This consistency
  check is run by `-dcore-lint`.

>
>
> With the advent of GADTs and type families, the type system of the
> Core had to grow a little.  For a few
> years we have been using an extension of System F, called System
> FC, as described in our 2007 paper [
> http://research.microsoft.com/\~simonpj/papers/ext-f/ FC](http://research.microsoft.com/~simonpj/papers/ext-f/ FC).  However, the way that System FC
> was actually *implemented* in GHC's Core language was a bit unsatisfactory
> so, with help from **Brent Yorgey**, Simon PJ is busy re-engineering it. 
> In particular, FC has *coercion terms*, and these will now 
> be represented by their own data type `Coercion`, rather than being
> squeezed into `Type`.  Moreover, these coercion terms can get big, 
> so there's a new "coercion optimiser" to replace big coercions by
> equivalent smaller ones. All this is described in our new paper [
> NewFC](http://research.microsoft.com/~simonpj/papers/ext-f/).
> These changes will (finally) complete the type-family story by
> making so-called "equality superclasses" work for the first time in GHC 7.2.
>
>

- **José Pedro Magalhães** has nearly completed his implementation of the
  **derivable type classes** mechanism described in his 2010
  Haskell Symposium paper [
  http://www.dreixel.net/research/pdf/gdmh\_nocolor.pdf Derivable](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf Derivable) and elsewhere in the HCAR **link**.  It will be in GHC 7.2.

- **Edward Yang** has spearheaded a flurry of work on the new
  code generation backend (`-fuse-new-codegen`, the rewrite of
  the part of GHC that turns STG syntax into C--).  Hoopl is now
  fully part of GHC [
  http://research.microsoft.com/\~simonpj/papers/c--/ Hoopl](http://research.microsoft.com/~simonpj/papers/c--/ Hoopl), and the new path uses it extensively; we’ve
  ironed out most of the bugs in the backend; and now we’re
  working on new optimization passes and fixing inefficiencies to
  get the generated code as good (or better) than the old code
  generator.  We’re still not at the point where the new code
  generator will generate better code, but we’re pretty close!
  Stay tuned.

- Simon Marlow and **Ryan Newton** have developed a neat new library for deterministic parallel progarmming in Haskell; read their ICFP submission [
  http://research.microsoft.com/\~simonpj/papers/parallel/ DetPar](http://research.microsoft.com/~simonpj/papers/parallel/ DetPar). The model is monadic and has explicit control over granularity, but allows dynamic construction of dataflow networks that are scheduled at runtime, while remaining deterministic and pure.

- **Simon Marlow** Has been busy implementing and benchmarking a new garbage collector.  GHC's current garbage collector is of the parallel "stop-the-world" variety, where to collect the heap all cores stop running the program and collect the heap in parallel.  The new collector is a "local heap" collector, in which each core has a private heap that can be collected independently of the other cores, meanwhile there is a shared global heap that is collected (much less frequently) by the usual parallel stop-the-world algorithm.  We have a paper describing the new design which has been accepted at ISMM'11 (and will be online shortly).  The results are mixed; while on average performance improves with the new collector for parallel programs, the improvements are not dramatic (at least up to 24 cores).  The new collector is significantly more complex than GHC's current collector.  Hence we do not plan to merge it into the mainline yet, but will maintain it on a git branch for the time being, while we continue to experiment with and tune it.  Some improvements from the branch that were independent of the new GC algorithm have already been merged into the mainline, so 7.2.1 will see some small improvements in GC performance and stats reporting.


 


- **Simon Marlow** has implemented a chunked stack representation, which should improve the performance of programs that need large stacks.  See the [
  http://hackage.haskell.org/trac/ghc/blog/stack-chunks ChunkedStack](http://hackage.haskell.org/trac/ghc/blog/stack-chunks ChunkedStack).  This is already in the mainline and will be in the 7.2.1 release.


We are fortunate to have a growing team of people willing to roll up their
sleeves and help us with GHC.  Amongst those who have been active recently are:


- Mark Lentczner and Dan Knapp have been working on cross-compilation support
- Continued work on the new I/O manager by Johan Tibell.
- Various improvements and build fixes for OS X, from PHO, Greg Wright, Thorkil Naur and William Knop
- Solaris fixes from Karel Gardas and Christian Maeder
- Gentoo fixes (for SE Linux and x86 FreeBSD support) from Sergei Trofimovich
- Other FreeBSD fixes from Marco Silva
- Linux PowerPC fixes from Erik de Castro Lopo
- Objective C support has been added by Austin Seipp
- Documentation updates from Orphi
- Various improvements from Michal Terepeta
- General tidyups from Matthias Kilian
- Primop improvements from Daniel Peebles
- Some GHCi improvements from Vivian McPhail and Boris Lykah
- More GHCi debugger fixes from Pepe Iborra
- LLVM development continues with David Terei
- Many people have given git help to those of us new to git


At GHC HQ we are having way too much fun; if you wait for us to
do something you have to wait a long time.  So don't wait; join in!


## Other developments



GHC continues to act as an incubator for interesting new language developments.
Here's a selection that we know about.


- **Jeff Epstein**, in collaboration with Andrew Black, has implemented a library that brings Erlang's programming model to Haskell programmers.  In particular, you can write a Haskell program that runs on a cluster of machines that do not share memory.  It is all based on a modest but powerful language extension that makes it possible for a programmer to work with "static" functions; that is, ones consisting of pure code with no free variables.  The paper that describes all this is called "Haskell for the cloud" [
  http://research.microsoft.com/\~simonpj/papers/parallel/ Cloud](http://research.microsoft.com/~simonpj/papers/parallel/ Cloud).

- **Max Bolingbroke** continues his PhD work on supercompilation, with a nice new paper [
  http://research.microsoft.com/\~simonpj/papers/supercompilation/ ImprovingSupercompilation](http://research.microsoft.com/~simonpj/papers/supercompilation/ ImprovingSupercompilation).  The plan is to make his supercompiler part of GHC, over the next year or so.


 


- **David Terei** at Stanford is busy implementing **[
  Safe Haskell](http://hackage.haskell.org/trac/ghc/wiki/SafeHaskell)**, a flag for GHC that will guarantee that your program has certain properties such as referential transparency and constructor access control, while still having the same semantics as it normally would. The flag basically allows you to trust the types of your program, giving you if you will a more 'pure' version of Haskell where 'unsafePerformIO' is outlawed, abstract data types are actually abstract and safety is provided by the compiler not the user. This is being done as part of a larger project by the [
  Stanford Secure Computing Systems](http://www.scs.stanford.edu/) group involving the use of dynamic information flow based security in Haskell to build a secure web framework that allows the inclusion of third party untrusted code.

- **Ranjit Jhala** at UC San Diego is working on implementing Liquid Types [
  http://goto.ucsd.edu/\~rjhala/liquid Liquid](http://goto.ucsd.edu/~rjhala/liquid Liquid) within GHC. The goal is to allow programmers to use lightweight refinement types to specify key invariants which can then be verified through a combination of type inference and SMT solving.

## The Parallel GHC Project



Microsoft Research is funding a 2-year project to develop the real-world use of parallel Haskell. The project is now underway with four industrial partners:


- Dragonfly (New Zealand)
- IIJ Innovation Institute Inc. (Japan)
- Los Alamos National Laboratory (USA)
- Willow Garage Inc. (USA)


with consulting and engineering support from Well-Typed. Each organisation is working on its own particular project making use of parallel Haskell. The overall goal is to demonstrate successful serious use of parallel Haskell, and along the way to apply engineering effort to any problems with the tools that the organisations might run into.



For more details, see the **link:**parallel GHC project entry, and the project home page [
http://www.haskell.org/haskellwiki/Parallel\_GHC\_Project ParallelGhcProject](http://www.haskell.org/haskellwiki/Parallel_GHC_Project ParallelGhcProject)


## Data Parallel Haskell



The main user-visible development concerning data-parallel programming with GHC since the last status report is the release of our library for regular, multi-dimensional, shape-polymorphic arrays: [
http://hackage.haskell.org/package/repa Repa](http://hackage.haskell.org/package/repa Repa).  The current release on Hackage performs well with GHC 7.0.3 and already includes Ben's recent work on high-performance stencil-based convolutions — see also the draft paper [
Stencil](http://www.cse.unsw.edu.au/~benl/papers/stencil/stencil-icfp2011-sub.pdf) and Ben's screencast [
http://code.ouroborus.net/beholder/video/Edge480.mov EdgeDetect](http://code.ouroborus.net/beholder/video/Edge480.mov EdgeDetect) of a real-time edge detection application, written in Objective-C and Haskell, using the new Repa library.



We have pushed back the release of a stable version of the main DPH libraries again.  They are now scheduled to be released with the forthcoming GHC 7.2.


## Bibliography


- \[ChunkedStack\] *An overhaul of stack management, and some performance improvements*, Simon Marlow, blog post, Dec2010, [
  http://hackage.haskell.org/trac/ghc/blog/stack-chunks](http://hackage.haskell.org/trac/ghc/blog/stack-chunks) 

- \[Cloud\] *Haskell for the cloud*, Epstein, Black, Peyton Jones, submitted to ICFP 2011, [
  http://research.microsoft.com/\~simonpj/papers/parallel/](http://research.microsoft.com/~simonpj/papers/parallel/)

- \[Derivable\] *A generic deriving mechanism for Haskell*, Magalhães, Dijkstra, Jeuring and Löh, Haskell Symposium 2010, [
  http://www.dreixel.net/research/pdf/gdmh\_nocolor.pdf](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf).

- \[DetPar\] *A monad for deterministic parallelism*, Marlow, Newton, and Peyton Jones, submitted to ICFP 2011, [
  http://research.microsoft.com/\~simonpj/papers/parallel/](http://research.microsoft.com/~simonpj/papers/parallel/)

- \[EdgeDetect\] *Edge-detection video*, [
  http://code.ouroborus.net/beholder/video/Edge480.mov](http://code.ouroborus.net/beholder/video/Edge480.mov)

- \[FC\] *System F with type equality coercions*, Sulzmann, Chakravarty, Peyton Jones, TLDI 2007, [
  http://research.microsoft.com/\~simonpj/papers/ext-f/](http://research.microsoft.com/~simonpj/papers/ext-f/)

- \[Hoopl\] *A modular, reusable library for dataflow analysis and transformation*, Dias, Ramsey, and Peyton Jones, Haskell Symposium 2010, [
  http://research.microsoft.com/\~simonpj/papers/c--/](http://research.microsoft.com/~simonpj/papers/c--/)

- \[ImprovingSupercompilation\] *Improving supercompilation: tag-bags, rollback, speculation, normalisation, and generalisation*, Bolingbroke and Peyton Jones, submitted to ICFP 2011, [
  http://research.microsoft.com/\~simonpj/papers/supercompilation/](http://research.microsoft.com/~simonpj/papers/supercompilation/)

- \[Liquid\] *Liquid types*, Ranjit Jhala, [
  http://goto.ucsd.edu/\~rjhala/liquid](http://goto.ucsd.edu/~rjhala/liquid) Liquid

- \[NewFC\] *Practical aspects of evidence-based compilation in System FC*, Vytiniotis and Peyton Jones, submitted to ICFP 2011, [
  http://research.microsoft.com/\~simonpj/papers/ext-f/](http://research.microsoft.com/~simonpj/papers/ext-f/)

- \[OutsideIn\] *Modular type inference iwth local assumptions*, Vytiniotis, Peyton Jones, Schrijvers, and Sulzmann, Journal of Functional Programming (to appear), [
  http://research.microsoft.com/\~simonpj/papers/constraints/](http://research.microsoft.com/~simonpj/papers/constraints/)

- \[ParallelGhcProject\] *The Parallel GHC Project home page*, [
  http://www.haskell.org/haskellwiki/Parallel\_GHC\_Project](http://www.haskell.org/haskellwiki/Parallel_GHC_Project)

- \[Repa\] *Regular, shape-polymorphic parallel arrays in Haskell*, Keller, Chakravarty, Leshchinskiy, Peyton Jones, and Lippmeier, ICFP 2010.  Paper: [
  http://research.microsoft.com/\~simonpj/papers/ndp/](http://research.microsoft.com/~simonpj/papers/ndp/), Hackage package: [
  http://hackage.haskell.org/package/repa](http://hackage.haskell.org/package/repa)

- \[SHE\] *The Strathclyde Haskell Enhancement*, Conor McBride, 2010, [
  http://personal.cis.strath.ac.uk/\~conor/pub/she/](http://personal.cis.strath.ac.uk/~conor/pub/she/)

- \[Stencil\] *Efficient Parallel Stencil Convolution in Haskell*, Lippmeier et al., [
  http://www.cse.unsw.edu.au/\~benl/papers/stencil/stencil-icfp2011-sub.pdf](http://www.cse.unsw.edu.au/~benl/papers/stencil/stencil-icfp2011-sub.pdf)
