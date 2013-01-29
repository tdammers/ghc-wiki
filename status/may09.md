# GHC Status May 2009



The last six months have been busy ones for GHC.  


## The GHC 6.10 branch



We finally released GHC 6.10.1 on 4 November 2008, with a raft of new
features we discussed in the October 2008 status report.



A little over five months later we released GHC 6.10.2, with more than 200 new
patches fixing more than 100 tickets raised against 6.10.1.  We hoped that'd be
it for the 6.10 branch, but we slipped up and 6.10.2 contained a couple
of annoying regresssions (concerning Control-C and editline). 
By the time you read this, GHC 6.10.3 (fixing these regressions) should
be out, after which we hope to shift all our attention to the 6.12 branch.


## The new build system



Our old multi-makefile build system had grown old, crufty, hard to 
understand.  And it didn't even work very well.  So we embarked
on a [plan to re-implement the build system](design/build-system).
Rather than impose the new system on everyone immediately, Ian and Simon (Marlow) 
did all the development on a branch, and invited others to give it a whirl.
Finally, on 25 April 2009, we went "live" on the HEAD. 



The new design is [extensively described](building) on the wiki.  It still
uses `make`, but it is now based on a [non-recursive make](building/architecture/idiom/non-recursive-make) strategy.  This means that dependency tracking is vastly more accurate than before,
so that if something *should* be built it *will* be built.



The new build system is also much less dependent on Cabal than it was before.
We now use Cabal only to read package metadata from the `<pkg>.cabal` file, 
and emit a bunch of Makefile bindings.  Everything else is done in `make`.
You can read more about the [design rationale](design/build-system) on the wiki.



We also advertised our [intent to switch to Git](design/version-control-system) as our
version control system (VCS).  We always planned to change the build system first, and only
then tackle the VCS.  Since then, there has been lots of activity on the Darcs front, so
it's not clear how high priority making this change is.  We'd welcome your opinion (email `ghc-devs@haskell.org`).


## The GHC 6.12 branch



The main list of new features in GHC 6.12 remains much the same as it was in our last status report.  Happily, there has been progress on all fronts.


### Parallel Performance



Simon Marlow has been working on improving performance for parallel programs, and there will be significant imporovements to be had in 6.12 compared to 6.10.  In particular


- There's an implementation of lock-free work-stealing queues, used for load-balancing of sparks and also
  in the parallel GC.  Initial work on this was done by Jost Berthold.

- The parallel GC itself has been tuned to retain locality in parallel programs.  Some speedups are
  dramatic.

- The overhead for running a spark is much lower, as sparks are now run in batches rather than creating
  a new thread for each one.  This makes it possible to take advantage of parallelism at a much finer
  granularity than before.

- There is optional "eager-blackholing", with the new `-feager-blackholing` flag, which can help eliminate
  duplicate computation in parallel programs.


Our recent ICFP submission [
Runtime Support for Multicore Haskell](http://ghcmutterings.wordpress.com/2009/03/03/new-paper-runtime-support-for-multicore-haskell/) describes all these in more detail, and gives extensive measurements.



Things aren't in their final state yet: for example, we still need to work on tuning the default flag settings to get good performance for more programs without any manual tweaking.  There are some larger possibilities on the horizon too, such as redesigning the garbage collector to support per-CPU independent GC, which will reduce the synchronization overheads of the current stop-the-world strategy.


### Parallel Profiling



GHC 6.12 will feature parallel profiling in the form of [
ThreadScope](http://raintown.org/?page_id=132), under development by Satnam Singh, Donnie Jones and Simon Marlow.  Support has been added to GHC for lightweight runtime tracing (work originally done by Donnie Jones), which is used by ThreadScope to generate profiles of the program's real-time execution behaviour.  This work is still in the very early stages, and there are many interesting directions we could take this in.


### Data Parallel Haskell



Data Parallel Haskell remains under very active development by Manuel Chakravarty, Gabriele Keller, Roman Leshchinskiy, and Simon Peyton Jones. The [current state of play](data-parallel) is documented on the wiki.  We also wrote a substantial paper [
Harnessing the multicores: nested data parallelism in Haskell](http://research.microsoft.com/~simonpj/papers/ndp) for FSTTCS 2008; you may find this paper a useful tutorial on the whole idea of nested data parallelism.



The system currently works well for small programs, such as computing a dot product or the product of a sparse matrix with a dense vector.  For such applications, the generated code is as close to hand written C code as GHC's current code generator enables us to be (i.e., within a factor of 2 or 3).  We ran three small benchmarks on an 8-core x86 server and on an 8-core UltraSPARC T2 server, from which we derived two comparative figures: [
a comparison between x86 and T2 on a memory-intensive benchmark (dot product)](http://justtesting.org/post/83014052/this-is-the-performance-of-a-dot-product-of-two) and [
a summary of the speedup of three benchmarks on x86 and T2.](http://justtesting.org/post/85103645/these-graphs-summarise-the-performance-of-data) Overall, we achieved good absolute performance and good scalability on the hardware we tested.



Our next step is to scale the implementation up to properly handle larger programs.  In particular, we are currently working on improving the interaction between vectorised code, the aggressively-inlined array library, and GHC's standard optimisation phases.  The current main obstacle is excessively long compile times, due to a temporary code explosion during optimisation.  Moreover, Gabriele started to work on integrating specialised support for regular multi-dimensional arrays into the existing framework for nested data parallelism.


### Type system improvements



The whole area of **GADTs, indexed type families, and associated types** remains in a ferment of development.  It's clear that type families are jolly useful: many people are using them even though they are only partially supported by GHC 6.10. (You might enjoy a programmers-eye-view tutorial [
Fun with type functions](http://research.microsoft.com/~simonpj/papers/assoc-types) that Oleg, Ken, and Simon wrote in April 2009.) 



But these new features have made the type inference engine pretty complicated, and Simon PJ, Manuel Chakravarty, Tom Schrijvers, Dimitrios Vytiniotis, and Martin Sulzmann have been busy thinking about ways to make type inference simpler and more uniform. Our ICFP'08 paper [
Type checking with open type functions](http://research.microsoft.com/~simonpj/papers/assoc-types) was a first stab (which we subsequently managed to simplify quite a bit).  Our new paper (to be presented at ICFP'09) [
Complete and decidable type inference for GADTs](http://research.microsoft.com/~simonpj/papers/gadt) tackles a different part of the problem.  And we are not done yet; for example, our new inference framework is designed to smoothly accommodate Dimitrios's work on [
FPH: First class polymorphism for Haskell (ICFP'08)](http://research.microsoft.com/~simonpj/papers/boxy/).   


### Other developments


- Max Bolingbroke has revised and simplified his **Dynamically Loaded Plugins** summer of code project, and we (continue to) plan to merge it into 6.12.  Part of this is already merged: a new, modular system for [user-defined '''annotations'''](annotations), rather like Java or C\# attributes.  These attributes are persisted into interface files, can be examined and created by plugins, or by GHC API clients.

- John Dias has continued work on **rewriting GHC's backend**.  You can find an [overview of the new architecture](commentary/compiler/new-code-gen-pipeline) on the wiki.  He and Norman and Simon wrote [
  Dataflow optimisation made simple](http://research.microsoft.com/~simonpj/papers/c--), a paper about the dataflow optimisation framework that the new back end embodies.  Needless to say, the act of writing the paper has made us re-design the framework, so at the time of writing it still isn't on GHC's main compilation path.  But it will be.

- **Shared Libraries**, are inching ever closer to being completed.  Duncan Coutts has taken up the reins and is pushing our shared library support towards a fully working state.  This project is supported by the Industrial Haskell Group.

- **Unicode text I/O** support is [
  at the testing stage](http://www.haskell.org/pipermail/glasgow-haskell-users/2009-February/016558.html), and should be merged in in time for 6.12.1.
