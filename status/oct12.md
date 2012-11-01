# GHC Status October 2012



We made a bug-fix release of GHC 7.4.2 in June, and a completely new release of GHC 7.6 in August.  As well as the usual raft of general improvements, GHC 7.6 included some new features:


- Multi-way if, and `\case`.
- Kind polymorphism and data kinds \[7\].
- Deferred type errors \[6\].
- Improved support for generic programming \[8\].
- The ability to change *at runtime* the number of processors running Haskell threads.
- The first supported GHC for 64-bit Windows.
- Type-level literal symbols.


We expect to do a 7.6.2 release quite soon, and a 7.8.1 release in a few months' time.



Here is what we have been up to in the last six months:


- **Kind polymorphism and data kinds** is a major new feature
  of GHC 7.6. It's
  described in "Giving Haskell a promotion" \[7\], and has already
  been used in interesting ways ("The Right Kind of Generic
  Programming" \[8\], "Dependently Typed Programming with Singletons" \[9\]).  Leading up to the GHC 7.6 release
  Simon PJ has been working hard on making kind polymorphism work
  properly, which was a lot more work than he anticipated. 

>
>
> There is plenty more to do here, such as exploiting kind polymorphism to make a better `Typeable` class.
>
>

- **Type holes**. Thijs Alkemade and Sean Leather have been working on another variant of deferred error messages, that would allow you to write a program that contains as-yet-unwritten sub-terms, or "holes" and have GHC report a fairly precise type for the hole. The HEAD now has an initial implementation (`-XTypeHoles`), and there are ongoing discussions about how to make it better still.  Details on their wiki page \[10\].

- **Data parallelism.** We are currently completely rewriting our implementation of *vectorisation avoidance* \[1\] in GHC's vectoriser. This leads to an overall much simpler and more robust vectoriser. In particular, it will be more liberal in allowing scalar subcomputations imported from modules compiled without vectorisation (such as the standard Prelude). This should finally enable us to get rid of the specialised, mini-Prelude in the DPH libraries.

>
>
> After having solved the problem of obtaining asymptotically work-efficient vectorisation \[2\], we are now turning to improving the constants in the DPH libraries, and in particular, to achieve more reliable fusion in the presence of segmented operations, folds, and parallelism. Ben Lippmeier has a few exciting ideas on major improvements in that direction that we will discuss in more detail once we have conducted more experiments. We plan to finish the new vectorisation-avoidance infrastructure in time for GHC 7.8, but the new fusion system will likely not be ready in time for that release.
>
>

>
>
> Moreover, Trevor McDonell has made good progress in devising a novel fusion system for the embedded Accelerate GPU language. We hope to be able to release it around the same time as GHC 7.8.
>
>

- **Overlapping type family instances.** Richard Eisenberg is close to finishing an implementation of overlapping type family instances. The overlap mechanism is distinct from overlapping type class instances, as the programmer has to give an explicit ordering to the overlapping instances. More information can be found on the wiki page \[11\].

- **Dynamic libraries by default.** In GHC 7.8, it will be possible to build GHC in such a way that by default it will dynamically link against Haskell libraries, rather than statically linking as it does now. As well as smaller binary sizes, this has the big advantage that GHCi will be able to use the system linker to load libraries, rather than our own linker implementation. This will mean fewer GHCi bugs, and make it a lot easier to add GHCi support to other platforms. We plan to make this the default way of building GHC on as many platforms as possible.

- **The new code generator.** Several years since this project was started, the new code generator is finally working  \[14\], and is now switched on by default in `master`.  It will be in GHC 7.8.1.  From a user's perspective there should be very little difference, though some programs will be faster.

>
>
> There are three important improvements in the generated code.  One is that `let-no-escape` functions are now compiled much more efficiently: a recursive `let-no-escape` now turns into a real loop in C--.  The second improvement is that global registers (R1, R2, etc.) are now available for the register allocator to use within a function, provided they aren't in use for argument passing.  This means that there are more registers available for complex code sequences.  The third improvement is that we have a new sinking pass that replaces the old "mini-inliner" from the native code generator, and is capable of optimisations that the old pass couldn't do.
>
>

>
>
> Hand-written C-- code can now be written in a higher-level style with real function calls, and most of the hand-written C-- code in the RTS has been converted into the new style.  High-level C-- does not mention global registers such as R1 explicitly, nor does it manipulate the stack; all this is handled by the C-- code generator in GHC.  This is more robust and simpler, and means that we no longer need a special calling-convention for primops - they now use the same calling convention as ordinary Haskell functions.
>
>

>
>
> We're interested in hearing about both performance improvements and regressions due to the new code generator.
>
>

- **Improved floating point register allocation.** On x86-64 there are now six machine registers available for any mixture of floating-point types. Previously a maximum of four values of type Float and two values of type Double could simultaneously be kept in machine registers.

- **SIMD primitives.** The `simd` branch now supports passing SSE vector values in machine registers. We expect the `simd` branch to be merged in time for 7.8.

- **Type-nat solver.** Iavor S. Diatchki has been working on the type-checker to add support for discharging constraints involving arithmetic operations at the type-level.  This work is on the `type-nats` branch of GHC.   The basic support for common operations is fairly stable, and now it is in the testing phase.  The most externally visible changes to the solver are: experimental support for matching on type-level naturals, using an auxiliary type family \[12\], and the module GHC.TypeLits was refactored to make it compatible with Richard Eisenberg's `singletons` library \[13\].  Next, we plan to work on integration with the `master` branch, and experimental support for the inverse operations of what's currently in the solver (i.e., (-), (/), Log, Root).


As always there is far more to do than we can handle, and there is loads of space for people to contribute.  Do join us!



\[1\] Vectorisation avoidance, Gabriele Keller et al, HS'12 [
http://www.cse.unsw.edu.au/\~chak/papers/KCLLP12.html](http://www.cse.unsw.edu.au/~chak/papers/KCLLP12.html) 

\[2\] Work-efficient higher-order vectorisation, Ben Lippmeier et al, ICFP'12 [
http://www.cse.unsw.edu.au/\~chak/papers/LCKLP12.html](http://www.cse.unsw.edu.au/~chak/papers/LCKLP12.html) 

\[6\] Equality proofs and deferred type errors, Dimitrios Vytiniotis et al, ICFP'12, [
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 

\[7\] Givng Haskell a promotion, Brent Yorgey et al, TLDI'12 [
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 

\[8\] The Right Kind of Generic Programming, José Pedro Magalhães, WGP'12 [
http://dreixel.net/research/pdf/trkgp.pdf](http://dreixel.net/research/pdf/trkgp.pdf) 

\[9\] Dependently typed programming with singletons, Richard Eisenberg et al, HS'12 [
http://www.cis.upenn.edu/\~eir/pubs.html](http://www.cis.upenn.edu/~eir/pubs.html)  

\[10\] Holes in GHC:  [
http://hackage.haskell.org/trac/ghc/wiki/Holes](http://hackage.haskell.org/trac/ghc/wiki/Holes) 

\[11\] Overlapping type family instances: [
http://hackage.haskell.org/trac/ghc/wiki/NewAxioms](http://hackage.haskell.org/trac/ghc/wiki/NewAxioms) 

\[12\] Matching on type nats: [
http://hackage.haskell.org/trac/ghc/wiki/TypeNats/MatchingOnNats](http://hackage.haskell.org/trac/ghc/wiki/TypeNats/MatchingOnNats) 

\[13\] Singletons and kinds: [
http://hackage.haskell.org/trac/ghc/wiki/TypeNats/SingletonsAndKinds](http://hackage.haskell.org/trac/ghc/wiki/TypeNats/SingletonsAndKinds) 

\[14\] The new codegen is nearly ready to go live [
http://hackage.haskell.org/trac/ghc/blog/newcg-update](http://hackage.haskell.org/trac/ghc/blog/newcg-update) 


