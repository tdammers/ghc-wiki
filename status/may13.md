# GHC Status Report May 2013



The big event in late 2012 was the news of Simon Marlow's move to Facebook.  Simon is the absolute master of huge tracts of GHC, including especially its runtime system, garbage collection, code generation, and support for parallelism. His contribution to GHC is a massive one, and this makes a good occasion for us to acknowledge it: **thank you Simon**!



Simon isn't going to disappear, of course, but he'll have less time to work on GHC than before. That means that everyone else, including  you, gentle reader, has new opportunities to contribute to the huge shared community endeavour that we call GHC. 



As planned, we made another minor release 7.6.2 from the 7.6 branch in January 2013. This included only bug and performance fixes; no new features were added.
We plan to put out a new major release 7.8.1 soon after ICFP 2013, including
some significant changes described below.



There remains more to do than we will ever have time for, so please do come and join in the fun!


## Source language and type system



Simon and Dimitrios overhauled the solver for type constraints,
once more.  No new functionality, but the result is smaller,
faster, and lacks many of the bugs of its predecessor.  You don't
want to know all the details, but it reminded us again of how
valuable it is that the constraint solve is now one coherent
piece of code, with a well-defined task, than being spread out in
bits and pieces across the type checker.



Meanwhile others have been adding new features.


- **Poly-kinded `Typeable`.**
  The `Typeable` class is now kind-polymorphic, meaning we can finally drop the boilerplate `TypeableN` classes.
  The new definition of `Typeable` is as follows:

  ```wiki
  class Typeable (a :: k) where typeRep :: proxy a -> TypeRep
  ```

>
>
> With this change comes the ability to derive `Typeable` instances for every user datatype, and even for type classes. This means user defined instances of `Typeable` are unnecessary. Furthermore, since ill-defined user instances can lead to runtime errors, they are now forbidden; the only way to get `Typeable` instances is by using the deriving mechanism. User-defined instances will be ignored, with a warning.
>
>

>
>
> Migrating to this new `Typeable` is easy. Code that only derived `Typeable` instances, and did not mention
> any of the `TypeableN` classes, should work as before. Code that mentioned the `TypeableN` classes should be
> adapted to replace these by the poly-kinded `Typeable` class. User-defined instances of `Typeable` should be
> replaced by derived instances.
>
>

>
>
> Additionally, a new compiler pragma `AutoDeriveTypeable` triggers automatic derivation of `Typeable` instances
> for all datatypes and classes defined in the module.
>
>

- **Type holes.** A GHC extension called "type holes" \[TYH\] was added by Thijs Alkemade, under supervision of Sean Leather and with help from Simon Peyton Jones. When GHC encounters a hole in an expression, written as "`_`", it will generate an error message describing the type that is needed in place of that hole. It gives some helpful additional information, such as the origins of the type variables in the hole's type and the local bindings that can be used. Together with `-fdefer-type-errors` this should make it easier to write code step-by-step, using hints from the compiler about the unfinished parts.

- **Rebindable list syntax.** A GHC extension called "overloaded lists" \[OL\] was added by Achim Krause, George Giorgidze, and colleagues. When this is turned on, the way GHC desugars explicit lists and lists in arithmetic sequence notation is changed. Instead of directly desugaring to built-in lists, a polymorphic witness function is used, similar to the desugaring of numeric literals. This allows for a more flexible use of list notations, supporting many different list-like types. In addition, the functions used in this desugaring process are completely rebindable.

- **Type level natural numbers**.  Iavor S. Diatchki has been working on a solver for equations involving type-level natural numbers.  This allows simplifying and reasoning about type-level terms involving
  arithmetic. Currently, the solver can evaluate equations and inequalities mentioning the type functions `(+)`, `(*)`, `(^)`, and `(<=)`.  The solver works pretty well when it can use evaluation to prove equalities (e.g., examples like `2 + 5 = x`, `2 + x = 5`).  There is also some support for taking advantage of the commutativity and associativity of `(+)`, `(*)`.   More experimental features include:  support for `(-)`, which, currently is implemented by desugaring to `(+)`;  the type-level function `FromNat1`, which has special support for working with natural number literals, and thus can be used to expose some of their inductive structure.  This work is currently on the `type-nats` branch, and the plan is to merge it into HEAD into the next few months.

- **Kinds without data** Trevor Elliott, Eric Mertens, and Iavor Diatchki have began implementing support for "data kind" declarations, described in more detail on the GHC wiki \[KD\]. The idea is to allow a new form of declaration that introduces a new kind, whose members are described by the (type) constructors in the declaration.   This is similar to promoting `data` declarations, except that no new value-level-constructors are declared, and it also allows the constructors to mention other kinds that do not have corresponding type-level representation (e.g., `*`).

- **Ordered overlapping type family instances.** Richard Eisenberg has implemented support for ordered overlapping type family instances, called *branched* instances. This allows type-level functions to use patterns in a similar way to term-level functions. For example:

  ```wiki
  type family Equals (x :: *) (y :: *) :: Bool
  type instance where
    Equals x x = True
    Equals x y = False
  ```

  Details can be found in the wiki page \[OTF\].

## Back end and code generation


- **The new code generator.** \[entry copied from Oct 2012 status report\] Several years since this project was started, the new code generator is finally working  \[CG\], and is now switched on by default in `master`.  It will be in GHC 7.8.1.  From a user's perspective there should be very little difference, though some programs will be faster.

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

- **Support for vector (SSE/AVX) instructions.** \[**Geoffrey Mainland**\] Support for SSE vector instructions, which permit 128-bit vectors, is now in HEAD. As part of this work, up to 6 arguments of type Double, Float, or vector can be passed in registers. Previously only 4 Float and 2 Double arguments could be passed in registers. AVX support will be added soon pending a refactoring of the code that implements vector primops.

## Data Parallel Haskell


- **Vectorisation Avoidance**. Gabriele Keller and Manuel Chakravarty have extended the DPH vectoriser with an analysis that determines when expressions cannot profitably be vectorised. Vectorisation avoidance improves compile times for DPH programs, as well as simplifying the handling of vectorised primitive operations. This work is now complete and will be in GHC 7.8. 

- **New Fusion Framework**.  Ben Lippmeier has been waging a protracted battle with the problem of array fusion. Absolute performance in DPH is critically dependent on a good array fusion system, but existing methods cannot properly fuse the code produced by the DPH vectoriser. An important case is when a produced array is consumed by multiple consumers. In vectorised code this is very common, but none of the "short cut" array fusion approaches can handle it -- eg stream fusion used in Data.Vector, delayed array fusion in Repa, build/foldr fusion etc. The good news is that we've found a solution that handles this case and others, based on Richard Waters's series expressions, and are now working on an implementation. The new fusion system is embodied by a GHC plugin that performs a custom core-to-core transformation, and some added support to the existing Repa library. We're pushing to get the first version working for a paper at the upcoming Haskell Symposium.

## A faster I/O manager



Andreas Voellmy performed a significant reworking of the IO manager to improve multicore scaling and sequential speed.  The most significant problems of the old IO manager were (1) severe contention (under some workloads) on a single MVar holding the table of callbacks, (2) invoking a callback typically requires messaging across capabilities, (3) polling for ready files performs an OS context switch, causing excessive context switching.  These problems contribute greatly to the response time of servers written in Haskell.



The redesigned IO manager addresses these problems in the following ways. We replace the single MVar for the callback table with a simple concurrent hash table, allowing for more concurrent registrations and callbacks. We use one IO manager service thread per capability, each with its own callback table and with the service thread for a given capability serving the waiting Haskell threads that were running (and will be woken up) on that capability. This further reduces contention on callback tables, ensures that notifying a thread is typically done without cross-capability messaging and allows the work of polling and notifying threads to be parallelized across cores.  To reduce context switching, we modify the service loops to first poll without waiting, which can be done without releasing the HEC (which would typically incur an OS context switch). 



The new IO manager also takes advantage of the edge-triggered and one-shot modes of epoll on Linux to achieve further performance improvements on Linux.



These changes result in substantial performance improvements in some applications. In particular, we implemented a minimal web server and found that performance with the new "parallel" IO manager improved by a factor of 19 versus the old IO manager;
with the old IO manager, our server achieved a peak performance of
roughly 45,000 http requests per second using 8 cores (performance
degraded after 8 cores), while the same server using the parallel IO
manager serves 860,000 requests/sec using 18 cores \[PIO\].  We have
measured similar improvements in the response time of servers written
in Haskell.



Kazu Yamamoto contributed greatly to the project by implementing the redesign for BSD-based systems using kqueue and by improving the code in order to bring it up to GHC's standards. In addition, Bryan O'Sullivan and Johan Tibell provided critical guidance and reviews.


## Dynamic linking



Ian Lynagh has changed GHCi to use dynamic libraries rather than static libraries. This means that we are now able to use the system linker to load packages, rather than having to implement our own linker. From the user's point of view, that means that a number of long-standing bugs in GHCi will be fixed, and it also reduces the amount of work needed to get a fully functional GHC port to a new platform. Currently, on Windows GHCi still uses static libraries, but we hope to have dynamic libraries working on Windows too by the time we release.


## Cross compilation



Three connected projects concerned cross-compilation


- **Registerised ARM support** added using David Terei's LLVM compiler back end with Stephen Blackheath doing an initial ARMv5 version and LLVM patch and Karel Gardas working on floating point support, ARMv7 compatibility and LLVM headaches. Ben Gamari did work on the runtime linker for ARM.

- **General cross-compiling** with much work by Stephen Blackheath and Gabor Greif (though many others have worked on this).

- **A cross-compiler for Apple iOS** \[IOS\]. iOS-specific parts were mostly Stephen Blackheath with Luke Iannini on the Cabal patch, testing and supporting infrastructure, also with assistance and testing by Miëtek Bak and Jonathan Fischoff, and thanks to many others for testing; The iOS cross compiler was started back in 2009 by Stephen Blackheath with funding from Ryan Trinkle of iPwn Studios.


Thanks to Ian Lynagh for making it easy for us with integration, makefile refactoring and patience, and to David Terei for LLVM assistance.



\[TYH\] Type holes [
Type Holes](http://www.haskell.org/haskellwiki/GHC/TypeHoles) 

\[OL\] Overloaded lists [overloaded lists](overloaded-lists) 

\[KD\] Kinds without data [
http://hackage.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData](http://hackage.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData) 

\[OTF\] Overlapping type family instances [
http://hackage.haskell.org/trac/ghc/wiki/NewAxioms](http://hackage.haskell.org/trac/ghc/wiki/NewAxioms) 

\[CG\] The new codegen is nearly ready to go live [
http://hackage.haskell.org/trac/ghc/blog/newcg-update](http://hackage.haskell.org/trac/ghc/blog/newcg-update) 

\[PIO\] The results are amazing [
https://twitter.com/bos31337/status/284701554458640384](https://twitter.com/bos31337/status/284701554458640384)\] 

\[IOS\] Building for Apple iOS targets [
http://hackage.haskell.org/trac/ghc/wiki/Building/CrossCompiling/iOS](http://hackage.haskell.org/trac/ghc/wiki/Building/CrossCompiling/iOS)


