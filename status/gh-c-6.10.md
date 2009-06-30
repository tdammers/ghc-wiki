# Plans for GHC 6.10



We expect to release GHC 6.10 around ICFP 2008.  


-  [
  Beta released](http://www.haskell.org/pipermail/glasgow-haskell-users/2008-September/015539.html). 

## Things that are done already


- Several **language extensions** advertised in the [November 2007 status report](status/nov07):

  - Improvements to record syntax
  - View patterns
  - Quasiquotation
  - Generalised list comprehensions

>
>
> **Done**: these are all in the HEAD already.
>
>

- **Parallel garbage collection** (see [
  Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm)).  **Done**.

- **External Core** (output only) is working again, thanks to Tim Chevalier.

- Better **versioning** to support separate compilation based on MD5 fingerprints. Already done (and documented! see [Commentary/Compiler/RecompilationAvoidance](commentary/compiler/recompilation-avoidance).

- GHC now uses **libffi** to implement parts of the FFI, replacing some of the home-grown and very architecture-specific code we had to do this.  Amongst other benefits, this will ease the task of porting GHC in the future. Done; but *maybe use it to solve the SE Linux paranoia problem?*

- **Backwards compatibility**: we've introduce "base3-compat", a backwards-compatible version of the base library
  that will provide essentially the same API as the base library that shipped with GHC 6.8.3, so that code
  depending on base-3 will continue to just work.

- **Haddock 2**

  - Build it with GHC (maybe ship it with GHC too)
  - Documentation for GHC API done via Haddock 2

- **Extensible exceptions**, along the lines of Simon's paper [
  An Extensible Dynamically-Typed Hierarchy of Exceptions](http://www.haskell.org/~simonmar/papers/ext-exceptions.pdf).  This is mainly a library change.

- **Haddock 2** (see also [\#1964](http://gitlabghc.nibbler/ghc/ghc/issues/1964) (GHC.Prim)).  (**Ian Lynagh**)

- **GHC API** improvement: **Thomas Schilling** is doing a SoC project.  Preserve comments and pragmas, generic traversals ([\#1467](http://gitlabghc.nibbler/ghc/ghc/issues/1467), [\#1886](http://gitlabghc.nibbler/ghc/ghc/issues/1886), [GhcApiStatus](ghc-api-status)). We'll ship whatever Thomas has committed by then.

- **[
  Type families](http://haskell.org/haskellwiki/GHC/Indexed_types)**, fully working. *Manuel Chakravarty and Simon PJ*

- **[
  Nested data parallelism](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)**, in some form. *Roman Leshchinskiy, Gabriele Keller, Manuel Chakravarty, Simon PJ*


 


- More library reorg ([\#1338](http://gitlabghc.nibbler/ghc/ghc/issues/1338)).  The goal here is to shift stuff out of boot-libs and into the Haskell Library Platform, which is independently upgradable.  Not hugely urgent, nice to have.

- `^C` should raise an exception by default (also SIGPIPE, see [\#1619](http://gitlabghc.nibbler/ghc/ghc/issues/1619), [\#2301](http://gitlabghc.nibbler/ghc/ghc/issues/2301)). Nearly done!  But not quite complete if you fork another process.  This latter part is lower priority.

## Things we plan to do for sure


- **Ship the Haskell Library Platform** instead of 'extralibs'.  **Don and Duncan** are leading.

# Beyond 6.10



This is a list of things that are floating about in our minds for what to do beyond 6.10.  Nothing is decided, and these items vary wildly in their size.


- **[Back-end revamp](commentary/compiler/new-code-gen)** (see also [\#1501](http://gitlabghc.nibbler/ghc/ghc/issues/1501)).  **John Dias** is in charge.  For 6.10 we will make sure that the whole existing path still exists, so we can choose at a late date whether to rely on the new path or not.

- **Unicode support for text I/O**.  This means adding Unicode encoding/decoding for Text I/O handles.   (**Simon Marlow**: a few days work.)

  - Consensus was that Text I/O should always use the current locale encoding.  
  - You can elect to have no encoding by opening in binary mode, but that's all.

- **Shared libraries**, as a result of Clemens Fruhwirth's Summer of Code project.  ([\#1876](http://gitlabghc.nibbler/ghc/ghc/issues/1876)) **Simon Marlow**: about a week's work.

  - Binaries get much smaller
  - Compile a package on Windows to a DLL; it just works
  - C program (or Excel) that calls multiple Haskell functions gets just one copy of the RTS, rather than one per DLL as now.
  - Performance penalty, but too small to measure

- **Opaque interfaces** (optionally), so you can upgrade a library without recompiling.

- **Parallelism*: better profiling tools.
  ***

- **Visual Haskell**: a Visual Studio plugin.  There is one, but it has suffered bit-rot.


 


- **GHC as a platform** is the aspiration that it should be easy to plug extensions into GHC, and easy to use GHC to extend other software.

- **Static verification** along the lines of Dana Xu's work.

- **Finish System.Process revamp** ([\#2233](http://gitlabghc.nibbler/ghc/ghc/issues/2233))

- Binary package DB, or at least make the one-file-per package work ([\#593](http://gitlabghc.nibbler/ghc/ghc/issues/593), [\#723](http://gitlabghc.nibbler/ghc/ghc/issues/723), [\#2089](http://gitlabghc.nibbler/ghc/ghc/issues/2089))
