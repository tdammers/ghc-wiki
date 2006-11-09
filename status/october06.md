# GHC Status October 2006



GHC is in good shape. We have no good way to measure how many GHC
users there are but if the traffic on the GHC mailing lists is
anything to go by, the numbers are increasing quite rapidly. Indeed,
GHC was rapidly becoming a success-disaster, so that we (Simon &
Simon) were becoming swamped in GHC-related mail.  Happily,
Microsoft Research has agreed to fund a full-time support engineer,
in the form of Ian Lynagh (Igloo), who has already made a huge difference.



A highlight of the last six months was the **GHC Hackathon**, which we ran a immediately before ICFP in Portland, with wonderful support from Galois and Portland State University.  Forty-plus people showed up to have GHC's innards inflicted on them, and appeared unharmed by the experience.



A significant outcome is that we have written a great deal of Wiki material about GHC's implementation (the "commentary") and about how to build and modify GHC (the "building guide").  Documents with these titles were available before but had become rather out of date.  These new, up-to-date documents live on the GHC developer's Wiki.  We urge you to read and improve them:   [
http://hackage.haskell.org/trac/ghc/wiki](http://hackage.haskell.org/trac/ghc/wiki) (near the bottom).



We (finally) released **GHC 6.6** in October 2006. To get GHC 6.6, go to [Download page](http://www.haskell.org/ghc/download_ghc_66.html). There was an extended period of release-candidate testing, so we fondly hope that this will be a relatively stable release. There are many improvements, all listed in the [
Release notes](http://haskell.org/ghc/docs/6.6/html/users_guide/release-6-6.html). The most important new features include:


- Now GHC can execute several Haskell threads simultaneously on different cpus/cores
- `ByteString` type for fast and memory-efficent string manipulations
- Unicode source files
- Further generalisation of newtype deriving
- Bang patterns to declare function arguments as strict
- Impredicative polymorphism
- Lastly, we finally bit the bullet and lifted the restriction that every module in a Haskell program must have a distinct name.  Why?  Because it's non-modular: two packages from different authors could accidentally collide.  This change is in GHC 6.6; there are some remaining open choices dicussed here [
  http://hackage.haskell.org/trac/ghc/wiki/GhcPackages](http://hackage.haskell.org/trac/ghc/wiki/GhcPackages).


Life still goes on and there is current development version (HEAD), that will ultimately become GHC 6.8. You can find binary snapshots at [download page](http://www.haskell.org/ghc/dist/current/dist/) or build from sources available via [
darcs repository](http://darcs.haskell.org/ghc/). This version already includes significant new features:


- We have completely replaced GHC's intermediate language with **System FC(X)**, an extension of System F with explicit equality witnesses.  This enables GHC to support GADTs and associated types, with two new simple but powerful mechanisms. The paper is at [
  http://research.microsoft.com/%7Esimonpj/papers/ext-f/](http://research.microsoft.com/%7Esimonpj/papers/ext-f/). Much of the conversion work was done by Kevin Donnelly, while he was on an internship at Microsoft.

- Manuel Chakravarty has implemented **type-indexed data types**, a modest generalisation of the *associated data types* of our POPL'05 paper  [
  http://research.microsoft.com/%7Esimonpj/papers/assoc%2Dtypes/](http://research.microsoft.com/%7Esimonpj/papers/assoc%2Dtypes/). The implementation is in the HEAD and is ready to be tried out; details are at [
  http://haskell.org/haskellwiki/GHC/Indexed\_types](http://haskell.org/haskellwiki/GHC/Indexed_types) Still to come are some bits around the edges on `deriving` and some small syntactic generalisations.

- Tim Harris added support for **invariants** to GHC's Software Transactional Memory (STM) implementation. Paper here: [
  http://research.microsoft.com/%7Esimonpj/papers/stm/](http://research.microsoft.com/%7Esimonpj/papers/stm/).

- Bjorn Bringert (a GHC Hackathon graduate) implemented **standalone deriving**, which allows you to write a `deriving` declaration anywhere, rather than only where the data type is declared.  Details of the syntax have not yet quite settled.  See also [
  http://www.haskell.org/pipermail/haskell-prime/2006-October/001725.html](http://www.haskell.org/pipermail/haskell-prime/2006-October/001725.html).

- Andy Gill implemented the [
  Haskell Program Coverage](http://haskell.org/haskellwiki/GHC/HPC) option (**-fhpc**) for GHC, which is solid enough to be used to test coverage in GHC itself.  (It turns out that the GHC testsuite gives remarkably good coverage over GHC already.)


If you want to know today's state-of-the-art, you should check [
GHC 6.8 status](http://haskell.org/haskellwiki/GHC/6.8) page. At this moment we are working on the following features which is planned to be included in GHC 6.8 in next few months:


- Roman Leshchinskiy has been hard at work developing libraries that support **data-parallel computation** in GHC.  It's not quite ready for public consumption but you can peek at what is going on by looking at the Haskell Wiki: [
  http://haskell.org/haskellwiki/GHC/Data\_Parallel\_Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)  Background material here: [
  http://www.cse.unsw.edu.au/\~chak/papers/CKLP01.html](http://www.cse.unsw.edu.au/~chak/papers/CKLP01.html).  We hope to release a first iteration of our data-parallel extensions before Christmas.

- At the moment GHC's **garbage collector** is single-threaded, even when GHC is running on a multiprocessor.  Roshan James spent the summer at Microsoft on an internship, implementing a [
  multi-threaded GC](http://hackage.haskell.org/trac/ghc/wiki/MotivationForParallelization).  We need to do a bit more work, but with a bit of luck we'll push a parallel garbage collector into the HEAD before Christmas.

- Simon PJ is determined to finally implement **implication constraints**, which are the key to fixing the interaction between GADTs and type classes.   GHC's users have been very polite about this collection of bugs, but they should really be fixed.  Implication constraints are described by Martin Sulzmann: [
  http://www.comp.nus.edu.sg/\~sulzmann/publications/tr-eadt.ps.gz](http://www.comp.nus.edu.sg/~sulzmann/publications/tr-eadt.ps.gz).

- Once the last bits of indexed data types are done, Manuel will be tackling indexed type synonyms (aka type functions), which are considerably trickier, at least so far as type inference is concerned.


Simon, Simon and sons, November 2006 :)


