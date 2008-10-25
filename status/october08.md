# GHC Status October 2008



For the last six months we have been primarily focussed on the 6.10.1 release, which should be out by the time you read this. We are extremely grateful for the increasing support we get for the community in putting GHC releases together; more people than ever before are now helping maintain subcomponents, implementing features, fixing bugs, testing release candidates, and much more besides. We couldn't have made this release without your help!


## The GHC 6.10 branch



GHC 6.10.1 is the first release in the 6.10 branch, and features many improvements over the 6.8 branch. The release notes have fully details, but the highlights are:


- Some **new language features** have been implemented:

  - Record syntax: wild-card patterns, punning, and field disambiguation
  - Generalised quasi-quotes (Geoff Mainland), from the paper [
    Why it's nice to be quoted: quasi-quoting in Haskell](http://www.eecs.harvard.edu/~mainland/ghc-quasiquoting/mainland07quasiquoting.pdf) (Haskell workshop 2007)
  - Generalised list comprehensions (Max Bolingbroke), from the paper [
    Comprehensive comprehensions: comprehensions with "Order by" and "Group by"](http://research.microsoft.com/%7Esimonpj/papers/list-comp/index.htm) (Haskell workshop 2007).
  - View patterns (Dan Licata); see [view patterns wiki page](view-patterns).

- **Type families** have been completely re-implemented, by Manuel Chakravarty, along the lines of our ICFP 2008 paper [
  Type checking with open type functions](http://research.microsoft.com/%7Esimonpj/papers/assoc-types/index.htm) --- only simpler.  As a result, we believe that type families work reliably in GHC 6.10.  There is one missing feature, however, namely the ability to have equalities in the superclass context of a class.   We'll add that to the HEAD in the next few months.  An [up-to-date wiki page](type-functions) tracks design issues and current status.

- GHC now comes with **Haddock 2**, which supports all GHC extensions, thanks to David Waern.

- **Parallel garbage collection** has been implemented by Simon Marlow.  This speeds up even purely-sequential programs, by using the extra processors during garbage collection.  Our ISMM'08 paper gives the details [
  Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm). 

- The base library now provides, and uses, **extensible exceptions**, as described in Simon Marlow's paper [
  An Extensible Dynamically-Typed Hierarchy of Exceptions](http://www.haskell.org/~simonmar/papers/ext-exceptions.pdf) (Haskell workshop 2006).

- Thomas Schilling has made the **GHC API** easier to use, by using a `Ghc` monad to carry the session state.  Furthermore, the API now has Haddock documentation.

- **External core** (output only) now works again, thanks to Tim Chevalier.

- **Data Parallel Haskell** (DPH) comes as part of GHC, as a result of Roman Leshchinskiy's efforts.  In 6.10, for the first time, DPH includes a full vectoriser, so the system is much more usable than before.  It's still really an alpha release though; we very much welcome friendly guinea pigs, but it's not ready for your 3 gigabyte genome search program.  We have a lot of performance tuning to do.  We've written a new paper [
  Harnessing the multicores: nested data parallelism in Haskell](http://research.microsoft.com/%7Esimonpj/papers/ndp/index.htm) (FSTTCS'08), which gives a tutorial overview of the system, focusing especially on vectorisation.

## The GHC 6.12 branch



Meanwhile, development goes on in the HEAD:


- John Dias has been working hard on **rewriting GHC's backend**, and his changes should be landing in the HEAD during October.  You can find an [overview of the new architecture on the wiki](commentary/compiler/new-code-gen-pipeline)

- **Data Parallel Haskell** remains under very active development. 

- We hope that Max Bolingbroke's **Dynamically Loaded Plugins** summer of code project will be merged in time for 6.12.  Part of this is a new, modular system for [user-defined '''annotations'''](annotations), rather like Java or C\# attributes.  These attributes are persisted into interface files, can be examined and created by plugins, or by GHC API clients.

- Likewise, Donnie Jones's project for **profiling parallel programs** should be merged in time for 6.12

- Simon Marlow is working on **improving parallel performance**, incorporating the work done by Jost Berthold during his internship at Microsoft in the summer of 2008.  The plan is to make writing performant parallel programs less of a trial-and-error process, by whacking as many bottlenecks as we can find in the runtime system.   We're already making significant improvements, and there's plenty more low-hanging fruit to pick.  One large project that we hope to tackle is the issue of doing independent per-CPU garbage collection.

- **Shared Libraries**, are inching ever closer to being completed.  Clemens Fruhwirth has been working on polishing the support for shared libraries on Unix systems in particular, and when the remaining issues are ironed out we should be able to roll them out in a release.

- Finally, **unicode text I/O** and **dynamic libraries** were slated for 6.10 but weren't quite ready in time, so we certainly expect those to make it for in 6.12


From a development point of view, there are a couple of changes on the horizon:


- We plan to change how GHC's build system works, to decouple it from Cabal's internals.  Our current plans are [here](design/build-system). 

- We plan to change from darcs to git for the version control system used by GHC; our plans are described [here](design/version-control-system).


We plan to make the build-system changes first, and only then tackle the version control system.


## Summary



Keeping GHC functioning for an increasingly-diverse user base is quite a challenge, especially as we keep changing the wheels while the bus is driving along.  Please do consider joining in; there are plenty of things that need doing, and do not require intimate knowledge of the internals.  We could particularly do with more help on supporting the Windows, Sparc, and BSD ports of GHC.


