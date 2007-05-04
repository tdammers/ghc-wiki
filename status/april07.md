# GHC Status April 2007



GHC continues to thrive.  One indicator of how widely GHC is used is the
number of bug reports we get.  Here is a graph showing how the number of
bug reports filed has varied with time:



[](/trac/ghc/attachment/wiki/Status/April07/bugs_per_day.png)



You could interpret these figures as saying that GHC is getting steadily
more unreliable!  But we don't think so...we believe that it's
mostly a result of more people using GHC, for more applications, on more
platforms.



As well as more bug reports, we are getting more help from the community, too.  
Some poeple regularly commit patches, and we get a steady trickle of patches
emailed in from folk who (mostly) do not have commit rights, but who have built GHC,
debugged a problem, sent us the patch. Our thanks go out to
Aaron Tomb, Alec Berryman, Alexey Rodriguez, Andrew Pimlott,
Andy Gill, Bas van Dijk, Bernie Pope, Bjorn Bringert,
Brian Alliet, Brian Smith, Chris Rodrigues, Claus Reinke,
David Himmelstrup, David Waern, Judah Jacobson, Isaac Jones,
Lennart Augustsson, Lennart Kolmodin, Manuel M T Chakravarty,
Pepe Iborra, Ravi Nanavati, Samuel Bronson, Sigbjorn Finne,
Spencer Janssen, Sven Panne, Tim Chevalier, Tim Harris,
Tyson Whitehead, Wolfgang Thaller, and anyone else who has contributed
but we have accidentally omitted.



As a result of this heavy usage, it has taken us nearly six months to
stabilise GHC 6.6.1, fixing over 100 reported bugs or infelicities in
the already-fairly-solid GHC 6.6.



The HEAD (which will become GHC 6.8) embodies nine months
of development work since we forked the tree for GHC 6.6.  We are now
aiming to get a stable set of features implemented in the HEAD, with
a view to forking off the GHC 6.8 branch in the early summer.  As
our last HCAR report indicated, there will be lots of new stuff in
GHC 6.8.  The rest of this entry describes the features that
are likely to end up in 6.8.



You can find binary snapshots at
the download page [http://www.haskell.org/ghc/dist/current/dist/](http://www.haskell.org/ghc/dist/current/dist/)
or build from sources available via the darcs
repository ([ http://darcs.haskell.org/ghc/](http://darcs.haskell.org/ghc/)).



Simon Peyton Jones, Simon Marlow, Ian Lynagh


## Type system and front end


- We have completely replaced GHC's intermediate language with
  System FC(X), an extension of System F with explicit equality
  witnesses.  This enables GHC to support GADTs and associated types,
  with two new simple but powerful mechanisms. The paper is 
  [
  System F with Type Equality Coercions](http://research.microsoft.com/~simonpj/papers/ext-f/).
  Much of the conversion work was done by Kevin Donnelly, while he
  was on an internship at Microsoft.

- Manuel Chakravarty has implemented *data-type families* (aka indexed data types),
  a modest generalisation of the *associated data types*
  of our POPL'05 paper
  [
  Associated types with class](http://research.microsoft.com/~simonpj/papers/assoc-types/).

  This part is done. Now we are working on *type-synonym families*
  (aka type functions or [
  associated type synonyms (ICFP'05)](http://research.microsoft.com/~simonpj/papers/assoc-types)), which are considerably trickier that
  data type families, at least so far as type inference is concerned.
  Tom Schrijvers is in Cambridge for three months to help us use ides
  from Constraint Handling Rules to solve the inference problem.
  Type synonym families will almost completely fill the spot occupied
  by the always-troublesome functional dependencies, so we are quite
  excited about this.

  Details are at [
  http://haskell.org/haskellwiki/GHC/Indexed\_types](http://haskell.org/haskellwiki/GHC/Indexed_types).

- Simon PJ finally implemented *implication constraints*, which are
  the key to fixing the interaction between
  GADTs and type classes.  GHC's users have been very polite about
  this collection of bugs, but they are now finally fixed.
  Implication constraints are described by Martin Sulzmann in
  [
  A framework for Extended Algebraic Data Types](http://www.comp.nus.edu.sg/~sulzmann/publications/tr-eadt.ps.gz).

- Bjoern Bringert (a GHC Hackathon graduate) implemented
  *standalone deriving*, which allows you to write a `deriving`
  declaration anywhere, rather than only where the data type is
  declared.  Details of the syntax have not yet quite settled. See
  also [
  http://haskell.org/haskellwiki/GHC/StandAloneDeriving](http://haskell.org/haskellwiki/GHC/StandAloneDeriving).

- Lennart Augustsson implemented overloaded string literals.  So now
  just as a numeric literal has type `FAa. Num a => a`,
  so a string literal has type `FAa. IsString a => a`,
  The documentation is here: [http://www.haskell.org/ghc/dist/current/docs/users\_guide/other-type-extensions.html\#overloaded-strings](http://www.haskell.org/ghc/dist/current/docs/users_guide/other-type-extensions.html#overloaded-strings).


A less successful feature of the last year has been the
story on impredicative instantiation (see the paper [
Boxy types: type inference for higher-rank types and impredicativity](http://research.microsoft.com/~simonpj/papers/boxy)).
The feature is implemented, but the implementation is significantly
more complicated than we expected; and it delivers fewer benefits than
we hoped.  For example, the system described in the paper does not
type-check (`runST $ foo`) and everyone complains. So Simon PJ added
an even more ad-hoc extension that does left-to-right instantiation.



The power-to-weight ratio is not good.  We're still hoping that
Dimitrios Vytiniotis and Stephanie Weirich will come out with a simpler
system, even if it's a bit less powerful.  So don't get too used to
impredicative instantiation as it now stands; it might change!


## Optimisations


- Simon PJ rewrote the Simplifier (again).  It isn't clear whether it was that alone, or
  whether something else happened too, but performance has improved quite significantly;
  on the order of 12%.

- Roman Leshchinskiy, Don Stewart, and Duncan Coutts did some beautiful
  work on *fusion*; see their paper [
  Rewriting Haskell strings](http://www.cse.unsw.edu.au/~dons/papers/CSL06.html).
  This fusion work is already being heavily used in the parallel array library
  (see below), and they are also working on replacing foldr/build fusion with
  stream fusion in the main base library (see their new paper
  [
  Stream Fusion: From Lists to Streams to Nothing at All](http://www.cse.unsw.edu.au/~dons/papers/CLS07.html)).

  Their work highlighted the importance of the [SpecConstr](spec-constr) transformation, which Simon PJ
  implemented several years ago.  Of course, they suggested many enhancements, many of
  which Simon PJ duly implemented; see the new paper [
  Constructor specialisation for Haskell programs](http://research.microsoft.com/~simonpj/papers/spec-constr/).

- Alexey Rodriguez visited us for three months from Utrecht, and implemented
  a new back-end optimisation called *dynamic pointer tagging*.  We have wanted
  to do this for ages, but it needed a skilled and insightful hacker to make it all
  happen, and Alexey is just that.  This optimisation alone buys us another 15%
  performance for compiled programs: see the paper [
  Dynamic pointer tagging](http://research.microsoft.com/~simonpj/papers/ptr-tag/index.htm).

## Concurrency


- Gabriele Keller, Manuel Chakravarty, and Roman Leshchinskiy, at the
  University of New South Wales, are collaborating with us on support
  for *nested data-parallel computation* in GHC.
  We presented a paper [
  Data parallel Haskell: a status report](http://research.microsoft.com/~simonpj/papers/assoc-types) at the Declarative Aspects of Multicore Programing
  workshop in January 2007,
  and made a first release of the
  library in March.
  It's a pretty ambitious project, and we have quite a way to go.
  You can peek at the current status on the project home page:
  [
  http://haskell.org/haskellwiki/GHC/Data\_Parallel\_Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell).

- Tim Harris added support for *invariants* to GHC's Software
  Transactional Memory (STM) implementation. Paper is [
  Transactional memory with data invariants](http://research.microsoft.com/~simonpj/papers/stm/).


    


- At the moment GHC's *garbage collector* is single-threaded,
  even when GHC is running on a multiprocessor.  Roshan James spent
  the summer at Microsoft on an internship, implementing a multi-threaded
  GC ([
  http://hackage.haskell.org/trac/ghc/wiki/MotivationForParallelization](http://hackage.haskell.org/trac/ghc/wiki/MotivationForParallelization)).
  It works!  But alas, doing GC with two processors runs no faster than
  with one!  (We do plan to investigate this further and find the source of
  the bottleneck.)


Peng Li, from the University of Pennsylvania, spent an exciting
three months at Cambridge, working on a whole new architecture for
concurrency in GHC.  (If you don't know Peng you should read his
wonderful paper [
Combining Events And Threads For Scalable Network Services](http://www.seas.upenn.edu/~lipeng/homepage/papers/lz07pldi.pdf)
on implementing a network protocol stack in
Haskell.)  At the moment GHC's has threads, scheduling, `forkIO`,
`MVars`, transactional memory, and more besides, all "baked
into" the run-time system and implemented in C.  If you want to
change this implementation you have to either be Simon Marlow, or else
very brave indeed.  With Peng (and help from Andrew Tolmach, Olin
Shivers, Norman Ramsey) we designed a new, much lower-level set of
primitives, that should allow us to implement all of the above
*in Haskell*.  If you want a different scheduler, just code it up
in Haskell, and plug it in.



Peng has a prototype running, but it has to jump the "Marlow barrier"
of being virtually as fast as the existing C runtime; so far we have
not committed to including this in GHC, and it certainly won't be in
GHC 6.8.  No paper yet, but look out for a Haskell Workshop 2007 submission.


## Programming environment



There have been some big developments in the programming
environment:


- Andy Gill implemented the Haskell Program Coverage
  ([
  http://haskell.org/haskellwiki/GHC/HPC](http://haskell.org/haskellwiki/GHC/HPC))
  option (`-fhpc`) for GHC, which is solid enough to be used to 
  test coverage in GHC itself.  (It turns out that the GHC testsuite
  gives remarkably good coverage over GHC already.)

- Pepe Iborra, Bernie Pope, and Simon Marlow have leveraged the same
  "tick" points used in the Haskell Program Coverage work to implement
  a [breakpoint debugger in GHCi](new-ghci-debugger).  Unlike HAT, which transforms the whole
  program into a new program that generates its own (massive) trace,
  this is a cheap-and-cheerful debugger.  It simply lets you set
  breakpoints and look around to see what is in the heap, more in the
  manner of a conventional debugger.  No need to recompile your program: it
  "just works".

- Aaron Tomb and Tim Chevalier are working on resurrecting External
  Core, whose implementation was not only bit-rotted, but also poorly 
  designed (by Simon PJ).  By GHC 6.8 we hope to be able to spit out External
  Core for any program, perhaps transform it in some external program,
  and read it in again, surviving the round trip unscathed. 

- It is now possible to compile to object code instead of bytecode inside GHCi, simply by setting a flag
  (`-fobject-code`).

- The GHC API has seen some cleanup, and it should now be both more complete and slightly easier to use.  There is still plenty of work to do here, though.

- David Waern has been working on integrating Haddock and GHC during his Google Summer of Code project
  last year.  The parts of this project that involved modifying GHC are done and integrated into the
  GHC tree.  The new version of Haddock based on GHC is usable but still experimental; the darcs repository is [
  here](http://darcs.haskell.org/SoC/haddock.ghc).

## Libraries


- The set of "corelibs" has been further streamlined, with `parsec`, `regex-base`,
  `regex-compat`, `regex-posix` and `stm` moved to extralibs in the HEAD. This disentangles
  releases of these packages from the GHC release process, and also means that development
  builds of GHC are quicker as they don't need to build those libraries.

- We plan to extract parts of the base package into separate smaller packages; see [
  this thread](http://www.haskell.org/pipermail/libraries/2007-April/007342.html) on the libraries mailing list.
