# The GHC reading list






Suppose you want to start contributing to GHC: what should you read by way of background?  Here is an annotated list.  Please add to it as you come across useful material. When you do so, please consider adding a link to a place where you are reasonably confident the resource will be available in 10 or 20 years. [
doi](http://doi.org/) purports to enable such links.



You can ask questions on `ghc-devs@haskell.org`. People are friendly.  See also [working on GHC](working-conventions) and [The GHC Team](team-ghc).



See also [
Stephen Deihl's Haskell implementation reading list](http://www.stephendiehl.com/posts/essential_compilers.html).


## General background


- The [GHC Commentary](commentary) is a Wiki that describes GHC's implementation.  It is a Wiki.  That means that you can, and should, fix errors and write new chapters.

- [ The Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html), in [
  The Architecture of Open Source Applications](http://www.aosabook.org/en/index.html), Volume II, ed Brown & Wilson. This paper gives an up to date (2012) technical overview of GHC.

- Simon PJ's [ home page](http://research.microsoft.com/~simonpj) and [
  publications page](http://research.microsoft.com/en-us/um/people/simonpj/papers/papers.html) have lots of relevant papers.  Some key ones appear below but not all.

- Simon PJ's books:

  - [
    The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm)
  - [
    Implementing Functional Languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/)
    give useful general background. They are not GHC-specific at all, but they have lots of information about functional-language compilers.

## Types and type inference


- [
  Dependent Types in Haskell: Theory and Practice](https://arxiv.org/abs/1610.07978) (2016), Richard Eisenberg. Eisenber's PhD thesis describes many aspects of GHC's type system, although the "bake" inference algorithm is not yet implemented in GHC.

- [
  System FC, as implemented in GHC](http://git.haskell.org/ghc.git/blob/refs/heads/master:/docs/core-spec/core-spec.pdf) (2013), Richard Eisenberg.

- [
  Modular type inference with local assumptions](http://haskell.org/haskellwiki/Simonpj/Talk:OutsideIn) [
  doi link](http://dx.doi.org/10.1017/S0956796811000098), Simon Peyton Jones, Dimitrios Vytiniotis, Tom Schrijvers, Martin Sulzmann, Journal of Functional Programming, 2011.  This epic 83-page JFP paper brings together, in a single uniform framework, a series of our earlier papers on type inference for type systems involving local constraints, including GADTs and indexed type families.  

- [
  Practical Type Inference for Arbitrary-Rank Types](http://repository.upenn.edu/cis_papers/315/). Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. JFP '07. [
  doi](http://dx.doi.org/10.1017/S0956796806006034) [
  technical appendix](http://repository.upenn.edu/cis_reports/58/) Describes type inference for higher-rank types.

- Type equalities in GHC's intermediate language:

  - [
    System FC with Explicit Kind Equality](http://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf). Stephanie Weirich, Justin Hsu, Richard A. Eisenberg. ICFP '13. [
    doi](http://dx.doi.org/10.1145/2500365.2500599) Merges types with kinds, allowing promotion of GADTs and type families. Implementation not yet merged (July 2015).
  - *Equality proofs and deferred type errors*, Simon Peyton Jones, Dimitrios Vytiniotis and Pedro Magalhaes (ICFP 2012).  An exploration of what happens when you take equality proofs seriously in a compiler. [
    doi](http://dx.doi.org/10.1145/2364527.2364554) [
    pdf](http://dreixel.net/research/pdf/epdtecp.pdf)
  - *Giving Haskell a promotion*, Brent Yorgey, Stepanie Weirich, Julien Cretin, Simon Peyton Jones, and Dimitrios Vytiniotis (TLDI 2012).  How to (a) add kind polymorphism and (b) promote data types to become data kinds. [
    doi](http://dx.doi.org/10.1145/2103786.2103795) [
    pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/promotion.pdf)
  - *Evidence Normalization in System FC*. Dimitrios Vytiniotis, Simon Peyton Jones. RTA '13. [
    doi](http://dx.doi.org/10.4230/LIPIcs.RTA.2013.20) [
    pdf](http://drops.dagstuhl.de/opus/volltexte/2013/4050/pdf/3.pdf) Explains the coercion optimizer.
  - *System F with Type Equality Coercions*, Martin Sulzmann, Manuel Chakravarty, and Simon Peyton Jones (TLDI 2007).  The first paper about System FC. [
    doi](http://dx.doi.org/10.1145/1190315.1190324) [
    extended pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf)

- Type families:

  - [
    Associated Types with Class](http://research.microsoft.com/en-us/um/people/simonpj/Papers/assoc-types/assoc.pdf). Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, Simon Marlow. POPL '05. [
    doi](http://dx.doi.org/10.1145/1040305.1040306) Introduces associated data families.
  - [
    Associated Type Synonyms](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/at-syns.pdf). Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones. ICFP '05. [
    doi](http://dx.doi.org/10.1145/1086365.1086397) Introduces associated type families.
  - [
    Closed Type Families with Overlapping Equations](http://www.seas.upenn.edu/~sweirich/papers/popl14-axioms.pdf). Richard A. Eisenberg, Dimitrios Vytiniotis, Simon Peyton Jones, Stephanie Weirich. POPL '14. [
    doi](http://dx.doi.org/10.1145/2535838.2535856) [
    extended version](http://repository.upenn.edu/cis_reports/990/) Introduces closed type families.
  - [
    Injective Type Families for Haskell](http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity.pdf). Jan Stolarek, Simon Peyton Jones, Richard Eisenberg. Haskell '15. [
    doi](http://dx.doi.org/10.1145/2804302.2804314). Introduces injective type families.

- [
  Safe Zero-Cost Coercions for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf). Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones, Stephanie Weirich. ICFP '14. [
  doi](http://dx.doi.org/10.1145/2628136.2628141) [
  extended pdf](http://www.seas.upenn.edu/~sweirich/papers/coercible-extended.pdf) Introduces the `Coercible` mechanism.

- [
  Partial Type Signatures for Haskell](https://lirias.kuleuven.be/bitstream/123456789/423475/3/paper.pdf). Thomas Winant, Dominique Devriese, Frank Piessens, Tom Schrijvers.  PADL 2014 [
  TR](https://lirias.kuleuven.be/bitstream/123456789/424883/1/CW649.pdf) [
  doi](http://dx.doi.org/10.1007/978-3-319-04132-2_2) Introduces partial type signatures.

- [
  Understanding Functional Dependencies via Constraint Handling Rules](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf). Martin Sulzmann, Gregory J. Duck, Simon Peyton Jones, Peter J. Stuckey. JFP '07. [
  doi](http://dx.doi.org/10.1017/S0956796806006137)

- [
  Type Classes in Haskell](http://research.microsoft.com/~simonpj/Papers/classhask.ps.gz), Cordelia Hall, Kevin Hammond, Simon Peyton Jones, and Philip Wadler, European Symposium On Programming 1994. Type inference for type classes.

- [
  Derivable type classes](http://research.microsoft.com/~simonpj/Papers/derive.htm), Ralf Hinze and Simon Peyton Jones; Haskell Workshop 2000.

- [
  Once Upon a Polymorphic Type](http://research.microsoft.com/en-us/um/people/simonpj/papers/usage-types/popl99-usage.ps.gz), Keith Wansbrough and Simon Peyton Jones, POPL 1999.

- from ghc-proposals (please organize them into the correct paragraph)

  - [
    Levity Polymorphism (extended version)](https://cs.brynmawr.edu/~rae/papers/2017/levity/levity.pdf), Richard A. Eisenberg, Simon Peyton Jones, PLDI 2017. (ghc-proposals [
    0003](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0003-levity-polymorphism.rst))
  - [
    Pattern Synonyms](https://www.microsoft.com/en-us/research/publication/pattern-synonyms/), Matthew Pickering, Gergő Érdi, Simon Peyton Jones, and Richard A. Eisenberg, Haskell Symposium 2016. (ghc-proposals [
    0016](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0016-as-patterns-synonyms.rst))
  - [
    Quantified class constraints](https://i.cs.hku.hk/~bruno//papers/hs2017.pdf), Gert-Jan Bottu,  Georgios Karachalias, Tom Schrijvers, Bruno C. d. S. Oliveira, Philip Wadler, Haskell Symposium 2017. (ghc-proposals [
    0018](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0018-quantified-constraints.rst))
  - [
    Deriving Via or, How to Turn Hand-Written Instances into an Anti-Pattern](https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf), Baldur Blöndal, Andres Löh, Ryan Scott, Haskell Symposium 2018. (ghc-proposals [
    0023](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-deriving-via.rst))
  - [
    Type variables in patterns](https://arxiv.org/abs/1806.03476), Richard Eisenberg, Joachim Breitner and Simon Peyton Jones, Haskell Symposium 2018. (ghc-proposals [
    0031](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0031-type-applications-in-patterns.rst))


Please add: System FC, GADTs, kind polymorphism etc


## Optimisations


- [
  A transformation-based optimiser for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/comp-by-trans-scp.ps.gz), SL Peyton Jones and A Santos, Science of Computer Programming 32(1-3), pp3-47, September 1998.  Gives an overview of many of the transformations GHC does on Core.  Andre's [
  PhD thesis](http://research.microsoft.com/en-us/um/people/simonpj/papers/santos-thesis.ps.gz) gives more details.

- [
  Secrets of the GHC inliner](http://research.microsoft.com/en-us/um/people/simonpj/papers/inlining/index.htm), Simon Peyton Jones and Simon Marlow, Journal of Functional Programming 12(4), July 2002, pp393-434.  Describes how the Simplifier does inlining.

-  [
  A short cut to deforestation](http://research.microsoft.com/en-us/um/people/simonpj/papers/deforestation-short-cut.ps.Z), A Gill, SL Peyton Jones, J Launchbury, Proc Functional Programming Languages and Computer Architecture (FPCA'93), Copenhagen, June 1993, pp223-232.  The famous foldr/build rule.  Andy's [
  PhD thesis](http://research.microsoft.com/en-us/um/people/simonpj/papers/andy-thesis.ps.gz) has more.  

- [
  Playing by the rules: rewriting as a practical optimisation technique in GHC](http://research.microsoft.com/en-us/um/people/simonpj/papers/rules.htm), Simon Peyton Jones, Andrew Tolmach and Tony Hoare, Haskell Workshop 2001.  Describes how RULES work, which are heavily used in GHC.

- [
  Call-pattern Specialisation for Haskell Programs](https://research.microsoft.com/en-us/um/people/simonpj/papers/spec-constr/spec-constr.pdf), Simon Peyton Jones, ICFP 2007. Describes the specialisation optimiser.

- [
  Let-floating: moving bindings to give faster programs](http://research.microsoft.com/pubs/67060/float.ps.gz), Simon Peyton Jones, Will Partain, and Andre Santos, ICFP 1996. Describes the let floating and full laziness optimisation passes.

- [
  Modular, Higher-Order Cardinality Analysis in Theory and Practice](http://research.microsoft.com/en-us/um/people/simonpj/papers/usage-types/cardinality-popl14.pdf), Ilya Sergey, Dimitrios Vytiniotis, Simon Peyton Jones, POPL 2014. Describes cardinality analysis and optimisations that it enables or improves (eg. let-floating).

- [
  Constructed Product Result Analysis for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/cpr/cpr.ps.gz), Clem Baker-Finch, Kevin Glynn, and Simon Peyton Jones, Journal of Functional Programming 14(2), 211–245, March 2004. Describes optimisation that allows to return tuple components in registers (for functions that return tuples).

- [
  Demand analysis](http://research.microsoft.com/en-us/um/people/simonpj/papers/demand-anal/demand.ps), Simon Peyton Jones, Peter Sestoft, and John Hughes, draft, 2006.

- [
  Arity Analysis](http://gallium.inria.fr/~naxu/research/arity.ps), Dana N. Xu and Simon Peyton Jones, Working Notes, 2006

- [
  Call Arity](http://www.joachim-breitner.de/publications/CallArity-TFP.pdf), Joachim Breitner, TFP, 2014, as well as [
  Formally Proving a Compiler Transformation Safe](http://www.joachim-breitner.de/publications/CallArity-Haskell15.pdf), Joachim Breitner, Haskell 2015

## Data Parallel Haskell and concurrency


- [
  Data Parallel Haskell: a status report](http://www.cse.unsw.edu.au/~chak/papers/data-parallel-haskell.pdf), Manuel M. T. Chakravarty, Roman Leshchinskiy, Simon Peyton Jones, Gabriele Keller, and Simon Marlow. , DAMP 2007: Workshop on Declarative Aspects of Multicore Programming, 2007

- [
  Harnessing the Multicores: Nested Data Parallelism in Haskell](http://www.cse.unsw.edu.au/~chak/papers/fsttcs2008.pdf), Simon Peyton Jones, Roman Leshchinskiy, Gabriele Keller, and Manuel M. T. Chakravarty , IARCS Annual Conference on Foundations of Software Technology and Theoretical Computer Science (FSTTCS 2008), IBFI, Schloss Dagstuhl, 2008. 

- [
  Vectorisation Avoidance](http://www.cse.unsw.edu.au/~chak/papers/vect-avoid.pdf), Gabriele Keller, Manuel M. T. Chakravarty, Roman Leshchinskiy, Ben Lippmeier, and Simon Peyton Jones, Proceedings of ACM SIGPLAN Haskell Symposium 2012, ACM Press, 2012. 

- [
  Work Efficient Higher-Order Vectorisation](http://www.cse.unsw.edu.au/~chak/papers/replicate.pdf), Ben Lippmeier, Manuel M. T. Chakravarty, Gabriele Keller, Roman Leshchinskiy, and Simon Peyton Jones, The 17th ACM SIGPLAN International Conference on Functional Programming, ACM Press, 2012

- [
  Runtime Support for Multicore Haskell](http://simonmar.github.io/bib/papers/multicore-ghc.pdf) (Simon Marlow, Simon Peyton Jones, Satnam Singh) In ICFP '09: Proceeding of the 14th ACM SIGPLAN International Conference on Functional Programming, Edinburgh, Scotland, August 2009

- [Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz), Simon Peyton Jones, Andrew Gordon, Sigbjorn Finne. Deals with the various concurrency constructs in GHC and the Haskell language. E.g., MVars.

- [
  Composable Memory Transactions](http://research.microsoft.com/pubs/67418/2005-ppopp-composable.pdf), Tim Harris, Simon Marlow, Simon Peyton-Jones, and Maurice Herlihy.  In Proceedings of the tenth ACM SIGPLAN symposium on Principles and practice of parallel programming (PPoPP '05)

- [
  Transactional Memory with Data Invariants](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm-invariants.pdf), Tim Harris and Simon Peyton Jones.  In TRANSACT '06

## Intermediate Representation of GHC (Core & Related)


- [An External Representation for the GHC Core Language](http://www.haskell.org/ghc/docs/6.10.4/html/ext-core/core.pdf) Gives an overview of the semantics and syntax of Core, GHC's internal intermediate representation for Haskell that most of the optimisation work is done on. A good language to understand when starting with GHC.

- [Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz), Simon L Peyton Jones and John Launchbury, Conference on Functional Programming Languages and Computer Architecture, September 1991. Describe the design of GHC language and internals for handling machine values and boxing / unboxing them as lazy values.

## Code generation and virtual machine


- [
  How to make a fast curry: push/enter vs eval/apply](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/index.htm), Simon Marlow and Simon Peyton Jones, International Conference on Functional Programming, Snowbird, Sept 2004, pp4-15.

- [
  Faster laziness using dynamic pointer tagging](http://simonmar.github.io/bib/papers/ptr-tagging.pdf) (Simon Marlow, Alexey Rodriguez Yakushev, Simon Peyton Jones) In ICFP '07: Proceedings of the ACM SIGPLAN international conference on Functional programming, Freiburg, Germany, ACM Press, October 2007

- [
  Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine](http://research.microsoft.com/~simonpj/papers/spineless-tagless-gmachine.ps.gz), SL Peyton Jones, Journal of Functional Programming 2(2), Apr 1992, pp127-202.  The original STG paper but still highly relevant.

- [The STG runtime system (revised)](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz), Simon Peyton Jones and Simon Marlow. A highly-detailed description of STG. It is probably the most up-to-date description, aside for later additions from [
  dynamic pointer tagging paper](http://simonmar.github.io/bib/papers/ptr-tagging.pdf) and [
  fast curry paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/index.htm).

- [
  Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/hoopl-haskell10.pdf), Norman Ramsey, John Dias, and Simon Peyton Jones. Haskell Symposium 2010

- [
  C--: a portable assembly language that supports garbage collection](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/ppdp.ps.gz), Simon Peyton Jones, Norman Ramsey, and Fermin Reig. Invited talk at PPDP'99. 

- [
  A Single Intermediate Language That Supports Multiple Implementations of Exceptions](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/c--exn-pldi.ps.gz), Norman Ramsey and Simon Peyton Jones, PLDI 2000.

- [
  An LLVM Backend for GHC](https://www.davidterei.com/downloads/papers/terei:2010:llvm.pdf), David Terei and Manuel M. T. Chakravarty. Haskell Symposium 2010

## IO and Related


- [
  Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](https://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf), Simon Peyton Jones. Deals with the incarnation of IO in Haskell and GHC. The history getting to monads, handling exceptions and handling concurrency.

- [Imperative Functional Programming](http://www.haskell.org/ghc/docs/papers/imperative.ps.gz), Simon Peyton Jones, Philip Wadler. POPL,  Jan 1993, pp71-84. Presents Monads as a way of implementing IO in Haskell.

- [Lazy Functional State Threads](http://www.haskell.org/ghc/docs/papers/lazy-functional-state-threads.ps.gz), John Launchbury and Simon Peyton Jones. PLDI 1993. A follow-up on "Imperative Functional Programming" paper.

- [
  Asynchronous Exceptions in Haskell](http://simonmar.github.io/bib/papers/async.pdf), Simon Marlow, Simon Peyton Jones, Andrew Moran and John Reppy, 2006.

- [A semantics for imprecise exceptions](http://www.haskell.org/ghc/docs/papers/except_ps.gz), Simon Peyton Jones, Alastair Reid, Tony Hoare and Simon Marlow, PLDI '99.

- [
  Imprecise Exceptions, Co-Inductively](http://research.microsoft.com/en-us/um/people/simonpj/Papers/imprecise-exn-sem.htm), Andy Moran, Soeren Lassen, and Simon Peyton Jones, HOOTS'99.

- [
  Mio: A High-Performance Multicore IO Manager for GHC](http://haskell.cs.yale.edu/wp-content/uploads/2013/08/hask035-voellmy.pdf), Andreas Voellmy, Junchang Wang, Paul Hudak and Kazuhiko Yamamoto. Haskell Symposium 2013.

## The run-time system, garbage collector, profiling, FFI


- [
  Parallel Generational-Copying Garbage Collection with a Block-Structured Heap](http://simonmar.github.io/bib/papers/parallel-gc.pdf) (Simon Marlow, Tim Harris, Roshan P. James, Simon Peyton Jones) In ISMM '08: Proceedings of the 7th international symposium on Memory management, Tucson, Arizona, ACM, June 2008

- [
  Efficient communication and collection with compact normal forms](http://ezyang.com/papers/ezyang15-cnf.pdf) (Edward Z. Yang, Giovanni Campagna, Ömer S. Ağacan, Ahmed El-Hassany, Abhishek Kulkarni, Ryan R. Newton), ICFP 2015.

- [
  Haskell on a Shared-Memory Multiprocessor](http://simonmar.github.io/bib/papers/multiproc.pdf) (Tim Harris, Simon Marlow, Simon Peyton Jones) In Haskell '05: Proceedings of the 2005 ACM SIGPLAN workshop on Haskell, pages 49--61, Tallinn, Estonia, ACM Press, September 2005

- [Time and space profiling for non-strict, higher-order functional languages](http://www.haskell.org/ghc/docs/papers/profiling.ps.gz), Patrick M. Sansom and Simon Peyton Jones, POPL 1995.

- [The Concurrent Haskell Foreign Function Interface](http://www.haskell.org/ghc/docs/papers/threads.ps.gz), Wolfgang Thaller. An Addendum to Haskell 98 FFI Report.

- [
  Extending the Haskell Foreign Function Interface with Concurrency](http://simonmar.github.io/bib/papers/conc-ffi.pdf), Simon Marlow, Simon Peyton Jones and Wolfgang Thaller. Haskell Workshop 2004.

## Modules & Packages


- [
  A Formal Specification of the Haskell 98 Module System](http://programatica.cs.pdx.edu/P/diatchi.pdf) (Iavor S. Diatchki, Mark P. Jones, Thomas Hallgren) In Haskell '02.

- [
  Backpack: Retrofitting Haskell with Interfaces](http://plv.mpi-sws.org/backpack/backpack-paper.pdf) (Scott Kilpatrick, Derek Dreyer, Simon Peyton Jones, Simon Marlow), POPL 2014.

## Other GHC features


- [
  Template Meta-programming for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf). Tim Sheard, Simon Peyton Jones. Haskell '02. [
  doi](http://dx.doi.org/10.1145/636517.636528) Introduces Template Haskell.

- [
  Scrap your Boilerplate: a Practical Design Pattern for Generic Programming](http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/index.htm). Ralf Lämmel, Simon Peyton Jones. TLDI '03. [
  doi](http://dx.doi.org/10.1145/604174.604179) Introduces `Typeable` and `Data`.
