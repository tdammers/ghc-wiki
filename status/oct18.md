```wiki

# GHC Status Report (November 2018)

GHC development continues with the release of 8.6.2, continued improvement in
testing infrastructure, and a slew of new features.

## Major changes in GHC 8.8

With GHC 8.6 behind us, we have started to focus on what will be GHC 8.8, which
should ship with a number of great features.

### Libraries, source language, and type system

 * Syntax for visible dependent quantification (Proposal #81), allowing
  users to express types with visible, dependent quantifiers more directly.

 * Top-level kind signatures, allowing users to add kind signatures alongside
  their type declarations.


### Compiler

 * The next phase of Trees That Grow, refactoring GHC's treatment of source
  spans in the Haskell AST.
  
 * Continued work on compiler performance

 * Support for SIMD operations in the native code generator

 * Support for sub-word sized values in the code generator and libraries

 * Further improvements to runtime performance:

   * A late-lambda lifting pass to further improve

   * A new code layout algorithm, significantly improving the 

 * Many, many bug fixes.

### Runtime system

 * Significantly improved Windows support with a new I/O manager (Tamar Christina).

## GHC proposals

Since the launch of the GHC proposals process
(<https://github.com/ghc-proposals/ghc-proposals>), over 171 proposals have
been opened for discussion, 49 have been submitted to the committee and 38 have
been accepted. Recently 

 * Lower precedence for `\{-# UNPACK #-\}`

 * Make rebindable fail work with `OverloadedStrings`

 * Replace `atomicModifyMutVar#`
 
 * Remove the `*` kind syntax (PR #143)

 * Module export deprecation pragmas (PR #134)

 * Allow ScopedTypeVariables to refer to types (PR #128)

 * Type applications in patterns (PR #126)

 * Add more array resizing primitives (PR #121)

 * Deriving Via (PR #120)

 * `UnliftedArray#` (PR #112)

At the time of writing,
10 proposals are under active discussion by the community
(<https://github.com/ghc-proposals/ghc-proposals/pulls?q=is\%3Aopen+is\%3Apr+no\%3Alabel>)
and
9 proposals
(<https://github.com/ghc-proposals/ghc-proposals/pulls?q=is\%3Aopen+is\%3Apr+label\%3A\%22Pending+committee+review\%22>)
are under review by the committee.

## Looking forward: What's hot

GHC is lucky to have a large number of volunteer contributors. Many of these
features will be present in the up-coming 8.8 release.

 * Matth\'ias P\'all Gissurarson has been adding support for significantly
   improved diagnostics messages for typed holes, including making the feature
   easier to integrate into IDE tooling.

 * Ryan Scott has been busily triaging and fixing bugs on a daily basis, and
   generally helps to keep things running smoothly.

 * Michal Terepeta has been performing a variety of refactoring and
   optimization in the backend as well as introducing support for sub-word-sized
   fields.

 * Abhiroop Sarkar has been working on introducing support for x86 SIMD
   instructions into GHC's native code generator, making these primitives
   applicable over a significantly wider range of settings.
   
 * Andreas Klebinger has been working on improving the code layout algorithms
   used by GHC's backend code generator. The result of his Google Summer of Code
   project resulted in speed-ups of between 1\% and 5\% on a variety of tested
   real-world libraries.
   
 * Tamar Christina has continued his work on making GHC run great on
   Windows. Recently he has been working to finish up a patchset enabling
   dynamic linking support on Windows. Tamar is also working on a rework
   of GHC's Windows IO manager implementation. The new implementation
   will take full advantage of Windows' asynchronous I/O interfaces and
   should solve dozens of long-standing tickets.

 * Sebastian Graf has been working on rekindling the late lambda-lifting work
   started by Nicholas Frisby some time ago. This transformation optimizes
   runtime allocations by turning free variables into call arguments.
   His explorations into performance this transformation on STG has
   resulted in extremely impressive allocations reductions on the nofib
   benchmark suuite.
   
 * Andrey Mokhov, David Eichmann, and Alp Mestanogullari have been working on the
   finishing the last mile of the switch to GHC's new Shake-based build system,
   Hadrian, which has now been merged into the GHC tree.
   
 * One of the larger projects in the pipeline for 8.6 is Alan Zimmerman
   and Shayan Najd's refactoring of GHC to use the extensible Trees That
   Grow AST structure.
   
 * Ben Gamari has been working on improving compilation time for programs
   making heavy use of type families. His patch fixes a long-standing performance
   cliff (#8095) of GHC's compilation pipeline and should significantly improve
   compilation times of programs with lots of fancy types.

 * Tobias Dammers and Ömer Sinan Ağacan have been working on too many
   projects to name, including fixing numerous bugs and improving compiler
   performance.

 * Kavon Farvardin has been working on numerous projects around the LLVM code
   generator, including working with the LLVM developers to teach LLVM about
   GHC's notion of proc points.

 * Ningning Xie and Richard Eisenberg have been chipping away at realizing
   Dependent Haskell.

 * Vladislav Zavialov, in addition to contributing a variety patches, has been
   invaluable in helping with code review and advising contributors.

 * Zubin Duggal has been working on a mechanism for exporting large swaths of
   GHC's typechecked representation for consumption by IDE tooling.

 * Simon Jakobi introduced support in GHCi for the `:doc` command,
   allowing users to view (currently unformatted) Haddock documentation from the
   REPL.

 * Peter Trommler has been working on improving GHC's portability to non-x86
   platforms.

 * Alec Theriault has also been doing a variety of work around Template
   Haskell, and the numerous projects in GHC's frontend,

As always, if you are interested in contributing to any facet of GHC, be
it the runtime system, type-checker, documentation, simplifier, or
anything in between, please come speak to us either on IRC (`#ghc` on
`irc.freeenode.net}`) or `ghc-devs@haskell.org`. Happy Haskelling!


\FurtherReading

  * GHC website:

    <https://haskell.org/ghc/>

  * GHC users guide:

    <https://downloads.haskell.org/~ghc/master/users_guide/>

  * `ghc-devs` mailing list:

    <https://mail.haskell.org/mailman/listinfo/ghc-devs>

```