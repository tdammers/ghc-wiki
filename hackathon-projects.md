# Project suggestions for the 2006 GHC hackathon



See also [FridayIdeas](friday-ideas) - someone should combine these lists :)



Add your suggestions for the hackathon below...


## GHC Projects


- Hack to allow RTS to integrate with an external event loop (eg to give us ideal threading when using Gtk+)

- Add a `ghc --clean` that just executes `find -name '*.o' -o -name '*.hi' -exec rm {} \;`, perhaps in a more portable fashion

- Further work on debugging in GHCi

- Improve the native code generator: see [BackEndNotes](back-end-notes)

- Get the front panel working again ([\#599](http://gitlabghc.nibbler/ghc/ghc/issues/599))

- WARNING pragma ([\#657](http://gitlabghc.nibbler/ghc/ghc/issues/657))

- Warning suppression ([\#602](http://gitlabghc.nibbler/ghc/ghc/issues/602))

- Accept more encodings for source code ([\#690](http://gitlabghc.nibbler/ghc/ghc/issues/690))

- Use gcc's libffi to implement Adjustor.c & ByteCodeFFI

- Add :edit support to GHCi ([\#95](http://gitlabghc.nibbler/ghc/ghc/issues/95))

- Option to allow compiling from within GHCi ([\#276](http://gitlabghc.nibbler/ghc/ghc/issues/276))

- GHC plugins: allow passes to be loaded at runtime from plugins

- Get shared libraries or DLLs working

- Build a Windows-native version of GHC (using MS tools instead of gcc).

- GHC API clients: hstags, code metrics (eg. feature use)

- Implemene the static argument transformation ([\#888](http://gitlabghc.nibbler/ghc/ghc/issues/888))

- Whole-program dead-code detection (with `--make`).

- Whole-program overloading elimination (with `--make`).


 


- Work on Haddock on top of the GHC API (see [
  this message](http://www.haskell.org/pipermail/haskell/2006-August/018415.html)).

- `-Ofile`: take the list of optimisation passes to run from a file instead of current hard-wired sequence.

- `-fmonad-comprehension`: Reuse existing list comprehension code to restore it for arbitrary monads.

- Improve the profiler (longer stacks reported while heap profiling, for example)


 


- Add type parameter to [HsSyn](commentary/compiler/hs-syn-type) for the post-typechecking type to ensure it does not exist until after typechecking

- Improve pretty-printer for Core to have better layout -- Geoff Washburn may look at this. 
- Resuscitate [ExternalCore](external-core) -- Do at the same time?

- Improve instance deriving: allow deriving Enum for more types, allow deriving Eq, Ord, Show for GADTs. -- Bjorn Bringert

- Hacking the build system (for example)

- Fix C-- (cmm) output?

- Low hanging optimizations?

- STGLint

- Rewrite the Evil Mangler in Haskell?

- GHC RTS in Cyclone?

- Work on GHC API, I would like to use it to provide feedback and interactivity to programmers

- Hook a multithreaded memory manager into the RTS

## Library Projects


- Work on the Streams library

- Replace GMP with OpenSSL mp library ([\#601](http://gitlabghc.nibbler/ghc/ghc/issues/601))

- A binding for your favourite C/C++ library...  (eg. GStreamer?)

## Cabal Projects


- Cabalisation of the GHC library ([\#654](http://gitlabghc.nibbler/ghc/ghc/issues/654))

- Put two solid days of work into getting cabal-get finished and stable.  Additionally, put as many packages into the DB as possible.

- Setting up GHC to build the base library using Cabal (since you have the GHC team, the Cabal team and the hmake team in one place!)

  - Isaac Jones - I can be available for general Cabal questions as well. 

- Hacking on Cabal-Get (not really a GHC thing!)

  - Isaac Jones 

## More Project Suggestions


- Generate Windows installers automatically from Cabal packages (or
  other OS-specific package format, eg. RPM)

- Any of the bugs (Ticket query: status: new, status: assigned,
  status: reopened, type: bug, order: priority), tasks (Ticket query:
  status: new, status: assigned, status: reopened, type: task, order: priority,
  group: difficulty), or feature requests (Ticket query: status: new,
  status: assigned, status: reopened, type: feature+request, order: priority).

- Guarantee that .hi files and .o files are consistent, possibly by means of some sort of digital signature that would cause a link-time failure

- A visualizer that would allow one to step forward and backward through the transformations that the compiler is doing (instead of crawling through endless outputs of `-ddump-*`).

- Get accurate identifier cross-reference information (out of the renamer?) and use it to generate an external file that can replace Emacs TAGS files with something more accurate (and that does not depend on hand-written type annotations).  (Norman Ramsey)
