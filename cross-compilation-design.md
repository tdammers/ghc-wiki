
There are several people now interested in working on turning GHC into a Real Cross Compiler (something akin to the design laid out [here](cross-compilation).) This page covers that WIP, as well as architectural/design notes.


# Current work


- Brian Bloniarz implemented a cross compilation mode for hsc2hs. You can find the patches [
  here](http://www.haskell.org/pipermail/cvs-ghc/2010-December/058508.html). These have yet to be merged by GHC HQ, but we would like them to be!
- Dan Knapp is working on the native code generators - removing everything that uses the pre-processor and instead threading an `Architecture` type through the compiler, containing ABI information so we can generate the correct code at runtime. You can check out his work by doing

  ```wiki
  darcs get http://dankna.com/software/darcs/ghc-darcs-dnk-cross-compilation/
  ```

# Other things worth considering/need doing/questions


- The build system. This is the Big Mother - the design is laid out [here](building/architecture). There are various things worth considering, like the fact that GHC builds programs as part of the build routine which it then uses, etc.
- Will the RTS need modifications? We must build it multiple times with both the host and target toolchain C compilers, so it should mostly be OK, but we may need to do more

# Design notes



TODO FIXME


