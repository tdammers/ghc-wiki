
This page collects information about the performance of GHC and GHC-compiled code. We're interested in programs that GHC compiles slowly, programs that run slowly (especially small ones), and ideas for improving performance of either GHC itself or GHC-compiled code.


# Performance of GHC



The Todo list for GHC performance:


- Emit the interface immediately after typechecking in non-optimised compilation
- Turn on the NCG by default for -O (possibly improve x87 float code first?)
- Stringbuffer could use mmap()
- Improve performance of generating output (SimonMarlow: done).
- Profile & do a space-leak sweep (SimonMarlow: done, fixed one space leak, space usage not great but could be worse).
- Improve binary interface representation (SimonMarlow: some improvement here).


Half-baked ideas:


- glom the interfaces for a package into a single uber-interface with one string table.
- Classic loop optimizations that are not invariant hoisting. (Jan-Willem Maessen)

## Modules that GHC compiles slowly



Simon PJ found a case of non-linearity in the simplifier; we are currently testing a patch that hopefully fixes all of the following:


- Language.Haskell.Syntax
- large constant structures taking non-linear time to compile
- large Happy-generated grammars
- WashNGo-2.4.6/WASH/HTMLMonad98.hs from WASH/CGI (SimonMarlow: investigating)
- dsrun011 from the test suite (SimonMarlow: fixed)
- record with lots of fields (20+), deriving Show, with -O (SimonMarlow: fixed)

# Performance of compiled code


## Programs that run too slowly


- Various of the shootout benchmarks (Todo: expand).

## Performance Todo list



Here we list some problems that we know about, and/or ideas for improving things.


- x86\_64: use more registers (the NCG needs to save/restore volatile regs over C calls, apart from that we're there).
- via-C produces code with too many jumps, especially with later versions of GCC. GCC commons up identical basic blocks containing a single jump instruction.
- GC has been reported to behave non-linearly [
  http://www.haskell.org//pipermail/glasgow-haskell-users/2005-April/008301.html](http://www.haskell.org//pipermail/glasgow-haskell-users/2005-April/008301.html) (SimonMarlow: turned out to be caused by swapping)
- Space leak: a function that is statically known to not use some or all of its arguments should have a function descriptor that reflects the unused pointers as non-pointers, so that a PAP applied to unused args doesn't retain them.
- Push heap check into a case branch if that means we can avoid a heap check in a tail-recursive loop.
- Make ticky-ticky profiling work again.
- Make the NCG handle loops, and perform some simple loop optimisations.
- Better optimisation pass sequences? (c.f. Laszlo Nemeth's research [
  http://www.tcs.informatik.uni-muenchen.de/\~hwloidl/TFP04/Abstracts/17.html](http://www.tcs.informatik.uni-muenchen.de/~hwloidl/TFP04/Abstracts/17.html))
- Can we do anything about Double alignment?
- better GC for chains of THUNK\_SELECTORs (see comments in GC.c)
- Int64 reported to be slow [
  http://www.haskell.org//pipermail/glasgow-haskell-users/2005-June/008574.html](http://www.haskell.org//pipermail/glasgow-haskell-users/2005-June/008574.html) (Lemmih: Fixed. It's now only three times slower to use Int64 over Int in that example. (patch not commited yet)).
- System.Random could be optimized more.


A few slightly larger projects:


- parallel GC for multi-processors
- Allow strict enumerations to be "unboxed"


Some more open-ended ideas:


- Strict or unlifted types, or strictness annotations on functions
- How about an anti-strictness annotation? Value is always re-evaluated when used?
- Can GC performance be improved at all? Perhaps: a combination of depth-first and breadth-first traversal instead of just breadth-first. (SimonMarlow: attempted, no joy. But managed to find some other opportunities for improvement in the GC while I was there.)
- improve compacting GC performance (ask SimonMarlow for ideas)
- Whole program optimisation infrastructure. Dump intermediate representation into a special section of the .o files (or use another file) and upon linking the program/library run another optimisation pass over the entire lot before finally generating c/cmm. Only enabled with -O2/-O3 or something.

# Size of compiled code



List modules or programs that produce code that is too large:



The Todo list for code size:


- Implement dynamic libraries on other platforms: x86/Linux, x86\_64/Linux, x86/\*BSD, x86\_64/\*BSD.
- Find common code fragments and share them
- Top level CSE of lambdas module alpha equivalence. This come up from time to time when inlining occurs. It doesn't look like it gets done, but I could be missing the pass. (Jan-Willem Maessen)
