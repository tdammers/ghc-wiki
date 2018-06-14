# Data Parallel Haskell



This page documents the integration of nested data parallelism into GHC at the developer level, including notes about where we are and what needs doing.  See also the [
user-level wiki page](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell), which includes examples and tutorial-style instructions.



Most of the material describing our approach is partitioned into a set of subpages:


- [Nested data parallelism by example](data-parallel/example)
- [Data parallelism on shared-memory machines](data-parallel/smp)
- [High-level design of adding NDP to GHC](data-parallel/design)
- [Design of the DPH packages](data-parallel/library)
- [Desugaring of array comprehensions](data-parallel/desugaring)
- [The VECTORISE pragma](data-parallel/vect-pragma)
- [Preventing space blow-up due to replicate](data-parallel/replicate)
- [Other nested data parallel work](data-parallel/related)
- [Live Fusion: An alternative runtime fusion system (WIP)](data-parallel/live-fusion)


See also


- [ The Repa home page](http://repa.ouroborus.net)

## Status



Work on the DPH project stopped around 2010 at which point the implementation began to bit-rot. In June 2018 the implementation was removed from the GHC source tree.


## Old status and work plan



Detailed information on how to use the current implementation is at the [
user-level wiki page](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell).  Here is information on the implementation status and outstanding work items:


- DPH [repositories & developer builds](data-parallel/repositories)
- Current [benchmark status](data-parallel/benchmark-status)
- Release plan [DataParallel/Dec2010Release](data-parallel/dec2010-release)
- Our [work plan](data-parallel/work-plan) \[OUT OF DATE\]
- Some early [benchmarks](data-parallel/benchmarks) from 2007

## Old material



Pages that have fallen out of use (and contain out dated information):


- [Our approach to integrating an optimised treatment of multi-dimensional regular arrays](data-parallel/regular)
- [Details of the implementation of closure conversion](data-parallel/closure-conversion)
- [Our plan for implementing vectorisation on top of closure conversion](data-parallel/vectorisation)
- [Optimisation, and problems therewith](data-parallel/optimisation)
