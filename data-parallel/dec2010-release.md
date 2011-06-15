## Plan for the December 2010 Release (moved to mid 2011)


### To be released components


- GHC ~~7.0.2~~ 7.2
- DPH packages
- Repa packages


(New release of vector is also planned.)


### Before the release we must achieve the following



Documentation:


- Haddock documentation of Data.Array.Parallel **\[MANUEL\]**
- Replace `-XPArr` by `-XParallelArrays` in the Users Guide, also add `-fvectorise`
- HowTo and examples on HaWiki [
  http://haskell.org/haskellwiki/GHC/Data\_Parallel\_Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell) \[DONE\]


API adaptation: APIs of vector, Repa, and Accelerate should be unified as far as possible


- Repa's current 'replicate' should be renamed and a new 'replicate' that turns a scalar into an array should be introduced [???](data-parallel/dec2010-release?)
- Check similarity of singleton/unit between all three libraries [???](data-parallel/dec2010-release?)


Changes:


- -fdph-par should be the default (and sensible error message if the dph package is not available) (still needs to be passed explicitly) \[DONE\]
- -Odph should be equivalent to '-O2 -fsimplifier-phases=3 -fsimplifier-iterations=20' \[DONE\]
- Move GHC.PArr into  the DPH libs.  (Needed for Haddock.) **\[MANUEL\]**
- Find out if we still need the `NoSpecConstr` annotation and remove it if not **\[ROMAN\]**


Bug fixes:


- Vectoriser needs to be adapted to Simon's recursive superclasses patch; partially done, but recursive data types don't work yet \[DONE\]
- The combination '-fvectorise -O0' should work **\[ROMAN\]**
- Trying to vectorise the `DotP` example from the tutorial on the Haskell Wiki, `-fdph-seq` fails with (`-fdph-par` works fine)

  ```wiki
  *** Vectorisation error ***
      Tycon not vectorised:  Data.Array.Parallel.Lifted.PArray.PArray
  ```
- LLVM back end only partially working with DPH (held up due to LLVM backend problems in the HEAD [\#4838](http://gitlabghc.nibbler/ghc/ghc/issues/4838)); only affects BarnesHut \[BEN\]
- Repa edge-detection is deadlocking with more than 2 threads \[DONE\]
- Fix the BH seg fault in DPH. Roman has found the problem \[DONE\]


Performance goals:


- rl wrote "Alas, I just noticed that with -fdph-par, some of the examples seem to trigger LiberateCase a lot, resulting in absolutely ridiculous code blow-up."

  - rl fixed bounds checking related issues and needs to release a new vector (which needs to be pulled into the GHC vector repo) **\[ROMAN\]**
  - In addition, there is a problem with join points in the presence of errors (diverging computations) **\[ROMAN, SIMON\]**
- Vector works fast, sequentially, compared to C, Haskell mutable-array version \[FINE\]

  - Benchmarks: NoSlow, vector versions of Repa benchmarks
- Repa works fast in parallel

  - MMult \[OK, but about 20% slower than in 6.13; try with LLVM and w/o bounds checks\] \[BEN\]
  - Laplace (new stencil implementation) \[BEN\]
  - Blur \[OK\]
  - EdgeDetect \[OK\]
  - FFT \[OK\]
- Statically-nested DPH programs should work fast, in parallel

  - SumSquares \[FINE\]
  - Dot product \[FINE\]
  - Evens \[OK (but more than 3 times slower than C; any improvement since [\#4830](http://gitlabghc.nibbler/ghc/ghc/issues/4830) was fixed?)\]

    - rl reckons this is due to GHC compiling modulo of powers of two inefficiently; c.f., [\#3065](http://gitlabghc.nibbler/ghc/ghc/issues/3065) (in `packByTags`)
  - SMVM (blocked on optimisation of lifted indexing) \[Done\]
- Dynamically-nested DPH programs without user-defined datatypes should run correctly and scale, but absolute performance may be lacking

  - Quicksort \[BROKEN ([SpecConstr](spec-constr) loop when using `-dph-seq`) & SLOW\] \[SIMON & BEN\]
  - Quickhull \[OK\]
- Dynamically-nested DPH programs with user-defined datatypes should run correctly, but not necessarily fast

  - Words \[BROKEN ([SpecConstr](spec-constr) loop again, when using `-dph-seq`; same as Quicksort)\] \[ROMAN & SIMON\]

    - [\#4831](http://gitlabghc.nibbler/ghc/ghc/issues/4831)
  - BarnesHut. \[BROKEN: ROMAN\]


Legend


<table><tr><th>\[FINE\]</th>
<td>
Works well
</td></tr>
<tr><th>\[OK\]</th>
<td>
Fine for the release, but could be better
</td></tr>
<tr><th>\[SLOW\]</th>
<td>
Not usable
</td></tr></table>



Tags in **bold** require attention before the release.



More benchmarks details at [DataParallel/BenchmarkStatus](data-parallel/benchmark-status)


