# Status of DPH Benchmarks



**Benchmarking results last updated on**: 3nd December 2010.



This page gives an overview of how well the benchmarks in the [
dph-examples/](http://darcs.haskell.org/packages/dph/dph-examples) directory of package dph are currently working.



The benchmarks are run each night by [
DPH BuildBot](http://darcs.haskell.org/packages/dph/dph-buildbot). The results are posted to ghc-builds and uploaded to [
http://log.ouroborus.net/limitingfactor/dph/](http://log.ouroborus.net/limitingfactor/dph/). Check there for the latest numbers.





# Summary


- Running debug threaded programs with heap profiling triggers an assertion in the RTS. Running them without debugging is a segfault.
- QuickHull: vectorised.par.N1 version is 6x slower than the immutable Data.Vector version in absolute terms.
- QuickSort: vectorised.seq version doesn't compile due to a blow-up in SpecConstr.
- SMVM: Fusion doesn't work. Vectorised program 1000x slower than the C version with small matrix sizes. For 1000x1000 with 10% fill ratio it takes about 1s and allocates 400M memory, while the C program is instantaneous. For larger sizes it dies with OOM.
- BarnesHut: Builds but runs very slowly. The immediate problem is that some dictionaries are recursive, their methods don't inlined, so fusion doesn't work. Version with -fllvm takes 30 min to compile, suspect complexity problems in LLVM mangler.

# ToDo


- Benchmarks are currently being run with -fasm, and not via the LLVM backend. This will affect comparisons with C, but not with Data.Vector as it uses the same backend.




---


---


# Flat Parallelism



Flat parallel programs are ones in which parallel computations do not invoke further parallel computations. For Repa, this means that the value of each element in a given array can be computed independently of the others. These should run as fast as equivalent programs using immutable Data.Vector. We'd also hope to get close to the performance of C programs using equivalent algorithms, though this is a harder comparison due to differences in the back-end code generator.





<table><tr><th>[
MMult](http://code.haskell.org/repa/repa-head/repa-examples/MMult/)</th>
<td>
Matrix-Matrix multiplication. Size=1024x1024.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes**
> </th></tr>
> <tr><th> repa.mmult.c.seq </th>
> <th>  3.792s </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> A 
> </th></tr>
> <tr><th> repa.mmult.par.N1 </th>
> <th> 8.484s </th>
> <th> 0.45 </th>
> <th> 0.45 </th>
> <th> 
> </th></tr>
> <tr><th> repa.mmult.par.N4 </th>
> <th> 2.147s </th>
> <th> 1.77 </th>
> <th> 0.44 </th>
> <th> 
> </th></tr>
> <tr><th> repa.mmult.par.N8 </th>
> <th> 1.097s </th>
> <th> 3.46 </th>
> <th> 0.43 </th>
> <th> 
> </th></tr></table>
>
>

>
>
> A: Straightforward C program using triple nested loops. A cache-friendly block-based version would be faster.
>
>

>
>
> **Status:** Ok, but about 20% slower than in 6.13.
>
> **ToDo:** Run without bounds checking.
>
>




<table><tr><th>[
Laplace](http://code.haskell.org/repa/repa-head/repa-examples/Laplace/) **(SLOWLORIS)**</th>
<td>
Solves the Laplace equation in the 2D plane. Size=400x400.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> repa.laplace.c.seq </th>
> <th>  1.299s </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> A 
> </th></tr>
> <tr><th> repa.laplace.par.N1 </th>
> <th> 9.405s </th>
> <th> 0.14 </th>
> <th> 0.14 </th>
> <th> 
> </th></tr>
> <tr><th> repa.laplace.par.N4 </th>
> <th> 2.521s </th>
> <th> 0.51 </th>
> <th> 0.13 </th>
> <th> 
> </th></tr>
> <tr><th> repa.laplace.par.N8 </th>
> <th> 2.124s </th>
> <th> 0.61 </th>
> <th> 0.08 </th>
> <th> 
> </th></tr></table>
>
>
>
> A: Straightforward C program using triple nested loops. A cache-friendly block-based version would be faster.
>
>

>
>
> **Status:** Too slow. We should check this again with LLVM.
>
> **ToDo:** Run without bounds checking. Run with more threads to see if we can get back to the C version's run time.
>
>




<table><tr><th>[
Blur](http://code.haskell.org/repa/repa-head/repa-examples/Blur/)</th>
<td>
Applies a Gaussian blur filter to a 2D image. Size=512x512.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> repa.blur.par.N1 </th>
> <th> 2.620s </th>
> <th> 1 </th>
> <th> - </th>
> <th> 
> </th></tr>
> <tr><th> repa.blur.par.N4 </th>
> <th> 0.717s </th>
> <th> 3.65 </th>
> <th> - </th>
> <th> 
> </th></tr>
> <tr><th> repa.blur.par.N8 </th>
> <th> 0.414s </th>
> <th> 6.33 </th>
> <th> - </th>
> <th> 
> </th></tr></table>
>
>

>
>
> **ToDo:** Runs ok, but need other versions for comparison.
>
>




<table><tr><th>[
EdgeDetect](http://code.haskell.org/repa/repa-head/repa-examples/EdgeDetect/)</th>
<td>
Performs Canny edge detection to a 2D image. Size=512x512.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> repa.edgedetect.par.N1 </th>
> <th> 206ms </th>
> <th> 1 </th>
> <th> - </th>
> <th> 
> </th></tr>
> <tr><th> repa.edgedetect.par.N4 </th>
> <th> 79ms </th>
> <th> 2.6 </th>
> <th> - </th>
> <th> 
> </th></tr>
> <tr><th> repa.edgedetect.par.N8 </th>
> <th> 55ms </th>
> <th> 3.75 </th>
> <th> - </th>
> <th> 
> </th></tr></table>
>
>

>
>
> **ToDo:** Runs ok, but need other versions for comparison.
>
>




<table><tr><th>[
FFT](http://code.haskell.org/repa/repa-head/repa-examples/FFT/)</th>
<td>
Performs high-pass filtering using 2D and 3D FFTs. These are naive benchmarks used for regression testing only. They divide right down to (rank generalise) two-point vectors and construct the result using copying append. Using an inplace algorithm (like with FFTW) would be significantly faster.
</td></tr></table>


>
>
> **ToDo:** Runs ok, but need other versions for comparison.
>
>




# Statically Nested Parallelism



Statically nested parallelism is where the parallelism has a fixed, finite depth. For example ``mapP f (filterP g xs)``. Statically nested programs are easier to vectorise than dynamically nested programs. At present, single threaded statically nested programs should run as fast as equivalent Data.Vector programs. Parallel versions should display a good speedup.


<table><tr><th>[
SumSquares](http://darcs.haskell.org/packages/dph/dph-examples/imaginary/SumSquares/)</th>
<td>
Computes the sum of the squares from 1 to N using `Int`.  N = 100M.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes**
> </th></tr>
> <tr><th> dph.sumsq.vector.seq.N4 </th>
> <th>  404ms </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> 
> </th></tr>
> <tr><th> dph.sumsq.vectorised.seq.N4 </th>
> <th> 434ms </th>
> <th> 0.93 </th>
> <th>  </th>
> <th> 
> </th></tr>
> <tr><th> dph.sumsq.vectorised.par.N1 </th>
> <th> 443ms </th>
> <th> 0.91 </th>
> <th> 0.91 </th>
> <th> 
> </th></tr>
> <tr><th> dph.sumsq.vectorised.par.N2 </th>
> <th> 222ms </th>
> <th> 1.82 </th>
> <th> 0.91 </th>
> <th> 
> </th></tr>
> <tr><th> dph.sumsq.vectorised.par.N4 </th>
> <th> 111ms </th>
> <th> 3.63 </th>
> <th> 0.91 </th>
> <th> 
> </th></tr>
> <tr><th> dph.sumsq.c.seq </th>
> <th> 116ms </th>
> <th> 3.48 </th>
> <th> </th>
> <th> 
> </th></tr></table>
>
>

>
>
> **Status**: fine
>
>




<table><tr><th>[
DotProduct](http://darcs.haskell.org/packages/dph/dph-examples/imaginary/DotProduct)</th>
<td>
Computes the dot product of two vectors of `Double`s. N=10M.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> dph.dotp.vector.seq.N4 </th>
> <th>  68ms </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> 
> </th></tr>
> <tr><th> dph.dotp.vectorised.seq.N4 </th>
> <th> 58ms </th>
> <th> 1.17 </th>
> <th> </th>
> <th> A 
> </th></tr>
> <tr><th> dph.dotp.vectorised.par.N1 </th>
> <th> 55ms </th>
> <th> 1.24 </th>
> <th> 1.24 </th>
> <th> B 
> </th></tr>
> <tr><th> dph.dotp.vectorised.par.N2 </th>
> <th> 33ms </th>
> <th> 2.06 </th>
> <th> 1.03 </th>
> <th> 
> </th></tr>
> <tr><th> dph.dotp.vectorised.par.N4 </th>
> <th> 25ms </th>
> <th> 2.72 </th>
> <th> 0.68 </th>
> <th> 
> </th></tr>
> <tr><th> dph.dotp.c.seq </th>
> <th> 45ms </th>
> <th> 1.35 </th>
> <th> </th>
> <th> 
> </th></tr></table>
>
>


 


>
>
> A: The core for the vectorised.seq version is equivalent to the vector version. We expect the backend has compiled it differently. Check this again with LLVM.
>
> B: The vectorised.par version runs faster than vectorised.seq because the latter has a duplicate counter in the inner loop. We need a duplicate-loop-counter removal optimisation.
>
>

>
>
> **Status**: fine
>
>




<table><tr><th>[
Evens](http://darcs.haskell.org/libraries/dph/dph-examples/imaginary/Evens/)</th>
<td>
Takes the even valued `Int`s from a vector. N=10M.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> dph.evens.vector.seq.N4 </th>
> <th> 98ms </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> 
> </th></tr>
> <tr><th> dph.evens.vectorised.seq.N4 </th>
> <th> 174ms </th>
> <th> 0.56 </th>
> <th>  </th>
> <th> 
> </th></tr>
> <tr><th> dph.evens.vectorised.par.N1 </th>
> <th> 182ms </th>
> <th>  0.53 </th>
> <th> 0.56 </th>
> <th> 
> </th></tr>
> <tr><th> dph.evens.vectorised.par.N2 </th>
> <th> 106ms </th>
> <th>  0.92 </th>
> <th> 0.46 </th>
> <th> 
> </th></tr>
> <tr><th> dph.evens.vectorised.par.N4 </th>
> <th>  80ms </th>
> <th>  1.23 </th>
> <th> 0.30 </th>
> <th> A 
> </th></tr>
> <tr><th> dph.evens.c.seq </th>
> <th> 31ms </th>
> <th> 3.16 </th>
> <th> </th>
> <th> 
> </th></tr></table>
>
>

>
>
> A : Benchmark is totally memory bound, so we're not expecting to see much speedup. 
>  
>
>


   


>
>
> **Status**: ok, but run again with LLVM to see if that fixes the slowdown wrt C.
> - rl reckons the slowdown is due to GHC compiling modulo of powers of two inefficiently; c.f., [\#3065](http://gitlabghc.nibbler/ghc/ghc/issues/3065) (in `packByTags`)
>
>

>
>
>
>

<table><tr><th>[
SMVM](http://darcs.haskell.org/packages/dph/examples/smvm/) **(BROKEN)**</th>
<td>
Multiplies a dense vector with a sparse matrix represented in the *compressed sparse row format (CSR).* 

</td></tr></table>


>
>
> **Status:** Fusion doesn't work. Runs on 1000x1000 matrices with 10% fill ratio, but about 1000x slower than the C program. Dies with OOM for 2000x2000. Segfaults with 10000x10000.
>
>




# Dynamically Nested Parallelism



Dynamically nested programs have a recursive structure where each level of the recursion invokes more parallel computations. This is common for benchmarks that use divide-and-conquer style algorithms.


<table><tr><th>[
Primes](http://darcs.haskell.org/packages/dph/examples/primes/)</th>
<td>
The Sieve of Eratosthenes using parallel writes into a sieve structure represented as an array of `Bool`s.  
</td></tr></table>


>
>
> **Todo**: We currently don't have a proper parallel implementation of this benchmark, as we are missing a parallel version of default backpermute.  This needs a parallel update operation, but we currently can't guarantee atomic updates of compound types such as tuples.
>
>




<table><tr><th>[
QuickSort](http://darcs.haskell.org/libraries/dph/dph-examples/spectral/QuickSort/) **(BROKEN) (SLOWDOWN)**</th>
<td>
Sort a vector of doubles by recursively splitting it and sorting the two halves. This is a naive benchmark used for regression testing only. We divide right down to two-point vectors and construct the result using copying append. A production algorithm would switch to an in-place sort once the size of the vector reaches a few thousand elements. N=100k.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> dph.quicksort.vectorised.par.N1 </th>
> <th> 428ms </th>
> <th>  1 </th>
> <th> 1 </th>
> <th> 
> </th></tr>
> <tr><th> dph.quicksort.vectorised.par.N2 </th>
> <th> 417ms </th>
> <th>  1.02 </th>
> <th> </th>
> <th> 
> </th></tr>
> <tr><th> dph.quicksort.vectorised.par.N4 </th>
> <th> 422ms </th>
> <th>  1.01 </th>
> <th> </th>
> <th> 
> </th></tr></table>
>
>

>
>
> **Status**: Sequential vectorised version does not compile due to a loop in SpecConstr ([\#4831](http://gitlabghc.nibbler/ghc/ghc/issues/4831)).
>
>




<table><tr><th>[
Quickhull](http://darcs.haskell.org/libraries/dph/dph-examples/spectral/QuickHull/) **(SLOWLORIS)**</th>
<td>
Given a set of points in the plane, compute the sequence of points that encloses all points in the set. This benchmark is interesting as it is the simplest code that exploits the ability to implement divide-and-conquer algorithms with nested data parallelism. N=1M.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> dph.quickhull.vector-immutable.seq.N4 </th>
> <th> 0.166s </th>
> <th> 1 </th>
> <th> 1 </th>
> <th> 
> </th></tr>
> <tr><th> dph.quickhull.vectorised.seq.N4 </th>
> <th> 0.677s </th>
> <th>  0.24 </th>
> <th>  </th>
> <th> 4x slower 
> </th></tr>
> <tr><th> dph.quickhull.vectorised.par.N1 </th>
> <th> 1.033s </th>
> <th>  0.16 </th>
> <th> 0.16 </th>
> <th> 6x slower
> </th></tr>
> <tr><th> dph.quickhull.vectorised.par.N2 </th>
> <th> 0.686s </th>
> <th>  0.24 </th>
> <th> 0.12 </th>
> <th> 
> </th></tr>
> <tr><th> dph.quickhull.vectorised.par.N4 </th>
> <th> 0.571s </th>
> <th>  0.29 </th>
> <th> 0.07 </th>
> <th> 
> </th></tr>
> <tr><th> dph.quickhull.vector-mutable.seq.N4 </th>
> <th> 0.086s </th>
> <th>  1.93 </th>
> <th> </th>
> <th> A 
> </th></tr>
> <tr><th> dph.quickhull.vector-forkIO.par.N4 </th>
> <th> 0.064s </th>
> <th>  2.59 </th>
> <th> 0.65 </th>
> <th> B 
> </th></tr>
> <tr><th> dph.quickhull.c.seq </th>
> <th> 0.044s </th>
> <th> 3.77 </th>
> <th> </th>
> <th> C 
> </th></tr></table>
>
>

>
>
> A: Uses mutable Data.Vectors for intermediate buffers.
>
> B: Uses mutable Data.Vectors, forkIO and atomicModifyIORef. Concurrent threads fill a shared output vector. Code is uglier than the C version.
>
> C: Sequential C version with pre-allocated mutable intermediate buffers.
>
>

>
>
> **Status**: Benchmark scales but single threaded vectorised.par version is 6x slower than version using immutable Data.Vectors. QuickHull is based around filtering operations, so the fact that Evens is also slow is probably related.
>
>




# Dynamically Nested Parallelism with Algebraic Data Types



These programs also use user defined algebraic data types. Vectorization of these programs is still a work in progress.


<table><tr><th>[
Words](http://darcs.haskell.org/libraries/dph/dph-examples/imaginary/Words/) **(BROKEN)**</th>
<td>
Counts the number of words in a string. This is a naive divide-and-conquer benchmark that divides right down to a single character. A production program would switch to a simple sequential algorithm once the string chunks were small enough. It's a good stress test for the vectoriser though.
</td></tr></table>


>
>
> **Status**: Sequential vectorised version does not compile due to loop in SpecConstr ([\#4831](http://gitlabghc.nibbler/ghc/ghc/issues/4831)). LLVM versions take \>10 min to compile ([\#4838](http://gitlabghc.nibbler/ghc/ghc/issues/4838))
>
> **Todo**: Generate some larger test data. Right now it's just got a small test string baked into the program.
>
>




<table><tr><th>[
BarnesHut](http://darcs.haskell.org/libraries/dph/dph-examples/real/NBody/) **(BROKEN)** **(SLOWLORIS)**</th>
<td>
This benchmark implements the Barnes-Hut algorithm to solve the *n*-body problem in two dimensions. There is a naive O(n<sup>2</sup>) version in the same package.
</td></tr></table>


>
> <table><tr><th> **name** </th>
> <th> **runtime** </th>
> <th> **speedup** </th>
> <th> **efficiency** </th>
> <th> **notes** 
> </th></tr>
> <tr><th> dph.nbody.vector.seq.N4 </th>
> <th> 100ms </th>
> <th> 1 </th>
> <th> </th>
> <th> A 
> </th></tr>
> <tr><th> dph.nbody.vectorised.seq.N4 </th>
> <th> 4681ms </th>
> <th> \~50x slower   </th>
> <th>  </th>
> <th> 
> </th></tr>
> <tr><th> dph.nbody.vectorised.par.N1 </th>
> <th> 2381ms </th>
> <th>  \~25x slower </th>
> <th> </th>
> <th> 
> </th></tr></table>
>
>
>
> A : Time stated is end-to-end, not just for the kernel.
>
>

>
>
> **Status**:  -fasm vesions compile but fusion doesn't work so it's very slow. LLVM versions take 30min to compile ([\#4838](http://gitlabghc.nibbler/ghc/ghc/issues/4838)) 
>
> **ToDo**: Make the vectorised version give the same output as the vector version. The benchmark setup is a bit different. Fixing this won't cause a 50x speed difference though.
>
>




---


---


# Key



dph.\<*benchmark*\>.\<*version*\>.\<*parallelism*\>.\[*threads*\]

repa.\<*benchmark*\>.\[*version*\].\[*threads*\]



*version*


- *vectorised* means it's been through the DPH vectorising transform. 
- *vector* is a hand written version using immutable Data.Vectors
- *vector-mutable* is a hand written version using mutable Data.Vectors.
- *vector-immutable* means the same as *vector* and is used when there is also an mutable version.


*parallelism*


- Whether a benchmark is natively parallel or sequential. 
- Parallel versions are also run single threaded (with -N1) and sequential versions are also run with (-N4) so we get the parallel GC.
- Parallel versions with -N1 will tend to be slower than natively sequential versions due to overheads for supporting parallelism.


*threads*


- Value passed to Haskell Runtime with -N threads flag.
- Number of Haskell Execution Contexts (HECs) used when running the benchmark. 
- Can be less than the number of hardware threads / cores in the physical machine.


**speedup**


- Runtime of reference / runtime of benchmark.
- Measures how much faster a benchmark is relative to the reference.


(relative) **efficiency**


- Speedup / number of threads.
- Indicates the communication overhead involved with running something in parallel.
- Can be \> 1 if the parallel version running with a single thread is faster than the sequential reference version.


**Status:**


- **BROKEN**: Benchmark doesn't compile, or crashes when run.
- **SLOWDOWN**: Benchmark gets slower as number of threads increases. 
- **SLOWLORIS**: Benchmark scales as the number of threads increases, but the absolute performance is not acceptable compared with equivalent versions using immutable Data.Vectors. We do not have a setup in which the parallel version runs faster than the sequential reference version. Cute, but too slow to be useful.




# Benchmark machine


- 2x 3.0GHz Quad-Core Intel Xeon 5400
- 12MB (2x6MB) on-die L2 cache per processor
- independent 1.6GHz frontside bus per processor
- 800MHz DDR2 FB-DIMM
- 256-bit-wide memory architecture
- Mac OS X Server 10.5.6
