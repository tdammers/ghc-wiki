## Work plan for implementing Data Parallel Haskell \[OUT OF DATE\]


### Issues that need discussion and planning


- **Code blow up:** Even for Quickhull, the generated Core is too much admit any sensible attempt at optimising it.  *This is our main road block at the moment.*

- **Unlifted functions:** specify the exact behaviour of this optimisation and how the unliftedness of a named function is propagated across module boundaries.

### Milestones


1. Get Quickhull to run fast.
1. The system should be usable for small applications for the GHC 6.12 release.


 


### Task assignments


<table><tr><th>*Roman*</th>
<td>
**Code blow up**, **Generate boilerplate with TH**, **Explicit shape information** & **Recycling for joinD**
– status: partly implemented, but still needs serious work

- All segmented operations have been removed from the backend library.
- Roman currently adapts the vectoriser to make use of the separation of data and shape in the library.  This turned out to be more work than expected, but it should also simplify the vectoriser.
- We still don't have the code blow up under control.
- Before any further major changes to the library, Roman needs to first re-arrange things such that the library boilerplate is generated, instead of being hardcode; otherwise, changes require a lot of tiresome code editing.  This is partially done.

</td></tr></table>


<table><tr><th>*Simon*</th>
<td>
**New dictionary representation and optimisation**
– status: prototype shipped to Roman, seems to work.   I am doing detailed perf comparisons before committing to HEAD.  **NB:** We won't be able to make progress with benchmarks, before we can use the new inliner (currently we have no version that applies to the HEAD).
</td></tr></table>


<table><tr><th>*Gabi*</th>
<td>
**Regular multi-dimensional arrays (language design and implementation)** & **Hierarchical matrix representation**
– status: partially implemented benchmark; regular multi-dimensional arrays are in an experimental state
</td></tr></table>


<table><tr><th>*Manuel*</th>
<td>
**Desugaring comprehensions** & **Benchmark status**
– status: not started on comprehensions and waiting for code blow up to be improved before continuing with benchmarks
</td></tr></table>


### Open tasks



Category: *Bugs*


- [\#3577](http://gitlabghc.nibbler/ghc/ghc/issues/3577)
- [\#2984](http://gitlabghc.nibbler/ghc/ghc/issues/2984)


Category: *Efficiency, short term* (improve scalability and/or baseline performance of generated code):


- **Recycling for joinD:** Use Roman's recycling optimisation (PADL'09) to avoid copying in `joinD`.

- **Test new inliner:** Retest package dph with new inliner and the simplifier changes and try to simplify the library on the basis of these new phases.

- **Desugaring comprehensions:** The current desugaring of array comprehensions produces very inefficient code.  This needs to be improved.  In particular, the `map/crossMap` base case in `dotP` for `x<-xs` and use `zipWith` instead of `map/zip`.  Moreover, `[:n..m:]` is being desugared to `GHC.PArr.enumFromToP` - it needs to use the implementation from the current dph backend instead.

- **Regular multi-dimensional arrays (language design and implementation):** Representing regular arrays by nested arrays is generally not efficient, but due to the current lack of fusion for segmented arrays that produce multiple arrays of different length (e.g., `filterSU :: (a -> Bool) -> SUArr a -> SUArr a`), matters are worse than one might think (e.g., when using a hierarchical matrix representation).  Hence, we need direct support for regular, multi-dimensional arrays.

- **Explicit shape information:** All library internal functions should get shape information (including the lengths of flat arrays) as extra arguments; in particular, lengths shouldn't be stored in every level of a flattened data structure.  This should simplify the use of rewrite rules that are shape sensitive.

- **Split/join fusion for segmented arrays:** Needs to be redone completely, as it currently only works for very simple cases.

- **Unlifted functions:** For some scalar functions (especially numeric functions and functions operating on enumerations), it is easier to not lift them at all; rather than to lift them and then attempt to recover the original form with fusion and other optimisations.  An example is the `SumSq` benchmark, where we have `sumP (mapP (\x -> x * x) [:1..n:]`.  Here, we would rather not lift `\x -> x * x` at all.  Roman implemented a very simple form of this idea (which works for `SumSq`).  However, we would like this in a more general form, where named functions that remain unlifted are marked suitably, as clients of a function can only be unlifted if all functions it calls are already unlifted.  How much does that tie in with **Selective vectorisation**?


Category: *Efficiency, long term* (larger work items that will probably get more important with more complex applications):


- **Fusion for segmented arrays:** Some segmented functions (e.g., `filterSU :: (a -> Bool) -> SUArr a -> SUArr a`) produce multiple arrays of varying lengths.  We currently have no good story for fusing such operations.

- **Replicate:** Implement an extended array representation that uses an optimised representation for arrays that are the result of common forms of replication (i.e., due to free variables in lifted expressions).  The optimised representation stores the data to be replicated and the replication count(s) instead of actually replicating the data.  This also requires all functions consuming arrays to be adapted.  *Update:* Since we have `INLINE CONLIKE`, we can handle `replicate` using rules as long as it gets close to its consumer during simplification.  This makes the use of an extended array representation less important.  We will delay it until we run into examples where its lack bites us significantly.


Category:  *Compile time* (improve compile times):


- **Code blow up:** GHC generates a lot of intermediate code when vectorisation is enabled, leading to excessive compilation times.  It all appears to come down to the treatment of dictionary instances.  We need a plan for how to make progress here.


Category: *Ease of use* (make the system easier or more convenient to use for end users):


- **Conversion of vectorised representations:** We need other than just identity conversions between vanilla and vectorised data representations, especially `[:a:] <-> PArray a`.  This will make the system more convenient to use.

- **Selective vectorisation:** The scheme from our DAMP'08 paper that enables mixing vectorised and unvectorised code in a single module.

- **Unboxed values:** Extend vectorisation to handle unboxed values.

- **Prelude:** Extend vectorisation to the point, where it can compile the relevant pieces of the standard Prelude, so that we can remove the DPH-specific mini-Prelude.  (Requires: **Unboxed values**)


Category:  *Refactoring and similar* (improve compile times):


- **Generate boilerplate with TH:** Use Template Haskell to automate boilerplate generation.


Category: *Case studies* (benchmarks and example applications):


- **Hierarchical matrix representation:** Sparse matrices can be space-efficiently represented by recursively decomposing them into four quadrants.  Decomposition stops if a quadrant is smaller than a threshold or contains only zeros.  Multiplication of such matrices is straight forward using Strassen's divide-and-conquer scheme, which is popular for parallel implementations.  Other operations, such as transposing a matrix, can also be efficiently implemented.  The plan is to experiment with the implementation of some BLAS routines using this representation.

- **Benchmark status:** Once the code blow up problem is under control, update and complete those benchmarks on [DataParallel/BenchmarkStatus](data-parallel/benchmark-status) that didn't work earlier.

- **N-body:** Get a fully vectorised n-body code to run and scale well on LimitingFactor.


Category: *Infrastructure* (fiddling with GHC's build system and similar infrastructure):


- **Template Haskell:** Rewrite the library to generate the boilerplate that's currently hardcoded.

---


### Done


- **Integrate DPH into the new build system** . \[Done w/ help from Ian.\]

- **Template Haskell support for type families and INLINE pragmas** . \[Implemented.\]

- **More expressive rewrite rules:** It seems that with more expressive rewrite rules may enable us to handle many of the problems with replicate an friends using rules.  (At least when the proper inlining happens.)  Much of this is needed to optimise shape computations, which in turn enables subsequent optimisations. \[Further investigation suggested the following approach: We want to handle some functions (until their are inlined) like data constructors during rule matching, so that they also match rule arguments when they are bound to a variable.  This is, for example, important to optimise `repeat` in `smvm`.  (GHC now supports `INLINE CONLIKE` pragmas.)\]

- **Scaling:** Investigate the scaling problems that we are seeing with vectorised code at the moment.  (**Replicate** and **Recycling** play a role here, but it is unclear whether that's all.  Some benchmarks are simply memory-bandwidth limited.)  \[So far, we only found scaling problems due to memory bandwidth of the tested architecture.  Scaling on the Sun T2 is excellent.\]

- **DUE 9 March.** Poster for *Microsoft External Research Symposium*.  \[Submitted to MER.\]

- **Benchmark status:** Update and complete [DataParallel/BenchmarkStatus](data-parallel/benchmark-status); at the same time clean up the benchmark portion of the repo.  \[Completed a first sweep through this with updated benchmark results for SumSq, DotP, and SMVM, cleaned up code, and a much better idea of what the most important work items from now on are.\]

- **Template Haskell:** Arrange for package DPH to be build in stage2, so that we can use TH to generate library boilerplate.

- **New build system:** Evaluate whether the preview of the new build system looks like it is what we want.  \[The new build system seems fine and we should have no problem building package dph in stage2 either.\]

- **CoreToStg**: Compiling package dph with the HEAD currently results in `ASSERT failed! file stgSyn/CoreToStg.lhs line 239` (with a DEBUG compiler).
