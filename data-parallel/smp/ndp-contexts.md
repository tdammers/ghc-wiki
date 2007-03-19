## Data parallelism on shared-memory machines: NDP contexts



One goal for the integration of NDP into GHC is to find a scheme that can cooperate with other approaches to concurrency and parallelism.  Specifically, we envisage three "layers" of parallelism:


- Explicitly-forked threads, forked with `forkIO`, and coordinated with STM.  The semantics is non-deterministic.
- Semi-implicit parallelism, using `par` and `seq` annotations.  The semantics is deterministic.
- Nested data parallelism (the subject of this page).  The semantics is deterministic.


Applications can, then, use a mix of different kinds of parallelism at different levels and at different times during the execution.  This implies that we need to be able to vary the number of threads that execute data parallel operations dynamically.  More precisely, Haskell programs start up as usual with one thread executing `main`.    Any NDP operations encountered during single-threaded execution are run sequentially (but using flattened and unboxed data structures to achieve good sequential array performance).  More threads are devoted to executing NDP operations by expressions of the form `ndp p e`, which use `p` NDP threads to evaluate `e`.



This also raised issues about the ../DegreeOfEvaluation of values involving parallel arrays.


---



A couple of questions about the semantics of `ndp`. Suppose we have something like `ndp p $ f (ndp q x)`; i.e., we do some computations with `p` threads, then switch to `q` threads and then back to `p`.


1. During the switch `p ==> q`, do we (a) terminate `p` threads and create `q` new ones, (b) suspend `p` threads and create `q` new ones or (c) reuse the existing `p` threads for performing the inner computation?
1. Assuming the answer to the above is (c) and `p > q`, do we (a) terminate or (b) suspend the superfluous threads?
1. Assuming (c) again but this time with `p < q`, during the `q ==> p` switch, do we (a) terminate or (b) suspend the superfluous threads?


I think the right answer to 1 is (c). It is not quite trivial to implement, though. I'm not so sure about 2 and 3 as (a) is less efficient but (b) might leak threads in either case.



In any case, to simplify things I would suggest to have an RTS option, e.g. `--ndp-threads=N`; `ndp e` (note the missing `p`) then would evaluate `e` in `N` threads. This has a clear semantics, is much simpler to implement and is probably what people want most of the time. In a later version, we can easily add something like `ndpWith :: Int -> a -> a` (or even `NDPSettings -> a -> a` to let users specify other parameters such as heap size) which overrides the RTS settings.


