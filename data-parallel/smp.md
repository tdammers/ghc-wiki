## Data parallelism on shared-memory machines



The implementation of NDP on shared-memory machines is based on Concurrent Haskell; in the following, a thread is always a Haskell thread. Parallel computations are executed by several threads (a *gang*), typically as many as there are processors or cores. The gang threads run continuously, waiting for work requests. Non-parallel code is executed in a separate thread (the main Haskell thread). Once it encounters a parallel computation, it passes work requests to the gang threads, such that each thread executes a part of the computation, blocks until the result becomes available and then resumes execution. For instance, `sumP [:1 .. n*n:] + 1` is roughly evaluated as follows:


```wiki
  Main thread                                 Gang threads
  -----------                                 ------------

  compute (m = n*n)                              sleep
      |                                            |
      |          request [:1 .. m:]                |
      +---------------------------------------->   |
      |                                            |  
    sleep                                    compute (xs = [:1 .. m:])
      |          yield xs                          |
      |  <-----------------------------------------+
      |                                            |
   wake up                                       sleep
      |          request (sumP xs)                 |
      +---------------------------------------->   |
      |                                            |
    sleep                                    compute (k = sumP xs)
      |          yield k                           |
      |  <-----------------------------------------+
      |                                            |
  compute (k+1)                                  sleep
```


This execution strategy is effectively equivalent to forking off a new gang for each parallel computation. It is more efficient than fork/join parallelism, however, as no new threads are forked during program execution.



The above example has 4 synchronisation points represented by arrows in the diagram: two for passing work requests to the gang threads and two for yielding the results. Note, however, that when the main thread receives `xs` from the gang it immediately passes it back as part of the `sumP xs` request. Similar situations arise quite frequently in functional programs which are often formulated as pipelines of (parallel, in our case) computations. Clearly, we would like to eliminate these superfluous synchronisation points and, indeed, our fusion framework allows us to do so automatically. After optimisation, the above example is evaluated as follows:


```wiki
  Main thread                                 Gang threads
  -----------                                 ------------

  compute (m = n*n)                              sleep
      |                                            |
      |          request (sumP [:1 .. m:])         |
      +---------------------------------------->   |
      |                                            |  
    sleep                                    compute (k = sumP [:1 .. m:])
      |          yield k                           |
      |  <-----------------------------------------+
      |                                            |
  compute (k+1)                                  sleep
```


The superfluous synchronisation points been eliminated; moreover, the resulting distributed computation (`sumP [:1 .. m:]`) can be optimised further, most importantly by performing sequential loop fusion.



Ultimately, we intend to implement a more flexible system which would allow gangs to be created dynamically. Unfortunately, it is not entirely clear how to specify what gang is to be used for a particular parallel computation, especially in the presence of laziness as the order of evaluation is usually hard to figure out in this case. More on this in [the discussion of NDP contexts](data-parallel/smp/ndp-contexts).


