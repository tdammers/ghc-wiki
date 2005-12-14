# Concurrent programming in GHC



This page contains notes and information about how to write concurrent programs in GHC.



Please feel free to add stuff here (login **guest**, password **guest**).


## Starting points


- **Basic concurrency: forkIO and MVars**.  Read [
  Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/marktoberdorf.ps.gz).
  The [original paper about Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz) contains quite a few examples about how to write concurrent programs.  A larger example is 


[
Writing High-Performance Server Applications in Haskell, Case Study: A Haskell Web Server](http://www.haskell.org/~simonmar/papers/web-server.ps.gz)


- **Software Transactional Memory** (STM) is a new way to coordinate concurrent threads. STM will be in GHC 6.6, and is described in the paper [
  Composable memory transactions](http://research.microsoft.com/~simonpj/papers/stm/index.htm).  The paper [
  Lock-free data structures using Software Transactional Memory in Haskell](http://research.microsoft.com/~simonpj/papers/stm/lock-free.htm) gives further examples of concurrent programming using STM.

- **Foreign function interface**.  If you are calling foreign functions in a concurrent program, you need to know about *bound threads*.  They are described in a Haskell workshop paper, [
  Extending the Haskell Foreign Function Interface with Concurrency](http://research.microsoft.com/~simonpj/Papers/conc-ffi/index.htm).

## Using concurrency in GHC


- You get access to concurrency operations by importing the library [Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html).

- The GHC manual gives a few useful flags that control scheduling (not usually necessary) [RTS options](http://www.haskell.org/ghc/docs/latest/html/users_guide/sec-using-parallel.html#parallel-rts-opts).

## Multiprocessor GHC



As of version 6.5, GHC supports running programs in parallel on an SMP or multi-core machine.  How to do it:


- You'll need to get a version of GHC that supports SMP.  Either download ghc from [CVS](http://www.haskell.org/ghc/docs/latest/html/building/sec-cvs.html) or use darcs: `darcs get --partial http://darcs.haskell.org/ghc`.  There are also [nightly snapshot distributions](http://www.haskell.org/ghc/dist/current/dist) available.


 


- All code currently has to be built using the `-smp` switch, including the libraries.  If you downloaded a binary snapshot, then you already have the required libraries.  If you build GHC from source, you need to add

  ```wiki
  GhcLibWays += s
  ```

  to the file `mk/build.mk` in the build tree before building.

- Compile your program with `-smp`


 


- Run the program with `+RTS -N2` to use 2 threads, for example.  You should use a `-N` value equal to the number of CPU cores on your machine (not including Hyper-threading cores).

- Concurrent threads (`forkIO` and `forkOS`) will run in parallel, and you can also use the `par` combinator and Strategies from the [Control.Parallel.Strategies](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Parallel-Strategies.html) module to create parallelism.

- Use `+RTS -sstderr` for timing stats.

## Links to related work on parallel and distributed Haskell (many based on GHC)


- [ Glasgow Parallel Haskell](http://www.macs.hw.ac.uk/~dsg/gph/)
- [ Glasgow Distributed Haskell](http://www.macs.hw.ac.uk/~dsg/gdh/)
- [
  http://www-i2.informatik.rwth-aachen.de/\~stolz/dhs/](http://www-i2.informatik.rwth-aachen.de/~stolz/dhs/)
- [
  http://www.informatik.uni-kiel.de/\~fhu/PUBLICATIONS/1999/ifl.html](http://www.informatik.uni-kiel.de/~fhu/PUBLICATIONS/1999/ifl.html)
