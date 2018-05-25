# The NoFib Benchmark Suite



The NoFib benchmark suite is a collection of (mostly old) Haskell programs that we use for benchmarking GHC. 



Trac [\#5793](http://gitlabghc.nibbler/ghc/ghc/issues/5793) is about improving NoFib.


## Prerequisites



Running NoFib requires the `time` utility to be present on the system path. On Ubuntu:


```wiki
  $ apt-get install time
```


To get a local copy of NoFib:


```wiki
  $ git clone http://git.haskell.org/nofib.git
```

## Benchmarking



Firstly, the `nofib-analyse` program requires the `html` and `regex-compat` cabal packages be installed:


```wiki
  $ cabal install html regex-compat
```


Then, to run the tests execute:



CPU frequency scaling can result in distorted measurement! Try temporarily disabling frequency scaling during `nofib` runs.


```wiki
  $ cd nofib
  $ make clean
  $ make boot                  # Generates Makefile dependencies
  $ make 2>&1 | tee nofib-log  # Compiles and runs benchmarks one by one
```


will put the results in the file `nofib-log`. 



If you encounter build errors ("Could not find module QSort") although `make` seems to work for many benchmarks, make sure that you did `make boot`. This will generate the necessary [
Makefile dependencies](http://ghc.readthedocs.io/en/latest/separate_compilation.html#dependency-generation), otherwise almost anything of the `spectral` and later suites won't build.



You can run single benchmarks by running `make` within their folder, or equivalently, by using `make`s `-C` option for changing the path.



Should you want to debug or enhance the benchmark harness, look into `mk/boilerplate.mk` which in turn calls `runstdtest/runstdtest.prl`, which generates the benchmark harness. Note that you have to at least clean and rebuild the harness if you change it (`make -C runstdtest/ clean all`), otherwise the old harness will continue to be used.


```wiki
  $ make -C shootout/fasta NoFibRuns=30  # runs the fasta benchmarks 30 times
  $ cd shootout/fasta
  $ make NoFibRuns=30                    # dito
```


To compare the results of multiple runs, use the program
[
nofib/nofib-analyse](https://git.haskell.org/nofib.git/tree/HEAD:/nofib-analyse).  Something like this:


```wiki
  $ nofib-analyse nofib-log-6.4.2 nofib-log-6.6
```


to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`.  When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you *wanted* to change.  There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more.  To be on the safe
side, make both runs on the same unloaded machine.



To get measurements for simulated instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of 
[ Valgrind](http://valgrind.org). You can run nofib under valgrind like this:


```wiki
  $ make EXTRA_RUNTEST_OPTS=-cachegrind
```

## Complete recipe


```wiki
cd nofib
make clean && make boot && make -k 2>&1 | tee log1
make clean && make boot && make -k EXTRA_HC_OPTS=-fenable-cool-optimisation 2>&1 | tee log2
nofib-analyse/nofib-analyse log1 log2
```


The output of the nofib-analyse tool is quite readable, with two provisos:


- Missing values in the output typically mean that the benchmark crashed and may indicate a problem with your optimisation
- If a difference between the two modes is displayed as an absolute quantity instead of a percentage, it means that the difference was below the threshold at which the analyser considers it significant


If the comparison identifies any particularly bad benchmark results, you can run them individually by changing into their directory and running something like:


```wiki
EXTRA_HC_OPTS="-fenable-cool-optimisation -ddump-simpl" make
```


You can add whatever dumping flags you need to see the output and understand what is going wrong.



Some tests may require packages that are not in the ghc tree. You can add these to the inplace package database (inplace/lib/package.conf.d) using cabal. For example you can install parsec using the inplace compiler and inplace package database by running the following command from the top-level of the GHC source tree:


```wiki
cabal install parsec --with-compiler=inplace/bin/ghc-stage2 --package-db=inplace/lib/package.conf.d
```


To run the parallel benchmarks with some number of cores, you need to compile the parallel benchmarks with the -threaded option and also pass the -N RTS argument; for example, the following runs the parallel benchmarks with 4 cores (run this from the parallel directory):


```wiki
make clean
make EXTRA_HC_OPTS="-threaded" EXTRA_RUNTEST_OPTS='+RTS -N4 -RTS'
```

## Tweaking things



To tweak things, add settings to your `mk/build.mk` (see [Commentary/SourceTree](commentary/source-tree)).


- By default nofib uses the stage-2 compiler from your build tree.  To tell nofib to use a different compiler, set `HC`.  For example:

  ```wiki
  make WithNofibHc=/home/simonpj/builds/HEAD/inplace/bin/ghc-stage1 2>&1 | tee log-stage1
  ```

- Many nofib programs have up to three test data sets. The `mode` variable tells the system which to use, thus:

  ```wiki
  make boot mode=slow && make -k mode=slow
  make boot mode=norm && make -k mode=norm
  make boot mode=fast && make -k mode=fast
  ```

  See `mk/opts.mk`. The default is `mode=norm`.

- If you just run it to get allocation numbers, or some debug output (ticky, `-ddump-simpl`), you can use `make NoFibRuns=1` to run the program only once.

## Other tips on measuring performance



It is often not necessary (or even useful) to do a full nofib run to assess performance changes. For example, you can tell whether compilation time has consistently increased by compiling a single file - a large one, and preferably not one of the perf tests 
because those contain repeated patterns and aren't indicative of typical code.  You can use [nofib/spectral/simple/Main.hs](/trac/ghc/browser/ghc/nofib/spectral/simple/Main.hs) for this purpose.


### Measuring backend performance



To get some insights into changes to optimisations in the backend you can compile all the programs in `codeGen/should_run` both ways (unmodified GHC HEAD and GHC HEAD + some changes that are being tested), and then compare the sizes of the corresponding object files.  Then investigate differences manually - this is a great way to get some insight into whether your optimisation is doing what you want it to do, and whether it has any unexpected consequences.  As an example, the sinking pass in the Cmm pipeline is the result of iterating this process many times until most of the cases of bad code generation had been squashed.  When you're satisfied that the optimisation is doing something sensible on these small examples, then move onto nofib and larger benchmarks.


