# Running the Testsuite



This section gives information on how to use the testsuite. Note that the testsuite driver requires Python 3.



The commands on this page can all be executed from the `testsuite` directory.


## Speed settings



You can run `make fasttest`, `make test` (which uses the default speed settings, and is the same as just `make`) or `make slowtest`.



This table shows the differences between these three speed settings.


<table><tr><th> make </th>
<th> how many tests </th>
<th> how many ways </th>
<th> used by whom 
</th></tr>
<tr><th> fasttest   </th>
<th> some </th>
<th> 1   </th>
<th> Travis (to stay within time limit) 
</th></tr>
<tr><th> test   </th>
<th> all  </th>
<th> 1   </th>
<th> Phabricator (slow takes too long?) 
</th></tr>
<tr><th> slowtest   </th>
<th> all  </th>
<th> all </th>
<th> Nightly (slow is ok) 
</th></tr></table>



Note: `make slow` is GHC \>= 7.11 only.



See also `Note [validate and testsuite speed]` in the toplevel `Makefile`.


## Commonly used options



You can run the testsuite in parallel to speed it up:


```wiki
$ make THREADS=4
```


By default the testsuite uses the stage2 compiler. If you want to use another stage then:


```wiki
$ make stage=1
```


To run the testsuite against a different GHC, say ghc-5.04 (this assumes the name specified corresponds to an executable on your path):


```wiki
$ make TEST_HC=ghc-5.04
```


To run an individual test or tests (eg. tc054):


```wiki
$ make TEST=tc054
```


To run only performance tests:


```wiki
$ make ONLY_PERF_TESTS=YES
```


To run everything but the performance tests:


```wiki
$ make SKIP_PERF_TESTS=YES
```


To make this even faster, you can also go straight to the subdirectory containing the test (e.g ./tests/typecheck/should\_compile/) and say 'make TEST=tc054' from there, which will save some time as the testsuite framework won't need to search as long to find the test you are referring to.



To run several tests, you just space separate them:


```wiki
$ make TEST="tc053 tc054"
```


You can also run a whole group of related tests by changing to a subdirectory in the test cases tree:


```wiki
$ cd ./tests/array
$ make
```


To run the tests one particular way only (eg. GHCi):


```wiki
$ make WAY=ghci
```


The testsuite ways are defined in `testsuite/config/ghc`.



To add specific options to the compiler:


```wiki
$ make EXTRA_HC_OPTS='+RTS -K32M -RTS' 
```


By default tests clean up after themselves. If you want to keep the temporary files that tests generate, you can run:


```wiki
$ make CLEANUP=0
```

## Additional Packages



Some of the tests in the testsuite rely on packages that aren't part of the standard libraries included with GHC (grep for `reqlib`). These tests will be skipped then unless you [install](debugging/installing-packages-inplace) the required packages for them. Since we don't run these tests very often, some might be currently broken.



The extra packages are:


- hmatrix
- mtl
- parallel
- parsec
- primitive
- QuickCheck
- random
- regex-compat
- syb
- stm
- utf8-string
- vector


These can be easily installed from the root of the source tree using,


```wiki
cabal install --with-compiler=`pwd`/inplace/bin/ghc-stage2 --package-db=`pwd`/inplace/lib/package.conf.d mtl parallel parsec primitive QuickCheck random regex-compat syb stm utf8-string vector
```


You may need to add `--disable-library-profiling` if your GHC tree is built without the profiling way. If version bounds are problematic you may also need to add `--allow-newer`.


## Problems running the testsuite



If the testsuite fails mysteriously, make sure that the timeout utility is working properly. This Haskell utility is compiled with the stage 1 compiler and invoked by the python driver, which does not print a nice error report if the utility fails. This can happen if, for example, the compiler produces bogus binaries. A workaround is to compile timeout with a stable ghc.


### Why are my tests being skipped?



There are a few possible explanations for a test being skipped run:


- You are missing libraries required by the test (see the "Additional Packages" section above)
- The test is a performance test yet the compiler was built with `-DDEBUG` (perhaps the tree was built with `./validate --slow` or with `BuildFlavour = devel2` in `mk/build.mk`?)
- There was an error evaluating a `.T` file; see the output from the testsuite driver's initialization phase for hints.
