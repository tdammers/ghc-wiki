


# Installing & Using the LLVM Back-end


## Installing



The LLVM backend is now included in GHC HEAD. Just grab the git HEAD version of GHC and build it. The backend now also supports all modes that GHC can be built in so you shouldn't need to change your build.mk file either.



For instructions on building GHC go [
here](http://hackage.haskell.org/trac/ghc/wiki/Building)


## LLVM Support


<table><tr><th> </th>
<th> **HEAD** </th>
<th> **8.6** </th>
<th> **8.4** </th>
<th> **8.2** </th>
<th> **8.0** </th>
<th> **7.10** </th>
<th> **7.8** </th>
<th> **7.6** </th>
<th> **7.4** </th>
<th> **7.2** </th>
<th> **7.0** 
</th></tr>
<tr><th> LLVM version </th>
<th> 6.0 </th>
<th> 5.0 </th>
<th> 3.9 </th>
<th> 3.7 </th>
<th> 3.5 (3.5.2 on ARM ([\#9920](http://gitlabghc.nibbler/ghc/ghc/issues/9920)) </th>
<th> ?? </th>
<th> ?? </th>
<th> ?? </th>
<th> 2.9 </th>
<th> 2.7 
</th>
<th></th></tr></table>



The above table lists the versions of the LLVM tools that are known to work with various GHC versions. These are not hard requirements in the sense that GHC will not immediately error if you use a different LLVM version than what is listed, but it is highly likely that using a different LLVM version can cause problems.



Simply install GHC and make sure the various llvm tools (opt, llc) are available on your path.


## Using



Once built you can check that you have the LLVM backend GHC will support these extra options:


- *-fllvm* - Compile code using the llvm backend
- *-pgmlo* - The program to use as the llvm optimiser
- *-pgmlc* - The program to use as the llvm compiler
- *-optlo* - Extra options to pass to the llvm optimiser
- *-optlc* - Extra options to pass to the llvm compiler
- *-ddump-llvm* - Dumps the llvm IR while compiling
- *-keep-llvm-files* - Keep a copy of the llvm intermediate file around

## Supported Platforms & Correctness


- Linux x86-32/x86-64: Currently well supported. The back-end can pass the test suite and build a working version of GHC (bootstrap test).
- Windows x86-32: Currently well supported. The back-end can pass the test suite and build a working version of GHC (bootstrap test).
- Mac OS X 10.5/10.6 (x86-32/x86-64): Currently well supported. The back-end can pass the test suite and bootstrap GHC. OS X has caused a lot more problems then Linux or Windows and does a few things slightly differently then them. It is quite stable these days though.
- ARM: Work is currently progressing to fully support GHC using the LLVM backend on ARM. You can see a blog with info about this [
  here](http://ghcarm.wordpress.com/).
- Other platforms haven't been tested at all.

## Shared Libraries



Shared libraries are supported on Linux x64 and Mac OSX x64. Other platforms aren't supported.


## Performance



(All done on linux/x86-32)



A quick summary of the results are that for the 'nofib' benchmark suite, the LLVM code generator was 3.8% slower than the NCG (the C code generator was 6.9% slower than the NCG). The DPH project includes a benchmark suite which I (David Terei) also ran and for this type of code using the LLVM back-end shortened the runtime by an average of 25% compared to the NCG. Also, while not included in my thesis paper as I ran out of time, I did do some benchmarking with the 'nobench' benchmark suite. It gave performance ratios for the back-ends of around:


<table><tr><th>NCG </th>
<th> 1.11
</th></tr>
<tr><th>C </th>
<th> 1.05
</th></tr>
<tr><th>LLVM </th>
<th> 1.14
</th></tr></table>



A nice demonstration of the improvements the LLVM back-end can bring to some code though can be see at [
http://donsbot.wordpress.com/2010/02/21/smoking-fast-haskell-code-using-ghcs-new-llvm-codegen/](http://donsbot.wordpress.com/2010/02/21/smoking-fast-haskell-code-using-ghcs-new-llvm-codegen/)


