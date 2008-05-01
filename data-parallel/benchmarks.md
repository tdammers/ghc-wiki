## First NDP benchmarks (2007)


### Sparse matrix vector multiplication



This benchmark is explained it much detail in [
Data Parallel Haskell: a status report](http://www.cse.unsw.edu.au/~chak/papers/CLPKM07.html).  Runtimes comparing to sequential C code on Intel Xeon (x86) and Sun SunFire9600 (Sparc) are in [time-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/time-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/time-colour.png).  The parallel Haskell code is more efficient from 2 PEs for the SunFire and from 4 PEs for the Xeon processors.  We blame the low sequential performance for the Xeon on the lack of effort that has been put into generating good straight-line code (both in the NCG and when compiling via C), this includes inadequate register allocation and lack of low-level optimisations.



The speedup for the Xeon box and the SunFire are in [speedup-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/speedup-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/speedup-colour.png) and the speedup for our 8x dualcore Opteron NUMA box is in [serenity-all-speedup-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/serenity-all-speedup-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/serenity-all-speedup-colour.png).  The speedup of on the NUMA machine is limited by the memory bandwidth for smvm.  When we only use one core per CPU, the benchmark scales much better.  Moreover, the memory traffic/compute ratio is slightly more favourable when processing arrays of `Float`s than when processing arrays of `Double`s.


### Connected components in undirected graphs



This is based on [
http://www.cs.cmu.edu/\~scandal/nesl/algorithms.html\#concomp](http://www.cs.cmu.edu/~scandal/nesl/algorithms.html#concomp). Currently, two of the tree algorithms described there are implemented: 
Awerbuch-Shiloach and the hybrid algorithm. The random mate algorithm needs a data-parallel random number genenerator and is left for later.



The algorithms are interesting because they actually don't do a lot of computations; they mostly filter and copy edges. Thus, it is perhaps not unreasonable to expect that if they scale well, then so will more computation-intensive ones. Also, they should make any inefficiencies introduced by missed fusion opportunities quite obvious.



Both algorithms take the number of nodes (`n :: Int`) and an array of edges (`es :: Int :*: Int`) and yield an array of `Int`s of length `n` where each node is assigned a number between `0` and `n-1`. Nodes which are assigned the same number are connected. The algorithms are described and compared in [
http://citeseer.ifi.unizh.ch/greiner94comparison.html](http://citeseer.ifi.unizh.ch/greiner94comparison.html). At the moment, I have only benchmarked them for a random graph with 1000000 nodes and 40000 edges; eventually, I'll add benchmarks for other kinds of graphs described in the paper. The benchmarks have been run on a dual Intel Xeon 2.8 GHz with two cores per processor which effectively gives us 4 processors overall.



The parallel versions are currently very much slower than the sequential ones, particularly so for Awerbuch-Shiloach. This is because fusion doesn't work for the parallel algorithms at the moment.



No benchmarks on 4 processors yet, as the 4th PE is currently busy.


#### Awerbuch-Shiloach



The sequential code is in [
http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/AwShU.hs](http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/AwShU.hs) and the parallel in [
http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/AwShUP.hs](http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/AwShUP.hs).


<table><tr><th> **Version** </th>
<th> **Threads** </th>
<th> **Time (ms)** </th>
<th> **Speedup** 
</th></tr>
<tr><th> sequential    </th>
<th>               </th>
<th>        1600     </th>
<th>               
</th></tr>
<tr><th> parallel      </th>
<th>       1       </th>
<th>       29800     </th>
<th>               
</th></tr>
<tr><th>               </th>
<th>       2       </th>
<th>       16800     </th>
<th>      1.8      
</th></tr>
<tr><th>               </th>
<th>       3       </th>
<th>       12800     </th>
<th>      2.3      
</th></tr>
<tr><th>               </th>
<th>       4       </th>
<th>       *???*   </th>
<th>      *???*  
</th></tr></table>


#### Hybrid



The sequential code is in [
http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/HybU.hs](http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/HybU.hs) and the parallel in [
http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/HybUP.hs](http://darcs.haskell.org/packages/ndp/Data/Array/Parallel/test/nesl/concomp/HybUP.hs).


<table><tr><th> **Version** </th>
<th> **Threads** </th>
<th> **Time (ms)** </th>
<th> **Speedup** 
</th></tr>
<tr><th> sequential    </th>
<th>               </th>
<th>       1850      </th>
<th>               
</th></tr>
<tr><th> parallel      </th>
<th>       1       </th>
<th>       7450      </th>
<th>               
</th></tr>
<tr><th>               </th>
<th>       2       </th>
<th>       4600      </th>
<th>      1.6      
</th></tr>
<tr><th>               </th>
<th>       3       </th>
<th>       3800      </th>
<th>      2.0      
</th></tr>
<tr><th>               </th>
<th>       4       </th>
<th>       *???*   </th>
<th>      *???*  
</th></tr></table>



I haven't completely parallelised this one yet (it's only a matter of implementing some parallel combinators).


