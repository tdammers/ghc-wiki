# Performance of programs compiled with GHC



Here is where we track various on-going efforts to improve the runtime performance of code produced by GHC. If you are interested in the performance of the compiler itself, see [Performance/Compiler](performance/compiler).


## Relevant tickets


- [\#10992](http://gitlabghc.nibbler/ghc/ghc/issues/10992): `Data.List.sum` is much slower than the naive recursive definition for it.  Does not happen in 7.8.
- [\#6166](http://gitlabghc.nibbler/ghc/ghc/issues/6166): An alleged runtime performance regression in `mwc-random`.

- [\#14980](http://gitlabghc.nibbler/ghc/ghc/issues/14980) (regressed in 8.4): Runtime performance regression with binary operations on vectors 


Identify tickets by using "Runtime performance bug" for the "Type of failure field".



**Open Tickets:**

<table><tr><th>[\#16064](http://gitlabghc.nibbler/ghc/ghc/issues/16064)</th>
<td>Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code</td></tr>
<tr><th>[\#16040](http://gitlabghc.nibbler/ghc/ghc/issues/16040)</th>
<td>Unboxing-Related Performance Issue with Polymorphic Functions</td></tr>
<tr><th>[\#16004](http://gitlabghc.nibbler/ghc/ghc/issues/16004)</th>
<td>Vector performance regression in GHC 8.6</td></tr>
<tr><th>[\#15969](http://gitlabghc.nibbler/ghc/ghc/issues/15969)</th>
<td>Generic1 deriving should use more coercions</td></tr>
<tr><th>[\#15842](http://gitlabghc.nibbler/ghc/ghc/issues/15842)</th>
<td>Exponentiation needs PrelRules</td></tr>
<tr><th>[\#15731](http://gitlabghc.nibbler/ghc/ghc/issues/15731)</th>
<td>Add sortOn/coerce rule</td></tr>
<tr><th>[\#15727](http://gitlabghc.nibbler/ghc/ghc/issues/15727)</th>
<td>bug: all generations are collected sequentially when compacting collection kicks in</td></tr>
<tr><th>[\#15717](http://gitlabghc.nibbler/ghc/ghc/issues/15717)</th>
<td>Performance regression in for\_ alternatives from GHC 8.2.2 to newer GHCs</td></tr>
<tr><th>[\#15652](http://gitlabghc.nibbler/ghc/ghc/issues/15652)</th>
<td>SerializedCompact has a \[(Ptr a, Word)\] instead of a custom datatype</td></tr>
<tr><th>[\#15642](http://gitlabghc.nibbler/ghc/ghc/issues/15642)</th>
<td>Improve the worst case performance of weak pointers</td></tr>
<tr><th>[\#15620](http://gitlabghc.nibbler/ghc/ghc/issues/15620)</th>
<td>Speed up Data.Unique</td></tr>
<tr><th>[\#15574](http://gitlabghc.nibbler/ghc/ghc/issues/15574)</th>
<td>C wrappers for Haskell foreign exports don't have finalizers (causes memory leak).</td></tr>
<tr><th>[\#15524](http://gitlabghc.nibbler/ghc/ghc/issues/15524)</th>
<td>Performance regression when using the GHC API to evaluate code compared to 8.4</td></tr>
<tr><th>[\#15503](http://gitlabghc.nibbler/ghc/ghc/issues/15503)</th>
<td>interpreter: sequence\_ (replicate 100000000 (return ()))  gobbles up memory</td></tr>
<tr><th>[\#15366](http://gitlabghc.nibbler/ghc/ghc/issues/15366)</th>
<td>GHC.Conc.Windows has a surprising queue</td></tr>
<tr><th>[\#15227](http://gitlabghc.nibbler/ghc/ghc/issues/15227)</th>
<td>Add PrelRules for par\#</td></tr>
<tr><th>[\#15185](http://gitlabghc.nibbler/ghc/ghc/issues/15185)</th>
<td>Enum instance for IntX / WordX are inefficient</td></tr>
<tr><th>[\#15176](http://gitlabghc.nibbler/ghc/ghc/issues/15176)</th>
<td>Superclass \`Monad m =\>\` makes program run 100 times slower</td></tr>
<tr><th>[\#15153](http://gitlabghc.nibbler/ghc/ghc/issues/15153)</th>
<td>GHC uses O\_NONBLOCK on regular files, which has no effect, and blocks the runtime</td></tr>
<tr><th>[\#15127](http://gitlabghc.nibbler/ghc/ghc/issues/15127)</th>
<td>Unbox around runRW\#</td></tr>
<tr><th>[\#14980](http://gitlabghc.nibbler/ghc/ghc/issues/14980)</th>
<td>Runtime performance regression with binary operations on vectors</td></tr>
<tr><th>[\#14941](http://gitlabghc.nibbler/ghc/ghc/issues/14941)</th>
<td>Switching direct type family application to EqPred (\~) prevents inlining in code using vector (10x slowdown)</td></tr>
<tr><th>[\#14929](http://gitlabghc.nibbler/ghc/ghc/issues/14929)</th>
<td>Program compiled with -O2 exhibits much worse performance</td></tr>
<tr><th>[\#14870](http://gitlabghc.nibbler/ghc/ghc/issues/14870)</th>
<td>Runtime performance regression in 8.4</td></tr>
<tr><th>[\#14827](http://gitlabghc.nibbler/ghc/ghc/issues/14827)</th>
<td>Recognize when inlining would create a join point</td></tr>
<tr><th>[\#14816](http://gitlabghc.nibbler/ghc/ghc/issues/14816)</th>
<td>Missed Called Arity opportunity?</td></tr>
<tr><th>[\#14797](http://gitlabghc.nibbler/ghc/ghc/issues/14797)</th>
<td>High-residency modules during GHC build</td></tr>
<tr><th>[\#14789](http://gitlabghc.nibbler/ghc/ghc/issues/14789)</th>
<td>GHCi fails to garbage collect declaration \`l = length \[1..10\^8\]\` entered at prompt</td></tr>
<tr><th>[\#14762](http://gitlabghc.nibbler/ghc/ghc/issues/14762)</th>
<td>Foreign.Marshal.Pool functions use inefficient O(n) operations</td></tr>
<tr><th>[\#14727](http://gitlabghc.nibbler/ghc/ghc/issues/14727)</th>
<td>Unboxed sum performance surprisingly poor</td></tr>
<tr><th>[\#14620](http://gitlabghc.nibbler/ghc/ghc/issues/14620)</th>
<td>Polymorphic functions not easily recognized as join points</td></tr>
<tr><th>[\#14610](http://gitlabghc.nibbler/ghc/ghc/issues/14610)</th>
<td>newtype wrapping of a monadic stack kills performance</td></tr>
<tr><th>[\#14565](http://gitlabghc.nibbler/ghc/ghc/issues/14565)</th>
<td>Performance degrades from -O1 to -O2</td></tr>
<tr><th>[\#14564](http://gitlabghc.nibbler/ghc/ghc/issues/14564)</th>
<td>CAF isn't floated</td></tr>
<tr><th>[\#14509](http://gitlabghc.nibbler/ghc/ghc/issues/14509)</th>
<td>Consider adding new stg\_ap\_\* functions</td></tr>
<tr><th>[\#14461](http://gitlabghc.nibbler/ghc/ghc/issues/14461)</th>
<td>Reuse free variable lists through nested closures</td></tr>
<tr><th>[\#14407](http://gitlabghc.nibbler/ghc/ghc/issues/14407)</th>
<td>rts: Threads/caps affinity</td></tr>
<tr><th>[\#14383](http://gitlabghc.nibbler/ghc/ghc/issues/14383)</th>
<td>Allocation in VS up 500%</td></tr>
<tr><th>[\#14359](http://gitlabghc.nibbler/ghc/ghc/issues/14359)</th>
<td>C-- pipeline/NCG fails to optimize simple repeated addition</td></tr>
<tr><th>[\#14337](http://gitlabghc.nibbler/ghc/ghc/issues/14337)</th>
<td>typeRepKind can perform substantial amounts of allocation</td></tr>
<tr><th>[\#14295](http://gitlabghc.nibbler/ghc/ghc/issues/14295)</th>
<td>tagToEnum\# leads to some silly closures</td></tr>
<tr><th>[\#14256](http://gitlabghc.nibbler/ghc/ghc/issues/14256)</th>
<td>GHCi is faster than compiled code</td></tr>
<tr><th>[\#14239](http://gitlabghc.nibbler/ghc/ghc/issues/14239)</th>
<td>Let -fspecialise-aggressively respect NOINLINE (or NOSPECIALISABLE?)</td></tr>
<tr><th>[\#14211](http://gitlabghc.nibbler/ghc/ghc/issues/14211)</th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th>[\#14208](http://gitlabghc.nibbler/ghc/ghc/issues/14208)</th>
<td>Performance with O0 is much better than the default or with -O2, runghc performs the best</td></tr>
<tr><th>[\#14072](http://gitlabghc.nibbler/ghc/ghc/issues/14072)</th>
<td>Code generated by GHC 8.2.1 faster than 8.0.1 but still somewhat slower than 7.10.3</td></tr>
<tr><th>[\#14003](http://gitlabghc.nibbler/ghc/ghc/issues/14003)</th>
<td>Allow more worker arguments in SpecConstr</td></tr>
<tr><th>[\#13904](http://gitlabghc.nibbler/ghc/ghc/issues/13904)</th>
<td>LLVM does not need to trash caller-saved registers.</td></tr>
<tr><th>[\#13873](http://gitlabghc.nibbler/ghc/ghc/issues/13873)</th>
<td>Adding a SPECIALIZE at a callsite in Main.hs is causing a regression</td></tr>
<tr><th>[\#13851](http://gitlabghc.nibbler/ghc/ghc/issues/13851)</th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th>[\#13763](http://gitlabghc.nibbler/ghc/ghc/issues/13763)</th>
<td>Performance regression (\~34%) in 8.2.1, poor register allocation(?) in an inner loop over an array</td></tr>
<tr><th>[\#13725](http://gitlabghc.nibbler/ghc/ghc/issues/13725)</th>
<td>Remove false dependency on the destination of the popcnt instruction</td></tr>
<tr><th>[\#13692](http://gitlabghc.nibbler/ghc/ghc/issues/13692)</th>
<td>Constructors and such should be able to move around seq\# sometimes</td></tr>
<tr><th>[\#13629](http://gitlabghc.nibbler/ghc/ghc/issues/13629)</th>
<td>sqrt should use machine instruction on x86\_64</td></tr>
<tr><th>[\#13362](http://gitlabghc.nibbler/ghc/ghc/issues/13362)</th>
<td>GHC first generation of GC to be as large as largest cache size by default</td></tr>
<tr><th>[\#13339](http://gitlabghc.nibbler/ghc/ghc/issues/13339)</th>
<td>Arbitrarily large expressions built out of cheap primops are not floated out</td></tr>
<tr><th>[\#13334](http://gitlabghc.nibbler/ghc/ghc/issues/13334)</th>
<td>Constant folding for repeated integer operation of unknown value</td></tr>
<tr><th>[\#13331](http://gitlabghc.nibbler/ghc/ghc/issues/13331)</th>
<td>Worker/wrapper can lead to sharing failure</td></tr>
<tr><th>[\#13309](http://gitlabghc.nibbler/ghc/ghc/issues/13309)</th>
<td>Use liftA2 in ApplicativeDo</td></tr>
<tr><th>[\#13296](http://gitlabghc.nibbler/ghc/ghc/issues/13296)</th>
<td>stat() calls can block Haskell runtime</td></tr>
<tr><th>[\#13280](http://gitlabghc.nibbler/ghc/ghc/issues/13280)</th>
<td>Consider deriving more Foldable methods</td></tr>
<tr><th>[\#13225](http://gitlabghc.nibbler/ghc/ghc/issues/13225)</th>
<td>Fannkuch-redux time regression from join point patch</td></tr>
<tr><th>[\#13193](http://gitlabghc.nibbler/ghc/ghc/issues/13193)</th>
<td>Integer (gmp) performance regression?</td></tr>
<tr><th>[\#13153](http://gitlabghc.nibbler/ghc/ghc/issues/13153)</th>
<td>Several Traversable instances have an extra fmap</td></tr>
<tr><th>[\#13080](http://gitlabghc.nibbler/ghc/ghc/issues/13080)</th>
<td>Memory leak caused by nested monadic loops</td></tr>
<tr><th>[\#13016](http://gitlabghc.nibbler/ghc/ghc/issues/13016)</th>
<td>SPECIALIZE INLINE doesn't necessarily inline specializations of a recursive function</td></tr>
<tr><th>[\#13014](http://gitlabghc.nibbler/ghc/ghc/issues/13014)</th>
<td>Seemingly unnecessary marking of a SpecConstr specialization as a loopbreaker</td></tr>
<tr><th>[\#13002](http://gitlabghc.nibbler/ghc/ghc/issues/13002)</th>
<td>:set -O does not work in .ghci file</td></tr>
<tr><th>[\#12953](http://gitlabghc.nibbler/ghc/ghc/issues/12953)</th>
<td>Use computed gotos in the interpreter when the compiler supports it</td></tr>
<tr><th>[\#12900](http://gitlabghc.nibbler/ghc/ghc/issues/12900)</th>
<td>Common up identical info tables</td></tr>
<tr><th>[\#12893](http://gitlabghc.nibbler/ghc/ghc/issues/12893)</th>
<td>Profiling defeats stream fusion when using vector library</td></tr>
<tr><th>[\#12817](http://gitlabghc.nibbler/ghc/ghc/issues/12817)</th>
<td>Degraded performance with constraint synonyms</td></tr>
<tr><th>[\#12808](http://gitlabghc.nibbler/ghc/ghc/issues/12808)</th>
<td>For closures, Loop Invariant Code Flow related to captured free values not lifted outside the loop...</td></tr>
<tr><th>[\#12798](http://gitlabghc.nibbler/ghc/ghc/issues/12798)</th>
<td>LLVM seeming to over optimize, producing inefficient assembly code...</td></tr>
<tr><th>[\#12737](http://gitlabghc.nibbler/ghc/ghc/issues/12737)</th>
<td>T12227 is failing on ghc-8.0</td></tr>
<tr><th>[\#12665](http://gitlabghc.nibbler/ghc/ghc/issues/12665)</th>
<td>Make Read instances for Integral types faster, and make them fail fast</td></tr>
<tr><th>[\#12640](http://gitlabghc.nibbler/ghc/ghc/issues/12640)</th>
<td>Class member functions not substituted for MultiParamTypeClasses</td></tr>
<tr><th>[\#12566](http://gitlabghc.nibbler/ghc/ghc/issues/12566)</th>
<td>Memory leak</td></tr>
<tr><th>[\#12232](http://gitlabghc.nibbler/ghc/ghc/issues/12232)</th>
<td>Opportunity to do better in register allocations</td></tr>
<tr><th>[\#12231](http://gitlabghc.nibbler/ghc/ghc/issues/12231)</th>
<td>Eliminate redundant heap allocations/deallocations</td></tr>
<tr><th>[\#12181](http://gitlabghc.nibbler/ghc/ghc/issues/12181)</th>
<td>Multi-threaded code on ARM64 GHC runtime doesn't use all available cores</td></tr>
<tr><th>[\#11677](http://gitlabghc.nibbler/ghc/ghc/issues/11677)</th>
<td>Dramatic de-optimization with "-O", "-O1", "-O2" options</td></tr>
<tr><th>[\#11668](http://gitlabghc.nibbler/ghc/ghc/issues/11668)</th>
<td>SPEC has a runtime cost if constructor specialization isn't performed</td></tr>
<tr><th>[\#11587](http://gitlabghc.nibbler/ghc/ghc/issues/11587)</th>
<td>Place shared objects in LIBDIR</td></tr>
<tr><th>[\#11561](http://gitlabghc.nibbler/ghc/ghc/issues/11561)</th>
<td>Have static ghci link against its own copy of its libraries</td></tr>
<tr><th>[\#11441](http://gitlabghc.nibbler/ghc/ghc/issues/11441)</th>
<td>RFC: Inline intermediate languages (Core, STG, Cmm, even StrictCore)</td></tr>
<tr><th>[\#11393](http://gitlabghc.nibbler/ghc/ghc/issues/11393)</th>
<td>Ability to define INLINE pragma for all instances of a given typeclass</td></tr>
<tr><th>[\#11271](http://gitlabghc.nibbler/ghc/ghc/issues/11271)</th>
<td>Costly let binding gets duplicated in IO action value</td></tr>
<tr><th>[\#11226](http://gitlabghc.nibbler/ghc/ghc/issues/11226)</th>
<td>Performance regression (involving sum, map, enumFromThenTo)</td></tr>
<tr><th>[\#11222](http://gitlabghc.nibbler/ghc/ghc/issues/11222)</th>
<td>Teach strictness analysis about \`catch\`-like operations</td></tr>
<tr><th>[\#11146](http://gitlabghc.nibbler/ghc/ghc/issues/11146)</th>
<td>Manual eta expansion leads to orders of magnitude less allocations</td></tr>
<tr><th>[\#11143](http://gitlabghc.nibbler/ghc/ghc/issues/11143)</th>
<td>Feature request: Add index/read/write primops with byte offset for ByteArray\#</td></tr>
<tr><th>[\#11134](http://gitlabghc.nibbler/ghc/ghc/issues/11134)</th>
<td>Limit frequency of idle GCs</td></tr>
<tr><th>[\#11029](http://gitlabghc.nibbler/ghc/ghc/issues/11029)</th>
<td>Performance loss due to eta expansion</td></tr>
<tr><th>[\#10992](http://gitlabghc.nibbler/ghc/ghc/issues/10992)</th>
<td>Performance regression due to lack of inlining of \`foldl\` and \`foldl'\`.</td></tr>
<tr><th>[\#10944](http://gitlabghc.nibbler/ghc/ghc/issues/10944)</th>
<td>powModInteger slower than computing pow and mod separately</td></tr>
<tr><th>[\#10922](http://gitlabghc.nibbler/ghc/ghc/issues/10922)</th>
<td>String inlining is inconsistent</td></tr>
<tr><th>[\#10906](http://gitlabghc.nibbler/ghc/ghc/issues/10906)</th>
<td>\`SPECIALIZE instance\` could be better</td></tr>
<tr><th>[\#10809](http://gitlabghc.nibbler/ghc/ghc/issues/10809)</th>
<td>Add prefetch{Small}{Mutable}Array\[0..3\]\#</td></tr>
<tr><th>[\#10804](http://gitlabghc.nibbler/ghc/ghc/issues/10804)</th>
<td>Rules conditional on strictess properties</td></tr>
<tr><th>[\#10730](http://gitlabghc.nibbler/ghc/ghc/issues/10730)</th>
<td>Spectral norm allocations increased 17% between 7.6 and 7.8</td></tr>
<tr><th>[\#10652](http://gitlabghc.nibbler/ghc/ghc/issues/10652)</th>
<td>Better cache performance in Array\#</td></tr>
<tr><th>[\#10648](http://gitlabghc.nibbler/ghc/ghc/issues/10648)</th>
<td>Some 64-vector SIMD primitives are absolutely useless</td></tr>
<tr><th>[\#10626](http://gitlabghc.nibbler/ghc/ghc/issues/10626)</th>
<td>Missed opportunity for SpecConstr</td></tr>
<tr><th>[\#10606](http://gitlabghc.nibbler/ghc/ghc/issues/10606)</th>
<td>avoid redundant stores to the stack when examining already-tagged data</td></tr>
<tr><th>[\#10482](http://gitlabghc.nibbler/ghc/ghc/issues/10482)</th>
<td>Not enough unboxing happens on data-family function argument</td></tr>
<tr><th>[\#10470](http://gitlabghc.nibbler/ghc/ghc/issues/10470)</th>
<td>Allocating StablePtrs leads to GC slowdown even after they're freed</td></tr>
<tr><th>[\#10434](http://gitlabghc.nibbler/ghc/ghc/issues/10434)</th>
<td>SPECIALISE instance does not specialize as far as SPECIALISE for type signatures</td></tr>
<tr><th>[\#10421](http://gitlabghc.nibbler/ghc/ghc/issues/10421)</th>
<td>exponential blowup in inlining (without INLINE pragmas)</td></tr>
<tr><th>[\#10417](http://gitlabghc.nibbler/ghc/ghc/issues/10417)</th>
<td>Rule matching not "seeing through" floating and type lambda (and maybe cast)</td></tr>
<tr><th>[\#10401](http://gitlabghc.nibbler/ghc/ghc/issues/10401)</th>
<td>state hack-related regression</td></tr>
<tr><th>[\#10371](http://gitlabghc.nibbler/ghc/ghc/issues/10371)</th>
<td>GHC fails to inline and specialize a function</td></tr>
<tr><th>[\#10346](http://gitlabghc.nibbler/ghc/ghc/issues/10346)</th>
<td>Cross-module SpecConstr</td></tr>
<tr><th>[\#10319](http://gitlabghc.nibbler/ghc/ghc/issues/10319)</th>
<td>Eta expand PAPs</td></tr>
<tr><th>[\#10229](http://gitlabghc.nibbler/ghc/ghc/issues/10229)</th>
<td>setThreadAffinity assumes a certain CPU virtual core layout</td></tr>
<tr><th>[\#10124](http://gitlabghc.nibbler/ghc/ghc/issues/10124)</th>
<td>Simple case analyses generate too many branches</td></tr>
<tr><th>[\#10120](http://gitlabghc.nibbler/ghc/ghc/issues/10120)</th>
<td>Unnecessary code duplication from case analysis</td></tr>
<tr><th>[\#10069](http://gitlabghc.nibbler/ghc/ghc/issues/10069)</th>
<td>CPR related performance issue</td></tr>
<tr><th>[\#10062](http://gitlabghc.nibbler/ghc/ghc/issues/10062)</th>
<td>Codegen on sequential FFI calls is not very good</td></tr>
<tr><th>[\#10049](http://gitlabghc.nibbler/ghc/ghc/issues/10049)</th>
<td>Lower level memcpy primop</td></tr>
<tr><th>[\#10016](http://gitlabghc.nibbler/ghc/ghc/issues/10016)</th>
<td>UNPACK support for existentials</td></tr>
<tr><th>[\#10012](http://gitlabghc.nibbler/ghc/ghc/issues/10012)</th>
<td>Cheap-to-compute values aren't pushed into case branches inducing unnecessary register pressure</td></tr>
<tr><th>[\#10005](http://gitlabghc.nibbler/ghc/ghc/issues/10005)</th>
<td>Operations on string literals won't be inlined</td></tr>
<tr><th>[\#9992](http://gitlabghc.nibbler/ghc/ghc/issues/9992)</th>
<td>Constructor specialization requires eta expansion</td></tr>
<tr><th>[\#9989](http://gitlabghc.nibbler/ghc/ghc/issues/9989)</th>
<td>GHCI is slow for precompiled code</td></tr>
<tr><th>[\#9944](http://gitlabghc.nibbler/ghc/ghc/issues/9944)</th>
<td>Performance issue re: simple loop</td></tr>
<tr><th>[\#9923](http://gitlabghc.nibbler/ghc/ghc/issues/9923)</th>
<td>Offer copy-on-GC sliced arrays</td></tr>
<tr><th>[\#9809](http://gitlabghc.nibbler/ghc/ghc/issues/9809)</th>
<td>Overwhelming the TimerManager</td></tr>
<tr><th>[\#9798](http://gitlabghc.nibbler/ghc/ghc/issues/9798)</th>
<td>Frustrating behaviour of the INLINE pragma</td></tr>
<tr><th>[\#9792](http://gitlabghc.nibbler/ghc/ghc/issues/9792)</th>
<td>map/coerce rule does not fire until the coercion is known</td></tr>
<tr><th>[\#9790](http://gitlabghc.nibbler/ghc/ghc/issues/9790)</th>
<td>Produce coercion rules for derived Functor instances</td></tr>
<tr><th>[\#9786](http://gitlabghc.nibbler/ghc/ghc/issues/9786)</th>
<td>Make quot/rem/div/mod with known divisors fast</td></tr>
<tr><th>[\#9701](http://gitlabghc.nibbler/ghc/ghc/issues/9701)</th>
<td>GADTs not specialized properly</td></tr>
<tr><th>[\#9688](http://gitlabghc.nibbler/ghc/ghc/issues/9688)</th>
<td>Improve the interaction between CSE and the join point transformation</td></tr>
<tr><th>[\#9661](http://gitlabghc.nibbler/ghc/ghc/issues/9661)</th>
<td>Branchless ==\# is compiled to branchy code</td></tr>
<tr><th>[\#9660](http://gitlabghc.nibbler/ghc/ghc/issues/9660)</th>
<td>unnecessary indirect jump when returning a case scrutinee</td></tr>
<tr><th>[\#9659](http://gitlabghc.nibbler/ghc/ghc/issues/9659)</th>
<td>Offer branchless conditional (CMOV) primop</td></tr>
<tr><th>[\#9655](http://gitlabghc.nibbler/ghc/ghc/issues/9655)</th>
<td>Do not UNPACK strict fields that are very wide</td></tr>
<tr><th>[\#9646](http://gitlabghc.nibbler/ghc/ghc/issues/9646)</th>
<td>Simplifer non-determinism leading to 8 fold difference in run time performance</td></tr>
<tr><th>[\#9645](http://gitlabghc.nibbler/ghc/ghc/issues/9645)</th>
<td>Optimize range checks for primitive types</td></tr>
<tr><th>[\#9617](http://gitlabghc.nibbler/ghc/ghc/issues/9617)</th>
<td>Implement \`quot\` and \`rem\` using \`quotRem\`; implement \`div\` and \`mod\` using \`divMod\`</td></tr>
<tr><th>[\#9601](http://gitlabghc.nibbler/ghc/ghc/issues/9601)</th>
<td>Make the rewrite rule system more powerful</td></tr>
<tr><th>[\#9542](http://gitlabghc.nibbler/ghc/ghc/issues/9542)</th>
<td>GHC-IO-Handle-Text.hPutStr' and writeBlocks look like they need refactoring</td></tr>
<tr><th>[\#9522](http://gitlabghc.nibbler/ghc/ghc/issues/9522)</th>
<td>SPECIALISE pragmas for derived instances</td></tr>
<tr><th>[\#9447](http://gitlabghc.nibbler/ghc/ghc/issues/9447)</th>
<td>Add support for resizing \`MutableByteArray\#\`s</td></tr>
<tr><th>[\#9431](http://gitlabghc.nibbler/ghc/ghc/issues/9431)</th>
<td>integer-gmp small Integer multiplication does two multiplications on x86</td></tr>
<tr><th>[\#9388](http://gitlabghc.nibbler/ghc/ghc/issues/9388)</th>
<td>Narrow the scope of the notorious "state hack"</td></tr>
<tr><th>[\#9374](http://gitlabghc.nibbler/ghc/ghc/issues/9374)</th>
<td>Investigate Static Argument Transformation</td></tr>
<tr><th>[\#9353](http://gitlabghc.nibbler/ghc/ghc/issues/9353)</th>
<td>prefetch primops are not currently useful</td></tr>
<tr><th>[\#9350](http://gitlabghc.nibbler/ghc/ghc/issues/9350)</th>
<td>Consider using xchg instead of mfence for CS stores</td></tr>
<tr><th>[\#9349](http://gitlabghc.nibbler/ghc/ghc/issues/9349)</th>
<td>excessive inlining due to state hack</td></tr>
<tr><th>[\#9342](http://gitlabghc.nibbler/ghc/ghc/issues/9342)</th>
<td>Branchless arithmetic operations</td></tr>
<tr><th>[\#9320](http://gitlabghc.nibbler/ghc/ghc/issues/9320)</th>
<td>Inlining regression/strangeness in 7.8</td></tr>
<tr><th>[\#9289](http://gitlabghc.nibbler/ghc/ghc/issues/9289)</th>
<td>add anyToAddr\# :: (\#a\#)-\> Addr\# primop (inverse of addrToAny\#)</td></tr>
<tr><th>[\#9279](http://gitlabghc.nibbler/ghc/ghc/issues/9279)</th>
<td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
<tr><th>[\#9251](http://gitlabghc.nibbler/ghc/ghc/issues/9251)</th>
<td>ghc does not expose branchless max/min operations as primops</td></tr>
<tr><th>[\#9246](http://gitlabghc.nibbler/ghc/ghc/issues/9246)</th>
<td>GHC generates poor code for repeated uses of min/max</td></tr>
<tr><th>[\#9192](http://gitlabghc.nibbler/ghc/ghc/issues/9192)</th>
<td>Add sameByteArray\#</td></tr>
<tr><th>[\#9137](http://gitlabghc.nibbler/ghc/ghc/issues/9137)</th>
<td>A way to match RULES only for literals</td></tr>
<tr><th>[\#9120](http://gitlabghc.nibbler/ghc/ghc/issues/9120)</th>
<td>Cache intermediate powers</td></tr>
<tr><th>[\#9088](http://gitlabghc.nibbler/ghc/ghc/issues/9088)</th>
<td>Per-thread Haskell thread list/numbering (remove global lock from thread allocation)</td></tr>
<tr><th>[\#9041](http://gitlabghc.nibbler/ghc/ghc/issues/9041)</th>
<td>NCG generates slow loop code</td></tr>
<tr><th>[\#8971](http://gitlabghc.nibbler/ghc/ghc/issues/8971)</th>
<td>Native Code Generator for 8.0.1 is not as optimized as 7.6.3...</td></tr>
<tr><th>[\#8955](http://gitlabghc.nibbler/ghc/ghc/issues/8955)</th>
<td>Syscall intrinsic</td></tr>
<tr><th>[\#8949](http://gitlabghc.nibbler/ghc/ghc/issues/8949)</th>
<td>switch -msse2 to be on by default</td></tr>
<tr><th>[\#8905](http://gitlabghc.nibbler/ghc/ghc/issues/8905)</th>
<td>Function arguments are always spilled/reloaded if scrutinee is already in WHNF</td></tr>
<tr><th>[\#8903](http://gitlabghc.nibbler/ghc/ghc/issues/8903)</th>
<td>Add dead store elimination</td></tr>
<tr><th>[\#8887](http://gitlabghc.nibbler/ghc/ghc/issues/8887)</th>
<td>Double double assignment in optimized Cmm on SPARC</td></tr>
<tr><th>[\#8871](http://gitlabghc.nibbler/ghc/ghc/issues/8871)</th>
<td>No-op assignment I64\[BaseReg + 784\] = I64\[BaseReg + 784\]; is generated into optimized Cmm</td></tr>
<tr><th>[\#8814](http://gitlabghc.nibbler/ghc/ghc/issues/8814)</th>
<td>7.8 optimizes attoparsec improperly</td></tr>
<tr><th>[\#8733](http://gitlabghc.nibbler/ghc/ghc/issues/8733)</th>
<td>I/O manager causes unnecessary syscalls in send/recv loops</td></tr>
<tr><th>[\#8732](http://gitlabghc.nibbler/ghc/ghc/issues/8732)</th>
<td>Global big object heap allocator lock causes contention</td></tr>
<tr><th>[\#8668](http://gitlabghc.nibbler/ghc/ghc/issues/8668)</th>
<td>SPECIALIZE silently fails to apply</td></tr>
<tr><th>[\#8662](http://gitlabghc.nibbler/ghc/ghc/issues/8662)</th>
<td>GHC does not inline cheap inner loop when used in two places</td></tr>
<tr><th>[\#8655](http://gitlabghc.nibbler/ghc/ghc/issues/8655)</th>
<td>Evaluate know-to-terminate-soon thunks</td></tr>
<tr><th>[\#8635](http://gitlabghc.nibbler/ghc/ghc/issues/8635)</th>
<td>GHC optimisation flag ignored when importing a local module with derived type classes</td></tr>
<tr><th>[\#8623](http://gitlabghc.nibbler/ghc/ghc/issues/8623)</th>
<td>Strange slowness when using async library with FFI callbacks</td></tr>
<tr><th>[\#8598](http://gitlabghc.nibbler/ghc/ghc/issues/8598)</th>
<td>IO hack in demand analyzer gets in the way of CPR</td></tr>
<tr><th>[\#8589](http://gitlabghc.nibbler/ghc/ghc/issues/8589)</th>
<td>Bad choice of loop breaker with INLINABLE/INLINE</td></tr>
<tr><th>[\#8578](http://gitlabghc.nibbler/ghc/ghc/issues/8578)</th>
<td>Improvements to SpinLock implementation</td></tr>
<tr><th>[\#8457](http://gitlabghc.nibbler/ghc/ghc/issues/8457)</th>
<td>-ffull-laziness does more harm than good</td></tr>
<tr><th>[\#8404](http://gitlabghc.nibbler/ghc/ghc/issues/8404)</th>
<td>Default to turning on architecture specific optimizations in the codegen</td></tr>
<tr><th>[\#8354](http://gitlabghc.nibbler/ghc/ghc/issues/8354)</th>
<td>Add INLINE (or at least INLINABLE) pragmas for methods of Ord in ghc-prim</td></tr>
<tr><th>[\#8336](http://gitlabghc.nibbler/ghc/ghc/issues/8336)</th>
<td>Sinking pass could optimize some assignments better</td></tr>
<tr><th>[\#8327](http://gitlabghc.nibbler/ghc/ghc/issues/8327)</th>
<td>Cmm sinking does not eliminate dead code in loops</td></tr>
<tr><th>[\#8326](http://gitlabghc.nibbler/ghc/ghc/issues/8326)</th>
<td>Place heap checks common in case alternatives before the case</td></tr>
<tr><th>[\#8317](http://gitlabghc.nibbler/ghc/ghc/issues/8317)</th>
<td>Optimize tagToEnum\# at Core level</td></tr>
<tr><th>[\#8313](http://gitlabghc.nibbler/ghc/ghc/issues/8313)</th>
<td>Poor performance of higher-order functions with unboxing</td></tr>
<tr><th>[\#8311](http://gitlabghc.nibbler/ghc/ghc/issues/8311)</th>
<td>suboptimal code generated for even :: Int -\> Bool by NCG (x86, x86\_64)</td></tr>
<tr><th>[\#8279](http://gitlabghc.nibbler/ghc/ghc/issues/8279)</th>
<td>bad alignment in code gen  yields substantial perf issue</td></tr>
<tr><th>[\#8272](http://gitlabghc.nibbler/ghc/ghc/issues/8272)</th>
<td>testing if SpLim=$rbp and Sp=$rsp changed performance at all</td></tr>
<tr><th>[\#8151](http://gitlabghc.nibbler/ghc/ghc/issues/8151)</th>
<td>ghc-7.4.2 on OpenIndiana (Solaris) createSubprocess fails</td></tr>
<tr><th>[\#8048](http://gitlabghc.nibbler/ghc/ghc/issues/8048)</th>
<td>Register spilling produces ineffecient/highly contending code</td></tr>
<tr><th>[\#8046](http://gitlabghc.nibbler/ghc/ghc/issues/8046)</th>
<td>Make the timer management scale better across multicore</td></tr>
<tr><th>[\#8032](http://gitlabghc.nibbler/ghc/ghc/issues/8032)</th>
<td>Worker-wrapper transform and NOINLINE trigger bad reboxing behavior</td></tr>
<tr><th>[\#8023](http://gitlabghc.nibbler/ghc/ghc/issues/8023)</th>
<td>dph-examples binaries don't use all CPUs</td></tr>
<tr><th>[\#7977](http://gitlabghc.nibbler/ghc/ghc/issues/7977)</th>
<td>Optimization: Shift dropped list heads by coeffecient to prevent thunk generation</td></tr>
<tr><th>[\#7741](http://gitlabghc.nibbler/ghc/ghc/issues/7741)</th>
<td>Add SIMD support to x86/x86\_64 NCG</td></tr>
<tr><th>[\#7679](http://gitlabghc.nibbler/ghc/ghc/issues/7679)</th>
<td>Regression in -fregs-graph performance</td></tr>
<tr><th>[\#7647](http://gitlabghc.nibbler/ghc/ghc/issues/7647)</th>
<td>UNPACK polymorphic fields</td></tr>
<tr><th>[\#7602](http://gitlabghc.nibbler/ghc/ghc/issues/7602)</th>
<td>Threaded RTS performing badly on recent OS X (10.8?)</td></tr>
<tr><th>[\#7596](http://gitlabghc.nibbler/ghc/ghc/issues/7596)</th>
<td>Opportunity to improve CSE</td></tr>
<tr><th>[\#7542](http://gitlabghc.nibbler/ghc/ghc/issues/7542)</th>
<td>GHC doesn't optimize (strict) composition with id</td></tr>
<tr><th>[\#7511](http://gitlabghc.nibbler/ghc/ghc/issues/7511)</th>
<td>Room for GHC runtime improvement \>\~5%, inlining related</td></tr>
<tr><th>[\#7398](http://gitlabghc.nibbler/ghc/ghc/issues/7398)</th>
<td>RULES don't apply to a newtype constructor</td></tr>
<tr><th>[\#7378](http://gitlabghc.nibbler/ghc/ghc/issues/7378)</th>
<td>Identical alts/bad divInt\# code</td></tr>
<tr><th>[\#7374](http://gitlabghc.nibbler/ghc/ghc/issues/7374)</th>
<td>rule not firing</td></tr>
<tr><th>[\#7367](http://gitlabghc.nibbler/ghc/ghc/issues/7367)</th>
<td>float-out causes extra allocation</td></tr>
<tr><th>[\#7309](http://gitlabghc.nibbler/ghc/ghc/issues/7309)</th>
<td>The Ix instance for (,) leaks space in range</td></tr>
<tr><th>[\#7307](http://gitlabghc.nibbler/ghc/ghc/issues/7307)</th>
<td>Share top-level code for strings</td></tr>
<tr><th>[\#7300](http://gitlabghc.nibbler/ghc/ghc/issues/7300)</th>
<td>Allow CAFs kept reachable by FFI to be forcibly made unreachable for GC</td></tr>
<tr><th>[\#7283](http://gitlabghc.nibbler/ghc/ghc/issues/7283)</th>
<td>Specialise INLINE functions</td></tr>
<tr><th>[\#7273](http://gitlabghc.nibbler/ghc/ghc/issues/7273)</th>
<td>Binary size increase in nofib/grep between 7.6.1 and HEAD</td></tr>
<tr><th>[\#7206](http://gitlabghc.nibbler/ghc/ghc/issues/7206)</th>
<td>Implement cheap build</td></tr>
<tr><th>[\#7114](http://gitlabghc.nibbler/ghc/ghc/issues/7114)</th>
<td>Cannot recover (good) inlining behaviour from 7.0.2 in 7.4.1</td></tr>
<tr><th>[\#7109](http://gitlabghc.nibbler/ghc/ghc/issues/7109)</th>
<td>Inlining depends on datatype size, even with INLINE pragmas</td></tr>
<tr><th>[\#7080](http://gitlabghc.nibbler/ghc/ghc/issues/7080)</th>
<td>Make RULES and SPECIALISE more consistent</td></tr>
<tr><th>[\#7063](http://gitlabghc.nibbler/ghc/ghc/issues/7063)</th>
<td>Register allocators can't handle non-uniform register sets</td></tr>
<tr><th>[\#6092](http://gitlabghc.nibbler/ghc/ghc/issues/6092)</th>
<td>Liberate case not happening</td></tr>
<tr><th>[\#6070](http://gitlabghc.nibbler/ghc/ghc/issues/6070)</th>
<td>Fun with the demand analyser</td></tr>
<tr><th>[\#5928](http://gitlabghc.nibbler/ghc/ghc/issues/5928)</th>
<td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
<tr><th>[\#5834](http://gitlabghc.nibbler/ghc/ghc/issues/5834)</th>
<td>Allow both INLINE and INLINABLE for the same function</td></tr>
<tr><th>[\#5775](http://gitlabghc.nibbler/ghc/ghc/issues/5775)</th>
<td>Inconsistency in demand analysis</td></tr>
<tr><th>[\#5645](http://gitlabghc.nibbler/ghc/ghc/issues/5645)</th>
<td>Sharing across functions causing space leak</td></tr>
<tr><th>[\#5567](http://gitlabghc.nibbler/ghc/ghc/issues/5567)</th>
<td>LLVM: Improve alias analysis / performance</td></tr>
<tr><th>[\#5463](http://gitlabghc.nibbler/ghc/ghc/issues/5463)</th>
<td>SPECIALISE pragmas generated from Template Haskell are ignored</td></tr>
<tr><th>[\#5444](http://gitlabghc.nibbler/ghc/ghc/issues/5444)</th>
<td>Slow 64-bit primops on 32 bit system</td></tr>
<tr><th>[\#5355](http://gitlabghc.nibbler/ghc/ghc/issues/5355)</th>
<td>Link plugins against existing libHSghc</td></tr>
<tr><th>[\#5344](http://gitlabghc.nibbler/ghc/ghc/issues/5344)</th>
<td>CSE should look through coercions</td></tr>
<tr><th>[\#5326](http://gitlabghc.nibbler/ghc/ghc/issues/5326)</th>
<td>Polymorphic instances aren't automatically specialised</td></tr>
<tr><th>[\#5302](http://gitlabghc.nibbler/ghc/ghc/issues/5302)</th>
<td>Unused arguments in join points</td></tr>
<tr><th>[\#5298](http://gitlabghc.nibbler/ghc/ghc/issues/5298)</th>
<td>Inlined functions aren't fully specialised</td></tr>
<tr><th>[\#5262](http://gitlabghc.nibbler/ghc/ghc/issues/5262)</th>
<td>Compiling with -O makes some expressions too lazy and causes space leaks</td></tr>
<tr><th>[\#5218](http://gitlabghc.nibbler/ghc/ghc/issues/5218)</th>
<td>Add unpackCStringLen\# to create Strings from string literals</td></tr>
<tr><th>[\#5171](http://gitlabghc.nibbler/ghc/ghc/issues/5171)</th>
<td>Misfeature of Cmm optimiser: no way to extract a branch of expression into a separate statement</td></tr>
<tr><th>[\#5075](http://gitlabghc.nibbler/ghc/ghc/issues/5075)</th>
<td>CPR optimisation for sum types if only one constructor is used</td></tr>
<tr><th>[\#5059](http://gitlabghc.nibbler/ghc/ghc/issues/5059)</th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th>[\#4960](http://gitlabghc.nibbler/ghc/ghc/issues/4960)</th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th>[\#4945](http://gitlabghc.nibbler/ghc/ghc/issues/4945)</th>
<td>Another SpecConstr infelicity</td></tr>
<tr><th>[\#4941](http://gitlabghc.nibbler/ghc/ghc/issues/4941)</th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th>[\#4937](http://gitlabghc.nibbler/ghc/ghc/issues/4937)</th>
<td>Remove indirections caused by sum types, such as Maybe</td></tr>
<tr><th>[\#4833](http://gitlabghc.nibbler/ghc/ghc/issues/4833)</th>
<td>Finding the right loop breaker</td></tr>
<tr><th>[\#4831](http://gitlabghc.nibbler/ghc/ghc/issues/4831)</th>
<td>Too many specialisations in SpecConstr</td></tr>
<tr><th>[\#4823](http://gitlabghc.nibbler/ghc/ghc/issues/4823)</th>
<td>Loop strength reduction for array indexing</td></tr>
<tr><th>[\#4470](http://gitlabghc.nibbler/ghc/ghc/issues/4470)</th>
<td>Loop optimization: identical counters</td></tr>
<tr><th>[\#4301](http://gitlabghc.nibbler/ghc/ghc/issues/4301)</th>
<td>Optimisations give bad core for foldl' (flip seq) ()</td></tr>
<tr><th>[\#4101](http://gitlabghc.nibbler/ghc/ghc/issues/4101)</th>
<td>Primitive constant unfolding</td></tr>
<tr><th>[\#4096](http://gitlabghc.nibbler/ghc/ghc/issues/4096)</th>
<td>New primops for indexing: index\*OffAddrUsing\# etc</td></tr>
<tr><th>[\#4081](http://gitlabghc.nibbler/ghc/ghc/issues/4081)</th>
<td>Strict constructor fields inspected in loop</td></tr>
<tr><th>[\#4005](http://gitlabghc.nibbler/ghc/ghc/issues/4005)</th>
<td>Bad behaviour in the generational GC with paraffins -O2</td></tr>
<tr><th>[\#3781](http://gitlabghc.nibbler/ghc/ghc/issues/3781)</th>
<td>Improve inlining for local functions</td></tr>
<tr><th>[\#3767](http://gitlabghc.nibbler/ghc/ghc/issues/3767)</th>
<td>SpecConstr for join points</td></tr>
<tr><th>[\#3765](http://gitlabghc.nibbler/ghc/ghc/issues/3765)</th>
<td>Rules should "look through" case binders too</td></tr>
<tr><th>[\#3755](http://gitlabghc.nibbler/ghc/ghc/issues/3755)</th>
<td>Improve join point inlining</td></tr>
<tr><th>[\#3744](http://gitlabghc.nibbler/ghc/ghc/issues/3744)</th>
<td>Comparisons against minBound/maxBound not optimised for (Int\|Word)(8\|16\|32)</td></tr>
<tr><th>[\#3606](http://gitlabghc.nibbler/ghc/ghc/issues/3606)</th>
<td>The Ord instance for unboxed arrays is very inefficient</td></tr>
<tr><th>[\#3557](http://gitlabghc.nibbler/ghc/ghc/issues/3557)</th>
<td>CPU Vector instructions in GHC.Prim</td></tr>
<tr><th>[\#3462](http://gitlabghc.nibbler/ghc/ghc/issues/3462)</th>
<td>New codegen: allocate large objects using allocateLocal()</td></tr>
<tr><th>[\#3458](http://gitlabghc.nibbler/ghc/ghc/issues/3458)</th>
<td>Allocation where none should happen</td></tr>
<tr><th>[\#3138](http://gitlabghc.nibbler/ghc/ghc/issues/3138)</th>
<td>Returning a known constructor: GHC generates terrible code for cmonad</td></tr>
<tr><th>[\#3107](http://gitlabghc.nibbler/ghc/ghc/issues/3107)</th>
<td>Over-eager GC when blocked on a signal in the non-threaded runtime</td></tr>
<tr><th>[\#3073](http://gitlabghc.nibbler/ghc/ghc/issues/3073)</th>
<td>Avoid reconstructing dictionaries in recursive instance methods</td></tr>
<tr><th>[\#3061](http://gitlabghc.nibbler/ghc/ghc/issues/3061)</th>
<td>GHC's GC default heap growth strategy is not as good as other runtimes</td></tr>
<tr><th>[\#3055](http://gitlabghc.nibbler/ghc/ghc/issues/3055)</th>
<td>Int / Word / IntN / WordN are unequally optimized</td></tr>
<tr><th>[\#3034](http://gitlabghc.nibbler/ghc/ghc/issues/3034)</th>
<td>divInt\# floated into a position which leads to low arity</td></tr>
<tr><th>[\#2731](http://gitlabghc.nibbler/ghc/ghc/issues/2731)</th>
<td>Avoid unnecessary evaluation when unpacking constructors</td></tr>
<tr><th>[\#2642](http://gitlabghc.nibbler/ghc/ghc/issues/2642)</th>
<td>Improve SpecConstr for join points</td></tr>
<tr><th>[\#2625](http://gitlabghc.nibbler/ghc/ghc/issues/2625)</th>
<td>Unexpected -ddump-simpl output for derived Ord instance and UNPACKed fields</td></tr>
<tr><th>[\#2607](http://gitlabghc.nibbler/ghc/ghc/issues/2607)</th>
<td>Inlining defeats selector thunk optimisation</td></tr>
<tr><th>[\#2598](http://gitlabghc.nibbler/ghc/ghc/issues/2598)</th>
<td>Avoid excessive specialisation in SpecConstr</td></tr>
<tr><th>[\#2465](http://gitlabghc.nibbler/ghc/ghc/issues/2465)</th>
<td>Fusion of recursive functions</td></tr>
<tr><th>[\#2439](http://gitlabghc.nibbler/ghc/ghc/issues/2439)</th>
<td>Missed optimisation with dictionaries and loops</td></tr>
<tr><th>[\#2387](http://gitlabghc.nibbler/ghc/ghc/issues/2387)</th>
<td>Optimizer misses unboxing opportunity</td></tr>
<tr><th>[\#2374](http://gitlabghc.nibbler/ghc/ghc/issues/2374)</th>
<td>MutableByteArray\# is slower than Addr\#</td></tr>
<tr><th>[\#2289](http://gitlabghc.nibbler/ghc/ghc/issues/2289)</th>
<td>Needless reboxing of values when returning from a tight loop</td></tr>
<tr><th>[\#2273](http://gitlabghc.nibbler/ghc/ghc/issues/2273)</th>
<td>inlining defeats seq</td></tr>
<tr><th>[\#2269](http://gitlabghc.nibbler/ghc/ghc/issues/2269)</th>
<td>Word type to Double or Float conversions are slower than Int conversions</td></tr>
<tr><th>[\#2255](http://gitlabghc.nibbler/ghc/ghc/issues/2255)</th>
<td>Improve SpecConstr for free variables</td></tr>
<tr><th>[\#2132](http://gitlabghc.nibbler/ghc/ghc/issues/2132)</th>
<td>Optimise nested comparisons</td></tr>
<tr><th>[\#2028](http://gitlabghc.nibbler/ghc/ghc/issues/2028)</th>
<td>STM slightly conservative on write-only transactions</td></tr>
<tr><th>[\#1687](http://gitlabghc.nibbler/ghc/ghc/issues/1687)</th>
<td>A faster (\^)-function.</td></tr>
<tr><th>[\#1600](http://gitlabghc.nibbler/ghc/ghc/issues/1600)</th>
<td>Optimisation: CPR the results of IO</td></tr>
<tr><th>[\#1544](http://gitlabghc.nibbler/ghc/ghc/issues/1544)</th>
<td>Derived Read instances for recursive datatypes with infix constructors are too inefficient</td></tr>
<tr><th>[\#1498](http://gitlabghc.nibbler/ghc/ghc/issues/1498)</th>
<td>Optimisation: eliminate unnecessary heap check in recursive function</td></tr>
<tr><th>[\#1216](http://gitlabghc.nibbler/ghc/ghc/issues/1216)</th>
<td>Missed opportunity for let-no-esape</td></tr>
<tr><th>[\#1168](http://gitlabghc.nibbler/ghc/ghc/issues/1168)</th>
<td>Optimisation sometimes decreases sharing in IO code</td></tr>
<tr><th>[\#1147](http://gitlabghc.nibbler/ghc/ghc/issues/1147)</th>
<td>Quadratic behaviour in the compacting GC</td></tr>
<tr><th>[\#932](http://gitlabghc.nibbler/ghc/ghc/issues/932)</th>
<td>Improve inlining</td></tr>
<tr><th>[\#917](http://gitlabghc.nibbler/ghc/ghc/issues/917)</th>
<td>-O introduces space leak</td></tr>
<tr><th>[\#855](http://gitlabghc.nibbler/ghc/ghc/issues/855)</th>
<td>Improvements to SpecConstr</td></tr>
<tr><th>[\#728](http://gitlabghc.nibbler/ghc/ghc/issues/728)</th>
<td>switch to compacting collection when swapping occurs</td></tr>
<tr><th>[\#605](http://gitlabghc.nibbler/ghc/ghc/issues/605)</th>
<td>Optimisation: strict enumerations</td></tr>
<tr><th>[\#149](http://gitlabghc.nibbler/ghc/ghc/issues/149)</th>
<td>missed CSE opportunity</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#16109](http://gitlabghc.nibbler/ghc/ghc/issues/16109)</th>
<td>cabal install H under Windows 10 does not terminate</td></tr>
<tr><th>[\#15802](http://gitlabghc.nibbler/ghc/ghc/issues/15802)</th>
<td>Inlining of constant fails when both cross-module and recursive</td></tr>
<tr><th>[\#15226](http://gitlabghc.nibbler/ghc/ghc/issues/15226)</th>
<td>GHC doesn't know that seq\# produces something in WHNF</td></tr>
<tr><th>[\#15143](http://gitlabghc.nibbler/ghc/ghc/issues/15143)</th>
<td>Passing an IO value through several functions results in program hanging.</td></tr>
<tr><th>[\#15131](http://gitlabghc.nibbler/ghc/ghc/issues/15131)</th>
<td>Speed up certain Foldable NonEmpty methods</td></tr>
<tr><th>[\#14978](http://gitlabghc.nibbler/ghc/ghc/issues/14978)</th>
<td>GADTs don't seem to unpack properly</td></tr>
<tr><th>[\#14855](http://gitlabghc.nibbler/ghc/ghc/issues/14855)</th>
<td>Implementation of liftA2 for Const has high arity</td></tr>
<tr><th>[\#14790](http://gitlabghc.nibbler/ghc/ghc/issues/14790)</th>
<td>eqTypeRep does not inline</td></tr>
<tr><th>[\#14519](http://gitlabghc.nibbler/ghc/ghc/issues/14519)</th>
<td>Exponential runtime performance regression in GHC 8.2 + Data.Text.Lazy + Text.RE.TDFA</td></tr>
<tr><th>[\#14336](http://gitlabghc.nibbler/ghc/ghc/issues/14336)</th>
<td>ghci leaks memory</td></tr>
<tr><th>[\#14258](http://gitlabghc.nibbler/ghc/ghc/issues/14258)</th>
<td>n-body runtime regressed badly due to CoreFVs patch</td></tr>
<tr><th>[\#14240](http://gitlabghc.nibbler/ghc/ghc/issues/14240)</th>
<td>CSE’ing w/w’ed code regresses program runtime</td></tr>
<tr><th>[\#14224](http://gitlabghc.nibbler/ghc/ghc/issues/14224)</th>
<td>zipWith does not inline</td></tr>
<tr><th>[\#14192](http://gitlabghc.nibbler/ghc/ghc/issues/14192)</th>
<td>Change to 1TB VIRT allocation makes it impossible to core-dump Haskell programs</td></tr>
<tr><th>[\#14187](http://gitlabghc.nibbler/ghc/ghc/issues/14187)</th>
<td>Transpose hangs on infinite by finite lists</td></tr>
<tr><th>[\#14140](http://gitlabghc.nibbler/ghc/ghc/issues/14140)</th>
<td>Better treatment for dataToTag</td></tr>
<tr><th>[\#14052](http://gitlabghc.nibbler/ghc/ghc/issues/14052)</th>
<td>Significant GHCi speed regression with :module and \`let\` in GHC 8.2.1</td></tr>
<tr><th>[\#13999](http://gitlabghc.nibbler/ghc/ghc/issues/13999)</th>
<td>Simple function not inlined within declaration marked NOINLINE</td></tr>
<tr><th>[\#13982](http://gitlabghc.nibbler/ghc/ghc/issues/13982)</th>
<td>HEAD GHC+Cabal uses too much memory</td></tr>
<tr><th>[\#13930](http://gitlabghc.nibbler/ghc/ghc/issues/13930)</th>
<td>Cabal configure regresses in space/time</td></tr>
<tr><th>[\#13690](http://gitlabghc.nibbler/ghc/ghc/issues/13690)</th>
<td>Running profiling tests in the GHCi way is extremely slow</td></tr>
<tr><th>[\#13654](http://gitlabghc.nibbler/ghc/ghc/issues/13654)</th>
<td>Optimize casMutVar\# for single-threaded runtime</td></tr>
<tr><th>[\#13623](http://gitlabghc.nibbler/ghc/ghc/issues/13623)</th>
<td>join points produce bad code for stream fusion</td></tr>
<tr><th>[\#13604](http://gitlabghc.nibbler/ghc/ghc/issues/13604)</th>
<td>ghci no longer loads dynamic .o files by default if they were built with -O</td></tr>
<tr><th>[\#13566](http://gitlabghc.nibbler/ghc/ghc/issues/13566)</th>
<td>Bigger core size in ghc8 compared to ghc7</td></tr>
<tr><th>[\#13536](http://gitlabghc.nibbler/ghc/ghc/issues/13536)</th>
<td>Program which terminates instantly in GHC 8.0.2 runs for minutes with 8.2.1</td></tr>
<tr><th>[\#13422](http://gitlabghc.nibbler/ghc/ghc/issues/13422)</th>
<td>INLINE CONLIKE sometimes fails to inline</td></tr>
<tr><th>[\#13376](http://gitlabghc.nibbler/ghc/ghc/issues/13376)</th>
<td>GHC fails to specialize a pair of polymorphic INLINABLE functions</td></tr>
<tr><th>[\#13328](http://gitlabghc.nibbler/ghc/ghc/issues/13328)</th>
<td>Foldable, Functor, and Traversable deriving handle phantom types badly</td></tr>
<tr><th>[\#13288](http://gitlabghc.nibbler/ghc/ghc/issues/13288)</th>
<td>Resident set size exceeds +RTS -M limit with large nurseries</td></tr>
<tr><th>[\#13246](http://gitlabghc.nibbler/ghc/ghc/issues/13246)</th>
<td>hPutBuf issues unnecessary empty write syscalls for large writes</td></tr>
<tr><th>[\#13228](http://gitlabghc.nibbler/ghc/ghc/issues/13228)</th>
<td>Surprising inlining failure</td></tr>
<tr><th>[\#13218](http://gitlabghc.nibbler/ghc/ghc/issues/13218)</th>
<td>\<$ is bad in derived functor instances</td></tr>
<tr><th>[\#13040](http://gitlabghc.nibbler/ghc/ghc/issues/13040)</th>
<td>realToFrac into Complex Double has no specialization</td></tr>
<tr><th>[\#13025](http://gitlabghc.nibbler/ghc/ghc/issues/13025)</th>
<td>Type family reduction irregularity (change from 7.10.3 to 8.0.1)</td></tr>
<tr><th>[\#13001](http://gitlabghc.nibbler/ghc/ghc/issues/13001)</th>
<td>EnumFromThenTo is is not a good producer</td></tr>
<tr><th>[\#12996](http://gitlabghc.nibbler/ghc/ghc/issues/12996)</th>
<td>Memory leak in recursion when switching from -O1 to -O2</td></tr>
<tr><th>[\#12990](http://gitlabghc.nibbler/ghc/ghc/issues/12990)</th>
<td>Partially applied constructors with unpacked fields simplified badly</td></tr>
<tr><th>[\#12964](http://gitlabghc.nibbler/ghc/ghc/issues/12964)</th>
<td>Runtime regression to RTS change</td></tr>
<tr><th>[\#12804](http://gitlabghc.nibbler/ghc/ghc/issues/12804)</th>
<td>forever contains a space leak</td></tr>
<tr><th>[\#12781](http://gitlabghc.nibbler/ghc/ghc/issues/12781)</th>
<td>Significantly higher allocation with INLINE vs NOINLINE</td></tr>
<tr><th>[\#12603](http://gitlabghc.nibbler/ghc/ghc/issues/12603)</th>
<td>INLINE and manually inlining produce different code</td></tr>
<tr><th>[\#12525](http://gitlabghc.nibbler/ghc/ghc/issues/12525)</th>
<td>Internal identifiers creeping into :show bindings</td></tr>
<tr><th>[\#12378](http://gitlabghc.nibbler/ghc/ghc/issues/12378)</th>
<td>Not enough inlining happens with single-method type classes</td></tr>
<tr><th>[\#12354](http://gitlabghc.nibbler/ghc/ghc/issues/12354)</th>
<td>Word foldl' isn't optimized as well as Int foldl'</td></tr>
<tr><th>[\#12241](http://gitlabghc.nibbler/ghc/ghc/issues/12241)</th>
<td>Surprising constructor accumulation</td></tr>
<tr><th>[\#12217](http://gitlabghc.nibbler/ghc/ghc/issues/12217)</th>
<td>PowerPC NCG: Remove TOC save for calls.</td></tr>
<tr><th>[\#12129](http://gitlabghc.nibbler/ghc/ghc/issues/12129)</th>
<td>Optimize the implementation of minusInteger in the integer-gmp package</td></tr>
<tr><th>[\#12022](http://gitlabghc.nibbler/ghc/ghc/issues/12022)</th>
<td>unsafeShiftL and unsafeShiftR are not marked as INLINE</td></tr>
<tr><th>[\#11989](http://gitlabghc.nibbler/ghc/ghc/issues/11989)</th>
<td>Performance bug reading large-exponent float without explicit type</td></tr>
<tr><th>[\#11965](http://gitlabghc.nibbler/ghc/ghc/issues/11965)</th>
<td>USE\_PTHREAD\_FOR\_ITIMER causes unnecessary wake-ups</td></tr>
<tr><th>[\#11808](http://gitlabghc.nibbler/ghc/ghc/issues/11808)</th>
<td>nofib's cryptarithm1 regresses due to deferred inlining of Int's Ord operations</td></tr>
<tr><th>[\#11795](http://gitlabghc.nibbler/ghc/ghc/issues/11795)</th>
<td>Performance issues with replicateM\_</td></tr>
<tr><th>[\#11725](http://gitlabghc.nibbler/ghc/ghc/issues/11725)</th>
<td>Performance Regression from 7.8.3 to 7.10.3</td></tr>
<tr><th>[\#11710](http://gitlabghc.nibbler/ghc/ghc/issues/11710)</th>
<td>Fusion of a simple listArray call is very fragile</td></tr>
<tr><th>[\#11707](http://gitlabghc.nibbler/ghc/ghc/issues/11707)</th>
<td>Don't desugar large lists with build</td></tr>
<tr><th>[\#11701](http://gitlabghc.nibbler/ghc/ghc/issues/11701)</th>
<td>ghc generates significant slower code</td></tr>
<tr><th>[\#11688](http://gitlabghc.nibbler/ghc/ghc/issues/11688)</th>
<td>Bytestring break failing rewrite to breakByte and failing to eliminate boxing/unboxing</td></tr>
<tr><th>[\#11568](http://gitlabghc.nibbler/ghc/ghc/issues/11568)</th>
<td>Regression in nofib/shootout/k-nucleotide</td></tr>
<tr><th>[\#11565](http://gitlabghc.nibbler/ghc/ghc/issues/11565)</th>
<td>Restore code to handle '-fmax-worker-args' flag</td></tr>
<tr><th>[\#11533](http://gitlabghc.nibbler/ghc/ghc/issues/11533)</th>
<td>Stack check not optimized out even if it could be</td></tr>
<tr><th>[\#11486](http://gitlabghc.nibbler/ghc/ghc/issues/11486)</th>
<td>info tables are no longer aligned</td></tr>
<tr><th>[\#11383](http://gitlabghc.nibbler/ghc/ghc/issues/11383)</th>
<td>CAFs lose sharing due to implicit call stacks</td></tr>
<tr><th>[\#11382](http://gitlabghc.nibbler/ghc/ghc/issues/11382)</th>
<td>Optimize Data.Char</td></tr>
<tr><th>[\#11372](http://gitlabghc.nibbler/ghc/ghc/issues/11372)</th>
<td>Loopification does not trigger for IO even if it could</td></tr>
<tr><th>[\#11365](http://gitlabghc.nibbler/ghc/ghc/issues/11365)</th>
<td>Worse performance with -O</td></tr>
<tr><th>[\#11318](http://gitlabghc.nibbler/ghc/ghc/issues/11318)</th>
<td>Data.Text.length allocates one closure per character</td></tr>
<tr><th>[\#11284](http://gitlabghc.nibbler/ghc/ghc/issues/11284)</th>
<td>Lambda-lifting fails in simple Text example</td></tr>
<tr><th>[\#11273](http://gitlabghc.nibbler/ghc/ghc/issues/11273)</th>
<td>PowerPC NCG: Assign all STG float and double regs to PowerPC registers</td></tr>
<tr><th>[\#11272](http://gitlabghc.nibbler/ghc/ghc/issues/11272)</th>
<td>Overloaded state-monadic function is not specialised</td></tr>
<tr><th>[\#11116](http://gitlabghc.nibbler/ghc/ghc/issues/11116)</th>
<td>GC reports memory in use way below the actual</td></tr>
<tr><th>[\#11054](http://gitlabghc.nibbler/ghc/ghc/issues/11054)</th>
<td>GHC on Windows could not use more than 64 logical processors</td></tr>
<tr><th>[\#10830](http://gitlabghc.nibbler/ghc/ghc/issues/10830)</th>
<td>maximumBy has a space leak</td></tr>
<tr><th>[\#10825](http://gitlabghc.nibbler/ghc/ghc/issues/10825)</th>
<td>Poor performance of optimized code.</td></tr>
<tr><th>[\#10788](http://gitlabghc.nibbler/ghc/ghc/issues/10788)</th>
<td>performance regression involving minimum</td></tr>
<tr><th>[\#10780](http://gitlabghc.nibbler/ghc/ghc/issues/10780)</th>
<td>Weak reference is still alive if key is alive, but weak reference itself not reachable</td></tr>
<tr><th>[\#10750](http://gitlabghc.nibbler/ghc/ghc/issues/10750)</th>
<td>silly assembly for comparing Doubles</td></tr>
<tr><th>[\#10744](http://gitlabghc.nibbler/ghc/ghc/issues/10744)</th>
<td>Allow oneShot to work with unboxed types</td></tr>
<tr><th>[\#10720](http://gitlabghc.nibbler/ghc/ghc/issues/10720)</th>
<td>New GHC fails to specialize imported function</td></tr>
<tr><th>[\#10717](http://gitlabghc.nibbler/ghc/ghc/issues/10717)</th>
<td>fannkuch-redux allocations increase by factor of 10000 between 7.4.2 and 7.6.3</td></tr>
<tr><th>[\#10678](http://gitlabghc.nibbler/ghc/ghc/issues/10678)</th>
<td>integer-gmp's runS seems unnecessarily expensive</td></tr>
<tr><th>[\#10677](http://gitlabghc.nibbler/ghc/ghc/issues/10677)</th>
<td>slightly silly assembly for testing whether a Word\# is 0\#\#</td></tr>
<tr><th>[\#10676](http://gitlabghc.nibbler/ghc/ghc/issues/10676)</th>
<td>silly assembly for comparing the result of comparisons that return Int\# against 0\#</td></tr>
<tr><th>[\#10649](http://gitlabghc.nibbler/ghc/ghc/issues/10649)</th>
<td>Performance issue with unnecessary reboxing</td></tr>
<tr><th>[\#10457](http://gitlabghc.nibbler/ghc/ghc/issues/10457)</th>
<td>Revise/remove custom mapM implementation for lists</td></tr>
<tr><th>[\#10415](http://gitlabghc.nibbler/ghc/ghc/issues/10415)</th>
<td>ForeignPtr touched in FFI wrapper is never discarded</td></tr>
<tr><th>[\#10400](http://gitlabghc.nibbler/ghc/ghc/issues/10400)</th>
<td>Run time increases by 40% in fractal plotter core loop</td></tr>
<tr><th>[\#10359](http://gitlabghc.nibbler/ghc/ghc/issues/10359)</th>
<td>Tuple constraint synonym led to asymptotic performance lossage</td></tr>
<tr><th>[\#10291](http://gitlabghc.nibbler/ghc/ghc/issues/10291)</th>
<td>compiling huge HashSet hogs memory</td></tr>
<tr><th>[\#10290](http://gitlabghc.nibbler/ghc/ghc/issues/10290)</th>
<td>compiling huge HashSet hogs memory</td></tr>
<tr><th>[\#10260](http://gitlabghc.nibbler/ghc/ghc/issues/10260)</th>
<td>last uses too much space with optimizations disabled</td></tr>
<tr><th>[\#10148](http://gitlabghc.nibbler/ghc/ghc/issues/10148)</th>
<td>Optimization causes repeated computation</td></tr>
<tr><th>[\#10137](http://gitlabghc.nibbler/ghc/ghc/issues/10137)</th>
<td>Rewrite switch code generation</td></tr>
<tr><th>[\#10129](http://gitlabghc.nibbler/ghc/ghc/issues/10129)</th>
<td>emitCmmLitSwitch could be better</td></tr>
<tr><th>[\#10108](http://gitlabghc.nibbler/ghc/ghc/issues/10108)</th>
<td>Dramatic slowdown with -O2 bytestream and list streams combined.</td></tr>
<tr><th>[\#10067](http://gitlabghc.nibbler/ghc/ghc/issues/10067)</th>
<td>The Read Integer instance is too slow</td></tr>
<tr><th>[\#10064](http://gitlabghc.nibbler/ghc/ghc/issues/10064)</th>
<td>Add support for "foo"\#\# literals to MagicHash</td></tr>
<tr><th>[\#10060](http://gitlabghc.nibbler/ghc/ghc/issues/10060)</th>
<td>The Traversable instance for Array looks unlikely to be good</td></tr>
<tr><th>[\#10034](http://gitlabghc.nibbler/ghc/ghc/issues/10034)</th>
<td>Regression in mapM\_ performance</td></tr>
<tr><th>[\#10014](http://gitlabghc.nibbler/ghc/ghc/issues/10014)</th>
<td>Data.Array.Base.elems needlessly calls bounds.</td></tr>
<tr><th>[\#9885](http://gitlabghc.nibbler/ghc/ghc/issues/9885)</th>
<td>ghc-pkg parser eats too much memory</td></tr>
<tr><th>[\#9848](http://gitlabghc.nibbler/ghc/ghc/issues/9848)</th>
<td>List.all does not fuse</td></tr>
<tr><th>[\#9827](http://gitlabghc.nibbler/ghc/ghc/issues/9827)</th>
<td>void does not use \<$</td></tr>
<tr><th>[\#9801](http://gitlabghc.nibbler/ghc/ghc/issues/9801)</th>
<td>Make listArray fuse</td></tr>
<tr><th>[\#9797](http://gitlabghc.nibbler/ghc/ghc/issues/9797)</th>
<td>Investigate rewriting \`\>\>=\` to \`\*\>\` or \`\>\>\` for appropriate types</td></tr>
<tr><th>[\#9796](http://gitlabghc.nibbler/ghc/ghc/issues/9796)</th>
<td>Implement amap/coerce rule for \`Array\`</td></tr>
<tr><th>[\#9781](http://gitlabghc.nibbler/ghc/ghc/issues/9781)</th>
<td>Make list monad operations fuse</td></tr>
<tr><th>[\#9740](http://gitlabghc.nibbler/ghc/ghc/issues/9740)</th>
<td>D380 caused fft2 regressions</td></tr>
<tr><th>[\#9715](http://gitlabghc.nibbler/ghc/ghc/issues/9715)</th>
<td>The most minimal Gloss project causes the profiler to fail silently.</td></tr>
<tr><th>[\#9696](http://gitlabghc.nibbler/ghc/ghc/issues/9696)</th>
<td>readRawBufferPtr and writeRawBufferPtr allocate memory</td></tr>
<tr><th>[\#9676](http://gitlabghc.nibbler/ghc/ghc/issues/9676)</th>
<td>Data.List.isSuffixOf can be very inefficient</td></tr>
<tr><th>[\#9638](http://gitlabghc.nibbler/ghc/ghc/issues/9638)</th>
<td>Speed up Data.Char.isDigit</td></tr>
<tr><th>[\#9577](http://gitlabghc.nibbler/ghc/ghc/issues/9577)</th>
<td>String literals are wasting space</td></tr>
<tr><th>[\#9546](http://gitlabghc.nibbler/ghc/ghc/issues/9546)</th>
<td>filterM is not a good consumer for list fusion</td></tr>
<tr><th>[\#9540](http://gitlabghc.nibbler/ghc/ghc/issues/9540)</th>
<td>words is not a good producer; unwords is not a good consumer</td></tr>
<tr><th>[\#9537](http://gitlabghc.nibbler/ghc/ghc/issues/9537)</th>
<td>concatMap is not a good producer for list fusion</td></tr>
<tr><th>[\#9520](http://gitlabghc.nibbler/ghc/ghc/issues/9520)</th>
<td>Running an action twice uses much more memory than running it once</td></tr>
<tr><th>[\#9510](http://gitlabghc.nibbler/ghc/ghc/issues/9510)</th>
<td>Prelude.!! is not a good consumer</td></tr>
<tr><th>[\#9509](http://gitlabghc.nibbler/ghc/ghc/issues/9509)</th>
<td>No automatic specialization of inlinable imports in 7.8</td></tr>
<tr><th>[\#9502](http://gitlabghc.nibbler/ghc/ghc/issues/9502)</th>
<td>mapAccumL does not participate in foldr/build fusion</td></tr>
<tr><th>[\#9476](http://gitlabghc.nibbler/ghc/ghc/issues/9476)</th>
<td>Implement late lambda-lifting</td></tr>
<tr><th>[\#9441](http://gitlabghc.nibbler/ghc/ghc/issues/9441)</th>
<td>CSE should deal with letrec</td></tr>
<tr><th>[\#9430](http://gitlabghc.nibbler/ghc/ghc/issues/9430)</th>
<td>implement more arithmetic operations natively in the LLVM backend</td></tr>
<tr><th>[\#9398](http://gitlabghc.nibbler/ghc/ghc/issues/9398)</th>
<td>Data.List.cycle is not a good producer</td></tr>
<tr><th>[\#9369](http://gitlabghc.nibbler/ghc/ghc/issues/9369)</th>
<td>Data.List.unfoldr does not fuse and is not inlined.</td></tr>
<tr><th>[\#9356](http://gitlabghc.nibbler/ghc/ghc/issues/9356)</th>
<td>scanl does not participate in list fusion</td></tr>
<tr><th>[\#9355](http://gitlabghc.nibbler/ghc/ghc/issues/9355)</th>
<td>scanr does not participate in stream fusion</td></tr>
<tr><th>[\#9345](http://gitlabghc.nibbler/ghc/ghc/issues/9345)</th>
<td>Data.List.inits is extremely slow</td></tr>
<tr><th>[\#9344](http://gitlabghc.nibbler/ghc/ghc/issues/9344)</th>
<td>takeWhile does not participate in list fusion</td></tr>
<tr><th>[\#9343](http://gitlabghc.nibbler/ghc/ghc/issues/9343)</th>
<td>foldl' is not a good consumer</td></tr>
<tr><th>[\#9339](http://gitlabghc.nibbler/ghc/ghc/issues/9339)</th>
<td>last is not a good consumer</td></tr>
<tr><th>[\#9332](http://gitlabghc.nibbler/ghc/ghc/issues/9332)</th>
<td>Memory blowing up for strict sum/strict foldl in ghci</td></tr>
<tr><th>[\#9326](http://gitlabghc.nibbler/ghc/ghc/issues/9326)</th>
<td>Minor change to list comprehension structure leads to poor performance</td></tr>
<tr><th>[\#9291](http://gitlabghc.nibbler/ghc/ghc/issues/9291)</th>
<td>Don't reconstruct sum types if the type subtly changes</td></tr>
<tr><th>[\#9234](http://gitlabghc.nibbler/ghc/ghc/issues/9234)</th>
<td>Compiled code performance regression</td></tr>
<tr><th>[\#9214](http://gitlabghc.nibbler/ghc/ghc/issues/9214)</th>
<td>UNPACK support for sum types</td></tr>
<tr><th>[\#9203](http://gitlabghc.nibbler/ghc/ghc/issues/9203)</th>
<td>Perf regression in 7.8.2 relative to 7.6.3, possibly related to HashMap</td></tr>
<tr><th>[\#9188](http://gitlabghc.nibbler/ghc/ghc/issues/9188)</th>
<td>quot with a power of two is not optimized to a shift</td></tr>
<tr><th>[\#9159](http://gitlabghc.nibbler/ghc/ghc/issues/9159)</th>
<td>cmm case, binary search instead of jump table</td></tr>
<tr><th>[\#9157](http://gitlabghc.nibbler/ghc/ghc/issues/9157)</th>
<td>cmm common block not eliminated</td></tr>
<tr><th>[\#9136](http://gitlabghc.nibbler/ghc/ghc/issues/9136)</th>
<td>Constant folding in Core could be better</td></tr>
<tr><th>[\#9132](http://gitlabghc.nibbler/ghc/ghc/issues/9132)</th>
<td>takeWhile&C. still not fusible</td></tr>
<tr><th>[\#9105](http://gitlabghc.nibbler/ghc/ghc/issues/9105)</th>
<td>Profiling binary consumes CPU even when idle on Linux.</td></tr>
<tr><th>[\#9075](http://gitlabghc.nibbler/ghc/ghc/issues/9075)</th>
<td>Per-thread weak pointer list (remove global lock on mkWeak\#)</td></tr>
<tr><th>[\#9067](http://gitlabghc.nibbler/ghc/ghc/issues/9067)</th>
<td>Optimize clearNursery by short-circuiting when we get to currentNursery</td></tr>
<tr><th>[\#9021](http://gitlabghc.nibbler/ghc/ghc/issues/9021)</th>
<td>\[CID43168\] rts/linker.c has a memory leak in the dlopen/dlerror code</td></tr>
<tr><th>[\#8901](http://gitlabghc.nibbler/ghc/ghc/issues/8901)</th>
<td>(very) bad inline heuristics</td></tr>
<tr><th>[\#8900](http://gitlabghc.nibbler/ghc/ghc/issues/8900)</th>
<td>Strictness analysis regression</td></tr>
<tr><th>[\#8835](http://gitlabghc.nibbler/ghc/ghc/issues/8835)</th>
<td>7.6.3 vs 7.8-RC performance regression</td></tr>
<tr><th>[\#8832](http://gitlabghc.nibbler/ghc/ghc/issues/8832)</th>
<td>Constant-folding regression wrt \`clearBit (bit 0) 0 \`</td></tr>
<tr><th>[\#8793](http://gitlabghc.nibbler/ghc/ghc/issues/8793)</th>
<td>Improve GHC.Event.IntTable performance</td></tr>
<tr><th>[\#8766](http://gitlabghc.nibbler/ghc/ghc/issues/8766)</th>
<td>length \[Integer\] is twice as slow but length \[Int\] is 10 times faster</td></tr>
<tr><th>[\#8763](http://gitlabghc.nibbler/ghc/ghc/issues/8763)</th>
<td>forM\_ \[1..N\] does not get fused (allocates 50% more)</td></tr>
<tr><th>[\#8680](http://gitlabghc.nibbler/ghc/ghc/issues/8680)</th>
<td>In STM: Variables only in left branch of orElse can invalidate the right branch transaction</td></tr>
<tr><th>[\#8647](http://gitlabghc.nibbler/ghc/ghc/issues/8647)</th>
<td>Reduce allocations in \`integer-gmp\`</td></tr>
<tr><th>[\#8638](http://gitlabghc.nibbler/ghc/ghc/issues/8638)</th>
<td>Optimize by demoting "denormalized" Integers (i.e. J\# -\> S\#)</td></tr>
<tr><th>[\#8609](http://gitlabghc.nibbler/ghc/ghc/issues/8609)</th>
<td>Clean up block allocator</td></tr>
<tr><th>[\#8585](http://gitlabghc.nibbler/ghc/ghc/issues/8585)</th>
<td>Loopification should omit stack check</td></tr>
<tr><th>[\#8513](http://gitlabghc.nibbler/ghc/ghc/issues/8513)</th>
<td>Parallel GC increases CPU load while slowing down program</td></tr>
<tr><th>[\#8508](http://gitlabghc.nibbler/ghc/ghc/issues/8508)</th>
<td>Inlining Unsaturated Function Applications</td></tr>
<tr><th>[\#8472](http://gitlabghc.nibbler/ghc/ghc/issues/8472)</th>
<td>Primitive string literals prevent optimization</td></tr>
<tr><th>[\#8456](http://gitlabghc.nibbler/ghc/ghc/issues/8456)</th>
<td>Control flow optimisations duplicate blocks</td></tr>
<tr><th>[\#8435](http://gitlabghc.nibbler/ghc/ghc/issues/8435)</th>
<td>Do not copy stack after stack overflow</td></tr>
<tr><th>[\#8345](http://gitlabghc.nibbler/ghc/ghc/issues/8345)</th>
<td>A more efficient atomicModifyIORef'</td></tr>
<tr><th>[\#8321](http://gitlabghc.nibbler/ghc/ghc/issues/8321)</th>
<td>improve basic block layout on LLVM backend by predicting stack/heap checks</td></tr>
<tr><th>[\#8255](http://gitlabghc.nibbler/ghc/ghc/issues/8255)</th>
<td>GC Less Operation</td></tr>
<tr><th>[\#8224](http://gitlabghc.nibbler/ghc/ghc/issues/8224)</th>
<td>Excessive system time -- new IO manager problem?</td></tr>
<tr><th>[\#8124](http://gitlabghc.nibbler/ghc/ghc/issues/8124)</th>
<td>Possible leaks when using foreign export.</td></tr>
<tr><th>[\#8082](http://gitlabghc.nibbler/ghc/ghc/issues/8082)</th>
<td>Ordering of assembly blocks affects performance</td></tr>
<tr><th>[\#8027](http://gitlabghc.nibbler/ghc/ghc/issues/8027)</th>
<td>Adding one call to getNumCapabilities triggers performance nose dive (6X slowdown)</td></tr>
<tr><th>[\#7954](http://gitlabghc.nibbler/ghc/ghc/issues/7954)</th>
<td>Strictness analysis regression</td></tr>
<tr><th>[\#7923](http://gitlabghc.nibbler/ghc/ghc/issues/7923)</th>
<td>Optimization for takeMVar/putMVar when MVar left empty</td></tr>
<tr><th>[\#7865](http://gitlabghc.nibbler/ghc/ghc/issues/7865)</th>
<td>SpecConstr duplicating computations</td></tr>
<tr><th>[\#7850](http://gitlabghc.nibbler/ghc/ghc/issues/7850)</th>
<td>Strangely high memory usage on optimized Ackermann function</td></tr>
<tr><th>[\#7837](http://gitlabghc.nibbler/ghc/ghc/issues/7837)</th>
<td>Rules involving equality constraints don't fire</td></tr>
<tr><th>[\#7785](http://gitlabghc.nibbler/ghc/ghc/issues/7785)</th>
<td>Module-local function not specialized with ConstraintKinds</td></tr>
<tr><th>[\#7611](http://gitlabghc.nibbler/ghc/ghc/issues/7611)</th>
<td>Rewrite rules application prevented by type variable application (map id vs. map (\\x -\> x))</td></tr>
<tr><th>[\#7561](http://gitlabghc.nibbler/ghc/ghc/issues/7561)</th>
<td>Unnecessary Heap Allocations - Slow Performance</td></tr>
<tr><th>[\#7556](http://gitlabghc.nibbler/ghc/ghc/issues/7556)</th>
<td>build/fold causes with ByteString unpack causes huge memory leak</td></tr>
<tr><th>[\#7460](http://gitlabghc.nibbler/ghc/ghc/issues/7460)</th>
<td>Double literals generated bad core</td></tr>
<tr><th>[\#7436](http://gitlabghc.nibbler/ghc/ghc/issues/7436)</th>
<td>Derived Foldable and Traversable instances become extremely inefficient due to eta-expansion</td></tr>
<tr><th>[\#7429](http://gitlabghc.nibbler/ghc/ghc/issues/7429)</th>
<td>Unexplained performance boost with +RTS -h</td></tr>
<tr><th>[\#7418](http://gitlabghc.nibbler/ghc/ghc/issues/7418)</th>
<td>Writing to stderr is 7x slower than writing to stdout</td></tr>
<tr><th>[\#7382](http://gitlabghc.nibbler/ghc/ghc/issues/7382)</th>
<td>Evaluating GHCi expressions is slow following the dynamic-by-default change</td></tr>
<tr><th>[\#7363](http://gitlabghc.nibbler/ghc/ghc/issues/7363)</th>
<td>runghc leaks space in IO</td></tr>
<tr><th>[\#7292](http://gitlabghc.nibbler/ghc/ghc/issues/7292)</th>
<td>Optimization works for Word but not Word32 or Word64</td></tr>
<tr><th>[\#7284](http://gitlabghc.nibbler/ghc/ghc/issues/7284)</th>
<td>plusAddr\# x 0 isn't optimised away</td></tr>
<tr><th>[\#7257](http://gitlabghc.nibbler/ghc/ghc/issues/7257)</th>
<td>Regression: pinned memory fragmentation</td></tr>
<tr><th>[\#7219](http://gitlabghc.nibbler/ghc/ghc/issues/7219)</th>
<td>Reinstate constant propagation in some form</td></tr>
<tr><th>[\#7211](http://gitlabghc.nibbler/ghc/ghc/issues/7211)</th>
<td>Huge space leak on a program that shouldn't leak</td></tr>
<tr><th>[\#7116](http://gitlabghc.nibbler/ghc/ghc/issues/7116)</th>
<td>Missing optimisation: strength reduction of floating-point multiplication</td></tr>
<tr><th>[\#7091](http://gitlabghc.nibbler/ghc/ghc/issues/7091)</th>
<td>DPH Matrix product memory usage</td></tr>
<tr><th>[\#7058](http://gitlabghc.nibbler/ghc/ghc/issues/7058)</th>
<td>Add strict version of modifySTRef</td></tr>
<tr><th>[\#7052](http://gitlabghc.nibbler/ghc/ghc/issues/7052)</th>
<td>Numeric types’ Read instances use exponential CPU/memory</td></tr>
<tr><th>[\#6166](http://gitlabghc.nibbler/ghc/ghc/issues/6166)</th>
<td>Performance regression in mwc-random since 7.0.x</td></tr>
<tr><th>[\#6121](http://gitlabghc.nibbler/ghc/ghc/issues/6121)</th>
<td>Very poor constant folding</td></tr>
<tr><th>[\#6111](http://gitlabghc.nibbler/ghc/ghc/issues/6111)</th>
<td>Simple loop performance regression of 7.4.1 relative to 7.0.4</td></tr>
<tr><th>[\#6110](http://gitlabghc.nibbler/ghc/ghc/issues/6110)</th>
<td>Data.Vector.Unboxed performance regression of 7.4.1 relative to 7.0.4</td></tr>
<tr><th>[\#6082](http://gitlabghc.nibbler/ghc/ghc/issues/6082)</th>
<td>Program compiled with 7.4.1 runs many times slower than compiled with 7.2.2</td></tr>
<tr><th>[\#6056](http://gitlabghc.nibbler/ghc/ghc/issues/6056)</th>
<td>INLINABLE pragma prevents worker-wrapper to happen.</td></tr>
<tr><th>[\#6000](http://gitlabghc.nibbler/ghc/ghc/issues/6000)</th>
<td>Performance of Fibonnaci compare to Python</td></tr>
<tr><th>[\#5996](http://gitlabghc.nibbler/ghc/ghc/issues/5996)</th>
<td>fix for CSE</td></tr>
<tr><th>[\#5991](http://gitlabghc.nibbler/ghc/ghc/issues/5991)</th>
<td>regression: huge number of wakeups in xmonad</td></tr>
<tr><th>[\#5949](http://gitlabghc.nibbler/ghc/ghc/issues/5949)</th>
<td>Demand analysis attributes manifestly wrong demand type</td></tr>
<tr><th>[\#5945](http://gitlabghc.nibbler/ghc/ghc/issues/5945)</th>
<td>Lambda lifting</td></tr>
<tr><th>[\#5926](http://gitlabghc.nibbler/ghc/ghc/issues/5926)</th>
<td>Add strict versions of modifyIORef and atomicModifyIORef</td></tr>
<tr><th>[\#5916](http://gitlabghc.nibbler/ghc/ghc/issues/5916)</th>
<td>runST isn't free</td></tr>
<tr><th>[\#5888](http://gitlabghc.nibbler/ghc/ghc/issues/5888)</th>
<td>Performance regression in 7.4.1 compared to 6.12.3</td></tr>
<tr><th>[\#5835](http://gitlabghc.nibbler/ghc/ghc/issues/5835)</th>
<td>Make better use of known dictionaries</td></tr>
<tr><th>[\#5809](http://gitlabghc.nibbler/ghc/ghc/issues/5809)</th>
<td>Arity analysis could be better</td></tr>
<tr><th>[\#5779](http://gitlabghc.nibbler/ghc/ghc/issues/5779)</th>
<td>SPECIALISE pragma generates wrong activations</td></tr>
<tr><th>[\#5776](http://gitlabghc.nibbler/ghc/ghc/issues/5776)</th>
<td>Rule matching regression</td></tr>
<tr><th>[\#5774](http://gitlabghc.nibbler/ghc/ghc/issues/5774)</th>
<td>main = forever (putStrLn =\<\< getLine)   continuously saturates a CPU when compiled</td></tr>
<tr><th>[\#5773](http://gitlabghc.nibbler/ghc/ghc/issues/5773)</th>
<td>main = forever (putStrLn =\<\< getLine)   continuously saturates a CPU when compiled</td></tr>
<tr><th>[\#5767](http://gitlabghc.nibbler/ghc/ghc/issues/5767)</th>
<td>Integer inefficiencies</td></tr>
<tr><th>[\#5749](http://gitlabghc.nibbler/ghc/ghc/issues/5749)</th>
<td>GHC 7.0.4 Performance Regression (Possibly Vector)</td></tr>
<tr><th>[\#5741](http://gitlabghc.nibbler/ghc/ghc/issues/5741)</th>
<td>openFile should fail if null bytes are in the argument</td></tr>
<tr><th>[\#5731](http://gitlabghc.nibbler/ghc/ghc/issues/5731)</th>
<td>Bad code for Double literals</td></tr>
<tr><th>[\#5715](http://gitlabghc.nibbler/ghc/ghc/issues/5715)</th>
<td>Inliner fails to inline a function, causing 20x slowdown</td></tr>
<tr><th>[\#5623](http://gitlabghc.nibbler/ghc/ghc/issues/5623)</th>
<td>GHC 7.2.1 Performance Regression: Vector</td></tr>
<tr><th>[\#5615](http://gitlabghc.nibbler/ghc/ghc/issues/5615)</th>
<td>ghc produces poor code for \`div\` with constant powers of 2.</td></tr>
<tr><th>[\#5598](http://gitlabghc.nibbler/ghc/ghc/issues/5598)</th>
<td>Function quotRem is inefficient</td></tr>
<tr><th>[\#5569](http://gitlabghc.nibbler/ghc/ghc/issues/5569)</th>
<td>Ineffective seq/BangPatterns</td></tr>
<tr><th>[\#5549](http://gitlabghc.nibbler/ghc/ghc/issues/5549)</th>
<td>\~100% performance regression in HEAD compared to ghc6.12, \~22% compared to 7.0.4</td></tr>
<tr><th>[\#5505](http://gitlabghc.nibbler/ghc/ghc/issues/5505)</th>
<td>Program runs faster with profiling than without</td></tr>
<tr><th>[\#5367](http://gitlabghc.nibbler/ghc/ghc/issues/5367)</th>
<td>Program in (-N1) runs 10 times slower than it with two threads (-N2)</td></tr>
<tr><th>[\#5339](http://gitlabghc.nibbler/ghc/ghc/issues/5339)</th>
<td>Data.Bits instances should use default shift instead of shiftL/shiftR</td></tr>
<tr><th>[\#5327](http://gitlabghc.nibbler/ghc/ghc/issues/5327)</th>
<td>INLINABLE pragma and newtypes prevents inlining</td></tr>
<tr><th>[\#5237](http://gitlabghc.nibbler/ghc/ghc/issues/5237)</th>
<td>Inefficient code generated for x\^2</td></tr>
<tr><th>[\#5205](http://gitlabghc.nibbler/ghc/ghc/issues/5205)</th>
<td>Control.Monad.forever leaks space</td></tr>
<tr><th>[\#5161](http://gitlabghc.nibbler/ghc/ghc/issues/5161)</th>
<td>Poor performance of division; unnecessary branching</td></tr>
<tr><th>[\#5152](http://gitlabghc.nibbler/ghc/ghc/issues/5152)</th>
<td>GHC generates poor code for large 64-bit literals</td></tr>
<tr><th>[\#5113](http://gitlabghc.nibbler/ghc/ghc/issues/5113)</th>
<td>Huge performance regression of 7.0.2, 7.0.3 and HEAD over 7.0.1 and 6.12 (MonoLocalBinds)</td></tr>
<tr><th>[\#5034](http://gitlabghc.nibbler/ghc/ghc/issues/5034)</th>
<td>Performance of Data.Graph.{preorderF, postorderF}</td></tr>
<tr><th>[\#5000](http://gitlabghc.nibbler/ghc/ghc/issues/5000)</th>
<td>Eliminate absent arguments in non-strict positions</td></tr>
<tr><th>[\#4986](http://gitlabghc.nibbler/ghc/ghc/issues/4986)</th>
<td>negative Double numbers print out all wrong</td></tr>
<tr><th>[\#4965](http://gitlabghc.nibbler/ghc/ghc/issues/4965)</th>
<td>60% performance regression in continuation-heavy code between 6.12 and 7</td></tr>
<tr><th>[\#4962](http://gitlabghc.nibbler/ghc/ghc/issues/4962)</th>
<td>Dead code fed to CorePrep because RULEs keep it alive spuriously</td></tr>
<tr><th>[\#4951](http://gitlabghc.nibbler/ghc/ghc/issues/4951)</th>
<td>Performance regression 7.0.1 -\> 7.0.1.20110201</td></tr>
<tr><th>[\#4943](http://gitlabghc.nibbler/ghc/ghc/issues/4943)</th>
<td>Another odd missed SpecConstr opportunity</td></tr>
<tr><th>[\#4930](http://gitlabghc.nibbler/ghc/ghc/issues/4930)</th>
<td>Case-of-case not eliminated when it could be</td></tr>
<tr><th>[\#4908](http://gitlabghc.nibbler/ghc/ghc/issues/4908)</th>
<td>Easy SpecConstr opportunity that is nonetheless missed</td></tr>
<tr><th>[\#4495](http://gitlabghc.nibbler/ghc/ghc/issues/4495)</th>
<td>GHC fails to inline methods of single-method classes</td></tr>
<tr><th>[\#4474](http://gitlabghc.nibbler/ghc/ghc/issues/4474)</th>
<td>3 ways to write a function (unexpected performance difference and regression)</td></tr>
<tr><th>[\#4463](http://gitlabghc.nibbler/ghc/ghc/issues/4463)</th>
<td>CORE notes break optimisation</td></tr>
<tr><th>[\#4448](http://gitlabghc.nibbler/ghc/ghc/issues/4448)</th>
<td>Another case of SpecConstr not specialising</td></tr>
<tr><th>[\#4442](http://gitlabghc.nibbler/ghc/ghc/issues/4442)</th>
<td>Add unaligned version of indexWordArray\#</td></tr>
<tr><th>[\#4431](http://gitlabghc.nibbler/ghc/ghc/issues/4431)</th>
<td>SpecConstr doesn't specialise</td></tr>
<tr><th>[\#4428](http://gitlabghc.nibbler/ghc/ghc/issues/4428)</th>
<td>Local functions lose their unfoldings</td></tr>
<tr><th>[\#4397](http://gitlabghc.nibbler/ghc/ghc/issues/4397)</th>
<td>RULES for Class ops don't fire in HEAD</td></tr>
<tr><th>[\#4365](http://gitlabghc.nibbler/ghc/ghc/issues/4365)</th>
<td>Error handle in readProcess not closed</td></tr>
<tr><th>[\#4344](http://gitlabghc.nibbler/ghc/ghc/issues/4344)</th>
<td>Better toRational for Float and Double</td></tr>
<tr><th>[\#4337](http://gitlabghc.nibbler/ghc/ghc/issues/4337)</th>
<td>Better power for Rational</td></tr>
<tr><th>[\#4322](http://gitlabghc.nibbler/ghc/ghc/issues/4322)</th>
<td>High CPU usage during idle time due to GC</td></tr>
<tr><th>[\#4306](http://gitlabghc.nibbler/ghc/ghc/issues/4306)</th>
<td>UNPACK can lead to unnecessary copying and wasted stack space</td></tr>
<tr><th>[\#4285](http://gitlabghc.nibbler/ghc/ghc/issues/4285)</th>
<td>STM bug on Windows?</td></tr>
<tr><th>[\#4280](http://gitlabghc.nibbler/ghc/ghc/issues/4280)</th>
<td>Proposal: Performance improvements for Data.Set</td></tr>
<tr><th>[\#4279](http://gitlabghc.nibbler/ghc/ghc/issues/4279)</th>
<td>Proposal: Performance improvements for Data.IntMap</td></tr>
<tr><th>[\#4278](http://gitlabghc.nibbler/ghc/ghc/issues/4278)</th>
<td>Proposal: Add strict versions of foldlWithKey and insertLookupWithKey to Data.Map</td></tr>
<tr><th>[\#4277](http://gitlabghc.nibbler/ghc/ghc/issues/4277)</th>
<td>Proposal: Significant performance improvements for Data.Map</td></tr>
<tr><th>[\#4276](http://gitlabghc.nibbler/ghc/ghc/issues/4276)</th>
<td>-O0 runs in constant space, -O1 and -O2 don't</td></tr>
<tr><th>[\#4262](http://gitlabghc.nibbler/ghc/ghc/issues/4262)</th>
<td>GHC's runtime never terminates unused worker threads</td></tr>
<tr><th>[\#4223](http://gitlabghc.nibbler/ghc/ghc/issues/4223)</th>
<td>LLVM slower then NCG, C example</td></tr>
<tr><th>[\#4184](http://gitlabghc.nibbler/ghc/ghc/issues/4184)</th>
<td>Squirrelly inliner behaviour leads to 80x slowdown</td></tr>
<tr><th>[\#4138](http://gitlabghc.nibbler/ghc/ghc/issues/4138)</th>
<td>Performance regression in overloading</td></tr>
<tr><th>[\#4120](http://gitlabghc.nibbler/ghc/ghc/issues/4120)</th>
<td>Iface type variable out of scope in cast</td></tr>
<tr><th>[\#4065](http://gitlabghc.nibbler/ghc/ghc/issues/4065)</th>
<td>Inconsistent loop performance</td></tr>
<tr><th>[\#4064](http://gitlabghc.nibbler/ghc/ghc/issues/4064)</th>
<td>SpecConstr broken for NOINLINE loops in 6.13</td></tr>
<tr><th>[\#4062](http://gitlabghc.nibbler/ghc/ghc/issues/4062)</th>
<td>Bad choice of loop breaker?</td></tr>
<tr><th>[\#4021](http://gitlabghc.nibbler/ghc/ghc/issues/4021)</th>
<td>Problem of Interaction Between the FreeBSD Kernel and the GHC RTS</td></tr>
<tr><th>[\#4018](http://gitlabghc.nibbler/ghc/ghc/issues/4018)</th>
<td>Concurrency space leak</td></tr>
<tr><th>[\#4007](http://gitlabghc.nibbler/ghc/ghc/issues/4007)</th>
<td>Look again at eta expansion during gentle simplification</td></tr>
<tr><th>[\#4004](http://gitlabghc.nibbler/ghc/ghc/issues/4004)</th>
<td>Improve performance of a few functions in Foreign.Marshal.\*</td></tr>
<tr><th>[\#3990](http://gitlabghc.nibbler/ghc/ghc/issues/3990)</th>
<td>UNPACK doesn't unbox data families</td></tr>
<tr><th>[\#3969](http://gitlabghc.nibbler/ghc/ghc/issues/3969)</th>
<td>Poor performance of generated code on x86.</td></tr>
<tr><th>[\#3938](http://gitlabghc.nibbler/ghc/ghc/issues/3938)</th>
<td>Data growth issue in System.Timeout</td></tr>
<tr><th>[\#3838](http://gitlabghc.nibbler/ghc/ghc/issues/3838)</th>
<td>Performance issues with blackholes</td></tr>
<tr><th>[\#3772](http://gitlabghc.nibbler/ghc/ghc/issues/3772)</th>
<td>Methods not inlined</td></tr>
<tr><th>[\#3738](http://gitlabghc.nibbler/ghc/ghc/issues/3738)</th>
<td>Typechecker floats stuff out of INLINE right hand sides</td></tr>
<tr><th>[\#3737](http://gitlabghc.nibbler/ghc/ghc/issues/3737)</th>
<td>inlining happens on foldl1 and does not happen on direct application of combinator</td></tr>
<tr><th>[\#3736](http://gitlabghc.nibbler/ghc/ghc/issues/3736)</th>
<td>GHC specialising instead of inlining</td></tr>
<tr><th>[\#3735](http://gitlabghc.nibbler/ghc/ghc/issues/3735)</th>
<td>GHC specialising instead of inlining</td></tr>
<tr><th>[\#3717](http://gitlabghc.nibbler/ghc/ghc/issues/3717)</th>
<td>Superfluous seq no eliminated</td></tr>
<tr><th>[\#3709](http://gitlabghc.nibbler/ghc/ghc/issues/3709)</th>
<td>Data.Either.partitionEithers is not lazy enough</td></tr>
<tr><th>[\#3698](http://gitlabghc.nibbler/ghc/ghc/issues/3698)</th>
<td>Bad code generated for zip/filter/filter loop</td></tr>
<tr><th>[\#3697](http://gitlabghc.nibbler/ghc/ghc/issues/3697)</th>
<td>Method selectors aren't floated out of loops</td></tr>
<tr><th>[\#3655](http://gitlabghc.nibbler/ghc/ghc/issues/3655)</th>
<td>Performance regression relative to 6.10</td></tr>
<tr><th>[\#3627](http://gitlabghc.nibbler/ghc/ghc/issues/3627)</th>
<td>Profiling loses eta-expansion opportunities unnecessarily</td></tr>
<tr><th>[\#3586](http://gitlabghc.nibbler/ghc/ghc/issues/3586)</th>
<td>Initialisation of unboxed arrays is too slow</td></tr>
<tr><th>[\#3526](http://gitlabghc.nibbler/ghc/ghc/issues/3526)</th>
<td>Inliner behaviour with instances is confusing</td></tr>
<tr><th>[\#3518](http://gitlabghc.nibbler/ghc/ghc/issues/3518)</th>
<td>GHC GC rises greatly on -N8 compared to -N7</td></tr>
<tr><th>[\#3501](http://gitlabghc.nibbler/ghc/ghc/issues/3501)</th>
<td>Error thunks not being exposed with "B" strictness</td></tr>
<tr><th>[\#3437](http://gitlabghc.nibbler/ghc/ghc/issues/3437)</th>
<td>Optimizer creates space leak on simple code</td></tr>
<tr><th>[\#3349](http://gitlabghc.nibbler/ghc/ghc/issues/3349)</th>
<td>poor responsiveness of ghci</td></tr>
<tr><th>[\#3331](http://gitlabghc.nibbler/ghc/ghc/issues/3331)</th>
<td>control-monad-queue performance regression</td></tr>
<tr><th>[\#3273](http://gitlabghc.nibbler/ghc/ghc/issues/3273)</th>
<td>memory leak due to optimisation</td></tr>
<tr><th>[\#3264](http://gitlabghc.nibbler/ghc/ghc/issues/3264)</th>
<td>Real World Haskell book example issue</td></tr>
<tr><th>[\#3245](http://gitlabghc.nibbler/ghc/ghc/issues/3245)</th>
<td>Quadratic slowdown in Data.Typeable</td></tr>
<tr><th>[\#3181](http://gitlabghc.nibbler/ghc/ghc/issues/3181)</th>
<td>Regression in unboxing</td></tr>
<tr><th>[\#3123](http://gitlabghc.nibbler/ghc/ghc/issues/3123)</th>
<td>make INLINE work for recursive definitions (generalized loop peeling/loop unrolling)</td></tr>
<tr><th>[\#3116](http://gitlabghc.nibbler/ghc/ghc/issues/3116)</th>
<td>missed opportunity for call-pattern specialisation</td></tr>
<tr><th>[\#3076](http://gitlabghc.nibbler/ghc/ghc/issues/3076)</th>
<td>Make genericLength tail-recursive so it doesn't overflow stack</td></tr>
<tr><th>[\#3065](http://gitlabghc.nibbler/ghc/ghc/issues/3065)</th>
<td>Reorder tests in quot to improve code</td></tr>
<tr><th>[\#2940](http://gitlabghc.nibbler/ghc/ghc/issues/2940)</th>
<td>Do CSE after CorePrep</td></tr>
<tr><th>[\#2915](http://gitlabghc.nibbler/ghc/ghc/issues/2915)</th>
<td>Arity is smaller than need be</td></tr>
<tr><th>[\#2902](http://gitlabghc.nibbler/ghc/ghc/issues/2902)</th>
<td>Example where ghc 6.10.1 fails to optimize recursive instance function calls</td></tr>
<tr><th>[\#2884](http://gitlabghc.nibbler/ghc/ghc/issues/2884)</th>
<td>Compiled code performance worsens when module names are long enough</td></tr>
<tr><th>[\#2840](http://gitlabghc.nibbler/ghc/ghc/issues/2840)</th>
<td>Top level string literals</td></tr>
<tr><th>[\#2831](http://gitlabghc.nibbler/ghc/ghc/issues/2831)</th>
<td>Floated error expressions get poor strictness, leaving bad arity</td></tr>
<tr><th>[\#2823](http://gitlabghc.nibbler/ghc/ghc/issues/2823)</th>
<td>Another arity expansion bug</td></tr>
<tr><th>[\#2822](http://gitlabghc.nibbler/ghc/ghc/issues/2822)</th>
<td>Arity expansion not working right</td></tr>
<tr><th>[\#2797](http://gitlabghc.nibbler/ghc/ghc/issues/2797)</th>
<td>ghci stack overflows when ghc does not</td></tr>
<tr><th>[\#2785](http://gitlabghc.nibbler/ghc/ghc/issues/2785)</th>
<td>Memory leakage with socket benchmark program</td></tr>
<tr><th>[\#2727](http://gitlabghc.nibbler/ghc/ghc/issues/2727)</th>
<td>DiffArray performance unusable for advertized purpose</td></tr>
<tr><th>[\#2720](http://gitlabghc.nibbler/ghc/ghc/issues/2720)</th>
<td>eyeball/inline1 still isn't optimised with -fno-method-sharing</td></tr>
<tr><th>[\#2712](http://gitlabghc.nibbler/ghc/ghc/issues/2712)</th>
<td>Parallel GC scheduling problems</td></tr>
<tr><th>[\#2581](http://gitlabghc.nibbler/ghc/ghc/issues/2581)</th>
<td>Record selectors not being inlined</td></tr>
<tr><th>[\#2463](http://gitlabghc.nibbler/ghc/ghc/issues/2463)</th>
<td>unsafePerformIO in unused record field affects optimisations</td></tr>
<tr><th>[\#2462](http://gitlabghc.nibbler/ghc/ghc/issues/2462)</th>
<td>Data.List.sum is slower than 6.8.3</td></tr>
<tr><th>[\#2450](http://gitlabghc.nibbler/ghc/ghc/issues/2450)</th>
<td>Data.Complex.magnitude squares using \^(2 :: Int), which is slow</td></tr>
<tr><th>[\#2440](http://gitlabghc.nibbler/ghc/ghc/issues/2440)</th>
<td>Bad code with type families</td></tr>
<tr><th>[\#2396](http://gitlabghc.nibbler/ghc/ghc/issues/2396)</th>
<td>Default class method not inlined</td></tr>
<tr><th>[\#2329](http://gitlabghc.nibbler/ghc/ghc/issues/2329)</th>
<td>Control.Parallel.Strategies: definitions of rnf for most collections are poor</td></tr>
<tr><th>[\#2325](http://gitlabghc.nibbler/ghc/ghc/issues/2325)</th>
<td>Compile-time computations</td></tr>
<tr><th>[\#2280](http://gitlabghc.nibbler/ghc/ghc/issues/2280)</th>
<td>randomR too slow</td></tr>
<tr><th>[\#2253](http://gitlabghc.nibbler/ghc/ghc/issues/2253)</th>
<td>Native code generator could do better</td></tr>
<tr><th>[\#2236](http://gitlabghc.nibbler/ghc/ghc/issues/2236)</th>
<td>Deep stacks make execution time go through the roof</td></tr>
<tr><th>[\#2185](http://gitlabghc.nibbler/ghc/ghc/issues/2185)</th>
<td>Memory leak with parMap</td></tr>
<tr><th>[\#2163](http://gitlabghc.nibbler/ghc/ghc/issues/2163)</th>
<td>GHC makes thunks for Integers we are strict in</td></tr>
<tr><th>[\#2105](http://gitlabghc.nibbler/ghc/ghc/issues/2105)</th>
<td>garbage collection confusing in ghci for foreign objects</td></tr>
<tr><th>[\#2092](http://gitlabghc.nibbler/ghc/ghc/issues/2092)</th>
<td>Quadratic amount of code generated</td></tr>
<tr><th>[\#2078](http://gitlabghc.nibbler/ghc/ghc/issues/2078)</th>
<td>INLINE and strictness</td></tr>
<tr><th>[\#1890](http://gitlabghc.nibbler/ghc/ghc/issues/1890)</th>
<td>Regression in mandelbrot benchmark due to inlining</td></tr>
<tr><th>[\#1889](http://gitlabghc.nibbler/ghc/ghc/issues/1889)</th>
<td>Regression in concurrency performance from ghc 6.6 to 6.8</td></tr>
<tr><th>[\#1818](http://gitlabghc.nibbler/ghc/ghc/issues/1818)</th>
<td>Code size increase vs. 6.6.1</td></tr>
<tr><th>[\#1752](http://gitlabghc.nibbler/ghc/ghc/issues/1752)</th>
<td>CSE can create space leaks by increasing sharing</td></tr>
<tr><th>[\#1607](http://gitlabghc.nibbler/ghc/ghc/issues/1607)</th>
<td>seq can make code slower</td></tr>
<tr><th>[\#1434](http://gitlabghc.nibbler/ghc/ghc/issues/1434)</th>
<td>Missing RULEs for truncate</td></tr>
<tr><th>[\#1117](http://gitlabghc.nibbler/ghc/ghc/issues/1117)</th>
<td>\[2,4..10\] is not a good list producer</td></tr>
<tr><th>[\#955](http://gitlabghc.nibbler/ghc/ghc/issues/955)</th>
<td>more object-code blow-up in ghc-6.8.3 vs. ghc-6.4.2 (both with optimization)</td></tr>
<tr><th>[\#876](http://gitlabghc.nibbler/ghc/ghc/issues/876)</th>
<td>Length is not a good consumer</td></tr>
<tr><th>[\#783](http://gitlabghc.nibbler/ghc/ghc/issues/783)</th>
<td>SRTs bigger than they should be?</td></tr>
<tr><th>[\#650](http://gitlabghc.nibbler/ghc/ghc/issues/650)</th>
<td>Improve interaction between mutable arrays and GC</td></tr>
<tr><th>[\#635](http://gitlabghc.nibbler/ghc/ghc/issues/635)</th>
<td>Replace use of select() in the I/O manager with epoll/kqueue/etc.</td></tr>
<tr><th>[\#594](http://gitlabghc.nibbler/ghc/ghc/issues/594)</th>
<td>Support use of SSE2 in the x86 native code genreator</td></tr>
<tr><th>[\#427](http://gitlabghc.nibbler/ghc/ghc/issues/427)</th>
<td>Random.StdGen slowness</td></tr></table>






## Nofib results


### Austin, 5 May 2015



Full results [
are here](https://gist.githubusercontent.com/thoughtpolice/498d51153240cc4d899c/raw/9a43f6bbfd642cf4e7b15188f9c0b053d311f7b9/gistfile1.txt) (updated **May 5th, 2015**)



**NB**: The baseline here is 7.6.3


### Ben, 31 July 2015



[
http://home.smart-cactus.org/\~ben/nofib.html](http://home.smart-cactus.org/~ben/nofib.html)



Baseline is 7.4.2.


### Nofib outliers


#### Binary sizes


##### 7.6 to 7.8


- Solid average binary size increase of **5.3%**.

#### Allocations


##### 7.4 to 7.6


- **fannkuch-redux**: increased by factor of 10,000?!?!

  - 7.6.3: `<<ghc: 870987952 bytes, 1668 GCs (1666 + 2), 0/0 avg/max bytes residency (0 samples), 84640 bytes GC work, 1M in use, 0.00 INIT (0.00 elapsed), 2.43 MUT (2.43 elapsed), 0.00 GC (0.00 elapsed), 0.00 GC(0) (0.00 elapsed), 0.00 GC(1) (0.00 elapsed), 1 balance :ghc>>`
  - 7.4.2: `<<ghc: 74944 bytes, 1 GCs (0 + 1), 0/0 avg/max bytes residency (0 samples), 3512 bytes GC work, 1M in use, 0.00 INIT (0.00 elapsed), 2.25 MUT (2.25 elapsed), 0.00 GC (0.00 elapsed), 0.00 GC(0) (0.00 elapsed), 0.00 GC(1) (0.00 elapsed), 1 balance :ghc>>`
  - According to \[[FoldrBuildNotes](foldr-build-notes)\] this test is very sensitive to fusion
  - Filed [\#10717](http://gitlabghc.nibbler/ghc/ghc/issues/10717) to track this.

##### 7.6 to 7.8


- **spectral-norm**: increases by **17.0%**.

  - A **lot** more calls to `map`, over 100 more! Maybe inliner failure?
  - Over **twice** as many calls to `ghc-prim:GHC.Classes.$fEqChar_$c=={v r90O}` (& similar functions). Also over twice as many calls to `elem`, 
  - Similarly, many more calls to other specializations, like `base:Text.ParserCombinators.ReadP.$fMonadPlusP_$cmplus{v r1sr}`, which adds even more allocations (from 301 to 3928 for this one entry!)
  - Basically the same story up to `HEAD`!

##### 7.8 to 7.10


- **gcd**: increases by **20.7%**.

  - Ticky tells us that this seems to be a combination of a few things; most everything seems fairly similar, but we see a large amount of allocations attributable to 7.10 that I can't figure out where they came from, aside from the new `integer-gmp`: `integer-gmp-1.0.0.0:GHC.Integer.Type.$WS#{v rwl}` accounts for 106696208 extra bytes of allocation! It also seems like there are actual extant calls to `GHC.Base.map` in 7.10, and none in 7.8. These are the main differences.
- **pidigits**: increases by **7.4%**.

  - Ticky tells us that this seems to be, in large part, due to `integer-gmp` (which is mostly what it benchmarks anyway). I think part of this is actually an error, because before integer-gmp, a lot of things were done in C-- code or whatnot, while the new `integer-gmp` does everything in Haskell, so a lot more Haskell code shows up in the profile. So the results aren't 1-to-1. One thing that seems to be happening is that there are a lot more specializations going on that are called repeatedly, it seems; many occurrences of things like `sat_sad2{v} (integer-gmp-1.0.0.0:GHC.Integer.Type) in rfK` which don't exist in the 7.8 profiles, each with a lot of entries and allocations.
- **primetest**: went down **27.5%** in 7.6-to-7.8, but **8.8%** slower than 7.6 now - in total it got something like **36.6%** worse.

  - Much like **pidigits**, a lot more `integer-gmp` stuff shows up in these profiles. While it's still just like the last one, there are some other regressions; for example, `GHC.Integer.Type.remInteger` seems to have 245901/260800 calls/bytes allocated, vs 121001/200000 for 7.8


TODO Lots of fusion changes have happened in the last few months too - but these should all be pretty diagnosable with some reverts, since they're usually very localized. Maybe worth looking through `base` changes.


#### Runtime


##### 7.6 to 7.8


- `lcss`: increases by **12.6%**.

  - Ticky says it seems to be `map` calls yet again! These jump hugely here from 21014 to 81002.
  - Also, another inner loop with `algb` it looks like gets called a huge number of times too - `algb2` is called **2001056 times vs 7984760 times**!

    - Same with `algb` and `algb1`, which seem to be called more often too.
  - Some other similar things; a few regressions in the \# of calls to things like `Text.ParserCombinator.ReadP` specializations, I think.
  - Same story with HEAD!

##### 7.8 to 7.10


- `lcss`: decreased by \~5% in 7.10, but still **7%** slower than 7.6.

  - See above for real regressions.
- `multiplier`: increases by **7.6%**.

  - `map` strikes again? 2601324 vs 3597333 calls, with an accompanying allocation delta.
  - But some other inner loops here work and go away correctly (mainly `go`), unlike e.g. `lcss`.


 


#### Comparing integer-gmp 0.5 and 1.0



One of the major factors that has changed recently is `integer-gmp`. Namely, GHC 7.10 includes `integer-gmp-1.0`, a major rework of `integer-gmp-0.5`. I've compiled GHC 7.10.1 with `integer-gmp` 0.5 and 1.0. [
Here](http://home.smart-cactus.org/~ben/nofib.html) is a nofib comparison. There are a few interesting points here,


- Binary sizes dropped dramatically and consistently (typically around 60 to 70%) from 0.5 to 1.0.
- Runtime is almost always within error. A few exceptions,

  - `binary-trees`: 6% slower with 1.0
  - `pidigits`: 5% slower
  - `integer`: 4% slower
  - `cryptarithm1`: 2.5% slower
  - `circsim`: 3% faster
  - `lcss`: 5% faster
  - `power`: 17% faster
- Allocations are typically similar. The only test that improves significantly
  is `prime` whose allocations decreased by 24% Many more tests regress
  considerably,

  - `bernoulli`: +15%
  - `gcd`: +21%
  - `kahan`: +40%
  - `mandel` +34%
  - `primetest`: +50%
  - `rsa`: +53%


The allocation issue is actually discussed in the commit message ([c774b28f76ee4c220f7c1c9fd81585e0e3af0e8a](/trac/ghc/changeset/c774b28f76ee4c220f7c1c9fd81585e0e3af0e8a/ghc)),


>
>
> Due to the different (over)allocation scheme and potentially different
> accounting (via the new `{shrink,resize}MutableByteArray#` primitives),
> some of the nofib benchmarks actually results in increased allocation
> numbers (but not necessarily an increase in runtime!).  I believe the
> allocation numbers could improve if `{resize,shrink}MutableByteArray#`
> could be optimised to reallocate in-place more efficiently.
>
>


The message then goes on to list exactly the nofib tests mentioned above. Given that there isn't a strong negative trend in runtime corresponding with these increased allocations, I'm leaning towards ignoring these for now.


