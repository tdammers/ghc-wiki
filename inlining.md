# Inlining



Inlining is the most important compiler optimisation pass as it enables most other optimisation opportunities. The pass is simple, saturated names are replaced with their definitions, the details are complicated. The compiler must make judgements as to whether inlining a function will lead to further optimisations, if not then it is easy to increase the code size needlessly.


## Getting Started


- [
  Secrets of the GHC inliner](http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/) -- quite an old paper but a great description of the main ideas
- [
  GHC User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=inline#inline-and-noinline-pragmas) -- Provides a description of `INLINE`, `INLINABLE` and `NOINLINE` pragmas. 
- [
  Inlining and Specialisation](http://mpickering.github.io/posts/2017-03-20-inlining-and-specialisation.html) -- A blog post explaining the basic operation of the inliner and specialiser and the interaction of different pragmas and options.

## Generics and Inlining



Inlining is essential to remove intermediate representations from generic programs. There are a number of papers about the topic.


- [
  Optimizing Generics Is Easy! (2010)](http://dreixel.net/research/pdf/ogie.pdf)
- [
  Optimizing SYB Is Easy! (2014)](http://michaeldadams.org/papers/syb-opt/syb-opt-2014-pepm-authors-copy.pdf)

## Debugging the inliner



Firstly, remember that the inliner only fires with optimisations turns on (at least `-O1`). This will save you a lot of time wondering why nothing is happening!



There are several flags which are useful when working with the inliner. 


<table><tr><th> Flag </th>
<th> Usage 
</th></tr>
<tr><th> [
\`--show-iface\`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html?highlight=show-iface#ghc-flag---show-iface) </th>
<th> Shows the contents of an interface file. Can be useful to check which unfoldings are being included. 
</th></tr>
<tr><th> [
\`-dshow-passes\`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--dshow-passes) </th>
<th> Shows the size of the program after each optimisation pass. 
</th></tr>
<tr><th> [
\`-ddump-inlinings\`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--ddump-inlinings) </th>
<th> Shows inlinings which take place 
</th></tr>
<tr><th> [
\`-ddump-simpl\`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--ddump-simpl) </th>
<th> Dump the (core) output of the simplifer 
</th></tr></table>


## Newcomer Tickets



<table><tr><th>[\#4960](http://gitlabghc.nibbler/ghc/ghc/issues/4960)</th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th>[\#9370](http://gitlabghc.nibbler/ghc/ghc/issues/9370)</th>
<td>unfolding info as seen when building a module depends on flags in a previously-compiled module</td></tr>
<tr><th>[\#11068](http://gitlabghc.nibbler/ghc/ghc/issues/11068)</th>
<td>Make Generic/Generic1 methods inlinable</td></tr></table>



## Relevant Tickets



There are lots of old relevant tickets related to inlining. Perfect for a keen newcomer!



Use `Keyword` = `Inlining` to ensure that a ticket ends up on these lists.



<table><tr><th>[\#3073](http://gitlabghc.nibbler/ghc/ghc/issues/3073)</th>
<td>Avoid reconstructing dictionaries in recursive instance methods</td></tr>
<tr><th>[\#3755](http://gitlabghc.nibbler/ghc/ghc/issues/3755)</th>
<td>Improve join point inlining</td></tr>
<tr><th>[\#3781](http://gitlabghc.nibbler/ghc/ghc/issues/3781)</th>
<td>Improve inlining for local functions</td></tr>
<tr><th>[\#4960](http://gitlabghc.nibbler/ghc/ghc/issues/4960)</th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th>[\#5834](http://gitlabghc.nibbler/ghc/ghc/issues/5834)</th>
<td>Allow both INLINE and INLINABLE for the same function</td></tr>
<tr><th>[\#5928](http://gitlabghc.nibbler/ghc/ghc/issues/5928)</th>
<td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
<tr><th>[\#7109](http://gitlabghc.nibbler/ghc/ghc/issues/7109)</th>
<td>Inlining depends on datatype size, even with INLINE pragmas</td></tr>
<tr><th>[\#7114](http://gitlabghc.nibbler/ghc/ghc/issues/7114)</th>
<td>Cannot recover (good) inlining behaviour from 7.0.2 in 7.4.1</td></tr>
<tr><th>[\#7283](http://gitlabghc.nibbler/ghc/ghc/issues/7283)</th>
<td>Specialise INLINE functions</td></tr>
<tr><th>[\#7511](http://gitlabghc.nibbler/ghc/ghc/issues/7511)</th>
<td>Room for GHC runtime improvement \>\~5%, inlining related</td></tr>
<tr><th>[\#7803](http://gitlabghc.nibbler/ghc/ghc/issues/7803)</th>
<td>Superclass methods are left unspecialized</td></tr>
<tr><th>[\#7829](http://gitlabghc.nibbler/ghc/ghc/issues/7829)</th>
<td>make better/more robust loopbreaker choices</td></tr>
<tr><th>[\#8099](http://gitlabghc.nibbler/ghc/ghc/issues/8099)</th>
<td>Alternate syntax for indicating when a function is "fully applied" for purposes of inlining</td></tr>
<tr><th>[\#8589](http://gitlabghc.nibbler/ghc/ghc/issues/8589)</th>
<td>Bad choice of loop breaker with INLINABLE/INLINE</td></tr>
<tr><th>[\#8662](http://gitlabghc.nibbler/ghc/ghc/issues/8662)</th>
<td>GHC does not inline cheap inner loop when used in two places</td></tr>
<tr><th>[\#8668](http://gitlabghc.nibbler/ghc/ghc/issues/8668)</th>
<td>SPECIALIZE silently fails to apply</td></tr>
<tr><th>[\#8774](http://gitlabghc.nibbler/ghc/ghc/issues/8774)</th>
<td>Transitivity of Auto-Specialization</td></tr>
<tr><th>[\#8814](http://gitlabghc.nibbler/ghc/ghc/issues/8814)</th>
<td>7.8 optimizes attoparsec improperly</td></tr>
<tr><th>[\#9020](http://gitlabghc.nibbler/ghc/ghc/issues/9020)</th>
<td>Massive blowup of code size on trivial program</td></tr>
<tr><th>[\#9320](http://gitlabghc.nibbler/ghc/ghc/issues/9320)</th>
<td>Inlining regression/strangeness in 7.8</td></tr>
<tr><th>[\#9370](http://gitlabghc.nibbler/ghc/ghc/issues/9370)</th>
<td>unfolding info as seen when building a module depends on flags in a previously-compiled module</td></tr>
<tr><th>[\#9418](http://gitlabghc.nibbler/ghc/ghc/issues/9418)</th>
<td>Warnings about "INLINE binder is (non-rule) loop breaker"</td></tr>
<tr><th>[\#9701](http://gitlabghc.nibbler/ghc/ghc/issues/9701)</th>
<td>GADTs not specialized properly</td></tr>
<tr><th>[\#9798](http://gitlabghc.nibbler/ghc/ghc/issues/9798)</th>
<td>Frustrating behaviour of the INLINE pragma</td></tr>
<tr><th>[\#10371](http://gitlabghc.nibbler/ghc/ghc/issues/10371)</th>
<td>GHC fails to inline and specialize a function</td></tr>
<tr><th>[\#10421](http://gitlabghc.nibbler/ghc/ghc/issues/10421)</th>
<td>exponential blowup in inlining (without INLINE pragmas)</td></tr>
<tr><th>[\#10710](http://gitlabghc.nibbler/ghc/ghc/issues/10710)</th>
<td>More self-explanatory pragmas for inlining phase control</td></tr>
<tr><th>[\#10766](http://gitlabghc.nibbler/ghc/ghc/issues/10766)</th>
<td>user manual: INLINE's interaction with optimization levels is not clear</td></tr>
<tr><th>[\#11068](http://gitlabghc.nibbler/ghc/ghc/issues/11068)</th>
<td>Make Generic/Generic1 methods inlinable</td></tr>
<tr><th>[\#11263](http://gitlabghc.nibbler/ghc/ghc/issues/11263)</th>
<td>"Simplifier ticks exhausted" that resolves with fsimpl-tick-factor=200</td></tr>
<tr><th>[\#12274](http://gitlabghc.nibbler/ghc/ghc/issues/12274)</th>
<td>GHC panic: simplifier ticks exhausted</td></tr>
<tr><th>[\#12454](http://gitlabghc.nibbler/ghc/ghc/issues/12454)</th>
<td>Cross-module specialisation of recursive functions</td></tr>
<tr><th>[\#12463](http://gitlabghc.nibbler/ghc/ghc/issues/12463)</th>
<td>SPECIALIZABLE pragma?</td></tr>
<tr><th>[\#12747](http://gitlabghc.nibbler/ghc/ghc/issues/12747)</th>
<td>INLINE vs NOINLINE vs \<nothing\> give three different results; two would be better</td></tr>
<tr><th>[\#13016](http://gitlabghc.nibbler/ghc/ghc/issues/13016)</th>
<td>SPECIALIZE INLINE doesn't necessarily inline specializations of a recursive function</td></tr>
<tr><th>[\#13851](http://gitlabghc.nibbler/ghc/ghc/issues/13851)</th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th>[\#14211](http://gitlabghc.nibbler/ghc/ghc/issues/14211)</th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th>[\#14275](http://gitlabghc.nibbler/ghc/ghc/issues/14275)</th>
<td>Large Haskell value unexpectedly gets an unfolding</td></tr></table>



## Relevant Wiki Pages


- [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances) -- About how default methods can lead to poor inliner performance due to recursion
- [Proposal/SelfExplinatoryInlinePragmas](proposal/self-explinatory-inline-pragmas) 
