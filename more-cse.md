# Notes on common sub-expression elimination (CSE)


## Tickets



The CSE pass is pretty simple at the moment.  Here are tickets that identify currently-missed opportunities.



Use Keyword = `CSE` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#149](http://gitlabghc.nibbler/ghc/ghc/issues/149)</th>
<td>missed CSE opportunity</td></tr>
<tr><th>[\#701](http://gitlabghc.nibbler/ghc/ghc/issues/701)</th>
<td>Better CSE optimisation</td></tr>
<tr><th>[\#947](http://gitlabghc.nibbler/ghc/ghc/issues/947)</th>
<td>ghc -O space leak: CSE between different CAFs</td></tr>
<tr><th>[\#5344](http://gitlabghc.nibbler/ghc/ghc/issues/5344)</th>
<td>CSE should look through coercions</td></tr>
<tr><th>[\#7596](http://gitlabghc.nibbler/ghc/ghc/issues/7596)</th>
<td>Opportunity to improve CSE</td></tr>
<tr><th>[\#9688](http://gitlabghc.nibbler/ghc/ghc/issues/9688)</th>
<td>Improve the interaction between CSE and the join point transformation</td></tr>
<tr><th>[\#12620](http://gitlabghc.nibbler/ghc/ghc/issues/12620)</th>
<td>Allow the user to prevent floating and CSE</td></tr>
<tr><th>[\#13219](http://gitlabghc.nibbler/ghc/ghc/issues/13219)</th>
<td>CSE for join points</td></tr>
<tr><th>[\#13589](http://gitlabghc.nibbler/ghc/ghc/issues/13589)</th>
<td>Possible inconsistency in CSE's treatment of NOINLINE</td></tr>
<tr><th>[\#13694](http://gitlabghc.nibbler/ghc/ghc/issues/13694)</th>
<td>CSE runs before SpecConstr</td></tr>
<tr><th>[\#14186](http://gitlabghc.nibbler/ghc/ghc/issues/14186)</th>
<td>CSE fails to CSE two identical large top-level functions</td></tr>
<tr><th>[\#14222](http://gitlabghc.nibbler/ghc/ghc/issues/14222)</th>
<td>Simple text fusion example results in rather duplicative code</td></tr>
<tr><th>[\#14684](http://gitlabghc.nibbler/ghc/ghc/issues/14684)</th>
<td>combineIdenticalAlts is only partially implemented</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#2940](http://gitlabghc.nibbler/ghc/ghc/issues/2940)</th>
<td>Do CSE after CorePrep</td></tr>
<tr><th>[\#9441](http://gitlabghc.nibbler/ghc/ghc/issues/9441)</th>
<td>CSE should deal with letrec</td></tr></table>



## More aggressive CSE



Joachim did some experiments trying to achieve more CSE, but could not achieve a uniform win in the benchmarks. This page holds some of the notes of what has been tried, and what happened. Some of this has also been noted at [\#7596](http://gitlabghc.nibbler/ghc/ghc/issues/7596). This is more a list of anecdotal insights; full insights would have led to a patch to master... :-)



The main idea was to make the float out phase flout out more stuff, so that later on the CSE pass sees more possibilities to CSE expressions up. In itself, this works as expected, and solves the motivating example from [\#7596](http://gitlabghc.nibbler/ghc/ghc/issues/7596), but for various reasons the results were not as good as hoped-for.



Some reasons why more CSE could hurt:


- When one very aggressively, this will float things like `GHC.Classes.<= @ a sc`, which is of no use.
- Sharing across case branches. ([ticket:7596\#comment:3](http://gitlabghc.nibbler/ghc/ghc/issues/7596)) The code in question has multiple case branches, all of which called `reverse rev`. Floating this expression out and sharing it does not gain anything in terms of saved work or allocations, but can increase allocation, i.e. if the value is not used in all branches.
- Different arguments to a function  behave similar as case branches, if the function may only evaluate one of them. Floating out subexpression of the arguments can then increase allocation.
- An additional CSE pass as the end, even without further floating, made `puzzle` worse: The thunks were `OnceL` before, so after CSE the whole thunk-updating-machinery was added. Even worse: The second occurrence of the expression was dead code (which the compiler cannot know).


There were also effects that I did not fully understand:


- In `kahan`, the two calls to `newArray` were shared (as functions, of course, as they are `ST` actions). This (or something else) caused the `inner` loop to be moved inside the `outer` loop, causing many allocations of that loop’s function.
- Even only floating saturated constructor applications (which had only a little effect for most benchmarks) made things works; the cause is hidden somewhere in `GHC.Show` and I did not find out why.


Summary:



More floating and/or CSE is tricky, as there are a few way this can make things worse. Also, it is difficult to find out what goes wrong, as the Core tend to change a lot even with small adjustments of the code.



It might be worth trying to implement a very careful floating/CSE pass that will make sure that only expressions that are going to be allocated in every case (i.e. because they are in a strict context, or arguments to functions in strict context) can be CSEd. There might be some small gains to achieve without too many regressions.


