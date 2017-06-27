# [SpecConstr](spec-constr)



`SpecConstr` is a GHC optimisation that specialises functions for particular values of their arguments.


- [
  Here is the paper about SpecConstr](https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions-2/)

## Tickets



Use Keyword = `SpecConstr` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#855](http://gitlabghc.nibbler/ghc/ghc/issues/855)</th>
<td>Improvements to SpecConstr</td></tr>
<tr><th>[\#2255](http://gitlabghc.nibbler/ghc/ghc/issues/2255)</th>
<td>Improve SpecConstr for free variables</td></tr>
<tr><th>[\#2598](http://gitlabghc.nibbler/ghc/ghc/issues/2598)</th>
<td>Avoid excessive specialisation in SpecConstr</td></tr>
<tr><th>[\#2642](http://gitlabghc.nibbler/ghc/ghc/issues/2642)</th>
<td>Improve SpecConstr for join points</td></tr>
<tr><th>[\#3767](http://gitlabghc.nibbler/ghc/ghc/issues/3767)</th>
<td>SpecConstr for join points</td></tr>
<tr><th>[\#3831](http://gitlabghc.nibbler/ghc/ghc/issues/3831)</th>
<td>SpecConstr should exploit cases where there is exactly one call pattern</td></tr>
<tr><th>[\#4941](http://gitlabghc.nibbler/ghc/ghc/issues/4941)</th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th>[\#4945](http://gitlabghc.nibbler/ghc/ghc/issues/4945)</th>
<td>Another SpecConstr infelicity</td></tr>
<tr><th>[\#5059](http://gitlabghc.nibbler/ghc/ghc/issues/5059)</th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th>[\#5075](http://gitlabghc.nibbler/ghc/ghc/issues/5075)</th>
<td>CPR optimisation for sum types if only one constructor is used</td></tr>
<tr><th>[\#10346](http://gitlabghc.nibbler/ghc/ghc/issues/10346)</th>
<td>Cross-module SpecConstr</td></tr>
<tr><th>[\#10626](http://gitlabghc.nibbler/ghc/ghc/issues/10626)</th>
<td>Missed opportunity for SpecConstr</td></tr>
<tr><th>[\#11668](http://gitlabghc.nibbler/ghc/ghc/issues/11668)</th>
<td>SPEC has a runtime cost if constructor specialization isn't performed</td></tr>
<tr><th>[\#13014](http://gitlabghc.nibbler/ghc/ghc/issues/13014)</th>
<td>Seemingly unnecessary marking of a SpecConstr specialization as a loopbreaker</td></tr>
<tr><th>[\#13346](http://gitlabghc.nibbler/ghc/ghc/issues/13346)</th>
<td>Run nofib with -fspec-constr-keen</td></tr>
<tr><th>[\#13681](http://gitlabghc.nibbler/ghc/ghc/issues/13681)</th>
<td>Remove deprecated ForceSpecConstr</td></tr>
<tr><th>[\#13694](http://gitlabghc.nibbler/ghc/ghc/issues/13694)</th>
<td>CSE runs before SpecConstr</td></tr>
<tr><th>[\#13867](http://gitlabghc.nibbler/ghc/ghc/issues/13867)</th>
<td>Silly definitions remain after SpecConstr</td></tr>
<tr><th>[\#14565](http://gitlabghc.nibbler/ghc/ghc/issues/14565)</th>
<td>Performance degrades from -O1 to -O2</td></tr>
<tr><th>[\#14844](http://gitlabghc.nibbler/ghc/ghc/issues/14844)</th>
<td>SpecConstr also non-recursive function</td></tr>
<tr><th>[\#14951](http://gitlabghc.nibbler/ghc/ghc/issues/14951)</th>
<td>SpecConstr needs two runs when one should suffice</td></tr>
<tr><th>[\#15069](http://gitlabghc.nibbler/ghc/ghc/issues/15069)</th>
<td>Missed SpecConstr opportunity</td></tr>
<tr><th>[\#15519](http://gitlabghc.nibbler/ghc/ghc/issues/15519)</th>
<td>Minor code refactoring leads to drastic performance degradation</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#7865](http://gitlabghc.nibbler/ghc/ghc/issues/7865)</th>
<td>SpecConstr duplicating computations</td></tr>
<tr><th>[\#7944](http://gitlabghc.nibbler/ghc/ghc/issues/7944)</th>
<td>GHC goes into an apparently infinite loop at -O2</td></tr>
<tr><th>[\#13410](http://gitlabghc.nibbler/ghc/ghc/issues/13410)</th>
<td>GHC HEAD regression: Template variable unbound in rewrite rule</td></tr>
<tr><th>[\#14936](http://gitlabghc.nibbler/ghc/ghc/issues/14936)</th>
<td>GHC 8.4 performance regressions when using newtypes</td></tr>
<tr><th>[\#14955](http://gitlabghc.nibbler/ghc/ghc/issues/14955)</th>
<td>Musings on manual type class desugaring</td></tr></table>



