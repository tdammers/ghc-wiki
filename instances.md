# Type-class and type-function instances



This page collects pointers to stuff about type-class, and type function, instances.  Especially things to do with overlap.



But not 'deriving'; there's a separate [page for that](deriving-instances)


## Tickets



Use Keyword = `Instances` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#2986](http://gitlabghc.nibbler/ghc/ghc/issues/2986)</th>
<td>:info printing instances often isn't wanted</td></tr>
<tr><th>[\#7102](http://gitlabghc.nibbler/ghc/ghc/issues/7102)</th>
<td>Type family instance overlap accepted in ghci</td></tr>
<tr><th>[\#9918](http://gitlabghc.nibbler/ghc/ghc/issues/9918)</th>
<td>GHC chooses an instance between two overlapping, but cannot resolve a clause within the similar closed type family</td></tr>
<tr><th>[\#10089](http://gitlabghc.nibbler/ghc/ghc/issues/10089)</th>
<td>feature: warn about unused data definitions (with typeclass instances)</td></tr>
<tr><th>[\#13061](http://gitlabghc.nibbler/ghc/ghc/issues/13061)</th>
<td>Incorrect constraints given single flexible undecidable instance.</td></tr>
<tr><th>[\#15177](http://gitlabghc.nibbler/ghc/ghc/issues/15177)</th>
<td>Faulty instance termination check, with PolyKinds and/or TypeInType</td></tr>
<tr><th>[\#15191](http://gitlabghc.nibbler/ghc/ghc/issues/15191)</th>
<td>Deriving via DeriveAnyClass not behaving the same as an emply instance declaration</td></tr>
<tr><th>[\#15632](http://gitlabghc.nibbler/ghc/ghc/issues/15632)</th>
<td>Undependable Dependencies</td></tr>
<tr><th>[\#15895](http://gitlabghc.nibbler/ghc/ghc/issues/15895)</th>
<td>Unable to match instance signatures</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#2118](http://gitlabghc.nibbler/ghc/ghc/issues/2118)</th>
<td>deriving for GRose</td></tr>
<tr><th>[\#3383](http://gitlabghc.nibbler/ghc/ghc/issues/3383)</th>
<td>Confluent type family instances confuse the solver</td></tr>
<tr><th>[\#3877](http://gitlabghc.nibbler/ghc/ghc/issues/3877)</th>
<td>Require XOverlappingInstances for the most specific instance only</td></tr>
<tr><th>[\#7777](http://gitlabghc.nibbler/ghc/ghc/issues/7777)</th>
<td>ghc panic: varargs + sets</td></tr>
<tr><th>[\#9582](http://gitlabghc.nibbler/ghc/ghc/issues/9582)</th>
<td>Associated Type Synonyms do not unfold in InstanceSigs</td></tr>
<tr><th>[\#11674](http://gitlabghc.nibbler/ghc/ghc/issues/11674)</th>
<td>GHC accepts overly general instance sigs</td></tr>
<tr><th>[\#12201](http://gitlabghc.nibbler/ghc/ghc/issues/12201)</th>
<td>Wrong instance selection with overlapping instance in a superclass</td></tr>
<tr><th>[\#12787](http://gitlabghc.nibbler/ghc/ghc/issues/12787)</th>
<td>Weird type constraint with undecidable instances</td></tr>
<tr><th>[\#13320](http://gitlabghc.nibbler/ghc/ghc/issues/13320)</th>
<td>Unfortunate compiler loop when creating type loop (with UndecidableInstances)</td></tr>
<tr><th>[\#13950](http://gitlabghc.nibbler/ghc/ghc/issues/13950)</th>
<td>IncoherentInstances</td></tr>
<tr><th>[\#14442](http://gitlabghc.nibbler/ghc/ghc/issues/14442)</th>
<td>InstanceSigs fails</td></tr>
<tr><th>[\#14846](http://gitlabghc.nibbler/ghc/ghc/issues/14846)</th>
<td>Renamer hangs (because of -XInstanceSigs?)</td></tr></table>



