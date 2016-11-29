# Levity polymorphism



This is a status page for levity polymorphism, as described in the [
Levity Polymorphism paper](https://www.microsoft.com/en-us/research/publication/levity-polymorphism/).



See also [UnliftedDataTypes](unlifted-data-types)


## Tickets



Use Keyword = `LevityPolymorphism` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#11715](http://gitlabghc.nibbler/ghc/ghc/issues/11715)</th>
<td>Constraint vs \*</td></tr>
<tr><th>[\#12708](http://gitlabghc.nibbler/ghc/ghc/issues/12708)</th>
<td>RFC: Representation polymorphic Num</td></tr>
<tr><th>[\#12980](http://gitlabghc.nibbler/ghc/ghc/issues/12980)</th>
<td>Unlifted class method rejected</td></tr>
<tr><th>[\#12989](http://gitlabghc.nibbler/ghc/ghc/issues/12989)</th>
<td>($) can have a more general type</td></tr>
<tr><th>[\#13105](http://gitlabghc.nibbler/ghc/ghc/issues/13105)</th>
<td>Allow type families in RuntimeReps</td></tr>
<tr><th>[\#13233](http://gitlabghc.nibbler/ghc/ghc/issues/13233)</th>
<td>typePrimRep panic while compiling GHC with profiling</td></tr>
<tr><th>[\#13592](http://gitlabghc.nibbler/ghc/ghc/issues/13592)</th>
<td>Newtype type class with compiler generated instances</td></tr>
<tr><th>[\#13595](http://gitlabghc.nibbler/ghc/ghc/issues/13595)</th>
<td>Should ‘coerce’ be levity polymorphic?</td></tr>
<tr><th>[\#14185](http://gitlabghc.nibbler/ghc/ghc/issues/14185)</th>
<td>Non-local bug reporting around levity polymorphism</td></tr>
<tr><th>[\#14196](http://gitlabghc.nibbler/ghc/ghc/issues/14196)</th>
<td>Replace ArrayArray\# with either UnliftedArray\# or Array\#</td></tr>
<tr><th>[\#14765](http://gitlabghc.nibbler/ghc/ghc/issues/14765)</th>
<td>Levity polymorphism panic</td></tr>
<tr><th>[\#14917](http://gitlabghc.nibbler/ghc/ghc/issues/14917)</th>
<td>Allow levity polymorphism in binding position</td></tr>
<tr><th>[\#15532](http://gitlabghc.nibbler/ghc/ghc/issues/15532)</th>
<td>Relaxing Levity-Polymorphic Binder Check for Lifted vs Unlifted pointers</td></tr>
<tr><th>[\#15883](http://gitlabghc.nibbler/ghc/ghc/issues/15883)</th>
<td>GHC panic: newtype F rep = F (forall (a :: TYPE rep). a)</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#11431](http://gitlabghc.nibbler/ghc/ghc/issues/11431)</th>
<td>GHC instantiates levity-polymorphic type variables with foralls</td></tr>
<tr><th>[\#11465](http://gitlabghc.nibbler/ghc/ghc/issues/11465)</th>
<td>Eliminate check\_lifted check in TcValidity</td></tr>
<tr><th>[\#11471](http://gitlabghc.nibbler/ghc/ghc/issues/11471)</th>
<td>Kind polymorphism and unboxed types: bad things are happening</td></tr>
<tr><th>[\#11473](http://gitlabghc.nibbler/ghc/ghc/issues/11473)</th>
<td>Levity polymorphism checks are inadequate</td></tr>
<tr><th>[\#11714](http://gitlabghc.nibbler/ghc/ghc/issues/11714)</th>
<td>Kind of (-\>) type constructor is overly constrained</td></tr>
<tr><th>[\#11724](http://gitlabghc.nibbler/ghc/ghc/issues/11724)</th>
<td>Must check for representation polymorphism in datatype declarations</td></tr>
<tr><th>[\#11736](http://gitlabghc.nibbler/ghc/ghc/issues/11736)</th>
<td>Allow unsaturated uses of unlifted types in Core</td></tr>
<tr><th>[\#11786](http://gitlabghc.nibbler/ghc/ghc/issues/11786)</th>
<td>Need -fno-print-explicit-runtime-reps to work on IfaceType, else RuntimeRep leaks</td></tr>
<tr><th>[\#12668](http://gitlabghc.nibbler/ghc/ghc/issues/12668)</th>
<td>Program that fails Core Lint terribly</td></tr>
<tr><th>[\#12670](http://gitlabghc.nibbler/ghc/ghc/issues/12670)</th>
<td>Representation polymorphism validity check is too strict</td></tr>
<tr><th>[\#12709](http://gitlabghc.nibbler/ghc/ghc/issues/12709)</th>
<td>GHC panic</td></tr>
<tr><th>[\#12718](http://gitlabghc.nibbler/ghc/ghc/issues/12718)</th>
<td>Segmentation fault, runtime representation polymorphism</td></tr>
<tr><th>[\#12809](http://gitlabghc.nibbler/ghc/ghc/issues/12809)</th>
<td>TYPE 'UnboxedTupleRep is still a lie</td></tr>
<tr><th>[\#12901](http://gitlabghc.nibbler/ghc/ghc/issues/12901)</th>
<td>Levity polymorphic expressions mustn't be floated out</td></tr>
<tr><th>[\#12905](http://gitlabghc.nibbler/ghc/ghc/issues/12905)</th>
<td>Core lint failure with pattern synonym and levity polymorphism</td></tr>
<tr><th>[\#12911](http://gitlabghc.nibbler/ghc/ghc/issues/12911)</th>
<td>Levity polymorphism check eliminates non-levity-polymorphic data constructor</td></tr>
<tr><th>[\#12973](http://gitlabghc.nibbler/ghc/ghc/issues/12973)</th>
<td>No levity-polymorphic arguments</td></tr>
<tr><th>[\#12987](http://gitlabghc.nibbler/ghc/ghc/issues/12987)</th>
<td>Core lint error with levity polymorphism</td></tr>
<tr><th>[\#13202](http://gitlabghc.nibbler/ghc/ghc/issues/13202)</th>
<td>Levity polymorphism panic in GHCi</td></tr>
<tr><th>[\#13244](http://gitlabghc.nibbler/ghc/ghc/issues/13244)</th>
<td>Error Dealing with Unboxed Types and Type Families</td></tr>
<tr><th>[\#13275](http://gitlabghc.nibbler/ghc/ghc/issues/13275)</th>
<td>ghci ignores -fprint-explicit-runtime-reps</td></tr>
<tr><th>[\#13343](http://gitlabghc.nibbler/ghc/ghc/issues/13343)</th>
<td>Levity polymorphism-related GHC panic: expectJust zonkTcTyVarToVar</td></tr>
<tr><th>[\#13435](http://gitlabghc.nibbler/ghc/ghc/issues/13435)</th>
<td>Segfaults on levity-polymorphic type class</td></tr>
<tr><th>[\#13458](http://gitlabghc.nibbler/ghc/ghc/issues/13458)</th>
<td>Panic with unsafeCoerce and -dcore-lint</td></tr>
<tr><th>[\#13509](http://gitlabghc.nibbler/ghc/ghc/issues/13509)</th>
<td>Perplexing type error with unboxed tuples</td></tr>
<tr><th>[\#13601](http://gitlabghc.nibbler/ghc/ghc/issues/13601)</th>
<td>GHC errors but hangs</td></tr>
<tr><th>[\#13603](http://gitlabghc.nibbler/ghc/ghc/issues/13603)</th>
<td>Can't resolve levity polymorphic superclass</td></tr>
<tr><th>[\#13929](http://gitlabghc.nibbler/ghc/ghc/issues/13929)</th>
<td>GHC panic with levity polymorphism</td></tr>
<tr><th>[\#13955](http://gitlabghc.nibbler/ghc/ghc/issues/13955)</th>
<td>Backpack does not handle unlifted types</td></tr>
<tr><th>[\#13963](http://gitlabghc.nibbler/ghc/ghc/issues/13963)</th>
<td>Runtime representation confusingly displayed</td></tr>
<tr><th>[\#14555](http://gitlabghc.nibbler/ghc/ghc/issues/14555)</th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th>[\#14561](http://gitlabghc.nibbler/ghc/ghc/issues/14561)</th>
<td>Panic on levity polymorphic very unsafe coerce</td></tr>
<tr><th>[\#14563](http://gitlabghc.nibbler/ghc/ghc/issues/14563)</th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th>[\#15180](http://gitlabghc.nibbler/ghc/ghc/issues/15180)</th>
<td>Make Control.Exception.throw levity polymorphic</td></tr>
<tr><th>[\#15181](http://gitlabghc.nibbler/ghc/ghc/issues/15181)</th>
<td>Levity Polymorphic type signatures in GHC.Prim</td></tr></table>



