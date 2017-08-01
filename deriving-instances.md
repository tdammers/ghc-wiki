# Deriving instances



All about the `deriving` keyword.


# Status



Use **Keyword** = `deriving` (or `deriving-perf` for performance-related tickets) to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th>[\#1544](http://gitlabghc.nibbler/ghc/ghc/issues/1544)</th>
<td>Derived Read instances for recursive datatypes with infix constructors are too inefficient</td></tr>
<tr><th>[\#3205](http://gitlabghc.nibbler/ghc/ghc/issues/3205)</th>
<td>Generalized isomorphic deriving</td></tr>
<tr><th>[\#5041](http://gitlabghc.nibbler/ghc/ghc/issues/5041)</th>
<td>Incorrect Read deriving for MagicHash constructors</td></tr>
<tr><th>[\#5642](http://gitlabghc.nibbler/ghc/ghc/issues/5642)</th>
<td>Deriving Generic of a big type takes a long time and lots of space</td></tr>
<tr><th>[\#7258](http://gitlabghc.nibbler/ghc/ghc/issues/7258)</th>
<td>Compiling DynFlags is jolly slow</td></tr>
<tr><th>[\#8731](http://gitlabghc.nibbler/ghc/ghc/issues/8731)</th>
<td>long compilation time for module with large data type and partial record selectors</td></tr>
<tr><th>[\#8827](http://gitlabghc.nibbler/ghc/ghc/issues/8827)</th>
<td>Inferring Safe mode with GeneralizedNewtypeDeriving is wrong</td></tr>
<tr><th>[\#9112](http://gitlabghc.nibbler/ghc/ghc/issues/9112)</th>
<td>support for deriving Vector/MVector instances</td></tr>
<tr><th>[\#9450](http://gitlabghc.nibbler/ghc/ghc/issues/9450)</th>
<td>GHC instantiates Data instances before checking hs-boot files</td></tr>
<tr><th>[\#9522](http://gitlabghc.nibbler/ghc/ghc/issues/9522)</th>
<td>SPECIALISE pragmas for derived instances</td></tr>
<tr><th>[\#9557](http://gitlabghc.nibbler/ghc/ghc/issues/9557)</th>
<td>Deriving instances is slow</td></tr>
<tr><th>[\#9669](http://gitlabghc.nibbler/ghc/ghc/issues/9669)</th>
<td>Long compile time/high memory usage for modules with many deriving clauses</td></tr>
<tr><th>[\#9790](http://gitlabghc.nibbler/ghc/ghc/issues/9790)</th>
<td>Produce coercion rules for derived Functor instances</td></tr>
<tr><th>[\#10980](http://gitlabghc.nibbler/ghc/ghc/issues/10980)</th>
<td>Deriving Read instance from datatype with N fields leads to N\^2 code size growth</td></tr>
<tr><th>[\#11008](http://gitlabghc.nibbler/ghc/ghc/issues/11008)</th>
<td>Difficulties around inferring exotic contexts</td></tr>
<tr><th>[\#12457](http://gitlabghc.nibbler/ghc/ghc/issues/12457)</th>
<td>Deriving should be (more closely) integrated with other metaprogramming methods</td></tr>
<tr><th>[\#12639](http://gitlabghc.nibbler/ghc/ghc/issues/12639)</th>
<td>Inconsistent treatment of FlexibleInstances and MPTCs with standard vs. flexible deriving</td></tr>
<tr><th>[\#12860](http://gitlabghc.nibbler/ghc/ghc/issues/12860)</th>
<td>GeneralizedNewtypeDeriving + MultiParamTypeClasses sends typechecker into an infinite loop</td></tr>
<tr><th>[\#13154](http://gitlabghc.nibbler/ghc/ghc/issues/13154)</th>
<td>Standalone-derived anyclass instances aren't as permissive as empty instances</td></tr>
<tr><th>[\#13280](http://gitlabghc.nibbler/ghc/ghc/issues/13280)</th>
<td>Consider deriving more Foldable methods</td></tr>
<tr><th>[\#13327](http://gitlabghc.nibbler/ghc/ghc/issues/13327)</th>
<td>Figure out how to make sense of Data instances for poly-kinded datatypes</td></tr>
<tr><th>[\#13403](http://gitlabghc.nibbler/ghc/ghc/issues/13403)</th>
<td>Derive instances (Applicative, Monad, ...) for structures lifted over functors</td></tr>
<tr><th>[\#13465](http://gitlabghc.nibbler/ghc/ghc/issues/13465)</th>
<td>Foldable deriving treatment of tuples is too surprising</td></tr>
<tr><th>[\#13731](http://gitlabghc.nibbler/ghc/ghc/issues/13731)</th>
<td>DeriveFunctor and friends don't understand type families</td></tr>
<tr><th>[\#13748](http://gitlabghc.nibbler/ghc/ghc/issues/13748)</th>
<td>Variables pretty-printed from -ddump-deriv are not scoped properly</td></tr>
<tr><th>[\#13957](http://gitlabghc.nibbler/ghc/ghc/issues/13957)</th>
<td>Allow deriving multiparameter type classes with representationally equal arguments</td></tr>
<tr><th>[\#14030](http://gitlabghc.nibbler/ghc/ghc/issues/14030)</th>
<td>Implement the "Derive Lift instances for data types in template-haskell" proposal</td></tr>
<tr><th>[\#14331](http://gitlabghc.nibbler/ghc/ghc/issues/14331)</th>
<td>Overzealous free-floating kind check causes deriving clause to be rejected</td></tr>
<tr><th>[\#14332](http://gitlabghc.nibbler/ghc/ghc/issues/14332)</th>
<td>Deriving clauses can have forall types</td></tr>
<tr><th>[\#14579](http://gitlabghc.nibbler/ghc/ghc/issues/14579)</th>
<td>GeneralizedNewtypeDeriving produces ambiguously-kinded code</td></tr>
<tr><th>[\#15151](http://gitlabghc.nibbler/ghc/ghc/issues/15151)</th>
<td>Better Interaction Between Specialization and GND</td></tr>
<tr><th>[\#15376](http://gitlabghc.nibbler/ghc/ghc/issues/15376)</th>
<td>GHC determine illegal kind for standalone deriving with Deriving via</td></tr>
<tr><th>[\#15434](http://gitlabghc.nibbler/ghc/ghc/issues/15434)</th>
<td>DerivingVia (and perhaps even GND) works badly with DeriveGeneric</td></tr>
<tr><th>[\#15650](http://gitlabghc.nibbler/ghc/ghc/issues/15650)</th>
<td>Add (or document if already exist) ability to derive custom typeclasses via source plugins</td></tr>
<tr><th>[\#15712](http://gitlabghc.nibbler/ghc/ghc/issues/15712)</th>
<td>GHC panic with -XDerivingVia</td></tr>
<tr><th>[\#15831](http://gitlabghc.nibbler/ghc/ghc/issues/15831)</th>
<td>DerivingVia allows bogus implicit quantification in \`via\` type</td></tr>
<tr><th>[\#15839](http://gitlabghc.nibbler/ghc/ghc/issues/15839)</th>
<td>DerivingStrategies defaulting warning has no associated enable/suppress flag</td></tr>
<tr><th>[\#15868](http://gitlabghc.nibbler/ghc/ghc/issues/15868)</th>
<td>Standard deriving should be less conservative when \`UndecidableInstances\` is enabled</td></tr>
<tr><th>[\#15969](http://gitlabghc.nibbler/ghc/ghc/issues/15969)</th>
<td>Generic1 deriving should use more coercions</td></tr>
<tr><th>[\#16181](http://gitlabghc.nibbler/ghc/ghc/issues/16181)</th>
<td>ghc panic when using DerivingVia</td></tr></table>




Closed Tickets:

<table><tr><th>[\#1356](http://gitlabghc.nibbler/ghc/ghc/issues/1356)</th>
<td>"derive instance" panics ghc-6.7.20070404</td></tr>
<tr><th>[\#1830](http://gitlabghc.nibbler/ghc/ghc/issues/1830)</th>
<td>Automatic derivation of Lift</td></tr>
<tr><th>[\#2721](http://gitlabghc.nibbler/ghc/ghc/issues/2721)</th>
<td>Newtype deriving doesn't work with type families</td></tr>
<tr><th>[\#2733](http://gitlabghc.nibbler/ghc/ghc/issues/2733)</th>
<td>Newtype deriving over phantom types broke between GHC 6.6.1 and GHC 6.8.1</td></tr>
<tr><th>[\#2734](http://gitlabghc.nibbler/ghc/ghc/issues/2734)</th>
<td>Newtype deriving over phantom types broke between GHC 6.6.1 and GHC 6.8.1</td></tr>
<tr><th>[\#3368](http://gitlabghc.nibbler/ghc/ghc/issues/3368)</th>
<td>Deriving Foldable doesn't work</td></tr>
<tr><th>[\#3422](http://gitlabghc.nibbler/ghc/ghc/issues/3422)</th>
<td>No match in record selector Var.tcTyVarDetails</td></tr>
<tr><th>[\#4019](http://gitlabghc.nibbler/ghc/ghc/issues/4019)</th>
<td>deriving Ord can produce incorrect and inefficient instances</td></tr>
<tr><th>[\#4028](http://gitlabghc.nibbler/ghc/ghc/issues/4028)</th>
<td>Derived Data instance requires Data instances for unused type parameters</td></tr>
<tr><th>[\#4235](http://gitlabghc.nibbler/ghc/ghc/issues/4235)</th>
<td>deriving Enum fails for data instances</td></tr>
<tr><th>[\#4309](http://gitlabghc.nibbler/ghc/ghc/issues/4309)</th>
<td>Painfully large errors with silly GADT instances</td></tr>
<tr><th>[\#4528](http://gitlabghc.nibbler/ghc/ghc/issues/4528)</th>
<td>stand-alone deriving sometimes fails for GADTs</td></tr>
<tr><th>[\#4529](http://gitlabghc.nibbler/ghc/ghc/issues/4529)</th>
<td>Deriving Data does not work for attached code</td></tr>
<tr><th>[\#4530](http://gitlabghc.nibbler/ghc/ghc/issues/4530)</th>
<td>Deriving Data for existentially quantified types</td></tr>
<tr><th>[\#4815](http://gitlabghc.nibbler/ghc/ghc/issues/4815)</th>
<td>Instance constraints should be used when deriving on associated data types</td></tr>
<tr><th>[\#4896](http://gitlabghc.nibbler/ghc/ghc/issues/4896)</th>
<td>Deriving Data does not work for attached code</td></tr>
<tr><th>[\#5007](http://gitlabghc.nibbler/ghc/ghc/issues/5007)</th>
<td>"deriving" seems to ignore class context for a type family</td></tr>
<tr><th>[\#7199](http://gitlabghc.nibbler/ghc/ghc/issues/7199)</th>
<td>Standalone deriving Show at GHCi prompt causes divergence when printing</td></tr>
<tr><th>[\#7401](http://gitlabghc.nibbler/ghc/ghc/issues/7401)</th>
<td>Can't derive instance for Eq when datatype has no constructor, while it is trivial do do so.</td></tr>
<tr><th>[\#7436](http://gitlabghc.nibbler/ghc/ghc/issues/7436)</th>
<td>Derived Foldable and Traversable instances become extremely inefficient due to eta-expansion</td></tr>
<tr><th>[\#7742](http://gitlabghc.nibbler/ghc/ghc/issues/7742)</th>
<td>StandaloneDeriving on Read fails for GADTs</td></tr>
<tr><th>[\#8128](http://gitlabghc.nibbler/ghc/ghc/issues/8128)</th>
<td>Standalone deriving fails for GADTs due to inaccessible code</td></tr>
<tr><th>[\#8165](http://gitlabghc.nibbler/ghc/ghc/issues/8165)</th>
<td>Use GeneralizedNewtypeDeriving to automatically create associated type families</td></tr>
<tr><th>[\#8263](http://gitlabghc.nibbler/ghc/ghc/issues/8263)</th>
<td>allow duplicate deriving / standalone deriving</td></tr>
<tr><th>[\#8631](http://gitlabghc.nibbler/ghc/ghc/issues/8631)</th>
<td>Need ImpredicativeTypes for GeneralizedNewtypeDeriving?</td></tr>
<tr><th>[\#8740](http://gitlabghc.nibbler/ghc/ghc/issues/8740)</th>
<td>Deriving instance conditionally compiles</td></tr>
<tr><th>[\#9444](http://gitlabghc.nibbler/ghc/ghc/issues/9444)</th>
<td>-ddump-deriv doesn't dump failed newtype-deriving</td></tr>
<tr><th>[\#9630](http://gitlabghc.nibbler/ghc/ghc/issues/9630)</th>
<td>compile-time performance regression (probably due to Generics)</td></tr>
<tr><th>[\#9850](http://gitlabghc.nibbler/ghc/ghc/issues/9850)</th>
<td>Template Haskell does not seem to support StandaloneDeriving</td></tr>
<tr><th>[\#10361](http://gitlabghc.nibbler/ghc/ghc/issues/10361)</th>
<td>DeriveAnyClass does not fill in associated type defaults</td></tr>
<tr><th>[\#10447](http://gitlabghc.nibbler/ghc/ghc/issues/10447)</th>
<td>DeriveFoldable rejects instances with constraints in last argument of data type</td></tr>
<tr><th>[\#10524](http://gitlabghc.nibbler/ghc/ghc/issues/10524)</th>
<td>PolyKinds doesn't interact well with DeriveFunctor</td></tr>
<tr><th>[\#10561](http://gitlabghc.nibbler/ghc/ghc/issues/10561)</th>
<td>"deriving (Functor)" on a polykinded type produces ill-kinded instance</td></tr>
<tr><th>[\#10577](http://gitlabghc.nibbler/ghc/ghc/issues/10577)</th>
<td>Use empty cases where appropriate when deriving instances for empty types</td></tr>
<tr><th>[\#10598](http://gitlabghc.nibbler/ghc/ghc/issues/10598)</th>
<td>DeriveAnyClass and GND don't work well together</td></tr>
<tr><th>[\#10607](http://gitlabghc.nibbler/ghc/ghc/issues/10607)</th>
<td>Auto derive from top to bottom</td></tr>
<tr><th>[\#10835](http://gitlabghc.nibbler/ghc/ghc/issues/10835)</th>
<td>Regression in standalone Data deriving for phantom types</td></tr>
<tr><th>[\#10858](http://gitlabghc.nibbler/ghc/ghc/issues/10858)</th>
<td>Smaller generated Ord instances</td></tr>
<tr><th>[\#10859](http://gitlabghc.nibbler/ghc/ghc/issues/10859)</th>
<td>Generated Eq instance associates && wrongly</td></tr>
<tr><th>[\#10938](http://gitlabghc.nibbler/ghc/ghc/issues/10938)</th>
<td>DeriveAnyClass + deriving Bifunctor causes GHC panic</td></tr>
<tr><th>[\#11174](http://gitlabghc.nibbler/ghc/ghc/issues/11174)</th>
<td>Traversable can't be derived for datatypes with unboxed arguments</td></tr>
<tr><th>[\#11396](http://gitlabghc.nibbler/ghc/ghc/issues/11396)</th>
<td>deriving Ix with custom ifThenElse causes "Bad call to tagToEnum\#"</td></tr>
<tr><th>[\#11509](http://gitlabghc.nibbler/ghc/ghc/issues/11509)</th>
<td>Incorrect error message in StandaloneDeriving: "The data constructors of \<typeclass\> are not all in scope"</td></tr>
<tr><th>[\#11837](http://gitlabghc.nibbler/ghc/ghc/issues/11837)</th>
<td>GHC fails to unify kinds when deriving polykinded typeclass instance for polykinded newtype</td></tr>
<tr><th>[\#12047](http://gitlabghc.nibbler/ghc/ghc/issues/12047)</th>
<td>Users Guide: GeneralizedNewtypeDeriving derives “instance Num Int =\> Num Dollars”</td></tr>
<tr><th>[\#12080](http://gitlabghc.nibbler/ghc/ghc/issues/12080)</th>
<td>RebindableSyntax breaks deriving Ord</td></tr>
<tr><th>[\#12234](http://gitlabghc.nibbler/ghc/ghc/issues/12234)</th>
<td>'deriving Eq' on recursive datatype makes ghc eat a lot of CPU and RAM</td></tr>
<tr><th>[\#12399](http://gitlabghc.nibbler/ghc/ghc/issues/12399)</th>
<td>DeriveFunctor fail</td></tr>
<tr><th>[\#12438](http://gitlabghc.nibbler/ghc/ghc/issues/12438)</th>
<td>DeriveDataTypeable - deriving instance Data (Mu (Const ()))</td></tr>
<tr><th>[\#12439](http://gitlabghc.nibbler/ghc/ghc/issues/12439)</th>
<td>DeriveDataTypeable - deriving Data for several type constructor applications</td></tr>
<tr><th>[\#12616](http://gitlabghc.nibbler/ghc/ghc/issues/12616)</th>
<td>type synonyms confuse generalized newtype deriving role checking</td></tr>
<tr><th>[\#12688](http://gitlabghc.nibbler/ghc/ghc/issues/12688)</th>
<td>derived Show instances aren't protected from RebindableSyntax+OverloadedStrings</td></tr>
<tr><th>[\#12768](http://gitlabghc.nibbler/ghc/ghc/issues/12768)</th>
<td>8.0.2 derives invalid code when class method is constrained by itself</td></tr>
<tr><th>[\#12814](http://gitlabghc.nibbler/ghc/ghc/issues/12814)</th>
<td>Should GND infer an instance context when deriving method-free classes?</td></tr>
<tr><th>[\#13056](http://gitlabghc.nibbler/ghc/ghc/issues/13056)</th>
<td>Deriving Foldable causes GHC to take a long time (GHC 8.0 ONLY)</td></tr>
<tr><th>[\#13117](http://gitlabghc.nibbler/ghc/ghc/issues/13117)</th>
<td>Derived functor instance for void types handles errors badly</td></tr>
<tr><th>[\#13175](http://gitlabghc.nibbler/ghc/ghc/issues/13175)</th>
<td>Documenting what can be derived 'out of the box' by GHC's "deriving"</td></tr>
<tr><th>[\#13218](http://gitlabghc.nibbler/ghc/ghc/issues/13218)</th>
<td>\<$ is bad in derived functor instances</td></tr>
<tr><th>[\#13263](http://gitlabghc.nibbler/ghc/ghc/issues/13263)</th>
<td>cant derive functor on function newtype with unboxed tuple result</td></tr>
<tr><th>[\#13272](http://gitlabghc.nibbler/ghc/ghc/issues/13272)</th>
<td>DeriveAnyClass regression involving a rigid type variable</td></tr>
<tr><th>[\#13314](http://gitlabghc.nibbler/ghc/ghc/issues/13314)</th>
<td>StandaloneDeriving and DeriveAnyClass don't work together</td></tr>
<tr><th>[\#13324](http://gitlabghc.nibbler/ghc/ghc/issues/13324)</th>
<td>Allow PartialTypeSignatures in the instance context of a standalone deriving declaration</td></tr>
<tr><th>[\#13328](http://gitlabghc.nibbler/ghc/ghc/issues/13328)</th>
<td>Foldable, Functor, and Traversable deriving handle phantom types badly</td></tr>
<tr><th>[\#13759](http://gitlabghc.nibbler/ghc/ghc/issues/13759)</th>
<td>Strange error message for deriving Data</td></tr>
<tr><th>[\#14070](http://gitlabghc.nibbler/ghc/ghc/issues/14070)</th>
<td>Allow ‘unsafe’ deriving strategy, deriving code with ‘unsafeCoerce’</td></tr>
<tr><th>[\#14094](http://gitlabghc.nibbler/ghc/ghc/issues/14094)</th>
<td>DeriveAnyClass doesn't warn about unimplemented type families</td></tr>
<tr><th>[\#14339](http://gitlabghc.nibbler/ghc/ghc/issues/14339)</th>
<td>GHC 8.2.1 regression when combining GND with TypeError (solveDerivEqns: probable loop)</td></tr>
<tr><th>[\#14357](http://gitlabghc.nibbler/ghc/ghc/issues/14357)</th>
<td>Document deriving strategies fully</td></tr>
<tr><th>[\#14364](http://gitlabghc.nibbler/ghc/ghc/issues/14364)</th>
<td>Reduce repetition in derived Read instances</td></tr>
<tr><th>[\#14365](http://gitlabghc.nibbler/ghc/ghc/issues/14365)</th>
<td>Panic with (bogus?) deriving in hs-boot: newTyConEtadArity</td></tr>
<tr><th>[\#14462](http://gitlabghc.nibbler/ghc/ghc/issues/14462)</th>
<td>deriving on associated data types fails to find constraints</td></tr>
<tr><th>[\#14578](http://gitlabghc.nibbler/ghc/ghc/issues/14578)</th>
<td>Incorrect parenthesization of types in -ddump-deriv</td></tr>
<tr><th>[\#14661](http://gitlabghc.nibbler/ghc/ghc/issues/14661)</th>
<td>Cannot derive (newtype I a b = I (F a -\> F b) deriving newtype Category) for type family F</td></tr>
<tr><th>[\#14682](http://gitlabghc.nibbler/ghc/ghc/issues/14682)</th>
<td>Atrocious parenthesization in -ddump-deriv output</td></tr>
<tr><th>[\#14692](http://gitlabghc.nibbler/ghc/ghc/issues/14692)</th>
<td>Deriving Show with -XEmptyDataDeriving cases on the wrong argument</td></tr>
<tr><th>[\#14728](http://gitlabghc.nibbler/ghc/ghc/issues/14728)</th>
<td>Is (GeneralizedNewtypeDeriving + associated type classes) completely bogus?</td></tr>
<tr><th>[\#14748](http://gitlabghc.nibbler/ghc/ghc/issues/14748)</th>
<td>Infer context for Data instance of (data Foo f = Foo (f Bool) (f Int))</td></tr>
<tr><th>[\#14916](http://gitlabghc.nibbler/ghc/ghc/issues/14916)</th>
<td>Missing checks when deriving special classes</td></tr>
<tr><th>[\#14918](http://gitlabghc.nibbler/ghc/ghc/issues/14918)</th>
<td>GHC 8.4.1 regression: derived Read instances with field names containing \# no longer parse</td></tr>
<tr><th>[\#14932](http://gitlabghc.nibbler/ghc/ghc/issues/14932)</th>
<td>DeriveAnyClass produces unjustifiably untouchable unification variables</td></tr>
<tr><th>[\#14933](http://gitlabghc.nibbler/ghc/ghc/issues/14933)</th>
<td>DeriveAnyClass can cause "No skolem info" GHC panic</td></tr>
<tr><th>[\#15012](http://gitlabghc.nibbler/ghc/ghc/issues/15012)</th>
<td>"Iface type variable out of scope" when compiling with -c</td></tr>
<tr><th>[\#15073](http://gitlabghc.nibbler/ghc/ghc/issues/15073)</th>
<td>Unable to newtype derive \`Prim\` via DerivingStrategies</td></tr>
<tr><th>[\#15178](http://gitlabghc.nibbler/ghc/ghc/issues/15178)</th>
<td>Implement DerivingVia</td></tr>
<tr><th>[\#15398](http://gitlabghc.nibbler/ghc/ghc/issues/15398)</th>
<td>GADT deriving Ord generates inaccessible code in a pattern with constructor.</td></tr>
<tr><th>[\#15637](http://gitlabghc.nibbler/ghc/ghc/issues/15637)</th>
<td>Ambiguous type variables in GeneralisedNewtypeDeriving</td></tr>
<tr><th>[\#15798](http://gitlabghc.nibbler/ghc/ghc/issues/15798)</th>
<td>Flag to warn when deriving strategy is not explicitly specified</td></tr>
<tr><th>[\#16179](http://gitlabghc.nibbler/ghc/ghc/issues/16179)</th>
<td>Mention DerivingStrategies in the warning when DeriveAnyClass and GeneralizedNewtypeDeriving are both enabled</td></tr>
<tr><th>[\#16194](http://gitlabghc.nibbler/ghc/ghc/issues/16194)</th>
<td>deriving, wrong code: newtype T cat a = MkT ((forall xx. cat xx xx) -\> a) deriving stock Functor</td></tr></table>



