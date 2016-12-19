# Adding kind equalities to GHC



This page -- a description of the first phase of integrating full dependent types into GHC -- has two main parts: 


- The first stretch describes user-facing changes in GHC 8.


 


- The second is notes I put together for discussion with other implementors, chiefly Simon PJ.


See also the parent page [DependentHaskell](dependent-haskell).


# Status



Use Keyword = `TypeInType` to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th>[\#7503](http://gitlabghc.nibbler/ghc/ghc/issues/7503)</th>
<td>Bug with PolyKinds, type synonyms & GADTs</td></tr>
<tr><th>[\#10141](http://gitlabghc.nibbler/ghc/ghc/issues/10141)</th>
<td>CUSK mysteries</td></tr>
<tr><th>[\#11196](http://gitlabghc.nibbler/ghc/ghc/issues/11196)</th>
<td>TypeInType performance regressions</td></tr>
<tr><th>[\#11197](http://gitlabghc.nibbler/ghc/ghc/issues/11197)</th>
<td>Overeager deferred type errors</td></tr>
<tr><th>[\#11198](http://gitlabghc.nibbler/ghc/ghc/issues/11198)</th>
<td>TypeInType error message regressions</td></tr>
<tr><th>[\#11962](http://gitlabghc.nibbler/ghc/ghc/issues/11962)</th>
<td>Support induction recursion</td></tr>
<tr><th>[\#12088](http://gitlabghc.nibbler/ghc/ghc/issues/12088)</th>
<td>Type/data family instances in kind checking</td></tr>
<tr><th>[\#12564](http://gitlabghc.nibbler/ghc/ghc/issues/12564)</th>
<td>Type family in type pattern kind</td></tr>
<tr><th>[\#12612](http://gitlabghc.nibbler/ghc/ghc/issues/12612)</th>
<td>Allow kinds of associated types to depend on earlier associated types</td></tr>
<tr><th>[\#12766](http://gitlabghc.nibbler/ghc/ghc/issues/12766)</th>
<td>Allow runtime-representation polymorphic data families</td></tr>
<tr><th>[\#12928](http://gitlabghc.nibbler/ghc/ghc/issues/12928)</th>
<td>Too easy to trigger CUSK condition using TH</td></tr>
<tr><th>[\#13365](http://gitlabghc.nibbler/ghc/ghc/issues/13365)</th>
<td>Notify user when adding a CUSK might help fix a type error</td></tr>
<tr><th>[\#13408](http://gitlabghc.nibbler/ghc/ghc/issues/13408)</th>
<td>Consider inferring a higher-rank kind for type synonyms</td></tr>
<tr><th>[\#13650](http://gitlabghc.nibbler/ghc/ghc/issues/13650)</th>
<td>Implement KPush in types</td></tr>
<tr><th>[\#13933](http://gitlabghc.nibbler/ghc/ghc/issues/13933)</th>
<td>Support Typeable instances for types with coercions</td></tr>
<tr><th>[\#14040](http://gitlabghc.nibbler/ghc/ghc/issues/14040)</th>
<td>Typed holes regression in GHC 8.0.2: No skolem info: z\_a1sY\[sk:2\]</td></tr>
<tr><th>[\#14119](http://gitlabghc.nibbler/ghc/ghc/issues/14119)</th>
<td>Refactor type patterns</td></tr>
<tr><th>[\#14155](http://gitlabghc.nibbler/ghc/ghc/issues/14155)</th>
<td>GHC mentions unlifted types out of the blue (to me anyway)</td></tr>
<tr><th>[\#14180](http://gitlabghc.nibbler/ghc/ghc/issues/14180)</th>
<td>Strange/bad error message binding unboxed type variable</td></tr>
<tr><th>[\#14198](http://gitlabghc.nibbler/ghc/ghc/issues/14198)</th>
<td>Inconsistent treatment of implicitly bound kind variables as free-floating</td></tr>
<tr><th>[\#14319](http://gitlabghc.nibbler/ghc/ghc/issues/14319)</th>
<td>Stuck type families can lead to lousy error messages</td></tr>
<tr><th>[\#14419](http://gitlabghc.nibbler/ghc/ghc/issues/14419)</th>
<td>Check kinds for ambiguity</td></tr>
<tr><th>[\#14514](http://gitlabghc.nibbler/ghc/ghc/issues/14514)</th>
<td>Error messages: suggest annotating with higher-rank kind</td></tr>
<tr><th>[\#14645](http://gitlabghc.nibbler/ghc/ghc/issues/14645)</th>
<td>Allow type family in data family return kind</td></tr>
<tr><th>[\#14668](http://gitlabghc.nibbler/ghc/ghc/issues/14668)</th>
<td>Ordering of declarations can cause typechecking to fail</td></tr>
<tr><th>[\#14873](http://gitlabghc.nibbler/ghc/ghc/issues/14873)</th>
<td>The well-kinded type invariant (in TcType)</td></tr>
<tr><th>[\#15474](http://gitlabghc.nibbler/ghc/ghc/issues/15474)</th>
<td>Error message mentions Any</td></tr>
<tr><th>[\#15549](http://gitlabghc.nibbler/ghc/ghc/issues/15549)</th>
<td>Core Lint error with EmptyCase</td></tr>
<tr><th>[\#15561](http://gitlabghc.nibbler/ghc/ghc/issues/15561)</th>
<td>TypeInType: Type error conditioned on ordering of GADT and type family definitions</td></tr>
<tr><th>[\#15577](http://gitlabghc.nibbler/ghc/ghc/issues/15577)</th>
<td>TypeApplications-related infinite loop (GHC 8.6+ only)</td></tr>
<tr><th>[\#15588](http://gitlabghc.nibbler/ghc/ghc/issues/15588)</th>
<td>Panic when abusing kind inference</td></tr>
<tr><th>[\#15589](http://gitlabghc.nibbler/ghc/ghc/issues/15589)</th>
<td>Always promoting metavariables during type inference may be wrong</td></tr>
<tr><th>[\#15658](http://gitlabghc.nibbler/ghc/ghc/issues/15658)</th>
<td>strange inferred kind with TypeInType</td></tr>
<tr><th>[\#15710](http://gitlabghc.nibbler/ghc/ghc/issues/15710)</th>
<td>Should GHC accept a type signature that needs coercion quantification?</td></tr>
<tr><th>[\#15862](http://gitlabghc.nibbler/ghc/ghc/issues/15862)</th>
<td>Panic with promoted types that Typeable doesn't support</td></tr>
<tr><th>[\#15869](http://gitlabghc.nibbler/ghc/ghc/issues/15869)</th>
<td>Discrepancy between seemingly equivalent type synonym and type family</td></tr>
<tr><th>[\#15883](http://gitlabghc.nibbler/ghc/ghc/issues/15883)</th>
<td>GHC panic: newtype F rep = F (forall (a :: TYPE rep). a)</td></tr>
<tr><th>[\#15942](http://gitlabghc.nibbler/ghc/ghc/issues/15942)</th>
<td>Associated type family can't be used at the kind level within other parts of parent class</td></tr>
<tr><th>[\#16188](http://gitlabghc.nibbler/ghc/ghc/issues/16188)</th>
<td>GHC HEAD-only panic (buildKindCoercion)</td></tr>
<tr><th>[\#16204](http://gitlabghc.nibbler/ghc/ghc/issues/16204)</th>
<td>GHC HEAD-only Core Lint error (Argument value doesn't match argument type)</td></tr>
<tr><th>[\#16225](http://gitlabghc.nibbler/ghc/ghc/issues/16225)</th>
<td>GHC HEAD-only Core Lint error (Trans coercion mis-match)</td></tr>
<tr><th>[\#16244](http://gitlabghc.nibbler/ghc/ghc/issues/16244)</th>
<td>Couldn't match kind ‘k1’ with ‘k1’</td></tr>
<tr><th>[\#16245](http://gitlabghc.nibbler/ghc/ghc/issues/16245)</th>
<td>GHC panic (No skolem info) with QuantifiedConstraints and strange scoping</td></tr>
<tr><th>[\#16247](http://gitlabghc.nibbler/ghc/ghc/issues/16247)</th>
<td>GHC 8.6 Core Lint regression (Kind application error)</td></tr></table>




Closed Tickets:

<table><tr><th>[\#11241](http://gitlabghc.nibbler/ghc/ghc/issues/11241)</th>
<td>Kind-level PartialTypeSignatures causes internal error</td></tr>
<tr><th>[\#11246](http://gitlabghc.nibbler/ghc/ghc/issues/11246)</th>
<td>Regression typechecking type synonym which includes \`Any\`.</td></tr>
<tr><th>[\#11252](http://gitlabghc.nibbler/ghc/ghc/issues/11252)</th>
<td>:kind command hides the explicit kind</td></tr>
<tr><th>[\#11266](http://gitlabghc.nibbler/ghc/ghc/issues/11266)</th>
<td>Can't :browse some modules with GHCi 7.11</td></tr>
<tr><th>[\#11334](http://gitlabghc.nibbler/ghc/ghc/issues/11334)</th>
<td>GHC panic when calling typeOf on a promoted data constructor</td></tr>
<tr><th>[\#11356](http://gitlabghc.nibbler/ghc/ghc/issues/11356)</th>
<td>GHC panic</td></tr>
<tr><th>[\#11357](http://gitlabghc.nibbler/ghc/ghc/issues/11357)</th>
<td>Regression when deriving Generic1 on poly-kinded data family</td></tr>
<tr><th>[\#11362](http://gitlabghc.nibbler/ghc/ghc/issues/11362)</th>
<td>T6137 doesn't pass with reversed uniques</td></tr>
<tr><th>[\#11399](http://gitlabghc.nibbler/ghc/ghc/issues/11399)</th>
<td>Ill-kinded instance head involving -XTypeInType can invoke GHC panic</td></tr>
<tr><th>[\#11400](http://gitlabghc.nibbler/ghc/ghc/issues/11400)</th>
<td>\* is not an indexed type family</td></tr>
<tr><th>[\#11401](http://gitlabghc.nibbler/ghc/ghc/issues/11401)</th>
<td>No match in record selector ctev\_dest</td></tr>
<tr><th>[\#11404](http://gitlabghc.nibbler/ghc/ghc/issues/11404)</th>
<td>The type variable used in a kind is still used</td></tr>
<tr><th>[\#11405](http://gitlabghc.nibbler/ghc/ghc/issues/11405)</th>
<td>Incorrect failure of type-level skolem escape check</td></tr>
<tr><th>[\#11407](http://gitlabghc.nibbler/ghc/ghc/issues/11407)</th>
<td>-XTypeInType uses up all memory when used in data family instance</td></tr>
<tr><th>[\#11410](http://gitlabghc.nibbler/ghc/ghc/issues/11410)</th>
<td>Quantification over unlifted type variable</td></tr>
<tr><th>[\#11416](http://gitlabghc.nibbler/ghc/ghc/issues/11416)</th>
<td>GHC mistakenly believes datatype with type synonym in its type can't be eta-reduced</td></tr>
<tr><th>[\#11453](http://gitlabghc.nibbler/ghc/ghc/issues/11453)</th>
<td>Kinds in type synonym/data declarations can unexpectedly unify</td></tr>
<tr><th>[\#11463](http://gitlabghc.nibbler/ghc/ghc/issues/11463)</th>
<td>Template Haskell applies too many arguments to kind synonym</td></tr>
<tr><th>[\#11471](http://gitlabghc.nibbler/ghc/ghc/issues/11471)</th>
<td>Kind polymorphism and unboxed types: bad things are happening</td></tr>
<tr><th>[\#11473](http://gitlabghc.nibbler/ghc/ghc/issues/11473)</th>
<td>Levity polymorphism checks are inadequate</td></tr>
<tr><th>[\#11484](http://gitlabghc.nibbler/ghc/ghc/issues/11484)</th>
<td>Type synonym using -XTypeInType can't be spliced with TH</td></tr>
<tr><th>[\#11485](http://gitlabghc.nibbler/ghc/ghc/issues/11485)</th>
<td>Very unhelpful message resulting from kind mismatch</td></tr>
<tr><th>[\#11519](http://gitlabghc.nibbler/ghc/ghc/issues/11519)</th>
<td>Inferring non-tau kinds</td></tr>
<tr><th>[\#11520](http://gitlabghc.nibbler/ghc/ghc/issues/11520)</th>
<td>GHC falls into a hole if given incorrect kind signature</td></tr>
<tr><th>[\#11548](http://gitlabghc.nibbler/ghc/ghc/issues/11548)</th>
<td>Absolutely misleading error message on kind error</td></tr>
<tr><th>[\#11549](http://gitlabghc.nibbler/ghc/ghc/issues/11549)</th>
<td>Add -fshow-runtime-rep</td></tr>
<tr><th>[\#11554](http://gitlabghc.nibbler/ghc/ghc/issues/11554)</th>
<td>Self quantification in GADT data declarations</td></tr>
<tr><th>[\#11560](http://gitlabghc.nibbler/ghc/ghc/issues/11560)</th>
<td>panic: isInjectiveTyCon sees a TcTyCon</td></tr>
<tr><th>[\#11592](http://gitlabghc.nibbler/ghc/ghc/issues/11592)</th>
<td>Self-kinded type variable accepted</td></tr>
<tr><th>[\#11614](http://gitlabghc.nibbler/ghc/ghc/issues/11614)</th>
<td>document TypeInType</td></tr>
<tr><th>[\#11635](http://gitlabghc.nibbler/ghc/ghc/issues/11635)</th>
<td>Higher-rank kind in datatype definition rejected</td></tr>
<tr><th>[\#11642](http://gitlabghc.nibbler/ghc/ghc/issues/11642)</th>
<td>Heterogeneous type equality evidence ignored</td></tr>
<tr><th>[\#11648](http://gitlabghc.nibbler/ghc/ghc/issues/11648)</th>
<td>assertPprPanic, called at compiler/types/TyCoRep.hs:1932</td></tr>
<tr><th>[\#11672](http://gitlabghc.nibbler/ghc/ghc/issues/11672)</th>
<td>Poor error message</td></tr>
<tr><th>[\#11699](http://gitlabghc.nibbler/ghc/ghc/issues/11699)</th>
<td>Type families mistakingly report kind variables as unbound type variables</td></tr>
<tr><th>[\#11716](http://gitlabghc.nibbler/ghc/ghc/issues/11716)</th>
<td>Make TypeInType stress test work</td></tr>
<tr><th>[\#11719](http://gitlabghc.nibbler/ghc/ghc/issues/11719)</th>
<td>Cannot use higher-rank kinds with type families</td></tr>
<tr><th>[\#11732](http://gitlabghc.nibbler/ghc/ghc/issues/11732)</th>
<td>Deriving Generic1 interacts poorly with TypeInType</td></tr>
<tr><th>[\#11785](http://gitlabghc.nibbler/ghc/ghc/issues/11785)</th>
<td>Merge types and kinds in Template Haskell</td></tr>
<tr><th>[\#11811](http://gitlabghc.nibbler/ghc/ghc/issues/11811)</th>
<td>GHC sometimes misses a CUSK</td></tr>
<tr><th>[\#11821](http://gitlabghc.nibbler/ghc/ghc/issues/11821)</th>
<td>Internal error: not in scope during type checking, but it passed the renamer</td></tr>
<tr><th>[\#11963](http://gitlabghc.nibbler/ghc/ghc/issues/11963)</th>
<td>GHC introduces kind equality without TypeInType</td></tr>
<tr><th>[\#11964](http://gitlabghc.nibbler/ghc/ghc/issues/11964)</th>
<td>Without TypeInType, inconsistently accepts Data.Kind.Type but not type synonym</td></tr>
<tr><th>[\#11966](http://gitlabghc.nibbler/ghc/ghc/issues/11966)</th>
<td>Surprising behavior with higher-rank quantification of kind variables</td></tr>
<tr><th>[\#11995](http://gitlabghc.nibbler/ghc/ghc/issues/11995)</th>
<td>Can't infer type</td></tr>
<tr><th>[\#12029](http://gitlabghc.nibbler/ghc/ghc/issues/12029)</th>
<td>Notify user to import \* from Data.Kind with TypeInType on</td></tr>
<tr><th>[\#12030](http://gitlabghc.nibbler/ghc/ghc/issues/12030)</th>
<td>GHCi Proposal: Display (Data.Kind.)Type instead of \*</td></tr>
<tr><th>[\#12045](http://gitlabghc.nibbler/ghc/ghc/issues/12045)</th>
<td>Visible kind application</td></tr>
<tr><th>[\#12081](http://gitlabghc.nibbler/ghc/ghc/issues/12081)</th>
<td>TypeInType Compile-time Panic</td></tr>
<tr><th>[\#12102](http://gitlabghc.nibbler/ghc/ghc/issues/12102)</th>
<td>“Constraints in kinds” illegal family application in instance (+ documentation issues?)</td></tr>
<tr><th>[\#12174](http://gitlabghc.nibbler/ghc/ghc/issues/12174)</th>
<td>Recursive use of type-in-type results in infinite loop</td></tr>
<tr><th>[\#12176](http://gitlabghc.nibbler/ghc/ghc/issues/12176)</th>
<td>Failure of bidirectional type inference at the kind level</td></tr>
<tr><th>[\#12239](http://gitlabghc.nibbler/ghc/ghc/issues/12239)</th>
<td>Dependent type family does not reduce</td></tr>
<tr><th>[\#12369](http://gitlabghc.nibbler/ghc/ghc/issues/12369)</th>
<td>data families shouldn't be required to have return kind \*, data instances should</td></tr>
<tr><th>[\#12442](http://gitlabghc.nibbler/ghc/ghc/issues/12442)</th>
<td>Pure unifier usually doesn't need to unify kinds</td></tr>
<tr><th>[\#12503](http://gitlabghc.nibbler/ghc/ghc/issues/12503)</th>
<td>Template Haskell regression: GHC erroneously thinks a type variable is also a kind</td></tr>
<tr><th>[\#12534](http://gitlabghc.nibbler/ghc/ghc/issues/12534)</th>
<td>GHC 8.0 accepts recursive kind signature that GHC 7.10 rejects</td></tr>
<tr><th>[\#12553](http://gitlabghc.nibbler/ghc/ghc/issues/12553)</th>
<td>Reference kind in a type instance declaration defined in another instance declaration</td></tr>
<tr><th>[\#12742](http://gitlabghc.nibbler/ghc/ghc/issues/12742)</th>
<td>Instantiation of invisible type family arguments is too eager</td></tr>
<tr><th>[\#12919](http://gitlabghc.nibbler/ghc/ghc/issues/12919)</th>
<td>Equality not used for substitution</td></tr>
<tr><th>[\#12922](http://gitlabghc.nibbler/ghc/ghc/issues/12922)</th>
<td>Kind classes compile with PolyKinds</td></tr>
<tr><th>[\#12931](http://gitlabghc.nibbler/ghc/ghc/issues/12931)</th>
<td>tc\_infer\_args does not set in-scope set correctly</td></tr>
<tr><th>[\#12933](http://gitlabghc.nibbler/ghc/ghc/issues/12933)</th>
<td>Wrong class instance selection with Data.Kind.Type</td></tr>
<tr><th>[\#12938](http://gitlabghc.nibbler/ghc/ghc/issues/12938)</th>
<td>Polykinded associated type family rejected on false pretenses</td></tr>
<tr><th>[\#13109](http://gitlabghc.nibbler/ghc/ghc/issues/13109)</th>
<td>CUSK improvements</td></tr>
<tr><th>[\#13333](http://gitlabghc.nibbler/ghc/ghc/issues/13333)</th>
<td>Typeable regression in GHC HEAD</td></tr>
<tr><th>[\#13337](http://gitlabghc.nibbler/ghc/ghc/issues/13337)</th>
<td>GHC doesn't think a type is of kind \*, despite having evidence for it</td></tr>
<tr><th>[\#13364](http://gitlabghc.nibbler/ghc/ghc/issues/13364)</th>
<td>Remove checkValidTelescope</td></tr>
<tr><th>[\#13391](http://gitlabghc.nibbler/ghc/ghc/issues/13391)</th>
<td>PolyKinds is more permissive in GHC 8</td></tr>
<tr><th>[\#13399](http://gitlabghc.nibbler/ghc/ghc/issues/13399)</th>
<td>Location of \`forall\` matters with higher-rank kind polymorphism</td></tr>
<tr><th>[\#13407](http://gitlabghc.nibbler/ghc/ghc/issues/13407)</th>
<td>Fix printing of higher-rank kinds</td></tr>
<tr><th>[\#13409](http://gitlabghc.nibbler/ghc/ghc/issues/13409)</th>
<td>Data types with higher-rank kinds are pretty-printed strangely</td></tr>
<tr><th>[\#13530](http://gitlabghc.nibbler/ghc/ghc/issues/13530)</th>
<td>Horrible error message due to TypeInType</td></tr>
<tr><th>[\#13546](http://gitlabghc.nibbler/ghc/ghc/issues/13546)</th>
<td>Kind error with type equality</td></tr>
<tr><th>[\#13549](http://gitlabghc.nibbler/ghc/ghc/issues/13549)</th>
<td>GHC 8.2.1's typechecker rejects code generated by singletons that 8.0 accepts</td></tr>
<tr><th>[\#13555](http://gitlabghc.nibbler/ghc/ghc/issues/13555)</th>
<td>Typechecker regression when combining PolyKinds and MonoLocalBinds</td></tr>
<tr><th>[\#13585](http://gitlabghc.nibbler/ghc/ghc/issues/13585)</th>
<td>ala from Control.Lens.Wrapped panics</td></tr>
<tr><th>[\#13601](http://gitlabghc.nibbler/ghc/ghc/issues/13601)</th>
<td>GHC errors but hangs</td></tr>
<tr><th>[\#13603](http://gitlabghc.nibbler/ghc/ghc/issues/13603)</th>
<td>Can't resolve levity polymorphic superclass</td></tr>
<tr><th>[\#13625](http://gitlabghc.nibbler/ghc/ghc/issues/13625)</th>
<td>GHC internal error: ‘Y’ is not in scope during type checking, but it passed the renamer</td></tr>
<tr><th>[\#13643](http://gitlabghc.nibbler/ghc/ghc/issues/13643)</th>
<td>Core lint error with TypeInType and TypeFamilyDependencies</td></tr>
<tr><th>[\#13674](http://gitlabghc.nibbler/ghc/ghc/issues/13674)</th>
<td>Poor error message which masks occurs-check failure</td></tr>
<tr><th>[\#13761](http://gitlabghc.nibbler/ghc/ghc/issues/13761)</th>
<td>Can't create poly-kinded GADT with TypeInType enabled, but can without</td></tr>
<tr><th>[\#13762](http://gitlabghc.nibbler/ghc/ghc/issues/13762)</th>
<td>TypeInType is not documented in the users' guide flag reference</td></tr>
<tr><th>[\#13777](http://gitlabghc.nibbler/ghc/ghc/issues/13777)</th>
<td>Poor error message around CUSKs</td></tr>
<tr><th>[\#13780](http://gitlabghc.nibbler/ghc/ghc/issues/13780)</th>
<td>Nightmarish pretty-printing of equality type in GHC 8.2 error message</td></tr>
<tr><th>[\#13781](http://gitlabghc.nibbler/ghc/ghc/issues/13781)</th>
<td>(a :: (k :: Type)) is too exotic for Template Haskell</td></tr>
<tr><th>[\#13790](http://gitlabghc.nibbler/ghc/ghc/issues/13790)</th>
<td>GHC doesn't reduce type family in kind signature unless its arm is twisted</td></tr>
<tr><th>[\#13822](http://gitlabghc.nibbler/ghc/ghc/issues/13822)</th>
<td>GHC not using injectivity?</td></tr>
<tr><th>[\#13871](http://gitlabghc.nibbler/ghc/ghc/issues/13871)</th>
<td>GHC panic in 8.2 only: typeIsTypeable(Coercion)</td></tr>
<tr><th>[\#13872](http://gitlabghc.nibbler/ghc/ghc/issues/13872)</th>
<td>Strange Typeable error message involving TypeInType</td></tr>
<tr><th>[\#13879](http://gitlabghc.nibbler/ghc/ghc/issues/13879)</th>
<td>Strange interaction between higher-rank kinds and type synonyms</td></tr>
<tr><th>[\#13895](http://gitlabghc.nibbler/ghc/ghc/issues/13895)</th>
<td>"Illegal constraint in a type" error - is it fixable?</td></tr>
<tr><th>[\#13909](http://gitlabghc.nibbler/ghc/ghc/issues/13909)</th>
<td>Misleading error message when partially applying a data type with a visible quantifier in its kind</td></tr>
<tr><th>[\#13910](http://gitlabghc.nibbler/ghc/ghc/issues/13910)</th>
<td>Inlining a definition causes GHC to panic (repSplitTyConApp\_maybe)</td></tr>
<tr><th>[\#13913](http://gitlabghc.nibbler/ghc/ghc/issues/13913)</th>
<td>Can't apply higher-ranked kind in type family</td></tr>
<tr><th>[\#13915](http://gitlabghc.nibbler/ghc/ghc/issues/13915)</th>
<td>GHC 8.2 regression: "Can't find interface-file declaration" for promoted data family instance</td></tr>
<tr><th>[\#13929](http://gitlabghc.nibbler/ghc/ghc/issues/13929)</th>
<td>GHC panic with levity polymorphism</td></tr>
<tr><th>[\#13938](http://gitlabghc.nibbler/ghc/ghc/issues/13938)</th>
<td>Iface type variable out of scope:  k1</td></tr>
<tr><th>[\#13963](http://gitlabghc.nibbler/ghc/ghc/issues/13963)</th>
<td>Runtime representation confusingly displayed</td></tr>
<tr><th>[\#13972](http://gitlabghc.nibbler/ghc/ghc/issues/13972)</th>
<td>GHC 8.2 error message around indexes for associated type instances is baffling</td></tr>
<tr><th>[\#13988](http://gitlabghc.nibbler/ghc/ghc/issues/13988)</th>
<td>GADT constructor with kind equality constraint quantifies unused existential type variables</td></tr>
<tr><th>[\#14042](http://gitlabghc.nibbler/ghc/ghc/issues/14042)</th>
<td>Datatypes cannot use a type family in their return kind</td></tr>
<tr><th>[\#14086](http://gitlabghc.nibbler/ghc/ghc/issues/14086)</th>
<td>Empty case does not detect kinds</td></tr>
<tr><th>[\#14091](http://gitlabghc.nibbler/ghc/ghc/issues/14091)</th>
<td>When PolyKinds is on, suggested type signatures seem to require TypeInType</td></tr>
<tr><th>[\#14139](http://gitlabghc.nibbler/ghc/ghc/issues/14139)</th>
<td>Kind signature not accepted (singletons)</td></tr>
<tr><th>[\#14162](http://gitlabghc.nibbler/ghc/ghc/issues/14162)</th>
<td>Core Lint error</td></tr>
<tr><th>[\#14174](http://gitlabghc.nibbler/ghc/ghc/issues/14174)</th>
<td>GHC panic with TypeInType and type family</td></tr>
<tr><th>[\#14175](http://gitlabghc.nibbler/ghc/ghc/issues/14175)</th>
<td>Panic repSplitTyConApp\_maybe</td></tr>
<tr><th>[\#14203](http://gitlabghc.nibbler/ghc/ghc/issues/14203)</th>
<td>GHC-inferred type signature doesn't actually typecheck</td></tr>
<tr><th>[\#14207](http://gitlabghc.nibbler/ghc/ghc/issues/14207)</th>
<td>Levity polymorphic GADT requires extra type signatures</td></tr>
<tr><th>[\#14209](http://gitlabghc.nibbler/ghc/ghc/issues/14209)</th>
<td>GHC 8.2.1 regression involving telescoping kind signature</td></tr>
<tr><th>[\#14230](http://gitlabghc.nibbler/ghc/ghc/issues/14230)</th>
<td>Gruesome kind mismatch errors for associated data family instances</td></tr>
<tr><th>[\#14238](http://gitlabghc.nibbler/ghc/ghc/issues/14238)</th>
<td>\`:kind\` suppresses visible dependent quantifiers by default in GHCi 8.2.1</td></tr>
<tr><th>[\#14350](http://gitlabghc.nibbler/ghc/ghc/issues/14350)</th>
<td>Infinite loop when typechecking incorrect implementation (GHC HEAD only)</td></tr>
<tr><th>[\#14352](http://gitlabghc.nibbler/ghc/ghc/issues/14352)</th>
<td>Higher-rank kind ascription oddities</td></tr>
<tr><th>[\#14366](http://gitlabghc.nibbler/ghc/ghc/issues/14366)</th>
<td>Type family equation refuses to unify wildcard type patterns</td></tr>
<tr><th>[\#14394](http://gitlabghc.nibbler/ghc/ghc/issues/14394)</th>
<td>Inferred type for pattern synonym has redundant equality constraint</td></tr>
<tr><th>[\#14441](http://gitlabghc.nibbler/ghc/ghc/issues/14441)</th>
<td>GHC HEAD regression involving type families in kinds</td></tr>
<tr><th>[\#14450](http://gitlabghc.nibbler/ghc/ghc/issues/14450)</th>
<td>GHCi spins forever</td></tr>
<tr><th>[\#14451](http://gitlabghc.nibbler/ghc/ghc/issues/14451)</th>
<td>Need proper SCC analysis of type declarations, taking account of CUSKs</td></tr>
<tr><th>[\#14507](http://gitlabghc.nibbler/ghc/ghc/issues/14507)</th>
<td>Core Lint error with Type.Reflection and pattern synonyms</td></tr>
<tr><th>[\#14515](http://gitlabghc.nibbler/ghc/ghc/issues/14515)</th>
<td>"Same" higher-rank kind synonyms behave differently</td></tr>
<tr><th>[\#14520](http://gitlabghc.nibbler/ghc/ghc/issues/14520)</th>
<td>GHC panic (TypeInType)</td></tr>
<tr><th>[\#14552](http://gitlabghc.nibbler/ghc/ghc/issues/14552)</th>
<td>GHC panic on pattern synonym</td></tr>
<tr><th>[\#14554](http://gitlabghc.nibbler/ghc/ghc/issues/14554)</th>
<td>Core Lint error mixing</td></tr>
<tr><th>[\#14555](http://gitlabghc.nibbler/ghc/ghc/issues/14555)</th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th>[\#14556](http://gitlabghc.nibbler/ghc/ghc/issues/14556)</th>
<td>Core Lint error: Ill-kinded result in coercion</td></tr>
<tr><th>[\#14563](http://gitlabghc.nibbler/ghc/ghc/issues/14563)</th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th>[\#14580](http://gitlabghc.nibbler/ghc/ghc/issues/14580)</th>
<td>GHC panic (TypeInType) (the 'impossible' happened)</td></tr>
<tr><th>[\#14584](http://gitlabghc.nibbler/ghc/ghc/issues/14584)</th>
<td>Core Lint error</td></tr>
<tr><th>[\#14605](http://gitlabghc.nibbler/ghc/ghc/issues/14605)</th>
<td>Core Lint error</td></tr>
<tr><th>[\#14607](http://gitlabghc.nibbler/ghc/ghc/issues/14607)</th>
<td>Core Lint error</td></tr>
<tr><th>[\#14720](http://gitlabghc.nibbler/ghc/ghc/issues/14720)</th>
<td>GHC 8.4.1-alpha regression with TypeInType</td></tr>
<tr><th>[\#14749](http://gitlabghc.nibbler/ghc/ghc/issues/14749)</th>
<td>T13822 fails</td></tr>
<tr><th>[\#14764](http://gitlabghc.nibbler/ghc/ghc/issues/14764)</th>
<td>Make $! representation-polymorphic</td></tr>
<tr><th>[\#14795](http://gitlabghc.nibbler/ghc/ghc/issues/14795)</th>
<td>Data type return kinds don't obey the forall-or-nothing rule</td></tr>
<tr><th>[\#14845](http://gitlabghc.nibbler/ghc/ghc/issues/14845)</th>
<td>TypeInType, index GADT by constraint witness</td></tr>
<tr><th>[\#14846](http://gitlabghc.nibbler/ghc/ghc/issues/14846)</th>
<td>Renamer hangs (because of -XInstanceSigs?)</td></tr>
<tr><th>[\#14847](http://gitlabghc.nibbler/ghc/ghc/issues/14847)</th>
<td>Inferring dependent kinds for non-recursive types</td></tr>
<tr><th>[\#14880](http://gitlabghc.nibbler/ghc/ghc/issues/14880)</th>
<td>GHC panic: updateRole</td></tr>
<tr><th>[\#14887](http://gitlabghc.nibbler/ghc/ghc/issues/14887)</th>
<td>Explicitly quantifying a kind variable causes a telescope to fail to kind-check</td></tr>
<tr><th>[\#14904](http://gitlabghc.nibbler/ghc/ghc/issues/14904)</th>
<td>Compiler panic (piResultTy)</td></tr>
<tr><th>[\#14938](http://gitlabghc.nibbler/ghc/ghc/issues/14938)</th>
<td>Pattern matching on GADT does not refine type family parameters</td></tr>
<tr><th>[\#14991](http://gitlabghc.nibbler/ghc/ghc/issues/14991)</th>
<td>GHC HEAD regression involving TYPEs in type families</td></tr>
<tr><th>[\#15039](http://gitlabghc.nibbler/ghc/ghc/issues/15039)</th>
<td>Bizarre pretty-printing of inferred Coercible constraint in partial type signature</td></tr>
<tr><th>[\#15076](http://gitlabghc.nibbler/ghc/ghc/issues/15076)</th>
<td>Typed hole with higher-rank kind causes GHC to panic (No skolem info)</td></tr>
<tr><th>[\#15079](http://gitlabghc.nibbler/ghc/ghc/issues/15079)</th>
<td>GHC HEAD regression: cannot instantiate higher-rank kind</td></tr>
<tr><th>[\#15116](http://gitlabghc.nibbler/ghc/ghc/issues/15116)</th>
<td>GHC internal error when GADT return type mentions its own constructor name</td></tr>
<tr><th>[\#15122](http://gitlabghc.nibbler/ghc/ghc/issues/15122)</th>
<td>GHC HEAD typechecker regression</td></tr>
<tr><th>[\#15142](http://gitlabghc.nibbler/ghc/ghc/issues/15142)</th>
<td>GHC HEAD regression: tcTyVarDetails</td></tr>
<tr><th>[\#15170](http://gitlabghc.nibbler/ghc/ghc/issues/15170)</th>
<td>GHC HEAD panic (dischargeFmv)</td></tr>
<tr><th>[\#15195](http://gitlabghc.nibbler/ghc/ghc/issues/15195)</th>
<td>Merge -XPolyKinds with -XTypeInType</td></tr>
<tr><th>[\#15245](http://gitlabghc.nibbler/ghc/ghc/issues/15245)</th>
<td>Data family promotion is possible</td></tr>
<tr><th>[\#15282](http://gitlabghc.nibbler/ghc/ghc/issues/15282)</th>
<td>Document how equality-bearing constructors are promoted in Core</td></tr>
<tr><th>[\#15308](http://gitlabghc.nibbler/ghc/ghc/issues/15308)</th>
<td>Error message prints explicit kinds when it shouldn't</td></tr>
<tr><th>[\#15330](http://gitlabghc.nibbler/ghc/ghc/issues/15330)</th>
<td>Error message prints invisible kind arguments in a visible matter</td></tr>
<tr><th>[\#15343](http://gitlabghc.nibbler/ghc/ghc/issues/15343)</th>
<td>Implicitly quantifying a kind variable causes GHC 8.6 to panic (coercionKind)</td></tr>
<tr><th>[\#15346](http://gitlabghc.nibbler/ghc/ghc/issues/15346)</th>
<td>Core Lint error in GHC 8.6.1: From-type of Cast differs from type of enclosed expression</td></tr>
<tr><th>[\#15361](http://gitlabghc.nibbler/ghc/ghc/issues/15361)</th>
<td>Error message displays redundant equality constraint</td></tr>
<tr><th>[\#15370](http://gitlabghc.nibbler/ghc/ghc/issues/15370)</th>
<td>Typed hole panic on GHC 8.6 (tcTyVarDetails)</td></tr>
<tr><th>[\#15380](http://gitlabghc.nibbler/ghc/ghc/issues/15380)</th>
<td>Infinite typechecker loop in GHC 8.6</td></tr>
<tr><th>[\#15419](http://gitlabghc.nibbler/ghc/ghc/issues/15419)</th>
<td>GHC 8.6.1 regression (buildKindCoercion)</td></tr>
<tr><th>[\#15428](http://gitlabghc.nibbler/ghc/ghc/issues/15428)</th>
<td>Oversaturated type family application panicks GHC (piResultTys2)</td></tr>
<tr><th>[\#15472](http://gitlabghc.nibbler/ghc/ghc/issues/15472)</th>
<td>GHC HEAD type inference regression post-"Remove decideKindGeneralisationPlan"</td></tr>
<tr><th>[\#15497](http://gitlabghc.nibbler/ghc/ghc/issues/15497)</th>
<td>Coercion Quantification</td></tr>
<tr><th>[\#15515](http://gitlabghc.nibbler/ghc/ghc/issues/15515)</th>
<td>Bogus "No instance" error when type families appear in kinds</td></tr>
<tr><th>[\#15545](http://gitlabghc.nibbler/ghc/ghc/issues/15545)</th>
<td>Forced to enable TypeInType because of (i \~ i)</td></tr>
<tr><th>[\#15583](http://gitlabghc.nibbler/ghc/ghc/issues/15583)</th>
<td>Treating Constraint as Type when using (type C = Constraint)</td></tr>
<tr><th>[\#15597](http://gitlabghc.nibbler/ghc/ghc/issues/15597)</th>
<td>GHC shouting: panic!</td></tr>
<tr><th>[\#15629](http://gitlabghc.nibbler/ghc/ghc/issues/15629)</th>
<td>"No skolem info" panic (GHC 8.6 only)</td></tr>
<tr><th>[\#15664](http://gitlabghc.nibbler/ghc/ghc/issues/15664)</th>
<td>Core Lint error</td></tr>
<tr><th>[\#15709](http://gitlabghc.nibbler/ghc/ghc/issues/15709)</th>
<td>GHC panic using TypeInType with minimal source code</td></tr>
<tr><th>[\#15711](http://gitlabghc.nibbler/ghc/ghc/issues/15711)</th>
<td>Kind inference of class variables does not examine associated types</td></tr>
<tr><th>[\#15740](http://gitlabghc.nibbler/ghc/ghc/issues/15740)</th>
<td>Type family with higher-rank result is too accepting</td></tr>
<tr><th>[\#15778](http://gitlabghc.nibbler/ghc/ghc/issues/15778)</th>
<td>GHC HEAD-only panic (zonkTcTyVarToTyVar)</td></tr>
<tr><th>[\#15787](http://gitlabghc.nibbler/ghc/ghc/issues/15787)</th>
<td>GHC panic using kind application</td></tr>
<tr><th>[\#15789](http://gitlabghc.nibbler/ghc/ghc/issues/15789)</th>
<td>GHC panic using visible kind applications and a higher-rank kind</td></tr>
<tr><th>[\#15795](http://gitlabghc.nibbler/ghc/ghc/issues/15795)</th>
<td>Core lint error with unused kind variable in data type return kind</td></tr>
<tr><th>[\#15817](http://gitlabghc.nibbler/ghc/ghc/issues/15817)</th>
<td>Data family quantification = GHC panic, ‘impossible’ happened</td></tr>
<tr><th>[\#15852](http://gitlabghc.nibbler/ghc/ghc/issues/15852)</th>
<td>Bad axiom produced for polykinded data family</td></tr>
<tr><th>[\#15859](http://gitlabghc.nibbler/ghc/ghc/issues/15859)</th>
<td>Dependent quantification, GHC panic</td></tr>
<tr><th>[\#15872](http://gitlabghc.nibbler/ghc/ghc/issues/15872)</th>
<td>Odd pretty printing of equality constraint in kind ('GHC.Types.Eq\# \<\>)</td></tr>
<tr><th>[\#15874](http://gitlabghc.nibbler/ghc/ghc/issues/15874)</th>
<td>Data families with higher-rank kinds</td></tr>
<tr><th>[\#15881](http://gitlabghc.nibbler/ghc/ghc/issues/15881)</th>
<td>GHC Panic: data A n (a :: n) :: a -\> Type</td></tr>
<tr><th>[\#16008](http://gitlabghc.nibbler/ghc/ghc/issues/16008)</th>
<td>GHC HEAD type family regression involving invisible arguments</td></tr></table>



# User-facing changes



The changes described below are intended to be controlled by a new extension `-XTypeInType`, which will imply `-XPolyKinds` and `-XDataKinds`. But in some cases, it would be quite hard for GHC to know that the new features are being used. These cases all fall under the use of `-XPolyKinds`. So the `-XPolyKinds` language will become modestly more expressive under this proposal. But Haskell98 and Haskell2010 modes remain as standards-compliant as they are today.


## Kinds and types are the same



There will be no distinction in the language between kinds and types. (Error messages will, however, do their best to use the words "type" and "kind" much like they do now. I take non-degradation of error messages very seriously.)



This means that inhabited kinds have type `*`. In particular, `*` has type `*`. Though this causes inconsistency in other dependently-typed languages, it does not in Haskell, essentially because equality proofs are written in a different sub-language than ordinary terms. See [
our paper](http://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf) for more details.



Essentially, this is the one master change coming with Phase 1. But there are many consequences, as I draw out below.


## Kind variables may be explicit



This will work:


```wiki
data Proxy k (a :: k) = Proxy
data Proxy2 :: forall k. k -> * where
  Proxy2 :: forall k (a :: k). Proxy2 a
```


Note that we're dealing with kind variables explicitly here. Explicit kind variables will work everywhere that explicit type variables do -- after all, kinds and types are the same. For backward compatibility and convenience, kind variables may be implicitly quantified, just like today.


## All types can be promoted to kinds



Actually, promotion from type to kind is a no-op. So this holds quite easily. Note that this means we now effectively have *kind* families.


## All data constructors can be promoted to types



This includes GADT constructors and constructors that use type families.


## GADTs may be GADT-like in kind parameters



But of course, because kinds and types are the same. Here are two examples:


```wiki
data (a :: k1) :~~: (b :: k2) where
  HRefl :: forall k (a :: k). a :~~: a
    -- explicit forall there unnecessary but informative

data TypeRep (a :: k) where
  TInt   :: TypeRep Int
  TMaybe :: TypeRep Maybe
```

## `*` is hard to parse, will become `Type`



Say the phrase `Foo * Int` appears in a type. Is that the type operator `*` applied to `Foo` and `Int` or the the type `Foo` applied to the kind `*` and `Int`? It's impossible to know. So we have to do something strange here.



Without `-XTypeInType`, GHC will continue to use its knowledge of whether you are in a type or a kind to distinguish between the type operator `*` and the kind `*`. So all existing code will continue to work, quite conveniently.



With `-XTypeInType`, GHC will treat the kind `*` as an identifier exported from the `Prelude` (and also from `GHC.Exts`). Currently, GHC must parse expressions with operators essentially as a space-separated list of tokens, because it can't know fixities until it figures out where all the operators have been imported from. Thus, when sorting out fixities, the kind `*` will just have a magical fixity which instructs the renamer to treat it like an alphanumeric identifier, not a symbol. This should all work out fine in most code. The only problem is when a user has both the kind `*` and some type operator `*` in scope, such as from `GHC.TypeLits`. Using `*` in this scenario will be a straightforward ambiguous identifier and is an error. Note that `-XTypeInType -XNoImplicitPrelude` will then mean that you cannot use the kind `*` in your program without importing it from somewhere.



In addition to the above treatment, some standard library module (probably `Data.Kind`, if that's not taken) will export `Type`, which will have the same meaning as `*`, as requested by several people in the community. The eventual plan is to deprecate and remove `*` as a parsing oddity. `Type`, naturally, will work both with and without `-XTypeInType`. `Type` does conflict with existing code, but the choice is backward compatible because it's not exported from the `Prelude`. `Type` will be a normal identifier in every way, and it can be aliased through a normal type synonym definition if necessary to avoid naming conflicts.



**QUESTION:** Should `*` just be disabled in `-XTypeInType` code? This is backward compatible but creates a terrible migration story for someone who wants to use `-XTypeInType` in GHC 8.0 but not in previous versions.


## Visible type application



With all the type variables floating around, it will be very convenient to sometimes specify a type variable visibly in source code.



So, if `id :: forall a. a -> a`, then `id @Int :: Int -> Int`. See also a [
draft paper](http://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf) on the subject.


## Type family equations can be constrained



**Did not make it for 8.0.**



Consider the following mess:


```wiki
type family F a

type family Blah (x :: k) :: F k

data Foo :: forall k. k -> F k -> * -> *

type family G a
type instance (F k ~ *) => G (Foo @k a (Blah a) (Blah a)) = Int
```


Note that there is no way to write the equation for `G` without assuming that `F k ~ *`. So we allow this with the syntax above. This will work quite similar to class instances, in that the patterns are matched *first* and the constraints examined only *after* an equation is chosen. This is likely not what you want out of constrained type family equations, but it seems the only way forward that plays nicely with the rest of GHC.



This feature will not carry over to data families (which for reasons beyond the scope of this post require full dependent types), though it will work for closed type families.


# Design questions


## Visibility changes in types



GHC tracks three different levels of visibility: `Invisible` binders are never user-written, `Specified` ones are available for visible type applications, and `Visible` ones are always user-written. See `Note [TyBinders and VisibilityFlags]` in [
TyCoRep](https://github.com/ghc/ghc/blob/master/compiler/types/TyCoRep.hs).



The problem is what to do in higher-rank kind situations. Consider these definitions:


```
data P1 k (a :: k)
-- P1 :: forall k -> k -> *
-- P1 *  Int   is OK
-- P1 _  Int   is OK
-- P1 @* Int   is NOT OK
-- P1    Int   is NOT OK

data P2 (a :: k)
-- P2 :: forall k. k -> *
-- P2 @* Int   is OK
-- P2 @_ Int   is OK
-- P2    Int   is OK
-- P2 *  Int   is NOT OK

data P3 a
-- P3 :: forall {k}. k -> *
-- P3    Int    is OK
-- P3 @* Int    is NOT OK
-- P3 @_ Int    is NOT OK
-- P3 *  Int    is NOT OK

data X (a :: forall k. k -> *)
-- X :: (forall k. k -> *) -> *
```


A few notes on these definitions:


- The notation `forall {k}. k -> *` on `P3`'s type says that the `k` is `Invisible` and is not available for visible type application,
- We say that `P2 @* Int` is OK, but visible type application is not yet implemented in types. This is just an implementation detail, and for the purposes of this discussion, we'll assume that this feature is available.
- It's quite likely `@*` parses as a single lexeme. Let's ignore that fact.
- Note that GHC does not currently parse the type `forall k -> k -> *`. But it does pretty-print that type.


The question before the house is: which of the following are accepted?


1. `X P1`
1. `X P2`
1. `X P3`


Before delving into possible answers, we should note that any of these are sound to accept. The types of `P1`, `P2`, and `P3` are all identical, except for the visibility of the binder for `k`. So it's not silly to consider this question. It comes down to how we'd like the language to behave.



There seem to be three defensible choices for which of these to accept.


1. 

  1. YES
  1. YES
  1. YES


This version simply ignores visibility flags on binders when doing an equality check -- very easy to implement.


- Simon advocated for this design in a call on Apr 1 2016. It is the simplest.
- Richard was concerned about what an equality between `forall k -> k -> *` and `forall k. k -> *` might mean. But we don't need these to be `tcEqType`, we just need a coercion between them. As long as the types are `eqType`, then `Refl (forall k. k -> *)` does nicely.
- Richard was also concerned that if we consider these types equal, it means that we can replace one with the other willy-nilly, in thinking about a *declarative* specification of the type system. But with visible type application rules, we have *two* subtyping relations (and hence two type-equivalence relations) one to use when both types to compare are known and one to use otherwise. (See the [
  paper](http://www.seas.upenn.edu/~sweirich/papers/esop2016-typeapp.pdf).) So having one of these relations accept a connection between these two types is OK.
- Conor finds it abhorrent to think about a system that equates (under any equivalence relation) types that have different user-visible arities, as do `forall k -> k -> *` (arity 2) and `forall k. k -> *` (arity 1).
- Consider now the expression `X (P1 k :: forall k. k -> *)`. Is that accepted? By the kind signature, `X`'s argument has the right kind. Note that the signature brings `k` into scope and then checks `P1 k` against `k -> *`. It indeed has this type, so it certainly seems like `(P1 k :: forall k. k -> *)` should be accepted. (If you're dubious, think about what happens in terms.) Sadly, this desugars into a situation where we need a type-level lambda. We don't have this yet. But we should be future-proof against a language that does have type-level lambda.
- The bullet above argues that `X (P1 k :: forall k. k -> *)` should be accepted. But plan (A) says that `X P1` should be accepted. It surely can't hurt to add a type signature, so that means that `X (P1 :: forall k. k -> *)` should be accepted. And this is quite bizarre to accept that last one alongside the first one.
- In the term-level type inference rules for a polytype type annotation, the first thing that happens is skolemization. It would be a bit odd for the type-level type inference rules to be different, yet such a difference is required if we are to accept `P1 :: forall k. k -> *`.

1. 

  1. NO
  1. YES
  1. YES


This version is a bit harder to implement, discerning between visible/not visible but not among specified and fully invisible.


- Stephanie and Conor like this one the most.
- It distinguishes between visible and not. Whether or not a binder is visible is easy to specify, but the user has less obvious control over whether a binder is specified or invisible.
- This plan (as does plan (A)) fails source level substitution. Specifically, if `a` (the variable of type `forall k. k -> *` bound in the declaration for `X`) is used as `a @* Int` in the definition of `X`, then `X P3` would expand to mention `P3 @* Int`, which is disallowed. Substitution is restored if we substitute along with a type annotation, thus: `(P3 :: forall k. k -> *) @* Int`.
- This one seems most similar to the term-level treatment. It's hard to fully compare, because case (1) does not exist in terms.

1. 

  1. NO
  1. YES
  1. NO


This is what Richard originally implemented, having type equality depend closely on visibility.


- This version leads to obnoxious error messages if `-fprint-explicit-foralls` is off, saying on `X P3` that `forall k. k -> *` does not match `forall k. k -> *`.
- This version has the advantage of allowing substitution without type annotations. However, we already don't have this property in the term-level language due to the specified/invisible variable distinction.
- This is really quite limiting.


Taking this all into account, Richard advocates for (B), but not all that strongly.


# Implementation notes


## The new type system



The new type system adheres rather closely to what is proposed in the original "FC with Kind Equality" paper, available [
here](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf), with the addition of [NoSubKinds](no-sub-kinds). The type system, as implemented, is in the [
Core Specification](https://github.com/goldfirere/ghc/raw/nokinds/docs/core-spec/core-spec.pdf) document. Here are the highlights of the changes:


- Two new constructors for `Type`: `CastTy :: Type -> Coercion -> Type` (which performs a kind cast) and `CoercionTy :: Coercion -> Type` which embeds coercions into types. The former eliminates kind coercions (that's the whole point of this change!) and the latter allows for the promotion of GADT data constructors, which will promote to a type taking a coercion argument.

- Forall-coercions now take a `ForAllCoBndr`:

  ```wiki
  data ForAllCoBndr = ForAllCoBndr Coercion TyCoVar TyCoVar (Maybe CoVar)
  ```

  Suppose `g :: forall a1:k1.t1 ~ forall a2:k2.t2`. Unlike previously, `k1` and `k2` can be different. This necessitates storing both `a1` and `a2` in the forall-coercion. The `Coercion` datum is a proof that `k1 ~ k2`, and the `Maybe CoVar` proves that `a1 ~ a2`, when `a1` and `a2` are type variables. (When they're coercion variables, we can just use proof-irrelevance, described below.)

- New coercion forms: `CoherenceCo` (which proves equality between a type and that type with a cast on it) and `KindCo` (which extracts a kind coercion from a heterogeneous type coercion).

- `UnivCo` provenances are now a datatype instead of a string:

  ```wiki
  data UnivCoProvenance
    = UnsafeCoerceProv   -- ^ From @unsafeCoerce#@
    | PhantomProv        -- ^ From the need to create a phantom coercion;
                         --   the UnivCo must be Phantom.
    | ProofIrrelProv     -- ^ From the fact that any two coercions are
                         --   considered equivalent
  ```

  A `ProofIrrelProv` `UnivCo` requires that the two types in the `UnivCo` are both just coercions. Proofs are irrelevant in FC. A `PhantomProv` `UnivCo` requires that the role of the `UnivCo` be `Phantom`. These checks are part of CoreLint.

- [NoSubKinds](no-sub-kinds).

- Roles on kind coercions, as described in my recent ICFP submission [
  here](http://www.cis.upenn.edu/~eir/papers/2015/equalities/equalities-extended.pdf).

- The new mutual recursion between types and coercions means that TypeRep has been renamed TyCoRep. There is also a non-trivial Coercion.hs-boot file.

## Changes to the typechecker



Type-checking types (that is, the functions in TcHsType) can now emit constraints deferred to the solver. This means that every call of these functions must be in a context that can deal with emitted constraints. For types that appear within expressions, this is automatic. For top-level declarations, though, it was necessary to add calls to `captureConstraints`. This is mostly done via the new function `TcSimplify.solveTopConstraints :: TcM a -> TcM (a, Bag EvBind)` which captures constraints and then solves them. The "top" in there is because there are no givens or skolems.



The resulting `EvBind`s then must be dealt with. However, there is often no place to put them. (For example, consider a `data` declaration.) So, I convert the `EvBind`s to a coercion substitution (see `TcEvidence.evBindsSubst` and `TcEvidence.evBindsCvSubstEnv`) and inline the coercions. This operation doesn't work as expected if the `EvTerm`s in the `EvBind`s can't be converted cleanly to `Coercion`s. (This will happen with superclass equalities and with deferred type errors.) Currently, my implementation just fails in this case, often with an unbound variable during CoreLint. We obviously need a better story here, but I'm not quite sure of the best approach.



(I am confident that *some* solution exists here. As a strawman, it is easy to detect when the `EvTerm` --\> `Coercion` conversion fails and we could issue an error. These errors would happen only with superclass equalities and deferred type errors, so they would be predictable and could be reasonably dealt with by programmers. Of course, I'd like to do better, but not having a solution to this particular problem isn't a dire situation.)


- TcCanonical didn't change all that much. It now must canonicalize casts (see `TcCanonical.canEqCast`), but that's straightforward. The biggest wrinkle is that I retain the invariant that canonical equalities are still homogeneous. So, before producing the `CTyEqCan`, we must homogenize the kind. This is done in `TcCanonical.homogeniseRhsKind`, the implementation of which is unsurprising.

- TcFlatten has a bit of a sorry story. It seems the following is a nice invariant to have: a flattened type always has a flattened kind. However, flattening now (even in HEAD) takes roles into account. Because the role of a kind coercion is representation, no matter the role of the type coercion, it only makes sense to say that a flattened type's kind is flattened with respect to *representational* equality.

>
>
> If we have `newtype Age = MkAge Int` and `data Proxy k (a :: k) = P` (where the kind parameter is explicit), flattening `Proxy Age` (w.r.t. nominal equality) gives us `(Proxy Age) |> (axAge -> <*>) :: Int -> *`, which is generally not what we want. See `Note [Kinds when flattening an AppTy]` in TcFlatten. This problem is surmountable, but this wrinkle demands more thought. There are several comments throughout TcFlatten about issues emanating from this one that will need to get fixed.
>
>

- Final zonking (in TcHsSyn) now works with a `CvSubstEnv` extracted from the `EvBind`s. This is so that the zonked types used in `TyCon` definitions have their coercion variables inlined. It wouldn't work just to do zonking as before and then substitute, because we would need to zonk again, and then substitute again, etc. (Plus, we're sometimes in the typechecking knot, when we're forced to do it all in one pass.)

- See `Note [Bidirectional type checking]` in TcHsType. In brief, I now support higher-rank kinds via a simple bidirectional type checking algorithm.

- Dealing with `LHsTyVarBndrs` has become more challenging, because there can be dependency between the variables: we must bring a variable into scope before checking any subsequent variables. See `{kt}cHsTyVarBndrs` and `splitTelescopeTvs` in TcHsType. See also `Note [Typechecking telescopes]`.

- Gobs of code dealing exclusively with kinds has been removed from TcHsType.

- In HEAD, GHC is not as good as it could be keeping proper `TyVar`s out of the typechecker and keeping skolems in. HEAD's kind-level `TyVar`s, in particular, may be encountered in the typechecker. This problem is worse in my branch, meaning that `TyVar`s can be encountered in types and in kinds. My solution is to just allow this to happen, calling `TyVar`s vanilla skolems: `tcTyVarDetails` no longer requires a `TcTyVar`. An alternate solution is to do a better job instantiating, etc.


**SLPJ**.  A project I have in the back of my mind is to separate `TcType` from `Type`.  The former have unification variables, and perhaps other clutter (e.g. evidence bindings perhaps).  The latter do not.  Desugaring woudl convert `TcType` to `Type`.


- `TcMType.quantifyTyCoVars` may deserve special attention. It now uses `quantifyPred` (taken from `decideQuantification`) to figure out what to do with covars. I believe it works as written, but it's a substantial change from before.

- Just like we sometimes have to promote tyvars in `simplifyInfer`, we now have to promote covars. Promoting a covar is simply re-emitting it as a Wanted in a larger context.

- We should discuss `simplifyRule`. I'm still a little mystified about how RULES are typechecked.

- In general, TcSimplify may deserve a little extra attention, as there are a lot of non-trivial and non-obvious changes there.

- `rejigConRes` has become more complicated. As it (at least at one point) had quite a lot of faffing about with coercions, I moved the tricky bits to `Coercion.mkGADTVars`.  See the comments around that function. Once non-dependent equalities are eliminated fully, some aspects of this will get slightly simpler.

## Points of interest


### `TyVar` --\> `TyCoVar`



In many functions and datatypes throughout GHC, I changed names including
`TyVar` to names include `TyCoVar`. These functions/types now may contain
coercion variables as well as type variables. The name change does two
things: it calls my attention to these functions when something changes
during a merge, and the new name reminds me (and, potentially, others
someday) to handle both type variables and coercion variables.



**SLPJ** type/kind variable are erased, but coercion variables are value-level and are not erased.  So a `TyCoVar` is a bit of a mysterious beast.



Similarly, a `TvSubst` without a coercion substitution just doesn't make sense. So, `TvSubst` and `CvSubst` have been combined to `TCvSubst`.



A pervasive effect is that `mkTyVarTy` has been split into `mkOnlyTyVarTy`, which works only on type variables, and `mkTyCoVarTy`, which works on both type and coercion variables. The latter checks what it's given and behaves accordingly.


### All coercion variables are Pi-bound



What is the type of `\ (c :: Int ~# Bool). 5 |> c`? In theory, it could be
`(Int ~# Bool) -> Bool` or `forall (c :: Int ~# Bool). Bool`. I always choose
the latter, to make `exprType` sane. That `forall` should really be spelled `pi`.


### `Binder`



`Type` now has merged `FunTy` and `ForAllTy`. Here is the declaration for the
new `ForAllTy`:


```wiki
  | ForAllTy Binder Type   -- ^ A ? type.
```


with


```wiki
-- | A 'Binder' represents an argument to a function. Binders can be dependent
-- ('Named') or nondependent ('Anon'). They may also be visible or not.
data Binder
  = Named Var VisibilityFlag
  | Anon Type   -- visibility is determined by the type (Constraint vs. *)
```


**SLPJ** So `ForAllTy (Anon ty1) ty2` is `ty1 -> ty2`. Worth saying this!  And presumably it's pretty-printed like that too.



The `Binder` type is meant to be abstract throughout the codebase. The only substantive difference between the combined `ForAllTy` and the separate `FunTy`/`ForAllTy` is that we now store visibility information. This allows use to distinguish between, for example


```wiki
data Proxy1 (a :: k) = P1
```


and


```wiki
data Proxy2 k (a :: k) = P2
```


`Proxy1`'s kind argument is `Invisible` and `Proxy2`'s is `Visible`.  **SLPJ** Don't understand. Is this some new source-Haskell feature?



Currently, any `Named` `ForAllTy`s classifying *terms* are all `Invisible`.



This design change has a number of knock-on effects. In particular, `splitForAllTys` now splits regular functions, too. In some cases, this actually simplified code. In others, the code had to use the new `splitNamedForAllTys`, which is equivalent to the old `splitForAllTys`.



**SLPJ**: Do you really mean `splitNamedForAllTys` here, or do you mean `splitTypeLevelForAlls`?  That is, are you trying to split on *visibilty* or on *erasibility*?  And why?



Another knock-on effect is that `mkForAllTy` now takes a `Binder`. To make this easier for callers, there is a new `mkInvForAllTy :: TyCoVar -> Type -> Type` which makes a `Named`, `Invisible` `Binder` for the `ForAllTy`.



In general, I've been happy with this new design. In some preliminary work toward Pi, `Binder` has added a few more bits, making this redesign even better going forward.



Previously, we've thought about adding visibility information to the anonymous case. I still think this is a good idea. I just haven't done it yet.


### `Coercion` and `TcCoercion`



The impedance mismatch between `Coercion` and `TcCoercion` has become more painful. This is chiefly because `TcType`s and `Type`s are the same, and `Type`s have `Coercion`s inside, not `TcCoercion`s. Recently, we have injected `Coercion` into `TcCoercion`, and this helped my branch considerably. In particular, this means that many algorithms that previously returned a `TcCoercion` now return a `Coercion`, which is more flexible: `Coercion`s can be used in types or can be easily converted to a `TcCoercion`, if required. (In particular, `TcUnify.uType` now returns a `Coercion`. `unifyType`, the exported function, still returns a `TcCoercion`.)



It is also sometimes necessary to convert a `TcCoercion` into a `Coercion`. This happens in the solver, when the witness of an equality constraint must be used in a type. On the surface, this conversion seems harder, but there's a trick that makes it easy: to convert a `TcCoercion` into a `Coercion`, emit a new bound EvVar `c` whose value is the `TcCoercion` in question. Then, your `Coercion` is `CoVarCo c`. Works like a charm. See `TcSMonad.dirtyTcCoToCo`. (I actually don't think this trick is all that dirty anymore. It felt wrong at first.)



**SLPJ** I'm still struggling with the idea that a term (the result of deaugaring a `TcCoercion`) can appear in a type (admittedly via its name).  What if it is bottom?  When is it evaluated?



Due to some eager inlining of coercions, the function `DsBinds.ds_tc_coercion` -- the function that converts a zonked `TcCoercion` to a `Coercion` -- is now `TcEvidence.tcCoercionToCoercion`.



All of this has gotten me thinking: I think we can do away with `TcCoercion`s altogether. The only reason to have `TcCoercion` is to support `TcLetCo`. However, it seems that this can be done with a new `EvLet :: TcEvBinds -> EvTerm -> EvTerm` constructor for `EvTerm`. If a `let` is needed deep within some coercion, just bind a new EvVar to an `EvLet` and use `CoVarCo`. Getting rid of `TcCoercion` would be a vast simplification, unless I'm missing some critical detail.



**SLPJ** But `TcCoercion` represents a lifted equality, whereas `Coercion` represents an unlifted one.



Moreover I don't think you can float out those coercions. What if it looks like


```wiki
forall a. let g = ...a... in ... 
```


where the `forall` is a `TcForAllCo` and the `let` is a `TcLetCo`.  Look at `TcSMonad.deferTcSForAllEq`.


### Lifted vs.\~unlifted equality predicates



Now, both `a ~ b` and `a ~# b` are considered predicates. This means that the solver deals with both lifted and unlifted equality. This is suboptimal, and the plan is to have the solver work only with unlifted equality, defining `class a ~# b => a ~ b` to make lifted equality unmagical. See [this page](dependent-haskell/internal#) for more discussion. Because of the two forms of equality, there are some extra steps in a few places within the typechecker.


### Kind equalities and data constructors



**Universal variables are type variables; existentials might be coercion variables**



A type constructor's type variables are just that: they are sure to be proper
type variables. There doesn't seem to be anything wrong, in theory, with including
coercion variables here, but there also doesn't seem to be a need. However,
a data constructor's *existential* variables might be coercions. Indeed,
this is how all GADTs are currently encoded. For example:


```wiki
data G1 a where
  MkG1 :: Int -> G1 Bool
data G2 (a :: k) where
  MkG2 :: Char -> G2 Double
```


The rejigged types look like this:


```wiki
MkG1 :: forall (a :: *). forall (gadt :: a ~# Bool). Int -> G1 a
MkG2 :: forall (k :: *) (a :: k).
        forall (gadt1 :: k ~# *) (gadt2 :: a |> gadt1 ~# Double).
        Char -> G2 k a
```


Thus, a `TyCon` can have coercion-variable arguments, but only if that
`TyCon` is really a promoted datacon.



**SLPJ** How does promotion work now? What is the kind of `'MkG2`?



**Separation between dependent and non-dependent equalities**



Various bits of code refer to dependent vs. non-dependent equalities. A "dependent
equality" is a coercion that is used in a type; a non-dependent equality is not
used in a type. At one point, I was thinking that a GADT datacon should be careful
to distinguish between dependent equalities and non-dependent ones. That way,
we could defer type errors for non-dependent equalities by using a lifted coercion
instead of an unlifted one there. But, now I think everything should just use
unlifted equality and that we should remove this distinction. Bottom line: don't
worry about this too much.



**GADT coercions are now existential variables**



In accordance with the two points above, all GADT-induced coercions are now considered
existential variables. This causes a little work around datacon signatures, because
a signature includes a separate field for existential variables as it does for GADT
equalities. This could be cleaned up somewhat, now that I've decided that all GADT
equalities really should be existentials.



**SLPJ** Given an example.  GADTs have the `eqSpec` stuff...


### Parsing is just wrong



I've removed the kind parser, in favor of just using the type parser. This is wrong, if only because of the type `*`. See proposed solution [here](dependent-haskell#), under "UPDATE".


### `tryTcS` is now really pure



In HEAD, `tryTcS` claims to "throw away all evidence generated". This isn't quite true. `tryTcS` can still set metavariables and may twiddle `EvBindsVar`s inside of implications. With kind equalities, this won't do. The problem is that solving may invent new coercion variables; these variables may end up in types. If a metavariable is then set to point to a type with a fresh coercion variable in it, we have a problem: after throwing away the evidence, that coercion variable is unbound. (This actually happens in practice.) So, `tryTcS` must be very careful to be properly pure. It does this by maintaining the set of filled-in metavariables *and* a way to roll back any changes to nested `EvBindsVar`s. After the inner `TcS` action is complete, `tryTcS` rolls back the changes.



This works nicely in practice, but one does have to be careful when reading a `-ddump-tc-trace`, because a `writeMetaTyVar` might not be the final word. (If that's an issue, it's easy to fix. The `TcS` monad could know whether it's pure or not and print out accordingly.)



**SLPJ** Ha ha.  `tryTcS` is no longer used. It's dead code.  (It was always only used for defaulting, in very restricted way, and I got rid of that part, but forgot to remove `tryTcS` itself.)


### CUSKs



I have a sinking feeling that a type has a CUSK now only when all types **and kinds** have known types. But I can't come up with an example that shows this clearly. However, we can say that anything to the right of a `::` is known to have type `*`, so this doesn't bite hard in practice. Thus `data T (a :: k)` has a CUSK, but `data S (a :: Proxy k)` does not. Does `data U (a :: Maybe k)`? I think it does, but that's not quite as obvious. What's the easy-to-articulate rule here? (Now, it's this nice rule: a type has a CUSK iff all of its type variables are annotated; if it's a closed type family, the result kind must be annotated, too.)


### Datacon wrappers are now rejigged



In HEAD, a datacon worker differs from a datacon wrapper in two distinct ways: the worker's types are `UNPACK`ed as requested, and the worker's type is rejigged,   la
`rejigConRes`. The wrapper has the datacon's original type.



This design caused endless headaches for me. (Sadly, I can't recall exactly what the problem was -- something to do with applying kind substitutions to variables. I can easily recall going round and round trying to figure out the right datacon design, though!) So, I changed wrappers to have a rejigged type. (Workers are unchanged.) This was actually a nice simplification in several places -- specifically in GADT record update. The only annoying bit is that we have to be careful to print out the right user-facing type, which is implemented in `DataCon.dataConUserType`.



**SLPJ** so `:t K`, where `K` is a GADT data constructor, will show... ah maybe it's ok.  And `:info K` knows to use `dataConUserType`.  Need a `Note` about this. Pattern matching may need care.


### Bad GADT return types cause panic



Writing a bogus GADT return type causes a panic. The problem is that
`checkValidDataCon` is supposed to check if `rejigConRes` was valid. To do
this, `checkValidDataCon` needs the user-specified result type. Normally,
this is retrieved from `dataConOrigResTy`. The problem is that, now, 
the `dataConOrigResTy` is substed by the kind substitution produced in
`rejigConRes`. This is an ugly circular dependency. We could (1) store the
original, unsubsted result ty in the `DataCon` for just this reason, or
(2) install lots of ugly plumbing in TcTyClsDecls to carry the unsubsted
result ty, or (3) do something else. I want your input, as both (1)
and (2) are terrible.



**SLPJ** We'd better discuss this.  It can't be that hard.


### `liftCoSubst`



The lifting operation has become subtler. Happily, it is well described in Section 5 of [
this paper](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf). The actual, implemented, role-aware version of lifting is included in Appendix B of [
this paper](http://www.cis.upenn.edu/~eir/papers/2015/equalities/equalities-extended.pdf).


### New `eqType`



**SLPJ** I'm also worried about the type-level equivalent of `CoreSubst.exprIsConApp_maybe`.  This is a complicated function!  Do we need to do something similar when trying to persuade a type to look like a `TyConApp`?  In general there are lots of functions in `Type` and every one of them must be adjusted to handle casts.  Is it clear how?



Is `Int` the same as `Int |> <*>`? In the formalism: no. This is OK, because we have a coercion form (`CoherenceCo`) that proves `Int ~ (Int |> <*>)`. But, in practice, this is very very annoying. It's tempting to write `eqType` simply to ignore casts... but that would be wrong, because it can equate two types with different kinds. So, the solution is to have an "erased equality check" that compares types ignoring coercions, but to use that check on the types in question *and their kinds*. This is all implemented in `eqType`. The upshot is that two types are equal when they have the same kinds and the types are the same, ignoring coercions. Nice and simple.



There are, of course, wrinkles:


- We wish to avoid ever comparing coercions. So, I removed `eqCoercion` and replaced it with a check looking at a coercion's type. After all, if two proofs prove the same thing, they should be interchangeable. This change includes a vast simplification to `CoercionMap` in TrieMap.


**SLPJ** good idea!  But it should still be called `eqCoercion` shouldn't it?  It just has a better implementation. **RAE** Yes, 
that's entirely unclear above. It still is called `eqCoercion`.


- There is a bizarre wrinkle around unification. We want unification to succeed whenever a unifying substitution exists. Take this example:

```wiki
type family Bool2 where
  Bool2 = Bool

data T :: Bool -> *
```

>
>
> Now, we wish to unify `T True` with `a b`, where `a :: Bool2 -> *` and `b :: Bool2`. A solution exists: `[a |-> T |> (sym axBool2 -> *), b |-> True |> sym axBool2]`. But the substitution requires `axBool2`, which isn't mentioned in the input. Figuring out this kind of unification is beyond the scope of the unifier. (It gets even harder to justify with open type families.)
>
>

>
>
> My solution is to say that `(T |> (axBool2 -> *)) (True |> sym axBool)` is **not** equal to `T True`. When doing the erased equality check, we also check the kind of every application argument. Because the kind of `True |> sym axBool` differs from the kind of `True`, the types above differ. With this change, unification is complete. Note that the issue comes up only with `AppTy`s, never `TyConApp`s, because a `TyCon`'s kind is always closed. If there is a difference in the kind of an argument, that difference must show up earlier in a kind argument. See also `Note [Non-trivial definitional equality]` in TyCoRep.
>
>

- We need a `TypeMap` now to treat all `eqType` types equally. This takes some work, implemented in TrieMap.

- Instance lookup now returns a matching instance along with a coercion witnessing the equality between the found instance and the desired instance. This is because, say, a lookup of `Foo (Int |> co)` should find the instance `Foo Int`. Similarly, unification returns a unifying substitution and a coercion.

### Substitution in the desugarer



Solving may produce top-level unlifted coercions. Of course, we can't have top-level unlifted things. So, the desugarer inlines these as it works. This causes *a lot* of line changes, but it's all very straightforward.



**SLPJ** How do we know they are non-recursive?


### `evBindsCvSubstEnv`



There are several scenarios (in particular, in TcTyClsDecls) where we need to extract a coercion substitution from a `Bag EvBind`. This happens when we don't have a convenient place to bind coercion variables.



**SLPJ** eh?


### Error messages



Now that kind errors in types can be deferred to the solver, all the error-message generating machinery in TcHsType is gone. Instead, I've had to add a lot of ad-hoc processing in TcErrors in an attempt to recreate the errors just as before. (We can debate whether the messages should be reformatted, but I wanted to ensure there was no degradation in the quality of errors.) The changes to TcErrors are mostly terrible, and the whole lot needs refactoring. This state of affairs is somewhat intentional, because I was really unsure what was going to be necessary to get good errors. As I get closer to 0 testsuite failures, the picture is becoming clearer. Soon, I'll be able to figure out a better way to do TcErrors and will refactor. In the meantime, we deal with the mess.



One particular step I had to take is to include extra information in the `TypeEqOrigin` `CtOrigin`. Previously, it had fields for "expected type" and "actual type". It now includes a flag whether the error message should say "type" or "kind", along with the thing that has the actual type. This "thing with actual type" is not used in term-level error message printing, in order to avoid spurious testsuite failures, but it could (and should) be. See `TcRnTypes.CtOrigin`.


### Unboxed tuples are more parameterized



Because an unboxed tuple can contain both boxed bits and unboxed bits, it is necessary to parameterize the type and data constructors over levity variables. For example:


```wiki
(#,,#) :: forall (v1 :: Levity) (v2 :: Levity) (v3 :: Levity)
                 TYPE v1 -> TYPE v2 -> TYPE v3 -> *
```

### Renaming in `LHsTyVarBndrs`



The salient difference between the two fields of `LHsTyVarBndrs` is no longer that one is kinds and one is types, but how the bits are declared. What was `hsq_kvs` is now `hsq_implicit` (for implicitly-declared) and what was `hsq_tvs` is now `hsq_explicit`.



**SLPJ** we could do with nailing down terminology


- implicit vs explicit
- visible vs invisible
- erased vs non-erased


and use it consistently.


### Refactoring in `iface/`



There's a bunch of changes to the `iface` code, but it's all rather boring.


### Fewer optimizations in zonking



There are a few little optimizations in TcHsSyn around zonking. For example, after finding a filled-in metavariable, its contents are zonked and then the variable is re-set to the zonked contents. This is problematic now.



The zonking algorithm in TcHsSyn knot-ties `Id`s. Of course, coercion variables are `Id`s, and so these knot-tied bits can appear in types. We thus must be very careful never, ever to look at a zonked type, which these optimizations do. So, I removed them.



I have not yet re-examined to see if there is a way to restore this behavior. There probably is, as coercion variables can't be recursive!


### Overring visibility assumptions



My limited experience in programming in the enhanced language tells me that we really
need ways to override a visibility specification. That is, we need to allow `_` to have
GHC infer a normally-visible argument, and we need a way of specifying an invisible
argument at call sites. Currently, because there is no override, there is no syntax
for providing a role annotation to an invisible argument. Thus, all invisible arguments
default to having a nominal role, in order to preserve abstraction.



**SLPJ** Don't understand.


### `MaybeNew` is back



In Simon's refactoring in fall 2014, the `MaybeNew` type disappeared from the solver infrastructure. I found this type useful, so I brought it back. It seemed like a better way to structure my algorithm than working without it.


### Lots more "`OrCoVar`" functions in `Id` module



A `CoVar` is now a distinct flavour of an `Id`, with its own `IdDetails`. This is necessary because we often want to see -- quickly -- whether or not a var is a covar. However, there are many places in the code that creates an `Id`, without really knowing if the `Id` should be a plain old `Id` or really a `CoVar`. There are also a bunch of places where we're sure it's really not a `CoVar`. The `OrCoVar` functions allow call sites to distinguish when the `CoVar`-check (done by looking at a var's type) should be made. This is not just an optimization: in one obscure scenario (in the simplifier, if I recall), the type is actually a panic.



This could stand some cleaning up, but it was hard for me to figure out when we're sure an `Id` isn't a `CoVar`.


### No more `instance Eq Type`



Somewhere along the way (I think in wildcard implementation?), an `instance Eq Type` slipped in. I removed it.


### `analyzeType`



Once upon a time, I embarked on a mission to reduce imports of `TyCoRep`, instead aiming to export functions to make exposing `Type`'s innards unnecessary. This effort became `analyzeType` and `mapType`, both in `Type.hs`. `mapType` is a clear win, removing gobs of zonking code and making a relatively clean interface. See simplifications in TcHsSyn and TcMType. It's not clear if `analyzeType` is paying its weight though. I could easily undo this change.


## Tasks


- Fully remove non-dependent GADT equalities.

- Try to restore optimizations in zonking. (Could be after merging)

- Check kind variables when determining whether or not a declaration has a CUSK.

- Sort out the debugger. It's scary, so I've ignored it. Any help/advice appreciated.

- Fix parser.

- Remove `TcCoercion`. (Could be after merging)

- Refactor TcErrors. (Could be after merging)

- Remove lifted equality predicates from the solver.

- Figure out what to do about superclass equalities. (Could be after merging)

- Figure out what to do about deferred kind errors. (Could be after merging)

- Fix flattening. See comments in TcFlatten.

- Fix pattern synonyms.

- Use `pushRefl` when splitting a coercion. Unless we're guaranteed that the input is non-Refl. And then ASSERT.

- Document the new weird type equality (which ignores casts)

## Questions


1. What to do about bad GADT return types

1. Clarify typechecking RULES

1. How to expose levity polymorphism to users

1. Keep `analyzeType`?

1. How to deal with superclass equalities and deferred kind errors

1. What concrete syntax to use for overriding visibility specifications?

## Answers


- Proposal: remove dependent coercion quantification.  

  - That is, not allow `forall (c::a~b). ...(ty |> c)....`.  Instead only allow anonymous quantification, thus `(a~b) => ....`.

  - Another way to say this: coercion variables are only bound by terms, not in types.

  - We do not lose any kind-indexed GADTs, because we have hererogeneous equality.  The prototypical example is

    ```wiki
    data Eq (a::k1) (b::k2) where
      EQ :: forall (c::k). Eq c c

      -- EQ :: forall k1 k2 (a::k1) (b::k2). (a ~ b) => Eq k1 k2 a b
    ```
  - Richard has an exotic example of what is lost.  We could not write this type:

    ```wiki
    foo :: forall k (a::k). (c: F k ~ G k) => Proxy [H1 a |> c, H2 a]
    where
      H1 :: forall k. k -> F k
      H2 :: forall k. k -> G k
    ```

    But you can write this without using dependent coercions:

    ```wiki
    foo :: forall (a::k) (b :: F k). (b ~ H2 a) => Proxy [H1 a, b]
    ```

    But what about

    ```wiki
       forall (c: t1~t2). K c   where   K :: (t1~t2) => *
    ```

- Coercion equalities solved by `TcCoVars`, *not* via the `EvBinds` stuff.  Enables getting rid of `TcLetCo` and hence collapse `Coercion` and `TcCoercion`.  Deferred type errors collected by zonker when zonking coercions.

- Give up on deferred kind errors.

- Nominal roles only in kinds.  Yay.


More minor


- Remove `SubCo` in favour of implicit sub-roling.  Do this in HEAD.
- Simon: make `GivenCt` contain `EvVar` only. In HEAD.
