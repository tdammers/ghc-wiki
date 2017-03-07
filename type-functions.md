# Type Functions, Type Families, and Associated Types in GHC - The Master Plan



This page serves as a collection of notes concerning the implementation of type families (aka type functions) and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.



See the Haskell Wiki for [
user-level documentation](http://haskell.org/haskellwiki/GHC/Indexed_types).


## Status



Detailed information about implemented and unimplemented features as well as bugs and plans for further improvements is at [implementation status](type-functions-status).  The following provides a summary:



Implemented features:


- All basic functionality of open data type families, open type synonym families, and equality constraints has been implemented.
- Type checking is fully integrated with GADTs.
- Type family instances can have ordered groups of equations. See [NewAxioms](new-axioms).


Missing features:


- Superclass equalities.
- Data family instances in GADT form.
- Re-implement functional dependencies using explicit equalities.


Speculative ideas:


- [Total type families.](type-functions/total-families)
- Closed synonym families.
- [Class families.](type-functions/class-families)
- Our type-indexed data types are open.  However, we currently don't allow case expressions mixing constructors from different indexes.  We could do that if we had a story for open function definitions outside of classes.  Class instances of entire data families (including `deriving` clauses at family declarations to derive for all instances) requires the same sort of capabilities as case expressions mixing data constructors from different indexes.  This is, as they require to build a dictionary that applies to all family instances (as opposed to a distinct dictionary per instance, which is what we have now).

## Tickets



Use Keyword = `TypeFamilies` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#4259](http://gitlabghc.nibbler/ghc/ghc/issues/4259)</th>
<td>Relax restrictions on type family instance overlap</td></tr>
<tr><th>[\#5224](http://gitlabghc.nibbler/ghc/ghc/issues/5224)</th>
<td>Improve consistency checking for family instances</td></tr>
<tr><th>[\#7102](http://gitlabghc.nibbler/ghc/ghc/issues/7102)</th>
<td>Type family instance overlap accepted in ghci</td></tr>
<tr><th>[\#7808](http://gitlabghc.nibbler/ghc/ghc/issues/7808)</th>
<td>data families and TH names do not mix well (e.g. cannot use TH deriving)</td></tr>
<tr><th>[\#8095](http://gitlabghc.nibbler/ghc/ghc/issues/8095)</th>
<td>TypeFamilies painfully slow</td></tr>
<tr><th>[\#8109](http://gitlabghc.nibbler/ghc/ghc/issues/8109)</th>
<td>Type family patterns should support as-patterns.</td></tr>
<tr><th>[\#8177](http://gitlabghc.nibbler/ghc/ghc/issues/8177)</th>
<td>Roles for type families</td></tr>
<tr><th>[\#8423](http://gitlabghc.nibbler/ghc/ghc/issues/8423)</th>
<td>Less conservative compatibility check for closed type families</td></tr>
<tr><th>[\#8441](http://gitlabghc.nibbler/ghc/ghc/issues/8441)</th>
<td>Allow family instances in an hs-boot file</td></tr>
<tr><th>[\#9269](http://gitlabghc.nibbler/ghc/ghc/issues/9269)</th>
<td>Type families returning quantified types</td></tr>
<tr><th>[\#9376](http://gitlabghc.nibbler/ghc/ghc/issues/9376)</th>
<td>More informative error messages when closed type families fail to simplify</td></tr>
<tr><th>[\#9394](http://gitlabghc.nibbler/ghc/ghc/issues/9394)</th>
<td>Show data/type family instances with ghci's :info command</td></tr>
<tr><th>[\#9429](http://gitlabghc.nibbler/ghc/ghc/issues/9429)</th>
<td>Alternative to type family Any</td></tr>
<tr><th>[\#9562](http://gitlabghc.nibbler/ghc/ghc/issues/9562)</th>
<td>Type families + hs-boot files = unsafeCoerce</td></tr>
<tr><th>[\#9587](http://gitlabghc.nibbler/ghc/ghc/issues/9587)</th>
<td>Type checking with type functions introduces many type variables, which remain ambiguous. The code no longer type checks.</td></tr>
<tr><th>[\#9607](http://gitlabghc.nibbler/ghc/ghc/issues/9607)</th>
<td>Programs that require AllowAmbiguousTypes in 7.8</td></tr>
<tr><th>[\#9667](http://gitlabghc.nibbler/ghc/ghc/issues/9667)</th>
<td>Type inference is weaker for GADT than analogous Data Family</td></tr>
<tr><th>[\#9780](http://gitlabghc.nibbler/ghc/ghc/issues/9780)</th>
<td>dep\_orphs in Dependencies redundantly records type family orphans</td></tr>
<tr><th>[\#9898](http://gitlabghc.nibbler/ghc/ghc/issues/9898)</th>
<td>Wanted: higher-order type-level programming</td></tr>
<tr><th>[\#9918](http://gitlabghc.nibbler/ghc/ghc/issues/9918)</th>
<td>GHC chooses an instance between two overlapping, but cannot resolve a clause within the similar closed type family</td></tr>
<tr><th>[\#10116](http://gitlabghc.nibbler/ghc/ghc/issues/10116)</th>
<td>Closed type families: Warn if it doesn't handle all cases</td></tr>
<tr><th>[\#10141](http://gitlabghc.nibbler/ghc/ghc/issues/10141)</th>
<td>CUSK mysteries</td></tr>
<tr><th>[\#10204](http://gitlabghc.nibbler/ghc/ghc/issues/10204)</th>
<td>Odd interaction between rank-2 types and type families</td></tr>
<tr><th>[\#10327](http://gitlabghc.nibbler/ghc/ghc/issues/10327)</th>
<td>Devise workaround for how infinite types prevent closed type family reduction</td></tr>
<tr><th>[\#10482](http://gitlabghc.nibbler/ghc/ghc/issues/10482)</th>
<td>Not enough unboxing happens on data-family function argument</td></tr>
<tr><th>[\#10789](http://gitlabghc.nibbler/ghc/ghc/issues/10789)</th>
<td>Notify user when a kind mismatch holds up a type family reduction</td></tr>
<tr><th>[\#10808](http://gitlabghc.nibbler/ghc/ghc/issues/10808)</th>
<td>Odd interaction between record update and type families</td></tr>
<tr><th>[\#10832](http://gitlabghc.nibbler/ghc/ghc/issues/10832)</th>
<td>Generalize injective type families</td></tr>
<tr><th>[\#10833](http://gitlabghc.nibbler/ghc/ghc/issues/10833)</th>
<td>Use injective type families (decomposition) when dealing with givens</td></tr>
<tr><th>[\#10996](http://gitlabghc.nibbler/ghc/ghc/issues/10996)</th>
<td>family is treated as keyword in types even without TypeFamilies enabled</td></tr>
<tr><th>[\#11070](http://gitlabghc.nibbler/ghc/ghc/issues/11070)</th>
<td>Type-level arithmetic of sized-types has weaker inference power than in 7.8</td></tr>
<tr><th>[\#11084](http://gitlabghc.nibbler/ghc/ghc/issues/11084)</th>
<td>Some type families don't reduce with :kind!</td></tr>
<tr><th>[\#11113](http://gitlabghc.nibbler/ghc/ghc/issues/11113)</th>
<td>Type family If is too strict</td></tr>
<tr><th>[\#11207](http://gitlabghc.nibbler/ghc/ghc/issues/11207)</th>
<td>GHC cannot infer injectivity on type family operating on GHC.TypeLits' Nat, but can for equivalent type family operating on user-defined Nat kind</td></tr>
<tr><th>[\#11243](http://gitlabghc.nibbler/ghc/ghc/issues/11243)</th>
<td>Flag to not expand type families</td></tr>
<tr><th>[\#11310](http://gitlabghc.nibbler/ghc/ghc/issues/11310)</th>
<td>Surprising accepted constructor for GADT instance of data family</td></tr>
<tr><th>[\#11424](http://gitlabghc.nibbler/ghc/ghc/issues/11424)</th>
<td>"Occurs check" not considered when reducing closed type families</td></tr>
<tr><th>[\#11511](http://gitlabghc.nibbler/ghc/ghc/issues/11511)</th>
<td>Type family producing infinite type accepted as injective</td></tr>
<tr><th>[\#11534](http://gitlabghc.nibbler/ghc/ghc/issues/11534)</th>
<td>Allow class associated types to reference functional dependencies</td></tr>
<tr><th>[\#12551](http://gitlabghc.nibbler/ghc/ghc/issues/12551)</th>
<td>Make type indices take local constraints into account in type instance declaration</td></tr>
<tr><th>[\#12564](http://gitlabghc.nibbler/ghc/ghc/issues/12564)</th>
<td>Type family in type pattern kind</td></tr>
<tr><th>[\#13192](http://gitlabghc.nibbler/ghc/ghc/issues/13192)</th>
<td>Ambiguity Caused By PolyKind and Not Helpful Error Messages</td></tr>
<tr><th>[\#13251](http://gitlabghc.nibbler/ghc/ghc/issues/13251)</th>
<td>Must perform family consistency check on non-imported identifiers</td></tr>
<tr><th>[\#13386](http://gitlabghc.nibbler/ghc/ghc/issues/13386)</th>
<td>Poor compiler performance with type families</td></tr>
<tr><th>[\#13571](http://gitlabghc.nibbler/ghc/ghc/issues/13571)</th>
<td>Injective type family syntax accepted without TypeFamilyDependencies</td></tr>
<tr><th>[\#13971](http://gitlabghc.nibbler/ghc/ghc/issues/13971)</th>
<td>Misleading "Kind mis-match on LHS of default declaration" error</td></tr>
<tr><th>[\#14040](http://gitlabghc.nibbler/ghc/ghc/issues/14040)</th>
<td>Typed holes regression in GHC 8.0.2: No skolem info: z\_a1sY\[sk:2\]</td></tr>
<tr><th>[\#14111](http://gitlabghc.nibbler/ghc/ghc/issues/14111)</th>
<td>strange error when using data families with levity polymorphism and unboxed sums and data families</td></tr>
<tr><th>[\#14279](http://gitlabghc.nibbler/ghc/ghc/issues/14279)</th>
<td>Type families interfere with specialisation rewrite rules</td></tr>
<tr><th>[\#14319](http://gitlabghc.nibbler/ghc/ghc/issues/14319)</th>
<td>Stuck type families can lead to lousy error messages</td></tr>
<tr><th>[\#14420](http://gitlabghc.nibbler/ghc/ghc/issues/14420)</th>
<td>Data families should not instantiate to non-Type kinds</td></tr>
<tr><th>[\#14645](http://gitlabghc.nibbler/ghc/ghc/issues/14645)</th>
<td>Allow type family in data family return kind</td></tr>
<tr><th>[\#15546](http://gitlabghc.nibbler/ghc/ghc/issues/15546)</th>
<td>Display coaxiom branch incompatibilities in GHCi</td></tr>
<tr><th>[\#15549](http://gitlabghc.nibbler/ghc/ghc/issues/15549)</th>
<td>Core Lint error with EmptyCase</td></tr>
<tr><th>[\#15557](http://gitlabghc.nibbler/ghc/ghc/issues/15557)</th>
<td>Reduce type families in equations' RHS when testing equation compatibility</td></tr>
<tr><th>[\#15561](http://gitlabghc.nibbler/ghc/ghc/issues/15561)</th>
<td>TypeInType: Type error conditioned on ordering of GADT and type family definitions</td></tr>
<tr><th>[\#15621](http://gitlabghc.nibbler/ghc/ghc/issues/15621)</th>
<td>Error message involving type families points to wrong location</td></tr>
<tr><th>[\#15683](http://gitlabghc.nibbler/ghc/ghc/issues/15683)</th>
<td>coerce fails for Coercible type families</td></tr>
<tr><th>[\#15869](http://gitlabghc.nibbler/ghc/ghc/issues/15869)</th>
<td>Discrepancy between seemingly equivalent type synonym and type family</td></tr>
<tr><th>[\#15905](http://gitlabghc.nibbler/ghc/ghc/issues/15905)</th>
<td>Data familes should end in Type</td></tr>
<tr><th>[\#16070](http://gitlabghc.nibbler/ghc/ghc/issues/16070)</th>
<td>Better inferred signatures</td></tr>
<tr><th>[\#16081](http://gitlabghc.nibbler/ghc/ghc/issues/16081)</th>
<td>Clean up family instance consistency checking</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#2721](http://gitlabghc.nibbler/ghc/ghc/issues/2721)</th>
<td>Newtype deriving doesn't work with type families</td></tr>
<tr><th>[\#3990](http://gitlabghc.nibbler/ghc/ghc/issues/3990)</th>
<td>UNPACK doesn't unbox data families</td></tr>
<tr><th>[\#5633](http://gitlabghc.nibbler/ghc/ghc/issues/5633)</th>
<td>TypeFamilies don't seem to play with LIberalTypeSynonyms</td></tr>
<tr><th>[\#6018](http://gitlabghc.nibbler/ghc/ghc/issues/6018)</th>
<td>Injective type families</td></tr>
<tr><th>[\#6035](http://gitlabghc.nibbler/ghc/ghc/issues/6035)</th>
<td>Kind-indexed type family failure with polymorphic kinds</td></tr>
<tr><th>[\#6044](http://gitlabghc.nibbler/ghc/ghc/issues/6044)</th>
<td>Regression error: Kind variables don't work inside of kind constructors in type families</td></tr>
<tr><th>[\#7005](http://gitlabghc.nibbler/ghc/ghc/issues/7005)</th>
<td>GHC 7.4.2 crashes with a panic when using type families and data kinds together</td></tr>
<tr><th>[\#7073](http://gitlabghc.nibbler/ghc/ghc/issues/7073)</th>
<td>Kind variable problem when declaring associated type family</td></tr>
<tr><th>[\#7156](http://gitlabghc.nibbler/ghc/ghc/issues/7156)</th>
<td>"Pattern match on GADT" error for non-GADT</td></tr>
<tr><th>[\#7176](http://gitlabghc.nibbler/ghc/ghc/issues/7176)</th>
<td>Failure to let kind variable remain uninstantiated when not needed</td></tr>
<tr><th>[\#7288](http://gitlabghc.nibbler/ghc/ghc/issues/7288)</th>
<td>type inference fails with where clause (RankNTypes, TypeFamilies)</td></tr>
<tr><th>[\#7477](http://gitlabghc.nibbler/ghc/ghc/issues/7477)</th>
<td>reifyInstances can't deal with polykinded type families</td></tr>
<tr><th>[\#7560](http://gitlabghc.nibbler/ghc/ghc/issues/7560)</th>
<td>Panic in conflictInstErr when branched type family instances conflict</td></tr>
<tr><th>[\#7585](http://gitlabghc.nibbler/ghc/ghc/issues/7585)</th>
<td>Core lint failure when optimizing coercions in branched axioms</td></tr>
<tr><th>[\#8165](http://gitlabghc.nibbler/ghc/ghc/issues/8165)</th>
<td>Use GeneralizedNewtypeDeriving to automatically create associated type families</td></tr>
<tr><th>[\#8913](http://gitlabghc.nibbler/ghc/ghc/issues/8913)</th>
<td>either bug or confusing error message mixing PolyKinds and TypeFamilies</td></tr>
<tr><th>[\#9582](http://gitlabghc.nibbler/ghc/ghc/issues/9582)</th>
<td>Associated Type Synonyms do not unfold in InstanceSigs</td></tr>
<tr><th>[\#9747](http://gitlabghc.nibbler/ghc/ghc/issues/9747)</th>
<td>Odd failure to deduce a constraint</td></tr>
<tr><th>[\#11062](http://gitlabghc.nibbler/ghc/ghc/issues/11062)</th>
<td>Type families + hs-boot files = panic (type family consistency check too early)</td></tr>
<tr><th>[\#11282](http://gitlabghc.nibbler/ghc/ghc/issues/11282)</th>
<td>Error warns about non-injectivity of injective type family</td></tr>
<tr><th>[\#11348](http://gitlabghc.nibbler/ghc/ghc/issues/11348)</th>
<td>Local open type families instances ignored during type checking</td></tr>
<tr><th>[\#11357](http://gitlabghc.nibbler/ghc/ghc/issues/11357)</th>
<td>Regression when deriving Generic1 on poly-kinded data family</td></tr>
<tr><th>[\#11375](http://gitlabghc.nibbler/ghc/ghc/issues/11375)</th>
<td>Type aliases twice as slow to compile as closed type families.</td></tr>
<tr><th>[\#11381](http://gitlabghc.nibbler/ghc/ghc/issues/11381)</th>
<td>Put injective type families in a separate language extension</td></tr>
<tr><th>[\#11400](http://gitlabghc.nibbler/ghc/ghc/issues/11400)</th>
<td>\* is not an indexed type family</td></tr>
<tr><th>[\#11407](http://gitlabghc.nibbler/ghc/ghc/issues/11407)</th>
<td>-XTypeInType uses up all memory when used in data family instance</td></tr>
<tr><th>[\#11708](http://gitlabghc.nibbler/ghc/ghc/issues/11708)</th>
<td>Typechecker hangs when checking type families with -ddump-tc-trace turned on</td></tr>
<tr><th>[\#12057](http://gitlabghc.nibbler/ghc/ghc/issues/12057)</th>
<td>TypeFamilyDependencies on   Data.Type.Bool's \`Not\`</td></tr>
<tr><th>[\#12089](http://gitlabghc.nibbler/ghc/ghc/issues/12089)</th>
<td>:kind command allows unsaturated type family,</td></tr>
<tr><th>[\#12104](http://gitlabghc.nibbler/ghc/ghc/issues/12104)</th>
<td>Type families, \`TypeError\`, and \`-fdefer-type-errors\` cause "opt\_univ fell into a hole"</td></tr>
<tr><th>[\#12119](http://gitlabghc.nibbler/ghc/ghc/issues/12119)</th>
<td>Can't create injective type family equation with TypeError as the RHS</td></tr>
<tr><th>[\#12199](http://gitlabghc.nibbler/ghc/ghc/issues/12199)</th>
<td>GHC is oblivious to injectivity when solving an equality constraint</td></tr>
<tr><th>[\#12239](http://gitlabghc.nibbler/ghc/ghc/issues/12239)</th>
<td>Dependent type family does not reduce</td></tr>
<tr><th>[\#12638](http://gitlabghc.nibbler/ghc/ghc/issues/12638)</th>
<td>GHC panic when resolving Show instance</td></tr>
<tr><th>[\#13025](http://gitlabghc.nibbler/ghc/ghc/issues/13025)</th>
<td>Type family reduction irregularity (change from 7.10.3 to 8.0.1)</td></tr>
<tr><th>[\#13398](http://gitlabghc.nibbler/ghc/ghc/issues/13398)</th>
<td>Associated type family instance validity checking is too conservative</td></tr>
<tr><th>[\#13420](http://gitlabghc.nibbler/ghc/ghc/issues/13420)</th>
<td>Bizarre pretty-printing of closed type families in GHCi</td></tr>
<tr><th>[\#13774](http://gitlabghc.nibbler/ghc/ghc/issues/13774)</th>
<td>Singletons code fails to typecheck when type signature involving type family is added</td></tr>
<tr><th>[\#13790](http://gitlabghc.nibbler/ghc/ghc/issues/13790)</th>
<td>GHC doesn't reduce type family in kind signature unless its arm is twisted</td></tr>
<tr><th>[\#13809](http://gitlabghc.nibbler/ghc/ghc/issues/13809)</th>
<td>TH-reified type family and data family instances have a paucity of kinds</td></tr>
<tr><th>[\#13901](http://gitlabghc.nibbler/ghc/ghc/issues/13901)</th>
<td>Lift the "Illegal polymorphic type" restriction on type families</td></tr>
<tr><th>[\#13913](http://gitlabghc.nibbler/ghc/ghc/issues/13913)</th>
<td>Can't apply higher-ranked kind in type family</td></tr>
<tr><th>[\#13915](http://gitlabghc.nibbler/ghc/ghc/issues/13915)</th>
<td>GHC 8.2 regression: "Can't find interface-file declaration" for promoted data family instance</td></tr>
<tr><th>[\#13962](http://gitlabghc.nibbler/ghc/ghc/issues/13962)</th>
<td>GHCi allows unsaturated type family</td></tr>
<tr><th>[\#13972](http://gitlabghc.nibbler/ghc/ghc/issues/13972)</th>
<td>GHC 8.2 error message around indexes for associated type instances is baffling</td></tr>
<tr><th>[\#13985](http://gitlabghc.nibbler/ghc/ghc/issues/13985)</th>
<td>GHC 8.0 regression: ‘k’ is not in scope during type checking, but it passed the renamer</td></tr>
<tr><th>[\#14000](http://gitlabghc.nibbler/ghc/ghc/issues/14000)</th>
<td>Out of scope errors with type families do not mention scope</td></tr>
<tr><th>[\#14042](http://gitlabghc.nibbler/ghc/ghc/issues/14042)</th>
<td>Datatypes cannot use a type family in their return kind</td></tr>
<tr><th>[\#14045](http://gitlabghc.nibbler/ghc/ghc/issues/14045)</th>
<td>Data family instances must list all patterns of family, despite documentation's claims to the contrary</td></tr>
<tr><th>[\#14131](http://gitlabghc.nibbler/ghc/ghc/issues/14131)</th>
<td>Difference between newtype and newtype instance</td></tr>
<tr><th>[\#14179](http://gitlabghc.nibbler/ghc/ghc/issues/14179)</th>
<td>"Conflicting family instance" error pretty prints data family instances poorly</td></tr>
<tr><th>[\#14203](http://gitlabghc.nibbler/ghc/ghc/issues/14203)</th>
<td>GHC-inferred type signature doesn't actually typecheck</td></tr>
<tr><th>[\#14333](http://gitlabghc.nibbler/ghc/ghc/issues/14333)</th>
<td>GHC doesn't use the fact that Coercible is symmetric</td></tr>
<tr><th>[\#14366](http://gitlabghc.nibbler/ghc/ghc/issues/14366)</th>
<td>Type family equation refuses to unify wildcard type patterns</td></tr>
<tr><th>[\#14440](http://gitlabghc.nibbler/ghc/ghc/issues/14440)</th>
<td>Duplicate type family instances are permitted</td></tr>
<tr><th>[\#14441](http://gitlabghc.nibbler/ghc/ghc/issues/14441)</th>
<td>GHC HEAD regression involving type families in kinds</td></tr>
<tr><th>[\#14462](http://gitlabghc.nibbler/ghc/ghc/issues/14462)</th>
<td>deriving on associated data types fails to find constraints</td></tr>
<tr><th>[\#14547](http://gitlabghc.nibbler/ghc/ghc/issues/14547)</th>
<td>Wrong warning by -Wincomplete-patterns</td></tr>
<tr><th>[\#14661](http://gitlabghc.nibbler/ghc/ghc/issues/14661)</th>
<td>Cannot derive (newtype I a b = I (F a -\> F b) deriving newtype Category) for type family F</td></tr>
<tr><th>[\#14728](http://gitlabghc.nibbler/ghc/ghc/issues/14728)</th>
<td>Is (GeneralizedNewtypeDeriving + associated type classes) completely bogus?</td></tr>
<tr><th>[\#14938](http://gitlabghc.nibbler/ghc/ghc/issues/14938)</th>
<td>Pattern matching on GADT does not refine type family parameters</td></tr>
<tr><th>[\#14991](http://gitlabghc.nibbler/ghc/ghc/issues/14991)</th>
<td>GHC HEAD regression involving TYPEs in type families</td></tr>
<tr><th>[\#15057](http://gitlabghc.nibbler/ghc/ghc/issues/15057)</th>
<td>Lint types created by newFamInst</td></tr>
<tr><th>[\#15142](http://gitlabghc.nibbler/ghc/ghc/issues/15142)</th>
<td>GHC HEAD regression: tcTyVarDetails</td></tr>
<tr><th>[\#15144](http://gitlabghc.nibbler/ghc/ghc/issues/15144)</th>
<td>Type inference regression between GHC 8.0.2 and 8.2.2</td></tr>
<tr><th>[\#15318](http://gitlabghc.nibbler/ghc/ghc/issues/15318)</th>
<td>Core Lint error involving newtype family instances with wrappers</td></tr>
<tr><th>[\#15341](http://gitlabghc.nibbler/ghc/ghc/issues/15341)</th>
<td>:info prints kinds in closed type family equations without enabling -fprint-explicit-kinds</td></tr>
<tr><th>[\#15380](http://gitlabghc.nibbler/ghc/ghc/issues/15380)</th>
<td>Infinite typechecker loop in GHC 8.6</td></tr>
<tr><th>[\#15428](http://gitlabghc.nibbler/ghc/ghc/issues/15428)</th>
<td>Oversaturated type family application panicks GHC (piResultTys2)</td></tr>
<tr><th>[\#15515](http://gitlabghc.nibbler/ghc/ghc/issues/15515)</th>
<td>Bogus "No instance" error when type families appear in kinds</td></tr>
<tr><th>[\#15568](http://gitlabghc.nibbler/ghc/ghc/issues/15568)</th>
<td>Kind variables in type family aren't quantified in toposorted order</td></tr>
<tr><th>[\#15591](http://gitlabghc.nibbler/ghc/ghc/issues/15591)</th>
<td>Inconsistent kind variable binder visibility between associated and non-associated type families</td></tr>
<tr><th>[\#15592](http://gitlabghc.nibbler/ghc/ghc/issues/15592)</th>
<td>Type families without CUSKs cannot be given visible kind variable binders</td></tr>
<tr><th>[\#15691](http://gitlabghc.nibbler/ghc/ghc/issues/15691)</th>
<td>Marking Pred(S n) = n as injective</td></tr>
<tr><th>[\#15704](http://gitlabghc.nibbler/ghc/ghc/issues/15704)</th>
<td>Different saturations of the same polymorphic-kinded type constructor aren't seen as apart types</td></tr>
<tr><th>[\#15740](http://gitlabghc.nibbler/ghc/ghc/issues/15740)</th>
<td>Type family with higher-rank result is too accepting</td></tr>
<tr><th>[\#15796](http://gitlabghc.nibbler/ghc/ghc/issues/15796)</th>
<td>Core Lint error with invalid newtype declaration</td></tr>
<tr><th>[\#15827](http://gitlabghc.nibbler/ghc/ghc/issues/15827)</th>
<td>Explicit foralls in type family equations are pretty-printed inconsistently (and strangely, at times)</td></tr>
<tr><th>[\#15828](http://gitlabghc.nibbler/ghc/ghc/issues/15828)</th>
<td>Type family equation foralls allow strange re-quantification of class-bound type variables</td></tr>
<tr><th>[\#15852](http://gitlabghc.nibbler/ghc/ghc/issues/15852)</th>
<td>Bad axiom produced for polykinded data family</td></tr>
<tr><th>[\#15986](http://gitlabghc.nibbler/ghc/ghc/issues/15986)</th>
<td>Poor error message source location reporting with unsaturated type family</td></tr>
<tr><th>[\#16002](http://gitlabghc.nibbler/ghc/ghc/issues/16002)</th>
<td>Type family equation with wrong name is silently accepted (GHC 8.6+ only)</td></tr>
<tr><th>[\#16008](http://gitlabghc.nibbler/ghc/ghc/issues/16008)</th>
<td>GHC HEAD type family regression involving invisible arguments</td></tr></table>



## Terminology



**Data-type family**: a data type declared with a `data family` declaration.



**Type-synonym family**, or **type function**: a type synonym declared with a `type family` declaration.



**Type family**: a data-type family or type-synonym family.



**Parametric type constructors**: the type constructor of a vanilla Haskell type.



**Family type constructor** or **Family `TyCon`**: the type constructor for a type family.



**Instance `TyCon`**: the `TyCon` arising from a `data/newtype/type instance` declaration.  Sometimes called the **representation `TyCon`**.  The instance `TyCon` is invisible to the programmer; it is only used internally inside GHC.  



**Associated type**: A type family that is declared in a type class.



**Kind signature**: Declaration of the name, kind, and arity of an indexed type constructor.  The *arity* is the number of type indexes - *not* the overall number of parameters - of an indexed type constructor.



**Definitions vs. declarations**: We sometimes call the kind signature of an indexed constructor its *declaration* and the subsequent population of the type family by type equations or indexed data/newtype declarations the constructor's *definition*.



Note: we previously used the term "indexed type", but have now switched to using "type family".  Please change any  uses of the former into the latter as you come across them.


## How It Works



The details of the implementation are split over a couple of subpages, due to the amount of the material:


- [syntax and representation,](type-functions-syntax)
- [renaming,](type-functions-renaming)
- [type checking,](type-functions-type-checking)
- [desugaring,](type-functions-core) and
- [interfaces.](type-functions-iface)


Furthermore, we have


- [details on the normalisation and solving of type equalities](type-functions-solving) and
- [integrating class and equality constraint solving.](type-functions/integrated-solver)

## References


- [
  Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. In Proceedings of ICFP 2008 : The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 51-62, 2008.
- [
  Associated Types with Class.](http://www.cse.unsw.edu.au/~chak/papers/CKPM05.html) Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, and Simon Marlow. In Proceedings of The 32nd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL'05), ACM Press, pages 1-13, 2005.
- [
  Associated Type Synonyms.](http://www.cse.unsw.edu.au/~chak/papers/CKP05.html) Manuel M. T. Chakravarty, Gabriele Keller, and Simon Peyton Jones. In Proceedings of The Tenth ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 241-253, 2005.
- [
  Towards Open Type Functions for Haskell.](http://www.cse.unsw.edu.au/~chak/papers/SSPC07.html) Tom Schrijvers, Martin Sulzmann, Simon Peyton-Jones, and Manuel M. T. Chakravarty. Presented at IFL 2007.
- [
  Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. ICFP 2008: The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, 2008.
- Old and outdated wiki material on [type checking with indexed synonyms.](type-functions-syn-tc)
