# Quantified constraints



This wiki page summarises the state of play on the idea of allowing quantification in class constraints.  For example


```wiki
data Rose f a = Branch a (f (Rose f a))

instance (Eq a, forall b. (Eq b) => Eq (f b))
       => Eq (Rose f a)
  where ...
```


The new bit is the `forall` in the context of the instance declaration. This is allowed in GHC 8.6 and later using the `QuantifiedConstraints` extension.



Here are some resources


- The [
  GHC Proposal discussing quantified constraints](https://github.com/ghc-proposals/ghc-proposals/pull/109)

- [
  Derivable type classes](https://www.microsoft.com/en-us/research/publication/derivable-type-classes), Section 7, where the idea was first proposed (I think).

- [\#2893](http://gitlabghc.nibbler/ghc/ghc/issues/2893), a ticket about the idea

- [
  Quantified class constraints](http://i.cs.hku.hk/~bruno//papers/hs2017.pdf), a Haskell 2017 paper that works out the idea in some detail, and a [
  Reddit thread](https://www.reddit.com/r/haskell/comments/6me3sv/quantified_class_constraints_pdf/) about it.

- [
  An old haskell.org Wiki page about it](http://haskell.org/haskellwiki/Quantified_contexts)
- [
  A Libraries thread (Dec 18)](https://mail.haskell.org/pipermail/libraries/2017-December/028377.html).

## Status



Use Keyword = `QuantifiedConstraints` to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th>[\#8516](http://gitlabghc.nibbler/ghc/ghc/issues/8516)</th>
<td>Add (-\>) representation and the Invariant class to GHC.Generics</td></tr>
<tr><th>[\#13153](http://gitlabghc.nibbler/ghc/ghc/issues/13153)</th>
<td>Several Traversable instances have an extra fmap</td></tr>
<tr><th>[\#14317](http://gitlabghc.nibbler/ghc/ghc/issues/14317)</th>
<td>Solve Coercible constraints over type constructors</td></tr>
<tr><th>[\#14831](http://gitlabghc.nibbler/ghc/ghc/issues/14831)</th>
<td>QuantifiedConstraints: Odd superclass constraint</td></tr>
<tr><th>[\#14832](http://gitlabghc.nibbler/ghc/ghc/issues/14832)</th>
<td>QuantifiedConstraints: Adding to the context causes failure</td></tr>
<tr><th>[\#14860](http://gitlabghc.nibbler/ghc/ghc/issues/14860)</th>
<td>QuantifiedConstraints: Can't quantify constraint involving type family</td></tr>
<tr><th>[\#14877](http://gitlabghc.nibbler/ghc/ghc/issues/14877)</th>
<td>QuantifiedConstraints: Can't deduce \`xx' from \`(xx =\> a, xx)'</td></tr>
<tr><th>[\#14879](http://gitlabghc.nibbler/ghc/ghc/issues/14879)</th>
<td>QuantifiedConstraints: Big error message + can't substitute (=\>) with a class alias</td></tr>
<tr><th>[\#14896](http://gitlabghc.nibbler/ghc/ghc/issues/14896)</th>
<td>QuantifiedConstraints: GHC does doesn't discharge constraints if they are quantified</td></tr>
<tr><th>[\#14937](http://gitlabghc.nibbler/ghc/ghc/issues/14937)</th>
<td>QuantifiedConstraints: Reify implication constraints from terms lacking them</td></tr>
<tr><th>[\#14943](http://gitlabghc.nibbler/ghc/ghc/issues/14943)</th>
<td>Make (=\>) polykinded (:: k -\> k -\> Constraint)</td></tr>
<tr><th>[\#14958](http://gitlabghc.nibbler/ghc/ghc/issues/14958)</th>
<td>QuantifiedConstraints: Doesn't apply implication for existential?</td></tr>
<tr><th>[\#14968](http://gitlabghc.nibbler/ghc/ghc/issues/14968)</th>
<td>QuantifiedConstraints: Can't be RHS of type family instances</td></tr>
<tr><th>[\#14983](http://gitlabghc.nibbler/ghc/ghc/issues/14983)</th>
<td>Have custom type errors imply Void</td></tr>
<tr><th>[\#14995](http://gitlabghc.nibbler/ghc/ghc/issues/14995)</th>
<td>QuantifiedConstraints: Incorrect pretty printing</td></tr>
<tr><th>[\#15347](http://gitlabghc.nibbler/ghc/ghc/issues/15347)</th>
<td>QuantifiedConstraints: Implication constraints with type families don't work</td></tr>
<tr><th>[\#15351](http://gitlabghc.nibbler/ghc/ghc/issues/15351)</th>
<td>QuantifiedConstraints ignore FunctionalDependencies</td></tr>
<tr><th>[\#15409](http://gitlabghc.nibbler/ghc/ghc/issues/15409)</th>
<td>Quantified constraints half-work with equality constraints</td></tr>
<tr><th>[\#15593](http://gitlabghc.nibbler/ghc/ghc/issues/15593)</th>
<td>QuantifiedConstraints: trouble with type family</td></tr>
<tr><th>[\#15639](http://gitlabghc.nibbler/ghc/ghc/issues/15639)</th>
<td>Surprising failure combining QuantifiedConstraints with Coercible</td></tr>
<tr><th>[\#15888](http://gitlabghc.nibbler/ghc/ghc/issues/15888)</th>
<td>Quantified constraints can be loopy</td></tr>
<tr><th>[\#15918](http://gitlabghc.nibbler/ghc/ghc/issues/15918)</th>
<td>GHC panic from QuantifiedConstraints(?)</td></tr>
<tr><th>[\#16139](http://gitlabghc.nibbler/ghc/ghc/issues/16139)</th>
<td>GHC confused about type synonym kind with QuantifiedConstraints</td></tr>
<tr><th>[\#16140](http://gitlabghc.nibbler/ghc/ghc/issues/16140)</th>
<td>Cannot create type synonym for quantified constraint without ImpredicativeTypes</td></tr>
<tr><th>[\#16173](http://gitlabghc.nibbler/ghc/ghc/issues/16173)</th>
<td>Move \`Data.Profunctor\` from \`profunctors\` package to \`base\`</td></tr>
<tr><th>[\#16245](http://gitlabghc.nibbler/ghc/ghc/issues/16245)</th>
<td>GHC panic (No skolem info) with QuantifiedConstraints and strange scoping</td></tr></table>




Closed Tickets:

<table><tr><th>[\#2256](http://gitlabghc.nibbler/ghc/ghc/issues/2256)</th>
<td>Incompleteness of type inference: must quantify over implication constraints</td></tr>
<tr><th>[\#2893](http://gitlabghc.nibbler/ghc/ghc/issues/2893)</th>
<td>Implement "Quantified constraints" proposal</td></tr>
<tr><th>[\#5927](http://gitlabghc.nibbler/ghc/ghc/issues/5927)</th>
<td>A type-level "implies" constraint on Constraints</td></tr>
<tr><th>[\#9123](http://gitlabghc.nibbler/ghc/ghc/issues/9123)</th>
<td>Emit quantified Coercible constraints in GeneralizedNewtypeDeriving</td></tr>
<tr><th>[\#12245](http://gitlabghc.nibbler/ghc/ghc/issues/12245)</th>
<td>Deriving Data at higher kinds</td></tr>
<tr><th>[\#14070](http://gitlabghc.nibbler/ghc/ghc/issues/14070)</th>
<td>Allow ‘unsafe’ deriving strategy, deriving code with ‘unsafeCoerce’</td></tr>
<tr><th>[\#14733](http://gitlabghc.nibbler/ghc/ghc/issues/14733)</th>
<td>Won't use (forall xx. f xx) with -XQuantifiedConstraints</td></tr>
<tr><th>[\#14734](http://gitlabghc.nibbler/ghc/ghc/issues/14734)</th>
<td>QuantifiedConstraints conflated with impredicative polymorphism?</td></tr>
<tr><th>[\#14735](http://gitlabghc.nibbler/ghc/ghc/issues/14735)</th>
<td>GHC Panic with QuantifiedConstraints</td></tr>
<tr><th>[\#14744](http://gitlabghc.nibbler/ghc/ghc/issues/14744)</th>
<td>Non-exhaustive patterns in case in GHCi with quantified class contexts</td></tr>
<tr><th>[\#14748](http://gitlabghc.nibbler/ghc/ghc/issues/14748)</th>
<td>Infer context for Data instance of (data Foo f = Foo (f Bool) (f Int))</td></tr>
<tr><th>[\#14799](http://gitlabghc.nibbler/ghc/ghc/issues/14799)</th>
<td>QuantifiedConstraints: Problems with Typeable</td></tr>
<tr><th>[\#14822](http://gitlabghc.nibbler/ghc/ghc/issues/14822)</th>
<td>-XQuantifiedConstraints: Turn term-level entailments (:-) into constraints (=\>)</td></tr>
<tr><th>[\#14833](http://gitlabghc.nibbler/ghc/ghc/issues/14833)</th>
<td>QuantifiedConstraints: GHC can't deduce (() :: Constraint)?</td></tr>
<tr><th>[\#14835](http://gitlabghc.nibbler/ghc/ghc/issues/14835)</th>
<td>QuantifiedConstraints: Can't deduce "(a, b)" from "a" and "b"</td></tr>
<tr><th>[\#14840](http://gitlabghc.nibbler/ghc/ghc/issues/14840)</th>
<td>QuantifiedConstraints: Can't define class alias</td></tr>
<tr><th>[\#14861](http://gitlabghc.nibbler/ghc/ghc/issues/14861)</th>
<td>QuantifiedConstraints: Can't use forall'd variable in context</td></tr>
<tr><th>[\#14863](http://gitlabghc.nibbler/ghc/ghc/issues/14863)</th>
<td>QuantifiedConstraints: Can't deduce \`c' from \`(a, b)' and \`a \|- b \|- c'</td></tr>
<tr><th>[\#14878](http://gitlabghc.nibbler/ghc/ghc/issues/14878)</th>
<td>Can't witness transitivity ((.)) of isomorphism of Constraints</td></tr>
<tr><th>[\#14883](http://gitlabghc.nibbler/ghc/ghc/issues/14883)</th>
<td>QuantifiedConstraints don't kick in when used in TypeApplications</td></tr>
<tr><th>[\#14897](http://gitlabghc.nibbler/ghc/ghc/issues/14897)</th>
<td>QuantifiedConstraints: Can't print type of quantified constraint</td></tr>
<tr><th>[\#14942](http://gitlabghc.nibbler/ghc/ghc/issues/14942)</th>
<td>QuantifiedConstraints: GHC can't infer</td></tr>
<tr><th>[\#14961](http://gitlabghc.nibbler/ghc/ghc/issues/14961)</th>
<td>QuantifiedConstraints: introducing classes through equality constraints fails</td></tr>
<tr><th>[\#14993](http://gitlabghc.nibbler/ghc/ghc/issues/14993)</th>
<td>QuantifiedConstraints and principal types</td></tr>
<tr><th>[\#15008](http://gitlabghc.nibbler/ghc/ghc/issues/15008)</th>
<td>Type synonyms with hidden, determined type variables</td></tr>
<tr><th>[\#15231](http://gitlabghc.nibbler/ghc/ghc/issues/15231)</th>
<td>UndecidableInstances validity checking is wrong in the presence of QuantifiedConstraints</td></tr>
<tr><th>[\#15244](http://gitlabghc.nibbler/ghc/ghc/issues/15244)</th>
<td>Ambiguity checks in QuantifiedConstraints</td></tr>
<tr><th>[\#15290](http://gitlabghc.nibbler/ghc/ghc/issues/15290)</th>
<td>QuantifiedConstraints: panic "addTcEvBind NoEvBindsVar"</td></tr>
<tr><th>[\#15316](http://gitlabghc.nibbler/ghc/ghc/issues/15316)</th>
<td>Regarding coherence and implication loops in presence of QuantifiedConstraints</td></tr>
<tr><th>[\#15334](http://gitlabghc.nibbler/ghc/ghc/issues/15334)</th>
<td>(forall x. c x, forall x. d x) is not equivalent to forall x. (c x, d x)</td></tr>
<tr><th>[\#15359](http://gitlabghc.nibbler/ghc/ghc/issues/15359)</th>
<td>Quantified constraints do not work with equality constraints</td></tr>
<tr><th>[\#15507](http://gitlabghc.nibbler/ghc/ghc/issues/15507)</th>
<td>Deriving with QuantifiedConstraints is unable to penetrate type families</td></tr>
<tr><th>[\#15625](http://gitlabghc.nibbler/ghc/ghc/issues/15625)</th>
<td>GHC panic, with QuantifiedConstraints</td></tr>
<tr><th>[\#15635](http://gitlabghc.nibbler/ghc/ghc/issues/15635)</th>
<td>Implication introduction for quantified constraints</td></tr>
<tr><th>[\#15636](http://gitlabghc.nibbler/ghc/ghc/issues/15636)</th>
<td>Implication constraint priority breaks default class implementations</td></tr>
<tr><th>[\#15943](http://gitlabghc.nibbler/ghc/ghc/issues/15943)</th>
<td>"ASSERT failed" with quantified constraints</td></tr>
<tr><th>[\#15974](http://gitlabghc.nibbler/ghc/ghc/issues/15974)</th>
<td>QuantifiedConstraints: Spurious error involving superclass constraints</td></tr>
<tr><th>[\#16123](http://gitlabghc.nibbler/ghc/ghc/issues/16123)</th>
<td>QuantifiedConstraints fails to deduce trivial constraint</td></tr></table>



