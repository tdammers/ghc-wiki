# Type Level Literals



This page collects information on how to work with type-level literals, as implemented in the Haskell compiler GHC (ticket [\#4385](http://gitlabghc.nibbler/ghc/ghc/issues/4385)).


## Status



Use Keyword = `TypeLits` to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th>[\#8422](http://gitlabghc.nibbler/ghc/ghc/issues/8422)</th>
<td>type nats solver is too weak!</td></tr></table>




Closed Tickets:

<table><tr><th>[\#9036](http://gitlabghc.nibbler/ghc/ghc/issues/9036)</th>
<td>ghc: panic! Binder's type (SingI Symbol \<a String\>) /= RHS type (String)</td></tr>
<tr><th>[\#10321](http://gitlabghc.nibbler/ghc/ghc/issues/10321)</th>
<td>GHC.TypeLits.Nat types no longer fully simplified.</td></tr>
<tr><th>[\#10742](http://gitlabghc.nibbler/ghc/ghc/issues/10742)</th>
<td>GHC cannot deduce (irrelevant) reflexive type equality.</td></tr>
<tr><th>[\#10774](http://gitlabghc.nibbler/ghc/ghc/issues/10774)</th>
<td>Use \`Natural\` rather than \`Integer\` in \`GHC.TypeLits\`</td></tr></table>



## User's Guide


- [Type-Level Literal Basics](type-nats/basics)
- [Type-Level Computation](type-nats/operations)
- [Typed examinations of Sing values](type-nats/inductive-definitions)
- [Matching on Type-Level Naturals (i.e., working with classes and type-families)](type-nats/matching-on-nats)

## Notes on Design


- [Details about the implementation of singleton families.](type-nats/singletons-and-kinds)
- [Alternative Design For Singletons](type-nats/alternative-singletons)
- [Avoiding Partial Type Functions](type-nats/avoiding-partial-type-functions)
- [Singletons and Existentials](type-nats/singletons-and-existentials)


    


## Notes on the Implementation


- [Implementation of GHC.TypeLits](type-nats/implementation)
- [The solver for type-level naturals](commentary/compiler/type-nat-solver)

## Source Code


- [
  type-nats branch of GHC](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=ghc.git;a=shortlog;h=refs/heads/type-nats)
- [
  type-nats branch of the base library](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=packages/base.git;a=shortlog;h=refs/heads/type-nats)
- [
  type nats branch of template-haskell](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=packages/template-haskell.git;a=shortlog;h=refs/heads/type-nats)
- Also, there is a type-nats branch for 'haddock'.


  


## XXX: Cleanup


- [Natural Numbers: From Values to Types](type-nats/naturals)
- More advanced example: [
  https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)
- [Examples](type-nats/examples)
- [
  Axioms for type-level type operators](http://github.com/yav/tc-solver/blob/master/docs/axioms.md)
