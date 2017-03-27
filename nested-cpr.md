
This is nomeata’s notepad about the nested CPR information:



See also sub-pages:


- [NestedCPR/AdvancedConverges](nested-cpr/advanced-converges)
- [NestedCPR/DmdAnalIdeas](nested-cpr/dmd-anal-ideas)
- [NestedCPR/better-ho-cardinality](nested-cpr/better-ho-cardinality)
- [NestedCPR/wave4main](nested-cpr/wave4main)
- [NestedCPR/Akio2017](nested-cpr/akio2017) tracks \@akio's attempt at rebasing and finishing this work after [SequentCore](sequent-core) is in HEAD.

### Related tickets


- [\#1600](http://gitlabghc.nibbler/ghc/ghc/issues/1600) Main tickets where I mention progress.


Tickets with stuff that would make nested CPR better:


- [\#8598](http://gitlabghc.nibbler/ghc/ghc/issues/8598) CPR after IO (partly done)


Tickets with example of code that would benefit from nested CPR:


- [\#1600](http://gitlabghc.nibbler/ghc/ghc/issues/1600), [\#2289](http://gitlabghc.nibbler/ghc/ghc/issues/2289), [\#2387](http://gitlabghc.nibbler/ghc/ghc/issues/2387) (see [below](nested-cpr#motivating-examples) for an analysis)
- (Maybe) [
  this thread on ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2016-March/011623.html)

### Related testcases


- Everything in [source:testsuite/tests/stranal/sigs/](/trac/ghc/browser/testsuite/tests/stranal/sigs)


 


### TODOs


- Paper-Writeup of CPR
- Shouldn’t nested CPR help a lot with Complex-heavy code? Is there something in nofib?
- Look at DmdAnal-related [Status/SLPJ-Tickets](status/slp-j--tickets) and see which ones are affected by nested-cpr.
- Do not destroy join points or improve the code genrator (see below).

### DONEs


- Try passing CPR information from the scrunitee to the pattern variables. For that: Reverse flow of analysis for complex scrunitees (for simple, we want the demand coming from the body, for complex, this is not so important.)

  - Done. Mostly no change besides `-0.9` in `compress2`. Maybe worth removing.

### Motivating examples



Motivation is always good. Here I try to look at examples where people were expecting or hoping for nested CPR, and see how we are fairing:


- `facIO` in [\#1600](http://gitlabghc.nibbler/ghc/ghc/issues/1600): Not eligible for nested CPR, as the result is not forced. Using `return $!` makes this work.
- `mean` in [\#2289](http://gitlabghc.nibbler/ghc/ghc/issues/2289): Not eligible for nested CRP. The base case `go x l s | x > m      = P s l` is – to the demand analyzer – lazy in `s` and `l`, so doing nested CRP would make that stricter. It works with `s `seq` l `seq` P s`. But `P` *is* a strict constructor! When the demand analyser runs, it still sees the wrapper `$WP`. Maybe it just needs to be inlined earlier? Tried inlining more aggressively, helps, and does not seem to hurt.
- [\#2387](http://gitlabghc.nibbler/ghc/ghc/issues/2387) works nicely! (but note that `go` uses a `!n` pattern already)

### Degradation exploration and explanation



At one point, I thought that a major contributor to increased allocations is nested-CPR’ing things returning `String`, causing them to return `(# Char#, String #)`. But removing the `CPR` information from `C#` calls has zero effect on the allocations, both on `master` and on `nested-cpr`. It had very small (positive) effect on code size. Will have to look at Core...



Here are some case studies with extensive commenting of steps and results:


- [wave4main](nested-cpr/wave4main)
- reverse-complement: The increase of 5% / 5MB allocations again manifests itself in the `ALLOC_FUN_gds` counter, this time in the libraries: `base:GHC.IO.Handle.Internals.wantReadableHandle_1`. I see some additional inlining that was not there before. The expression `a` inlined has type `State# RealWorld -> (# State# RealWorld, a4 #)`, and CPR information `m(t,)`, which is correct, but useless, as the tuple is already unboxed. Code in `WorkWrap` would nevertheless take this as a reason to add an `INLINE` flag. Fixing that removed the increase – and overall better results now!


And here a summary of the problems identified, and solution attempts


- CPR kill join-points, because the wrapper does not completely cancel with anything else.

  - Detecting join-points at the position of its binding is not enough.
- A recursive function can have a CPR-beneficial recursive call that makes CPR worthwhile, even if it does not help at the initial call. But it is also not unlikely that the recursive call is a tail-call, and CPR-ing has zero effect on that. Then it all depends on the external call.
- With sum types, CPR is much less often useful. And indeed, nesting CPR information inside sum-type-constructors has only negative effect (-0.0%/+0.0%/+0.4%).

### Converges detection



Nested CPR is only sound if we know that the nested values are known to converge for sure. The semantics is clear: If `f` has CPR `<...>m(tm(),)`, then in the body of `case f x of (a,b)`, when entering `a`, we are guaranteed termination.



What is the semantics of an outer `t`? Given `f` with CPR `<L>tm()` and `g` with CPR `<S>tm()`? Does the latter even make sense? If so, should `f undefined` have CPR `m()` or `tm()`? Three possibilities:


1. The convergence information a function is what holds if its strictness annotations are fulfilled: So if `g x`  has `tm()` if `x` has `t` (possibly because it has previously been evaluated by the caller), otherwise `m()`. `f x` always has `m ()` (presumably because `x` is \_never\_ entered when evaluating `f`.
1. The convergence information a function is what holds always. This would in effect prevent `<S>tm()` from happening.
1. The convergence information always holds, but special care is taken for unlifted types: `I#`, like any function expecting an unlifted parameter or free variable, would get `<S>tm()`. (For unlifted types, `<L>` and `<S>` are identical. One might turn that into a third way `<#>`, but unless there is more use to that than just clarification, we do not do that). The implementation now simply makes the demand of any argument strict if it has an unlifted type, so that the strictness annotation does not matter so much.


Clearly, 1. and 3. hold strictly more information than 2.: Under variant `2`, `<S>tm()` would not occur, while the other variants allow that. Also, under 2, `I#` would not be known to terminate for sure, as it is strict. This would destroy any hope for nested CPR for things like `(Int, Int)`.



I worked on 1, but it turned out to be too complicated. Notes at [AdvancedConverges](nested-cpr/advanced-converges). So I’ll proceed with 3. now.


### join points



CPR can kill join points. Attempts to mitigate that:


#### Common Context



Idea to fix this, and possibly more general benefits:
[
http://www.haskell.org/pipermail/ghc-devs/2013-December/003481.html](http://www.haskell.org/pipermail/ghc-devs/2013-December/003481.html); prototype in branch `wip/common-context`.


- On its own, improvements are present but very small: [
  http://www.haskell.org/pipermail/ghc-devs/2013-December/003500.html](http://www.haskell.org/pipermail/ghc-devs/2013-December/003500.html)
- Enabling CPR for sum types in non-top-level-bindings (which is currently disabled due to worries abut lost join points) yields mixed results (min -3.8%, mean -0.0%, max 3.4%).
- Enabling sum types inside nested CPR: Also yields mixed, not very promising results (-6.9% / +0.0% / +11.3%).

#### Direct detection



Alternative: Detect join points during `dmdAnal` and make sure that their CPR info is not greater than that of the expression they are a join-point for. Would also fix [\#5075](http://gitlabghc.nibbler/ghc/ghc/issues/5075), see [5075\#comment:19](http://gitlabghc.nibbler/ghc/ghc/issues/5075) for benchmark numbers.


- On its own, no changes.
- Enabling CPR for sumtypes: (min -3.8%, mean -0.0%, max 1.7%) (slightly better than with Common Context)
- Enabling sum types inside nested CPR: TBD


Unfortunately, naive approaches are not possible: We need to know if `j` is a joint point not only for `let j = .. in ... j ..`, but also for expressions further out. Not nice.


#### Improvement to the code generator



It seems feasible to make the code generate generate better code for local functions that are not quite join-points any more, by jumping, passing both a continuation and a stack delta to the live variables. To be investigated.


#### Late lambda lifting



Might also help. Need to see if his branch can be merged onto master. (But I like the code generator idea better.)


### Side tracks


- ~~Use `Converges` in `exprOkForSpeculation`: Mostly done, see [8655\#comment:8](http://gitlabghc.nibbler/ghc/ghc/issues/8655).~~

  - I should get dynamic numbers, but given the static ones I doubt that these are worth collecting.
- Why is `cacheprof` not deterministic? (→ [\#8611](http://gitlabghc.nibbler/ghc/ghc/issues/8611))
- What became of Simon’s better-ho-cardinality branch? See [better-ho-cardinality](nested-cpr/better-ho-cardinality).
- Try vtunes to get better numbers.
- ~~Implement [\#2110](http://gitlabghc.nibbler/ghc/ghc/issues/2110)~~ (pushed)
- ~~Make worker-wrapper unbox data families: [\#7619](http://gitlabghc.nibbler/ghc/ghc/issues/7619)~~
- ~~Make foldl into a good consumer: [\#7994](http://gitlabghc.nibbler/ghc/ghc/issues/7994)~~ (pushed)

  - Ideas: [DmdAnalIdeas](nested-cpr/dmd-anal-ideas) ← Outdated
  - Related: See how often the demand on a function is better than its vanilla demand ([\#6070](http://gitlabghc.nibbler/ghc/ghc/issues/6070))
  - ~~Clean up my new shiny Caller Arity analysis.~~
  - Investigate in Takano’s WW-stuff.

    - Mail with question sent
    - He is investigating nofib regressions.
- Experiment with aggressive CSE: [\#7596](http://gitlabghc.nibbler/ghc/ghc/issues/7596)

  - Prevent floating past multi-way cases.
  - Experiment with CSE before and after DmdAnal.
