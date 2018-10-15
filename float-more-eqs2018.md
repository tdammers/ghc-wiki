
I've created this wiki page to track my learning/research as I try to improve my feature request that I made in the comments of [\#15009](http://gitlabghc.nibbler/ghc/ghc/issues/15009).


## 20181014



Just using small examples today, no reference to earlier days -- kind of a fresh start.


### Example 1


```wiki
forall[2] x. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ Int)
```


`Note [Let-bound skolems]` applies here: it is safe to float `w` past `gx`, regardless of skolems and givens outside the scope of `x`.



Note that a more general `w : <W>` can only float after being simplified by `gx`, otherwise the skolem `x` would escape!


### Example 2


```wiki
forall[2] x. () =>
forall[3]. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ Int)
```


In this case, I think it is also safe to float `w` past `gx`, even though `Note [Let-bound skolems]` doesn't apply. This kind of example motivates my investigation here.


### Example 3


```wiki
forall[2] x. () => (
    (u : <U>)
  ,
    forall[3]. (gx : x ~ Int) =>
      (w : alpha[tau:2] ~ Int)
  )
```


In this case, it \*NOT\* safe to float `w` past `gx`. Simplification of `u` might assign `alpha := x`, in which case `gx` may be required in order to solve `w`. If that assignment happened after the float, we'd be stuck.


### Example 4


```wiki
forall[2] x. (gx : x ~ Int) => (
    (u : <U>)
  ,
    (w : alpha[tau:2] ~ Int)
  )
```


`Note [Let-bound skolems]` still applies, and it's safe to float `w`! (GHC 8.6 does.) This is because \*all\* occurrences of `x`, even those in `<U>`, will have been eliminated by `gx`, and so no problematic assignment can take place after the float. `x` is a dead-binder once `gx` has been applied within the scope that it shares with `x`.



I will no longer explicitly write the "elsewhere" constraint `u`; I will instead simply assume that any unification variable `alpha[tau:i]` where `i > 0` can rewrite to any type whose free variables' levels are all `<= i`. (Level `0` is reserved for flattening variables, and I anticipate I'll have to treat those somewhat differently.)



Note that Examples 3-4 with `u` left implicit are the same as Examples 1-2 with `alpha`'s level increased from 1 to 2.


### Example 5


```wiki
forall[1] x. () =>
forall[2] y. (gx : x ~ T y) =>
forall[3]. (gy : y ~ Int) =>
  (w : alpha[tau:1] ~ T Int)
```


`w` should not float past `gy`. If we decide elsewhere that `alpha := x`, then `w` can only be solved via a combination of `gx` and `gy`.



The line of reasoning is:


1. The type of `w` has `alpha` free, by observation.
1. `alpha` can rewrite to something with `x` free, by our heuristic regarding unification variable levels.
1. `x` can rewrite to something with `y` free, by `gx`.
1. The type of `w` can rewrite to something with `y` free, by transitivity.
1. Thus, floating `w` out from under `gy` could ultimately cause an avoidable type error.


Note that contrary to the given-given interaction rules in `jpf-outsidein.pdf`, GHC does not interact `gx` and `gy` to create a new given `x ~ T Int` (presumably at level 3). So we have to explicitly guard against this transitive scenario. (For example, inspect the `-ddump-tc-trace` for `f :: (x :~: [y]) -> (y :~: Int) -> x -> c; f Refl Refl = id`; it seems like the transitivity is actually "handled" during flattening.)



I found intermediate conclusion within that line of reasoning that `alpha[tau:1]` can rewrite to something with `y[sk:2]` free to be counter-intuitive because that rewriting \_increases\_ the maximum level of the source term's free type variables. This increase originates in the otherwise completely typical `gx : x[sk:1] ~ T y[sk:2]` CTyEqCan.


### Refinement of Example 5



I see a much simpler argument against floating here.


1. `w` would have to float past both `gy` \*and also\* `gx` for its `alpha` to become touchable.
1. `w` manifestly cannot pass `gx`.
1. Thus, do not float `w` at all.


If we consider an example with `alpha`'s level increased to `2` so that it would not need to pass `gx`, then it manifestly cannot pass `gy`.



So maybe an inherited attributed along the lines of "the lowest level tyvar with a CTyEqCan in the outer givens between level 1 and here is 1" would be enough to know that `w` cannot float all the way to its target level (`1`, the level of its LHS `alpha`) and so should not float at all.


### Example 6



This case is simpler than it looks at first.


```wiki
forall[1] x. () =>
forall[2] y. () =>
forall[3]. (gy1 : y ~ T x) =>
forall[4]. (gy2 : y ~ T Int) =>
  (w : alpha[tau:1] ~ T Int)
```


The subtlety is that those givens are actually not inert; GHC will interact `gy1` and `gy2` to create a constraint at level 4 `x ~ T Int`, according to the `EQSAME` rule from `jfp-outsidein.pdf` (somewhat in contrast to Example 5, where `EQDIFF` is apparently not directly implemented in GHC).


```wiki
forall[1] x. () =>
forall[2] y. () =>
forall[3]. (gy1 : y ~ T x) =>
forall[4]. (gx : x ~ Int) =>   -- evidence binding gx from gy1 and gy2 interacting
  (w : alpha[tau:1] ~ T Int)
```


Now it's clear `w` should not float, because of the new `gx` --- it's quite similar to Example 5 now.


### Example 7



Similar to the previous example, this set is not yet inert.


```wiki
forall[1] x. () =>
forall[2]. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ x)
```


`gx` can simplify `w`, which yields Example 3, essentially.


### Example 8



Unification variables can occur in givens.


```wiki
forall[1]. () =>
forall[2]. (gbeta : beta[tau:1] ~ Int) =>   -- beta must be < level 2, according to (GivenInv) of TcType.Note [TcLevel and untouchable type variables]
  (w : alpha[tau:1] ~ Int)
```


`w` should not float past `gbeta`, since we might elsewhere decide that `beta := alpha` or `alpha := beta`.



Even if `alpha` were of a higher level


```wiki
forall[2]. () =>
forall[3]. (gbeta : beta[tau:1] ~ Int) =>
  (w : alpha[tau:2] ~ Int)
```


the assignment {{{alpha := beta}} is still possible.


### Example 9



This is my actual original motivation.


```wiki
forall[2] x y. () =>
forall[3]. ( (g1 : x ~ alpha[tau:1]) , (g2 : y ~ beta[tau:1]) ) => (
    (w1 : alpha[tau:1] ~ Int)
  ,
    (w2 : beta[tau:1] ~ Int)
  )
```


I think they should both float.



The remaining examples will explore variations of this. I'll leave out `g2` and `w2` when possible.


```wiki
forall[2] x. () =>
forall[3]. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ Int)
```

### Example 10



`TcUnify.Note [TyVar/TyVar orientation]` specifies that `x[sk:i] ~ alpha[sk:i]` should be swapped to `alpha[sk:i] ~ x[sk:i]`.


```wiki
forall[2] x. () =>
forall[3]. (g : alpha[tau:2] ~ x) =>
  (w : x ~ Int)
```


GHC won't consider floating that, because `w` is not of the form `alpha[tau:_] ~ <T>`.



I definitely don't have enough context to seriously challenge the current swap decision procedure. But, if we didn't reorient, then I don't see why `w` shouldn't float...


```wiki
forall[2] x. () =>
forall[3]. (g : x ~ alpha[tau:2]) =>   -- hypothetically ignoring current swap decision procedure
  (w : alpha[tau:2] ~ Int)
```

### Example 11



Of course, occurrences of skolems on the righthand-side of `w` should prevent floating past their binder.


```wiki
forall[2] x. () =>
forall[3] y. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ Maybe y)                 -- Decide no float, since 1 < maximum (map level (fvs (Maybe y)))
```

### Example 12



I don't understand irreducible constraints terribly well, so I'll just treat them all conservatively for now by refining the inherited attribute.


```wiki
forall[2] x. () =>
forall[3]. beta[tau:1] =>
forall[4] y. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ y)
```


The inherited attribute is a vector, one for each parent implication, in which each vector component is the level of the lowest level tyvar on the LHS of a CTyEqCan or that occurs free in an irreducible constraint.



Maybe insoluble constraints just set the vector component to `0`, thereby preventing floating past them?


```wiki
forall[2] x. () =>                       -- vec [Inf,Inf,Inf]   (i.e. levels 2,1,0)
forall[3]. beta[tau:1] =>                -- vec [1,1,1,1]
forall[4] y. (g : x ~ alpha[tau:1]) =>   -- vec [2,1,1,1,1]     (i.e. levels 4,3,2,1,0)
  (w : alpha[tau:1] ~ y)                 -- Decide no float, since 1 >= vec !! (4-1).
```

### Conclusion



TODO ... I haven't yet thought of anything clever that isn't caught by the inherited attribute test I proposed in the discussion of Example 5, even with type family applications and/or constraint kinds.


## 20181013



This section distills the narrative of the 20181008 section into narrower observations and questions.



My current goal is a "simple enough", algorithm that could conservatively decide whether it is safe to float `w : alpha[tau:k] ~ <R>` out from under `g : x[sk:i] ~ <X>` where `k < i` and `alpha` might occur free in `<X>`. Informally, it's only safe to do so if the eventual right-hand side of `w` will definitely not (transitively) depend on `g`.



A wanted `w : <W>` can gain `x[_:i]` as a free variable in two ways.


- WAY1 A uvar `alpha[tau:j]` that occurs in `<W>` could be assigned a type `alpha := <T>` where `x` occurs free in `<T>`, which would require `j >= i`.
- WAY2 `w` could be simplified by a given CTyEqCan `g : y ~ <T>` where `x` occurs free in `<T>`.


WAY1 cannot increase the maximum level of the free variables in `<W>`. In contrast, WAY2 can: consider `g : y[_:2] ~ Maybe x[sk:3]`.



Within one subtree of a binding group's entire forest of implication trees, we must assume that a uvar `alpha[tau:j]` could be assigned a type `<T>` in which any variable of level `<= j` other than `alpha` itself could occur free.



The reduction of a type family application `F <X>` cannot introduce new free type variables beyond those already free in `<X>` -- except for variables that arise from flattening the type family applications possibly introduced by the reduction. But it might remove some free variables.



An assignment to a uvar `alpha[tau:j]` that occurs in the givens could directly or indirectly (via enabled given-given interactions) yield the given `g` of WAY2.


## 20181008



I've found a useful perspective on this. Consider this implication tree.


```wiki
forall[3] y. () =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


The naive intuition is


>
>
> We can float `w1` out from under `g` if `y` doesn't occur (free) in `<W1>`.
>
>


In fact, according to the jfp-outsidein.pdf "Simplification Rules", `g` will eventually simplify `w1` into some `w2 : <W2>` such that `y` does not occur in `<W2>`.



The subtlety, however, is that `y` might occur in `<U>` and therefore the solving of `u` may assign a u(nification)var(iable) `alpha[tau:i]` where `i >= 3` to some type in which `y` occurs; if `alpha` occurs in `<W2>`, then now `<W2>` suddenly has an occurrence of `y` again!



This example demonstrates:


```wiki
forall[3] y. () =>
  ( (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   (w1 : alpha[3] ~ y)
  )

-- use g to simplify w1 -->

forall[3] y. () =>
  ( (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   (w2 : alpha[3] ~ Int)
  )

-- float w2, since y does not occur in its type -->

forall[3] y. () =>
  ( (w3 : alpha[3] ~ Int)
  , (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   ()
  )

-- solve u, by alpha[3] := y -->

forall[3] y. () =>
  ( (w3 : y ~ Int)
  , forall[4]. (g : y ~ Int)   =>   ()
  )

-- Stuck because we floated!
```

### Comparison to status quo



Compare this to the case where `Note [Let-bound skolems]` applies.


```wiki
forall[3] y. (g : y ~ <Y>)   =>   (w1 : <W1>)
```


There is no `u : <U>` that might assign to a uvar `alpha` shared with `<W1>` here, so the failure mode describe above can't happen. If we add a `u` alongside `w1`,


```wiki
forall[3] y. (g : y ~ <Y>)   =>   ( (w1 : <W1>) , (u : <U>) )
```


then now it's possible solving `u` could assign `alpha[tau:3] := ...y...`. However, as part of floating `w1`, we immediately promote any uvars that occur in it, so the problematic `alpha[tau:3]` would be assigned `alpha[tau:3] := alpha[tau:2]`, which cannot be assigned `alpha[tau:2] := ...y...` since `y` is `[sk:3]` (and also `alpha[tau:2]` is untouchable in `u[3]`).



This silly example demonstrates:


```wiki
forall[3] y. (g : y ~ Int)   =>   (w1 : alpha[3] ~ y,u : alpha[3] ~ y)

-- use g to simplify w1 -->

forall[3] y. (g : y ~ Int)   =>   (w2 : alpha[3] ~ Int,u : alpha[3] ~ y)

-- float w2, since y does not occur in its type (NB the promotion of alpha 3 to 2) -->

(w3 : alpha[2] ~ Int)
forall[3] y. (g : y ~ Int)   =>   (u : alpha[2] ~ y)

-- Cannot solve u as in previous example, because alpha is now untouchable in u.
-- solve w3, by alpha[2] := Int -->

forall[3] y. (g : y ~ Int)   =>   (u : Int ~ y)

-- re-orient, reflexivity -->

-- Solved!
```

### Refinement for Assignment



So we can refine the rule for


```wiki
forall[3] y. () =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


from


>
>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>`.
>
>


to


>
>
> We can float `w1` out from under `g` if `y` does not **and can never again** occur in `<W1>`.
>
>


How do we decide that much stronger predicate? There are a few options (e.g. there is no sibling `u` wanted), but I currently favor checking that no uvar in `<W1>` has a level `>= 3` (this check should inspect the RHS of any flattening vars that occur in `<W1>`). That prohibits the problematic reintroduction of `y` via uvar assignment after floating.



Thus:


>
>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>` **and all unification variables in `<W1>` have level `< 3`**.
>
>


After I thought about this for a bit, I realized there might be another way for `y` to be reintroduced into `<W1>` in the more general case where that outer implication has givens.


```wiki
forall[3] y. (o : <O>) =>
  forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
```


If it's something like `o : x[sk:2] ~ Maybe y`, and `x` occurs in `<W1>`, then simplifying via `g` and floating `w1` and then simplifying it via `o` might reintroduce `y`.


### Refinement for Outer Givens (a wrong turn)



So we can generalize the rule for


```wiki
forall[3] y. (o : <O>) =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


by refining from


>
>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>` and all unification variables in `<W1>` have level `< 3`.
>
>


to


>
>
> We can float `w1` out from under `g` if **all givens are inert** and `y` does not occur in `<W1>` and all unification variables in `<W1>` have level `< 3`.
>
>


If I understand the jfp-outsidein.pdf "Interaction Rules", `o : x[sk:2] ~ Maybe y` and `g` ought to interact to yield something like


```wiki
forall[3] y. (o : x[sk:2] ~ Maybe y) =>
  ( (u : <U>)
  , forall[4]. ( (g : y ~ <Y>) , (g2 : x[sk:2] ~ Maybe <Y>) )  =>   (w1 : <W1>)
  )
```


Now that givens are inert, our rule will force `g` and `g2` to eliminate the skolems and assess uvars (level `< 2` now) in `w1` before floating it.



NOPE, the above is not how `g` and `o` "interact". I searched `TcInteract` for something like `EQDIFF`, but I ended up at 



[
line 1630](https://github.com/ghc/ghc/blob/8bed140099f8ab78e3e728fd2e50dd73d7210e84/compiler/typecheck/TcInteract.hs#L1630)


```wiki
  | isGiven ev         -- See Note [Touchables and givens]
  = continueWith workItem
```


That Note hasn't existed since


```wiki
commit 27310213397bb89555bb03585e057ba1b017e895
Author: simonpj@microsoft.com <unknown>
Date:   Wed Jan 12 14:56:04 2011 +0000

    Major refactoring of the type inference engine
```


I don't see what touchables has to do with line 1630. /shrug



So I whipped up a test


```wiki
f :: (x :~: [y]) -> (y :~: Int) -> x -> c
f Refl Refl = id
```


which never gets further than


```wiki
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given = co_a1bQ :: [y_a1bM[sk:1]] GHC.Prim.~# x_a1bL[sk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 3
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given = co_a1bR :: Int GHC.Prim.~# y_a1bM[sk:1]
              Wanted =
                WC {wc_simple =
                      [WD] hole{co_a1c2} {2}:: c_a1bN[sk:1]
                                               GHC.Prim.~# [Int] (CNonCanonical)}
              Binds = EvBindsVar<a1bV>
              a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
              in an equation for `f_a1bO' }}
    Binds = EvBindsVar<a1bW>
    a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
    in an equation for `f_a1bO' }
```


and even explicitly says


```wiki
  Inerts: {Equalities: [G] co_a1bZ {1}:: x_a1bL[sk:1]
                                         GHC.Prim.~# [y_a1bM[sk:1]] (CTyEqCan)
                       [G] co_a1c0 {1}:: y_a1bM[sk:1] GHC.Prim.~# Int (CTyEqCan)
           Unsolved goals = 0}
```


Note that `co_a1bZ` and `co_a1c0` never "interacted" as `EQDIFF` suggests they should. However, they both rewrote the wanted. (In fact, it's unflattening that seems to do this, even though there are no type families involved! See [
the definition of](https://github.com/ghc/ghc/blob/8bed140099f8ab78e3e728fd2e50dd73d7210e84/compiler/typecheck/TcFlatten.hs#L1846) `flatten_tyvar2`.)


### Comparison to status quo



`Note [Let-bound skolems]` need not worry about outer givens because `y` is not in scope outside of this implication.


### Refinement for Outer Givens with Interactions (a turn too far)



So we'll need a different refinement to ensure that `y` cannot be reintroduced by an outer given.



In our `o : x[sk:2] ~ Maybe y` example, `x` has a significant relationship with `y`: any occurrence of `x` in the scope of `o` will be rewritten to have an occurrence of `y` instead. Loosely, "`x` can become `y`".



Similarly, any uvar `alpha[tau:i]` where i `>= 3` "can become `y`" due to solving other parts of the implication tree. So the "can become" relation is a nice commonality between the two failure modes I've considered.



How might we determine if "`x` can become `y`" in general? Here's another failure, also based on the "`x` can become `y`" idea, but with more moving parts.


```wiki
forall[3] y. ( (o1 : z[sk:2] ~ T x[sk:2] ) , (o2 : alpha[tau:2] ~ T (Maybe y)) ) =>
  ( (u : <U>)
  , forall[4]. ( (g : y ~ <Y>) , (g2 : x[sk:2] ~ Maybe <Y>) )  =>   (w1 : <W1>)
  )
```


Note that the equalities `o1` and `o2` contain `x` and `y` respectively, but do not have any shared variables. However, solving another part of the implication tree might assign `alpha := z`, in which case `o1` and `o2` will interact, yielding a constraint that then canonicalizes to the familiar `o : x ~ Maybe y`. This is actually the same lesson as in the first failure mode "`alpha[tau:i]` can become any variable at level `<= i`", but in the givens this time and one step removed from affecting our float.



Our second refinement makes


>
>
> We can float `w1` out from under `g` if `y` does not and can never again occur in `<W1>`.
>
>


more formal as


>
>
> We can float `w1` out from under `g` if the transitive closure of the "can step to" relation does not relate any free variable of `<W1>` to `y`.
>
>


which we can shorten to


>
>
> We can float `w1` out from under `g` if no free variable of `<W1>` "can become" `y`.
>
>


by defining the following (over-estimating) properties.


- CB1 "can become" includes the transitive closure of "can step to"
- CS1 `x` (any flavor and level) can step to itself
- CS2 `alpha[tau:i]` can step to any variable at level `<= i`
- CS3 `x` (any flavor and level) can step to any free variable of `<X>`, given CTyEqCan `x ~ <X>`
- CS4 Any free variable of `<A>` can step to any free variable of `<B>` or vice versa, given a non-CTyEqCan equality `<A> ~ <B>`


That's enough to catch the `o : x[sk:2] ~ Maybe y` failure. But it's not enough for the `o1` and `o2` failure that involved the interaction after assignment. So we must further extend "can become".


- CB2 Any free variable of `<A>` can become any free variable of `D` or vice versa if a free variable of `<B>` can become a free variable of `<C>` or vice versa, given `<A> ~ <B>` and `<C> ~ <D>` (or either/both of their symmetries)


(Could there be less symmetric special cases of `CB2` for CTyEqCans-like things?)



Let "can become" be the smallest relation satisfying all the CB\* properties.



... That seems simultaneously too complicated and also too conservative. So maybe we should start with relatively simple and therefore acceptably too conservative.


###
Another Refinement for Outer Givens with Interactions (a turn too far in the other direction?)



This seems reasonably balanced for starters.


```wiki
forall[3] y. theta =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```

>
>
> We can float `w1` out from under `g` if no free variable of `<W1>` "can become" `y`.
>
>

- CB1 "can become" includes the transitive closure of "can step to"
- CS1 `x` (any flavor and level) can step to itself
- CS2 `alpha[tau:i]` can step to any variable at level `<= i`
- CS3 `x` (any flavor and level) can step to any free variable of `<X>`, given CTyEqCan `x ~ <X>`
- CS4 Any free variable of `<A>` can step to any free variable of `<B>` or vice versa, given a non-CTyEqCan equality `<A> ~ <B>`
- CS5 Any free variable of the possible equalities in `theta` can step to any other and to any variable of level `<= i`, if there is a uvar of level `i` in `theta`


Let "can become" be the smallest relation satisfying CB1.


###
Yet Another Refinement for Outer Givens with Interactions (are we going in circles?)



TODO I'm currently thinking about about what to do if `g` has siblings. I suspect CS5 should consider those too. Should it also consider `g`?



My current goal is for `w : alpha[tau:1] ~ Int` to float out from under `g : x[sk:2] ~ alpha[tau:1]`. Thus CS5 is likely disappointingly conservative if it includes siblings:


```wiki
forall[3] x y. () =>
  ( (u : <U>)
  , forall[4]. ( (g1 : x ~ alpha[tau:1]) , (g1 : y ~ beta[tau:1]) )   =>   ( (w1 : alpha[tau:1] ~ Int) , (w2 : beta[tau:1] ~ Int) )
  )
```


Maybe CS5 should ignore uvars on the RHS of a skolem's CTyEqCan?


## Experiments


### How do given-given interactions manifest?


```wiki
f :: (x :~: [y]) -> (x :~: z) -> x -> c
f Refl Refl = id
```


gives a trace that includes


```wiki
addTcEvBind
  a1c0
  [G] co_a1c5 = CO: co_a1bW ; co_a1c4
```


and


```wiki
getNoGivenEqs
  May have given equalities
  Skols: []
  Inerts: {Equalities: [G] co_a1c4 {1}:: x_a1bP[sk:1]
                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CTyEqCan)
                       [G] co_a1c5 {1}:: z_a1bR[sk:1]
                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CTyEqCan)
           Unsolved goals = 0}
```


(The number is braces is the SubGoalDepth -- see TcRnTypes.Note \[SubGoalDepth\].)



and


```wiki
reportImplic
  Implic {
    TcLevel = 1
    Skolems = x_a1bP[sk:1] y_a1bQ[sk:1] z_a1bR[sk:1] c_a1bS[sk:1]
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given = co_a1bV :: [y_a1bQ[sk:1]] GHC.Prim.~# x_a1bP[sk:1]
              Wanted =
                WC {wc_impl =
                      Implic {
                        TcLevel = 3
                        Skolems =
                        No-eqs = False
                        Status = Unsolved
                        Given = co_a1bW :: z_a1bR[sk:1] GHC.Prim.~# x_a1bP[sk:1]
                        Wanted =
                          WC {wc_simple =
                                [WD] hole{co_a1c7} {2}:: c_a1bS[sk:1]
                                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CNonCanonical)}
                        Binds = EvBindsVar<a1c0>
                        a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
                        in an equation for `f_a1bT' }}
              Binds = EvBindsVar<a1c1>
              a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
              in an equation for `f_a1bT' }}
    Binds = EvBindsVar<a1c2>
    the type signature for:
      f :: forall x y z c. (x :~: [y]) -> (x :~: z) -> x -> c }
```