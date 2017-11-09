# Making LetUp more precise



The [demand analyser](commentary/compiler/demand) uses two different rules for handling let bindings, as explained in the [
cardinality paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf). TLDR:


- **LetDown** is used for top-level bindings, recursive bindings and local, non-recursive functions. It closely mimics semantics of an actual function call, as it unleashes the demand signature of the bound RHS at the call site within the let body.
- **LetUp** is used for local, non-recursive thunks. This works more like a type-system: Analysing the let-body reveals its demand on the identifier, in which the RHS of the bindings is analysed exactly once. Only now it is that the results from the body and the RHS get sequentially combined.


There are reasons why we currently need both rules ([
cardinality paper §3.5 and §3.6](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf)), although the general approach of **LetDown** is more flow-sensitive and thus strictly more precise than **LetUp**.



Consider the following running example:


```wiki
let y = ... 
in let x = 2*y
   in if ... then x else y
```


This evaluates `y` only once, but the **LetUp** rule (in contrast to **LetDown**) fails to recognise this:
The inner let-body has a demand environment of `[x :-> <L,1*U>, y :-> <L, 1*U>]`, which combines with the DmdEnv of the RHS `[y :-> <L, 1*U>` to a total DmdEnv for the let-expression binding `x` of `[y :-> <L,U>]`.
E.g., `y` is possibly evaluated multiple times!


## Co-call graphs



Meanwhile, [Call Arity](call-arity), using a **LetUp** strategy at let-bindings exclusively, has countered the exact same loss of precision by employing *Co-call graphs*. Instead of having cardinality information stored in a plain identifier map, where identifiers are assumed to be evaluated with each other, co-call graphs additionally assert that some ids are never evaluated during the same evaluation of the containing expression. 



For the above example, the co-call graph for the inner let-body would specifically *not* contain an edge between `x` and `y`, because they are evaluated on mutually exclusive code paths. Thus, substituting the co-call graph of `x`s RHS into the co-call graph of the let-body will *not* induce a loop on `y`, so y will be recognised as evaluated at most once, which is what we want here.


## BOTH/OR trees



The idea behind co-call graphs translates to demand analysis by *being as lazy as possible* in computing `DmdEnv`s. 



In particular, `bothDmdType` and `lubDmdType` currently smash together both incoming `DmdEnv`s. This forgets important structure for the above example: Namely that `x` and `y` are evaluated mutually exclusively.



To remedy that, we could model `DmdEnv`s as a tree structure (leaving `Termination`-based nastiness aside for a moment):


```wiki
data DmdEnv'
  = Empty
  | Var Id Demand
  | Or DmdEnv' DmdEnv'
  | Both DmdEnv' DmdEnv'

flattenDmdEnv' :: DmdEnv' -> DmdEnv
flattenDmdEnv' env =
  case env of
    Empty -> emptyVarEnv
    Var id dmd -> unitVarEnv id dmd
    Or env1 env2 -> lubDmdEnv env1 env2
    Both env1 env2 -> bothDmdEnv env1 env2
```


This is essentially the interpreter pattern, with `flattenDmdEnv'` being an interpreter in terms of old `DmdEnv`.



However, as we'll see in a second, this buys us the ability to perform proper substitution when resolving let-bindings with **LetUp**, instead of just deleting the bound id from the body env and `both`ing with the result of the RHS.



For the above example, we get `(Both (...) (Or (Var x <S,1*U>) (Var y <S,1*U>)))` as the `DmdEnv'` of the let-body and `(Var y <L,1*U>)` as the `DmdEnv'` of `x`s RHS under the recorded demand `<L,1*U>` on `x` within the body.
We can now *substitute* the `DmdEnv'` of `x`s RHS into all occurences of `x` within the `DmdEnv'` of the let-body:


```wiki
  (Both (...) (Or env             (Var y <S,1*U>)))[env := (Var y <L,1*U>)]
= (Both (...) (Or (Var y <L,1*U>) (Var y <S,1*U>)))
```


If we look up the demand on `y` in the resulting `DmdEnv'`, we find that `y` is put under demand `<L,1*U>`, so used at most once, which is what we wanted.



Note however that it is still not detected as being used strictly! 
For this, we would need to analyse `x`s RHS under the demand of the use site we substitute the `DmdEnv'` into, much like **LetDown** would.
Let's revisit this later.


## Thunk Sharing



There's an(other) issue with the substitution model.



Substitution appeals because it approximates **LetDown** in terms of precision, but it also suffers from the same imprecision when it comes to modeling thunk sharing:


```wiki
let y = ..
in let x = 2*y
   in x + x
```


Note that although `x` is evaluated twice, the evaluation of its RHS is shared, so that `y` is evaluated at most once.
This is also what the current demand analysis finds out.



Let's look at the `DmdEnv'` for the inner let-body:


```wiki
(Both
  (Var x <S,1*U>) 
  (Var x <S,1*U>))
```


Substituting the `DmdEnv'` of `x`s RHS:


```wiki
(Both
  (Var y <L,1*U>) 
  (Var y <L,1*U>))
```


Ouch! Substitution forgets about sharing and let's us see an imprecise upper bound of `w*U`.
What we are lacking is some kind of model for the runtime heap.



How does **LetUp** solve this? 
Well, it operates under the simple assumption that the binding is evaluated exactly once in the entire body, if it's evaluated at all.
So, it's OK to `both` the `DmdEnv'` of the RHS to that of the let body at the root. This won't destroy sharing!



Now, for the first example (`if ... then x else y`), it is quite obvious that the bound id `x` doesn't even occur in the first `Or` branch of the `DmdEnv'` of the let body (after deleting `x` from it).
Only the second branch evaluates `x` at all, so it should be safe to 'push' the *graft point*, where we graft the `DmdEnv'` of the RHS of `x` onto the `DmdEnv'` of the let body, down to the [
''most recent common ancestor'' (MCRA)](https://de.wikipedia.org/wiki/Most_recent_common_ancestor) of all occurences of `x` in the body.



For the first example:


```wiki
-- [] marks the MCRA of all previous occurences of `x`, which were already deleted
  graft (\t -> (Both t (Var y <L,1*U>))) onto (Both (...) (Or [Empty] (Var y <S,1*U>))))
= (Both (...) (Or (Both Empty (Var y <L,1*U>)) (Var y <S,1*U>))))
```

## Multiple graft points



Grafting the `DmdEnv'` of the RHS of `x` at the MCRA of all occurences of `x` in the `DmdEnv'` of the body is AFAICT the only solution that doesn't duplicate the `DmdEnv'` of the RHS into the tree, so should be quite predictable from a performance perspective.



However, there are shortcomings, exposed by examples like this:


```wiki
let y = ... in
let x = 2*y in
if ... then
  x
else if ... then
  y 
else if ... then
  x

(Both 
  (...)
  (Or
    (Var x <S,1*U>)
    (Both
      (...)
      (Or
        (Var y <S,1*U>)
        (Var x <S,1*U>)))))
```


The MCRA of all occurences of `x` is the root of the `DmdEnv'`. That's unfortunate, as this means we aren't more precise than **LetUp** here, which is insufficient to realise that `y` is only evaluated at most once.



This tension can only be resolved by having multiple graft points, one at each occurence of `x`.



When is this safe to do? 
It's always safe to push down a graft point when there's only one child node in which `x` occured (that's why it's safe to choose the MCRA).
For the case when `x` occured in both children (can only be `Both` or `Or`):


1. It's safe to push graft points down both branches of an `Or`, as this can't destroy sharing. The only gripe is that the tree grows linear in the number of `Or` nodes with this property compared to the MCRA or plain **LetUp** approach.
1. It's not generally safe push graft points down both branches of a `Both`: `(Both (Var x <S,1*U>) (Var x <S,1*U>))` was an earlier example that proofs that.


So, by additionally pushing down graft points along both branches of an `Or` node, if needed, we can handle cases like the above. 
We buy increased precision by possibly super-linear space complexity.


## Distributing `Both` over `Or`



There's one additional trick we can apply for ... a simpler model of the `DmdEnv'`?



In order to arrive at a disjunctive normal form-like model for `DmdEnv'`, we can apply the distributive law `(Both env1 (Or env2 env3) = (Or (Both env1 env2) (Both env1 env3))`.
This allows for the following `DmdEnv'`:


```wiki
newtype DmdEnv'' = DNF [DmdEnv]

flattenDmdEnv'' :: DmdEnv'' -> DmdEnv
flattenDmdEnv'' (DNF envs) =
  lubDmdEnvs envs

dnf :: DmdEnv' -> DmdEnv''
dnf = DNF . go
  where
    go env =
      case env of
      Empty -> [emptyDmdEnv]
      Var id dmd -> [unitDmdEnv id dmd]
      Or env1 env2 -> go env1 ++ go env2
      Both env1 env2 -> concatMap (\env1 -> map (bothDmdEnv env1) (go env2)) (go env1) -- ugh
```


`dnf` shows the biggest drawback of this: This is pretty much **exponential** in the number `Both` nodes.



'Substitution' is pretty easy here: Because there are no `Both` nodes (or rather that the top-level is `Or` only), we can just substitute into each other.



*sgraf: Actually, I'm not sure what this buys us, other than simplicity of implementation. 
And performance-wise, I have a strong feeling that this blows up pretty fast. 
Effectively, we model **every possible path of execution** separately.
It's the same as in logic, I guess: DNF is always possible, and easily handled, but conversion blows up exponentially in general.*


## `Termination`



The previous ideas greatly simplify by ignoring `Termation`/`DmdResult`s. 
In fact, the bugs encountered while implementing the first proposal were all due to complex interplay with `Termination`.



Consider the two `DmdType`s `{}` (e.g. empty, terminating) and `{y-><S,1*U>}x` (uses `y` exactly once and throws, like in `error y`).
When these are `lub`ed, we want the `DmdType` `{y-><L,1*U>}` (uses `y` at most once, possibly terminating) as a result.



The status quo is to compute the new `DmdEnv` by `plusVarEnv_CD lubDmd env1 (defaultDmd res1) env2 (defaultDmd res2)`.


- Where \*either\* the left or right `env*` is defined, we compute a new entry by `lub`ing, using `defaultDmd res*` (where `res*` is the `DmdResult` of the corresponding `DmdType`) when there is no entry in one of the `env*`s. 
- When there is no entry in either `env*`, then there won't be in the result!


Since this approach gets its precision from delaying `lub`s as long as possible, we have to store `Termination` information in the `DmdEnv'`/`DmdEnv''` data structure. 
At the least, we have to tag the children of `Or` nodes, for the first two approaches (`DmdEnv'`) we also have to tag `Both` nodes, e.g.


```wiki
data DmdEnv'
  = Empty
  | Var Id Demand
  | Or DmdEnv' (Termination ()) DmdEnv' (Termination ())
  | Both DmdEnv' (Termination ()) DmdEnv' (Termination ())

newtype DmdEnv''
  = DNF [(DmdEnv, Termination ())] -- Not quite, see below
```


Variables not mentioned in either `lub` operand are assumed to be used according to `defaultDmd res`, where `res` is the `DmdResult` currently part of the containing `DmdType`.
Note that due to `ExnStr`/`catch#`, the `defaultDmd res` can change after computing the actual `lub` above, so in general `defaultDmd res*` and `defaultDmd res` can vary independently!



That's also why \[`findIdDemand`\]([
https://github.com/sgraf812/ghc/blob/922db3dac896b8cf364c9ebaebf1a27c2468c709/compiler/basicTypes/Demand.hs\#L1577](https://github.com/sgraf812/ghc/blob/922db3dac896b8cf364c9ebaebf1a27c2468c709/compiler/basicTypes/Demand.hs#L1577)) in `Demand.hs` takes a `DmdResult` as argument.



And that's also why the above definition for `DmdEnv''` is incorrect!
It assumes that a single `Termination` result per `lub` operand is enough, which isn't the case, as the following continued example shows:



We started with `lubDmdType {} {y-><S,1*U>}x = {y-><L,1*U>}` (in terms of the status quo). 
Now, `lub` that again with `{z-><S,1*U>}` to get `{y-><L,1*U>,z-><L,1*U>}`.



So far so good. 
The DNF would look something like `[({},Dunno ()), ({y-><S,1*U>},ThrowsExn), ({z-><S,1*U>},Dunno ())]`, which `flattenDmdEnv''`s to pretty much the same `DmdEnv` as in the `DmdType` above.



Now consider what happens when `Termination` changed in-between the two `lub`s, e.g. because the first result of `lubDmdType` was an argument to `error`:


```wiki
  lubDmdType (markThrowsExn (lubDmdType {} {y-><S,1*U>}x)) {z-><S,1*U>}
= lubDmdType (markThrowsExn {y-><L,1*U>}                 ) {z-><S,1*U>}
= lubDmdType  {y-><L,1*U>}x                                {z-><S,1*U>}
= {y-><L,1*U>,z-><S,1*U>}
```


Note that, different to before, `z` is now evaluated strictly!
In our current DNF formulation, we wouldn't be able to express the difference.



The problem here is that `lub` is no longer associative, because it depends on the `Termination` at the time of the call.
So, we can't actually model `DmdEnv''` as a DNF, but have to resort to


```wiki
data DmdEnv''
  = Lit DmdEnv
  | Or DmdEnv'' (Termination ()) DmdEnv'' (Termination ())
```


Which is only marginally less complex than a version of `DmdEnv'` where we subsume the `Empty` and `Var` cases with an equivalent `Lit` case:


```wiki
data DmdEnv'
  = Lit DmdEnv
  | Or DmdEnv' (Termination ()) DmdEnv' (Termination ())
  | Both DmdEnv' (Termination ()) DmdEnv' (Termination ())

dnf :: DmdEnv' -> DmdEnv''
dnf (DmdEnv'.Lit env) = DmdEnv''.Lit env                               -- hypothetical disambiguating syntax
dnf (DmdEnv'.Or fv1 t1 fv2 t2) = DmdEnv''.Or (dnf fv1) t1 (dnf fv2) t2
dnf (DmdEnv'.Both fv1 t1 fv2 t2) = distr (dnf fv1) t1 (dnf fv2) t2 -- case in point
  where
    -- both env1 with defaultDmd t1 into every Lit of fv2
    distr (DmdEnv''.Lit env1) t1 fv2 t2 = undefined
    -- both env2 with defaultDmd t2 into every Lit of fv1
    distr fv1 t1 (DmdEnv''.Lit env2) t2 = undefined
    -- Not exactly sure how to compute this. 
    -- This case leads to the exponential explosion, 
    -- as we have to combine every Lit in fv1 with every Lit in fv2.
    -- This gets insane as we have to keep in mind which `Termination` to use for each point
    -- in each `Lit` separately.
    distr DmdEnv''.Or{} t1 DmdEnv''.Or{} t2 = undefined
```


`dnf` mirrors how `bothDmdEnv'` would have to change in the current implementation to get to the DNF-like version.



In fact, the currently working prototype of the first proposal had a complicated bug in the [
\`lookupDmdTree\`](https://github.com/sgraf812/ghc/blob/43c045249402319170fa421438a1f05887269a26/compiler/basicTypes/Demand.hs#L1287) implementation, which forced me to think about how the lookup of a single point can potentially access \*all\* `Termination ()` tags on the path to a `Lit` it is contained in.
This also applies to the implementation of `dnf`: I'm pretty sure the step that distributes `Both` over both sub trees needs to model all points separately to know with which `Termination` to `both`.
Sorry if this is a little incomprehensible without an example, but this is already pretty deep down the rabbit hole.



The bottom-line is that there still is some DNF-like structure possible, but in addition to the incurred exponential space complexity, there's also the issue of how to distribute `Both` nodes into leafs of the Or-tree without losing sanity to the implementation.


## Evaluation


### Both/Or trees, grafted at MCRAs



The current [
working prototype of the first proposal](https://github.com/sgraf812/ghc/blob/43c045249402319170fa421438a1f05887269a26/compiler/basicTypes/Demand.hs) (progress tracked at [
\`and-or\` branch](https://github.com/sgraf812/ghc/blob/and-or/compiler/basicTypes/Demand.hs)) passes `./validate` and nofib.



GHC is built with 0.1% more allocations and 7.1% (!) more executed instructions (cachegrind). 
There may still be potential in optimizing the `DmdTree` (called `DmdEnv'` on this page) representation in its smart constructors (`bothDmdTree`, `lubDmdTree`) to avoid big trees.



Running nofib revealed two benchmarks where allocation changed, `fft2` with -0.1% and `transform` with -0.0%.
Changes in counted instructions where all around +-0.0%, with the exception of `fft2` (-0.2%).
Seems like a net gain, but pretty much insignificant, especially compared to the significant increase in compilation time.


### Both/Or trees, pushed into Or



The implementation of the second proposal can be found [
here](https://github.com/sgraf812/ghc/blob/6bd6ab3c521de5dfa4042642c767b7906315ca28/compiler/basicTypes/Demand.hs) (progress tracked at [
\`and-or-distr\` branch](https://github.com/sgraf812/ghc/blob/and-or-distr/compiler/basicTypes/Demand.hs).
This was a [
minimal change](https://github.com/sgraf812/ghc/commit/6bd6ab3c521de5dfa4042642c767b7906315ca28) compared to the single graft point solution, with great repercussions:



Compilation of modules with big records (looking at you, [
\`Distribution.Types.BuildInfo\`](https://github.com/haskell/cabal/blob/4726b2ada46a4f0757ec4fcaf5508c40faa98307/Cabal/Distribution/Types/BuildInfo.hs) eats up all resources on my machine without coming to an end.
Given the minimal invasive change, this can only be due to combinatorial explosion.
Some debugging pinned this down to derived `Eq` and `Show` instances, where the generated `DmdTree`s grow bigger than e.g. 10000 nodes.
I'm not entirely sure what causes the duplication that leads to the slowdown, so I tried falling back to the status quo for dictionary Ids (like we do for unlifted lets anyway) to no avail.


### DNF



When trying to implement this, I stumbled on the problems wrt. `Termination` outlined above.
Also considering that the space complexity of this approach will be even less predictable, I've put the implementation of this is on ice for now.


