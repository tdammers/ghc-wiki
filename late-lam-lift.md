# Late lambda lifting





## Overview



Late Lambda Lifting is a not-very-well-explored optimisation for GHC. The basic idea is to transform


```wiki
f x = let g y = ...y...x...
      in ...(g v1)...(g v2)...
===>
g' x y = ...y...x...
g x = ...(g' x v1)...(g' x v2)...
```


That is, pretty standard lambda-lifting.  The idea is that instead of *allocating a closure* for `g`, we pass extra parameter(s) to it.  More below, under "Background".



Since heap allocation is expensive, this has the possibility of making programs run faster.
The challenge is all about getting consistent speedups.



There is a ticket to track progress: [\#9476](http://gitlabghc.nibbler/ghc/ghc/issues/9476). There's a paper in the making at [
https://github.com/sgraf812/late-lam-lift/blob/master/paper.pdf](https://github.com/sgraf812/late-lam-lift/blob/master/paper.pdf).


## Tickets



Use Keyword = `LateLamLift` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#9279](http://gitlabghc.nibbler/ghc/ghc/issues/9279)</th>
<td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
<tr><th>[\#9374](http://gitlabghc.nibbler/ghc/ghc/issues/9374)</th>
<td>Investigate Static Argument Transformation</td></tr></table>




**Closed Tickets:**  NB: closed tickets may be closed as duplicates; but may still have very illuminating test cases.

<table><tr><th>[\#5945](http://gitlabghc.nibbler/ghc/ghc/issues/5945)</th>
<td>Lambda lifting</td></tr>
<tr><th>[\#9476](http://gitlabghc.nibbler/ghc/ghc/issues/9476)</th>
<td>Implement late lambda-lifting</td></tr></table>



## Implementation



The implementation is spread over three modules:


```wiki
StgLiftLams
├── Analysis
├── LiftM
└── Transformation
```


`Transformation` contains the actual lambda-lifting implementation on STG 
programs. For the decision of whether or not to lift a binding, it calls out
into `Analysis.goodToLift`. The transformation produces and ultimately runs
effects in `LiftM`, which nicely encapsulates nitty gritty details such as
dealing with substitution, conjuring fresh binders, cost centre stacks and
caffyness.


### Flags



Late lambda lifting is configurable via these flags:


```wiki
data GeneralFlag =
   ...
   | Opt_StgLiftLam -- ^ Enable late lambda lifting. 
                    --   -fstg-lift-lams -- Enabled by default in -O*.
   ...

data DynFlags = DynFlags {
  ...
  liftLamsRecArgs       :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   recursive function.
                                        --   -fstg-lift-lams-rec-args
  liftLamsNonRecArgs    :: Maybe Int,   -- ^ Maximum number of arguments after lambda lifting a
                                        --   non-recursive function.
                                        --   -fstg-lift-lams-nonrec-args
  liftLamsKnown         :: Bool,        -- ^ Lambda lift even when this turns a known call
                                        --   into an unknown call.
                                        --   -fstg-lift-lams-known
  ...
}
```

## Short wrapup on history



The original work by Nicolas Frisby started out in 2013 as part of his internship on the `wip/llf` branch.
Sebastian Graf has rebased this branch in mid April 2018. After some debugging
and fixups, it passed `./validate` (modulo some compiler perf tests) in
[
c1f16ac](https://github.com/sgraf812/ghc/tree/c1f16ac245ca8f8c8452a5b3c1f116237adcb577).
The most recent variant of Nicolas' original Core transformation can be found
here: [
https://github.com/sgraf812/ghc/tree/llf](https://github.com/sgraf812/ghc/tree/llf).



In July 2018, [
Sebastian argued](https://ghc.haskell.org/trac/ghc/ticket/9476#comment:15)
that it's probably a good idea to reimplement the transformation on STG instead
of Core, the promising implementation of which is available
[
here](https://github.com/sgraf812/ghc/tree/9b9260c1d45d127edf9ebdfe04a3daaff24a9dea/compiler/simplStg/StgLiftLams).



If you want to know more about the original work on Core, look into the history
of this Wiki page (before October 2018).


## Background



The primary objective of late lambda lifting is to replace
heap allocation of closures by an increase in the number of arguments
passed, by floating local functions to top-level.



The following examples aren't strictly in STG, so assume that all
non-trivial arguments are given a name and all closures are properly
annotated with their free variables.



Consider lambda-lifting `f` out of `before`.


```wiki
before a b =
  let f x = RHS[f, b, x]
  in BODY[f, a, b]
```


Lifting `f` out of `before` would yield:


```wiki
$lf b x = RHS[$lf b, b, x]

after a b = BODY[$lf b, a, b]
```


Note that all occurrences of `f` in `RHS` and `BODY` have been
replaced by the entire expression.



`before1` (an instance of `before`) is the basic motivating case for
the LLF.


```wiki
before1 a b c =
  let f1 x y = (b + x) + (c + y)
  in f1 a b * f1 c a
```

```wiki
$lf1 b c x y = (b + x) + (c + y)

after1 a b c =
  $lf1 b c a b * $lf1 b c c a
```


In this case, `after1` allocates less on the heap than does
`before1`. `before1` allocates a closure for `f1` and then enters it
twice. `after1` does no such allocation, but it does pass more arguments
when entering the statically allocated `$f1`. If `before1` is called
numerous times, then this transformation could drastically reduce the
overall program allocation. The run-time effect of the additional
arguments may be negligible.


## When to lift



The considerations of this section can be found in `StgLiftLams.Analysis`.


### Why lambda-lift late?



We lift late in the pipeline primarily because lambda-lifting is known
to drastically interfere with many of the other optimizations in the pipeline.



Ultimately, we lambda-lift at the end of the STG pipeline, just before
code gen. In fact, we like to think about LLF as a *code gen strategy*
that turns closures into extra arguments *when doing so is viable*.


### Syntactic Consequences of Lifting



Lifting a dynamic function closure has four immediate consequences.


>
>
> **C1** It eliminates the let.
>
>

>
>
> **C2** It creates a new top-level definition.
>
>

>
>
> **C3** It replaces all occurrences of the lifted function in the let's
> body with a (partial) application. E.g., all occurences of `f1` were
> replaced by `$lf1 b`.
>
>

>
>
> **C4** All non-top-level variables that occured in the let's RHS become
> occurrences of parameters.
>
>

### Operational Consequences of Lifting



The resulting operational consequences can be good or bad, both statically and
dynamically. These consequences finally lead to a number of heuristics,
implemented in `StgLiftLams.Analysis.goodToLift`.


- C1 reduces heap allocation, unless the function is let-no-escape (LNE),
  which don't allocate to begin with. Consequently, we don't lift LNEs.

- C3 might increase heap allocation and run-time by creating PAPs if
  the lifted function was previously a function argument. While this won't
  *always* increase allocation, it certainly will only improve allocations
  if this pushes allocation away from the hot path into some cold leaf.
  We don't allow this, because it also significantly complicates the
  transformation for arguably very questionable gain.

- C3 might increase or decrease heap allocation. If `f` occurs in
  a closure in the let body, then

>
> >
> >
> > A) it can increase heap allocation, if the additional arguments to
> > `f` did not previously occur in that closure
> >
> >
>

>
> >
> >
> > B) it can decrease heap allocation, if those additional arguments
> > did already occur in the closure.
> >
> >
>

>
>
> We call the sum result of A and B *closure growth*. Estimating closure
> growth is paramount to achieving unambiguously good results.
> See "Closure Growth" below.
>
>

- C4 might increase stack allocation and/or increase register
  pressure, depending on how many arguments the new top-level function
  function would take. We determined 5 extra arguments (the number of
  available free argument registers on x86\_64) to be a sweet spot for cut off.

- C4 might cause a slowdown because it can convert known calls into
  unknown calls when the lifted function previously closed over a
  function.
  We don't perform the lift in this case.


Finally, we obviously only lift functions, no closures or constructor
applications.


### Closure Growth



Both A and B above could hold within the same let body. Moreover, A or B
could hold wrt a closure that is under a (multi-shot) lambda; so that
the changes in heap allocation could happen 0 or billions of
times. The conservative choice, then, is that A under a lambda
infinitely increases the heap allocation and B under a lambda is
ignored.



We look at demand information for a more precise, yet still conservative
estimate: Usage information can be harnessed to identify one-shot bodies
for A under lambdas, while strictness information can be used to propagate
the benefits of B under lambdas.



`StgLiftLams.Analysis.closureGrowth` estimates closure growth, operating
on a *skeleton* of the actual STG tree. Skeletons are an abstraction, only
capturing the relevant syntactic constructs. E.g., closures with their fvs,
RHSs with relative cardinality, sequential composition, choice and a neutral
element.
This is so that not every lifting decision has to traverse the entire syntax
tree, while also keeping `closureGrowth` plain and simple. To this purpose, each
let binder of the STG syntax tree is tagged with the Skeleton of its RHS.



From the resulting closure growth, the effects of C1 are subtracted to determine
whether the lift would be beneficial.


