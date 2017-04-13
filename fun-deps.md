# Functional Dependencies



This is the root page for material about functional dependencies in GHC.


## Tickets



Use Keyword = `FunDeps` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#816](http://gitlabghc.nibbler/ghc/ghc/issues/816)</th>
<td>Weird fundep behavior (with -fallow-undecidable-instances)</td></tr>
<tr><th>[\#9210](http://gitlabghc.nibbler/ghc/ghc/issues/9210)</th>
<td>"overlapping instances" through FunctionalDependencies</td></tr>
<tr><th>[\#10675](http://gitlabghc.nibbler/ghc/ghc/issues/10675)</th>
<td>GHC does not check the functional dependency consistency condition correctly</td></tr>
<tr><th>[\#11534](http://gitlabghc.nibbler/ghc/ghc/issues/11534)</th>
<td>Allow class associated types to reference functional dependencies</td></tr>
<tr><th>[\#11641](http://gitlabghc.nibbler/ghc/ghc/issues/11641)</th>
<td>Allow wildcards for parameters functionally determined (also type synonyms)</td></tr>
<tr><th>[\#11655](http://gitlabghc.nibbler/ghc/ghc/issues/11655)</th>
<td>Ambiguous types in pattern synonym not determined by functional dependencies</td></tr>
<tr><th>[\#12647](http://gitlabghc.nibbler/ghc/ghc/issues/12647)</th>
<td>Can't capture improvement of functional dependencies</td></tr>
<tr><th>[\#12704](http://gitlabghc.nibbler/ghc/ghc/issues/12704)</th>
<td>Check if constraint synonym satisfies functional dependencies</td></tr>
<tr><th>[\#12860](http://gitlabghc.nibbler/ghc/ghc/issues/12860)</th>
<td>GeneralizedNewtypeDeriving + MultiParamTypeClasses sends typechecker into an infinite loop</td></tr>
<tr><th>[\#14745](http://gitlabghc.nibbler/ghc/ghc/issues/14745)</th>
<td>Functional dependency conflicts in givens</td></tr>
<tr><th>[\#14778](http://gitlabghc.nibbler/ghc/ghc/issues/14778)</th>
<td>FunDep origin not correctly attributed</td></tr>
<tr><th>[\#15632](http://gitlabghc.nibbler/ghc/ghc/issues/15632)</th>
<td>Undependable Dependencies</td></tr>
<tr><th>[\#15927](http://gitlabghc.nibbler/ghc/ghc/issues/15927)</th>
<td>Weird interaction between fundeps and overlappable instances</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#3108](http://gitlabghc.nibbler/ghc/ghc/issues/3108)</th>
<td>Do a better job of solving recursive type-class constraints with functional dependencies</td></tr>
<tr><th>[\#7128](http://gitlabghc.nibbler/ghc/ghc/issues/7128)</th>
<td>Panic "lookupVarEnv\_NF" when using a functional dependency with a kind variable</td></tr>
<tr><th>[\#7171](http://gitlabghc.nibbler/ghc/ghc/issues/7171)</th>
<td>erroneous overlapping instances reported with FunDeps</td></tr>
<tr><th>[\#7384](http://gitlabghc.nibbler/ghc/ghc/issues/7384)</th>
<td>Panic "ctEvTerm" when using functional dependencies and PolyKinds</td></tr>
<tr><th>[\#7777](http://gitlabghc.nibbler/ghc/ghc/issues/7777)</th>
<td>ghc panic: varargs + sets</td></tr>
<tr><th>[\#10109](http://gitlabghc.nibbler/ghc/ghc/issues/10109)</th>
<td>Kinds aren't checked in the coverage condition</td></tr>
<tr><th>[\#10564](http://gitlabghc.nibbler/ghc/ghc/issues/10564)</th>
<td>GHC 7.10.2 RC cannot build HList-0.4.0.0</td></tr>
<tr><th>[\#10570](http://gitlabghc.nibbler/ghc/ghc/issues/10570)</th>
<td>Terrible error message with fundeps and PolyKinds</td></tr>
<tr><th>[\#10797](http://gitlabghc.nibbler/ghc/ghc/issues/10797)</th>
<td>Kind-level functional dependencies are not resolved properly</td></tr>
<tr><th>[\#12763](http://gitlabghc.nibbler/ghc/ghc/issues/12763)</th>
<td>Incorrect behavior with empty functional dependencies</td></tr>
<tr><th>[\#13506](http://gitlabghc.nibbler/ghc/ghc/issues/13506)</th>
<td>Spurious extra error message due to functional dependencies</td></tr>
<tr><th>[\#13774](http://gitlabghc.nibbler/ghc/ghc/issues/13774)</th>
<td>Singletons code fails to typecheck when type signature involving type family is added</td></tr>
<tr><th>[\#14327](http://gitlabghc.nibbler/ghc/ghc/issues/14327)</th>
<td>Type error in program caused by unrelated definition</td></tr>
<tr><th>[\#14763](http://gitlabghc.nibbler/ghc/ghc/issues/14763)</th>
<td>GHC 8.4.1-alpha regression with FunctionalDependencies</td></tr>
<tr><th>[\#15355](http://gitlabghc.nibbler/ghc/ghc/issues/15355)</th>
<td>Functional dependencies can get GHC to print "UnkSkol"</td></tr></table>



## Consistency of Functional Dependencies



(This section was written by Iavor; I am unsure of its current status. SLPJ Apr 17.)



The functional dependencies on a class restrict the instances that may
be declared for a given class.  The instances of a class:


```wiki
    class C a b c | b -> c where f :: ...
```


are consistent with its functional dependency if the following invariant holds:


```wiki
    byFD(C,1): forall a1 a2 b c1 c2. (C a1 b c1, C a2 b c2) => (c1 ~ c2)
```


Please note that the question of FD-consistency is orthogonal to
instance coherence (i.e, uniqueness of evidence, overlapping instances,
etc.), and the decidability of type-checking of terms---for examples
of their independence, please see the `Examples` at the end of the document.



If we check that all instances in scope are consistent with their FDs,
then we can use the FD invariant `byFD` during type inference to
infer more precise types, report errors involving unsolvable contexts,
or accept programs that would be rejected without the invariant.



For example, if we have an instance:


```wiki
  I: instance F Int Int Char
```


and we have a constraint `C: F Int Int a`, then we can use the
FD-invariant to prove that `a` must be `Char`:


```wiki
    byFD(C,1) (C,I) :: a ~ Char
```

### Checking FD-Consistency



To ensure FD-consistency, before accepting an instance we need to check
that it is compatible with all other instances that are already in
scope.  Note that we also need to perform the same check when combining
imported instances.  Consider adding a new instance to an FD-consistent
set of instances:


```wiki
    I: instance P(as,bs) => C t1(as,bs) t2(as) t3(as,bs)
```


The notation `t(as)` states the variables in `t` are a subset of `as`.


1. Check that `I` is self-consistent (i.e., we can't use different
  instantiations of `I` to violate FD-consistency).  Self consistency
  follows if we can prove the following theorem:

  ```wiki
           forall as bs cs. (P, P[cs/bs]) => t3[cs/bs] ~ t3[cs/bs]
  ```
1. Check that `I` is FD-consistent with all existing instances of the class.
  So, for each existing instance, `J`:

  ```wiki
           J: instance Q(xs) => C s1(xs) s2(xs) s3(xs)
  ```

  we need to show that:

  ```wiki
           forall as bs xs. (P,Q,s2 ~ t2) => (s3 ~ t3)
  ```

  Assuming no type-functions in instance heads, the equality
  assumption is equivalent to stating that `s2` and `t2` may be
  unified, so another way to state our goal is:

  ```wiki
           forall as bs xs. (P[su], Q[su]) => (s3[su] ~ t3[su])
  ```

  where `su` is the most general unifier of `s2` and `t2`.


Proving these two goals before accepting an instance is similar to
the process of finding evidence for super-classes before accepting
and instance.  Also, note that while proving (2), it is not a problem
if we find that we have assumed a contradiction:  this simply means
that the two instances can never be used at the same time, so
the FD-consistency follows trivially.


### Examples


- FD-consistency is orthogonal to instance coherence.

>
> >
> >
> > FD-consistent but not coherent:
> >
> >
> > ```wiki
> >          instance C Int Int Int where f = definition_1
> >          instance C Int Int Int where f = definition_2
> > ```
> >
> >
> > Coherent but not FD-consistent:
> >
> >
> > ```wiki
> >          instance C Int  Int Char where ...
> >          instance C Char Int Bool where ...
> > ```
>
>

- FD-consistency is orthogonal to termination of instances.

>
> >
> > >
> > >
> > > FD-consistent but "non-terminating":
> > >
> > >
> > > ```wiki
> > >          instance C a b c => C a b c
> > > ```
> > >
> > >
> > > Terminating but not FD-consistent:
> > >
> > >
> > > ```wiki
> > >          instance C Int  Int Char where ...
> > >          instance C Char Int Bool where ...
> > > ```
> >
> >
>

