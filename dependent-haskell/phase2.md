# Implementing Dependent Haskell, Phase 2



This page is to track design ideas and questions in the second phase of implementing Dependent Haskell, which is the development of a dependently typed Core. Phase 3 will include modifying surface Haskell and implementing type inference.


## Phase 2a: Homogeneous equality



The "System FC with Explicit Kind Equality" paper (ICFP'13) describes a *heterogeneous* equality: that is, we can form `a ~# b` even when `a :: k1` and `b :: k2`. A proof of `a ~# b` implies both that `k1` equals `k2` and that `a` equals `b`. This choice was necessitated by, among other things, the unusual binding structure in that paper's coercion between forall-types.



During the implementation of `TypeInType` (directly based on that paper), we realized several simplifications:


1. We do not need to make the binding structure of forall-coercions so strange. Instead, we can use an asymmetrical rule:

```wiki
G |- g1 : t1 ~ t3
G,a:k1 |- g2 : t2 ~ t4
------------------------------------------------------------------------
G |- forall a:g1.g2 : (forall a:t1.t2) ~ (forall b:t3.t4[b |> sym g1/a])
```

>
>
> Though it's asymmetrical, it's far simpler than the rule in the ICFP'13 paper. This is the rule implemented in GHC 8.x.
>
>

1. The ICFP'13 paper allows the binding of coercion variables in types. That is, we can have `forall c:phi.t` as a type. However, the need for this in practice was slight, and so it was removed from the implementation.


With the simpler (asymmetrical) forall-coercion rule above, one of the primary motivations for heterogeneous equality was removed. And so, in [
A Specification for Dependent Types in Haskell](https://cs.brynmawr.edu/~rae/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf) (ICFP'17), we use more of a mixed economy of heterogeneity: a coercion can still related two types of different kinds, but coercion *variables* must be homogeneous. That is, if `c :: t1 ~# t2`, then `t1` and `t2` have equal (that is, alpha-equivalent) kinds. But if a coercion `g` relates `t1` and `t2`, then `t1` and `t2` might have different kinds `k1` and `k2`. However, we can always extract a proof that `k1 ~# k2` from `g`.



We propose to make this change to GHC too.


### Why homogeneous equality is good



Homogeneous equality is simpler than heterogeneous equality so, all else being equal, it's better to have homogeneous equality. However, even beyond simplicity, we are able to prove **congruence** only with the homogeneous variant. Richard's thesis uses heterogeneous equality, and he was unable to prove congruence there. The ICFP'17 paper, with homogeneous equality, proves congruence. (It's called substitutivity there.) So this seems like a nice step forward. 



Congruence means that if a type `t` has a free variable `a`, and we have a coercion `co` proving `s1` equals `s2` (and `s1` and `s2` have the same kind), then we can always find a proof that `t[s1/a]` equals `t[s2/a]`. The lack of congruence has no effect in the current (GHC 8.6) implementation because it bites only in the presence of coercion quantification in types. See Section 5.8.5.4 of Richard's thesis. Given that we will need coercion quantification in types in order to have full dependent types \[example needed\], we will want congruence, too.



More practically, congruence is what allows `liftCoSubst` to function. The details aren't germane here, but `liftCoSubst` produces a coercion, which is assumedly well-typed. However, we cannot prove that it is indeed well-typed with heterogeneous equality: this is precisely what the congruence theorem claims. So, currently, the use of this function might induce a Core Lint error (see `almostDevoidCoVarOfCo` check in `ty_co_subst` in Coercion).  With homogeneous equality, we can prove that this won't be the case.


### What would homogeneous equality look like in GHC?



How should this homogeneous equality take form? It's simple: make `~#`, the type of primitive equality, homogeneous. That is, we want


```
(~#) :: forall k. k -> k -> TYPE (TupleRep '[])
```


(The return kind says that equality proofs take up 0 bits at runtime.) All coercion variables must have a kind headed by `~#`. With the new kind of `~#`, then we effectively make all assumptions homogeneous. (Axioms were already, and have always been, homogeneous.) 



However, *coercions themselves can remain heterogeneous*, that is, a coercion can witness equality between two types of different kinds.  
Some things follow from this:


- It does not make sense to ask for the "type of a coercion", `coercionType :: Coercion -> Type`.  For a heterogeneous coercion, we can't give it type `t1 ~# t2` because `(~#)` is no homogeneous.

- Instead we have only

  ```
  coercionKind :: Coercion -> Pair Type   -- the two types might have different kinds
  ```

>
>
> which explicitly returns two types separately.   We may informally write `co :: t1 = t2` to say that `co` witnesses the equality between `t1` and `t2`, so that `coercionKind co = Pair t1 t2`.
>
>

- From a coercion `co :: (t1::k2) = (t2::k2)` we can get a coercion `kco :: k1 = k2`:

  ```
  -- if Pair t1 t2 = coercionKind co, k1 = typeKind t1, and k2 = typeKind t2, then
  -- Pair k1 k2 = corcionKind (promoteCoercion co)
  promoteCoercion :: Coercion -> Coercion 
  ```

>
>
> `promoteCoercion` is a function that transforms one coercion (tree) into another; it is no longer a coercion constructor (i.e. the existing `KindCo` vanishes).
>
>


Summarising (details in  [
A specification of dependent types for Haskell](https://cs.brynmawr.edu/~rae/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf)):


- A coercion *variable* (which has a type `t1 ~# t2`) must be homogeneous
- A *coercion* can be heterogeneous.  It does not have a type.

### A small wrinkle: we need coercion quantification back



If `~#` is homogeneous in Core, then how do we support heterogeneous equality in Haskell? Heterogeneous equality is important in Haskell to support, for example, the new `TypeRep` (see [
the paper](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1002&context=compsci_pubs)).   Easy: just use an equality between the kinds and then one between the types. But it's not so easy in practice. Examine the definition of `:~~:`:


```
-- this version is wrong!
data (:~~:) :: forall k1 k2. k1 -> k2 -> Type where
  MkHEq :: forall k1 k2 (a :: k1) (b :: k2). 
           (k1 ~# k2) -> (a ~# b) -> a :~~: b
```


Sadly, this is ill-kinded: we're using `a ~# b` even though `a` and `b` have different kinds. Of course, we know that `k1` and `k2` are the same, but that doesn't quite help us here. Instead, we need to *name* the coercion between `k1` and `k2`, thus:


```
data (:~~:) :: forall k1 k2. k1 -> k2 -> Type where
  MkHEq :: forall k1 k2 (a :: k1) (b :: k2). 
           forall (co :: k1 ~# k2) -> 
           ((a |> co) ~# b) -> a :~~: b
```


This version names the kind coercion `co` so it can be used in the type proposition. All is well again. Sadly, the implementation does not support coercion quantification in types like this. 



Note that, while heterogeneous equality `(:~~:)` is, in a sense, the canonical motivation for coercion quantification (in that, if we can implement `(:~~:)`, we can implement anything else), it's not the only place coercion quantification would appear. *Any* kind-indexed GADT will need it. For example:


```
data Rep :: forall k. k -> Type where
  RepBool :: Rep Bool
  RepMaybe :: Rep Maybe
```


becomes the following in Core:


```wiki
Rep :: forall k. k -> Type
RepBool :: forall k (a :: k). forall (cv :: k ~# Type). ((a |> cv) ~# Bool) -> Rep k a
RepMaybe :: forall k (a :: k). forall (cv :: k ~# (Type -> Type)). ((a |> cv) ~# Maybe) -> Rep k a
```


Note the coercion quantification.



Bottom line: a consequence of switching to homogeneous equality is that we need coercion quantification in types.  So it's time to implement it.



Note that this currently proposes to use coercion quantification only in GADT types -- never elsewhere. In particular, the user will not be able to write a coercion-quantified type. Addressing this idea is ticket [\#15710](http://gitlabghc.nibbler/ghc/ghc/issues/15710), but it's not on the critical path toward homogeneous equality.


### Coercions are quantified both relevantly and dependently



Up until now, GHC has kept two ideas separate: relevance and dependence. 


- A variable quantified *relevantly* is preserved until runtime
- A variable quantified *dependently* is available for use in a type.


Term variables are relevantly quantified (with `FunTy`), while type variables are dependently quantified (with `ForAllTy`) and are irrelevant. 



Coercion variables today are relevant but not dependent.  (Even though they have 0 bits, a coercion variable is treated very much like a normal term-level variable.)
Now, though, need to be both dependent *and* relevant.  We thus have a problem:


```
typeKind (\ (x :: Nat). ...) = FunTy Nat ...
typeKind (\ (a :: Type). ...) = ForAllTy (a::Type) ...
typeKind (\ (c :: a ~# b). ...) = ????????
```


This choice is important, because we will need to check the type of an expression by comparing it to some other type with `eqType`. We cannot make arbitrary decisions between `ForAllTy` and `FunTy` here. Here is the design Simon and Richard (with Ningning's input) have come up with:


- The type of a coercion-lambda shall be a `ForAllTy` iff the coercion variable is mentioned in the result type. It shall be a `FunTy` iff the coercion variable is not mentioned in the result type.


Equivalently:


- INVARIANT: If a `ForAllTy` quantifies over a coercion variable, that variable *must* be mentioned later in the type.


With these choices, we never have to equate `ForAllTy`s with `FunTy`s in, say, `eqType`. The downside to this design is that it is non-performant: we must do a free-variable check when building `ForAllTy`s to maintain the invariant. However, the key observation here is that we need to do this check only when building a `ForAllTy` with a coercion variable -- *something we never do today*. So it will be rare in the near future. And, we should be able to easily distinguish when we're about to quantify over a coercion, so the normal `mkForAllTy` can just `ASSERT` that its variable is a tyvar. We'll have a new `mkTyCoForAllTy` that quantifies over coercions and does the free-variable check.



We believe that, in the future, `FunTy` and `ForAllTy` may merge into `PiTy`. That future is not yet here, and there's no need to rush it. 


### Problem of defining heterogeneous equality based on homogeneous equality



Previously morally we have


```
class (a ~# b) => a ~~ b
```


Under the new definition with coercion quantification, it becomes something like


```
class (co :: k1 ~# k2, (a |> co) ~# b) => a ~~ b
```


where the superclass `(a |> co) ~# b` refers to `co`, so we add `co :: k1 ~# k2` into the superclass, otherwise we get `co` out of scope.



This definition is, however, wrong, as we cannot have existential variables in superclass. (Skip to the solution to these problems by searching for "Plan of record".)



**Problem with existential variables in superclass.** Let's consider the example


```
class (C a, D a b) => E a where 
  mkE = ...
```


For every instance of any class, we should be able to extract the instance of its superclass from it. Thus we define


```
selD :: E a -> D a b
selD (mkE x y) = y
```


Once we use `mkE`, the type variable `b` is instantiated to some type (e.g. `Int`), but we have no way to recover it. Instead, now the return value is of type `forall b. D a b`, which does not make sense!



**Proposed solution.**
The above discussion rests on the idea that we treat `(~~)` uniformly with other classes. But perhaps (at least in the short term), we don't have to. Specifically, when we access the superclasses of a class constraint in the solver, we can treat `(~~)` specially.



If we have a Given `(~~)`, we can unpack both a kind equality and a type equality from it. Because doing so requires a case-match instead of a `let`, we need a new form of `EvBind`:


```
data EvBind
     -- this first one is old
  = EvBind { eb_lhs      :: EvVar
           , eb_rhs      :: EvTerm
           , eb_is_given :: Bool  -- True <=> given
    }

     -- this one is new (always given)
  | HeteroEqEvBind { eb_kind_eq :: EvVar
                   , eb_type_eq :: EvVar
                   , eb_rhs     :: EvTerm  -- scrutinee of the case
                   }
```


The desugarer would produce a case expression for `HeteroEqEvBind`.



One challenge here is that `HeteroEqEvBind` binds *two* variables, and yet an `EvBindsMap` maps the variables to the bindings that bind them. This is OK: the new `HeteroEqEvBind` could simply be listed twice in the map. When iterating through an `EvBindsMap`, we'll just be careful not to, say, desugar it twice. We can do this easily by, say, ignoring the mapping from the kind var to the `HeteroEqEvBind` (because we know we'll see the `HeteroEqEvBind` when we look at the mapping from the type var).



For wanteds, the story is much easier: we just make a hetero equality constraint, which we'll have for wanteds anyway. (Recall that a homogeneous equality means that coercion *variables* will be homogeneous, but coercions may still be hetero.)



So this will take a little custom coding and a Note or two, but it seems easier than other approaches. This still requires that equality evidence be strict (causing [\#11197](http://gitlabghc.nibbler/ghc/ghc/issues/11197)), but the situation is no worse than today, and the approach outlined in [\#11197](http://gitlabghc.nibbler/ghc/ghc/issues/11197), of fixing it in FloatIn, still works.



*Small implementation wrinkle*: Desugaring a `HeteroEqEvBind` is harder than desugaring an `EvBind`. Currently, `EvBind`s are desugared into `CoreBind`s, which can be thought of as a `(Id, CoreExpr)`, that is, something that you can use to build a Core `let`. However, a `HeteroEqEvBind` is really a `case`, not a `let`. So how will we desugar it?



More concretely: `dsEvBinds :: Bag EvBind -> DsM [CoreBind]`. These `[CoreBind]` are then often passed into `mkCoreLets :: [CoreBind] -> CoreExpr -> CoreExpr`. In order to accommodate both `let`s and `case`s, we could just return `DsM (CoreExpr -> CoreExpr)` from `dsEvBinds`.



The problem with this approach is that it fails in `dsAbsBinds`, which includes a special case where the `[CoreBind]` are returned (search for the call to `flattenBinds` in `dsAbsBinds`. The special case happens when there are no local tyvars and no local dictionaries. This means that we don't need to make an abstraction to desugar `AbsBinds`. Any evidence bindings are floated out of the `AbsBinds`. This would be impossible at top-level if we have a `HeteroEqEvBind`, of course, because we can't use `case` to bind top-level evidence.



The solution is actually to ignore the problem. I conjecture that this invariant will hold: In an `AbsBinds`, if the `abs_tvs` and the `abs_ev_vars` are both empty, then there will be no `HeteroEqEvBind`s in the `abs_ev_binds`. Why? Because `HeteroEqEvBind` happens only for *givens*, never wanteds. And, with `abs_ev_vars` empty, there are no givens. This invariant should be checked and documented, but it smells right to me.



On a practical level, this means that `dsEvBinds` will have to, say, return both a `CoreExpr -> CoreExpr` for the common case and also a `[CoreBind]` for this special case in `dsAbsBinds`. The `[CoreBind]` would be bogus if there is a `HeteroEqEvBind`, but that won't happen in the `dsAbsBinds` special case because of the invariant. Perhaps a better design is available to avoid these strange return type; I'll leave it to Ningning to see if there is.



**Alternative approaches, now abandoned.** Adam suggests


```
class (a :: k1 ~~ b :: k2) where 
  MkHEq :: forall (c :: k1 ~# k2) . a |> c ~# b => a ~~ b
```


However we have no way to extract `(k1 ~#k2)` from this definition.



Stephanie suggests a CPS-style definition


```
class (a :: k1 ~~ b :: k2) where 
  MkHEq :: (forall (c :: k1 ~# k2) (a |> c ~# b) => d) -> d
```


This will change how `~~` behaves.



Another suggestion from Stephanie:


```
class (a :: k1 ~~ b :: k2) where  
  kind :: (k1 ~~ k2) 
  theProof :: forall (c :: k1 ~~ k2) . a |> c ~# b => a ~~ b
```


with inlining. However, this means constraints need to be strict so we know the definition terminates, which is important for the consistency of the coercion language. In other words, we need to change the representation of constraint. Right now, we have`Constraint` be like `TYPE LiftedRep` (i.e. `Type`). This suggestion would make it be like `TYPE UnliftedRep`. However, strict constraints clash with deferred type errors, as deferred type errors make use of the laziness. Richard argues that deferred type errors are already in conflict with `TypeInType`. See [
this ticket](https://ghc.haskell.org/trac/ghc/ticket/11197/).



Another option is to drop heterogeneous equality. It turns out we can still define hetero-datatype. In source, we write


```
data Rep a where
    RepBool :: Rep Bool

-- or
data Rep :: forall k. (a :: k) -> Type where
    RepBool :: forall k a.  (k ~ Type) (a ~ Bool) => Rep k a
```


If the compiler is smart enough, with coercion quantification it can translate the definition to


```
data Rep :: forall k. (a :: k) -> Type where
    RepBool :: forall k a.  (c :: k ~ Type) (a |> c ~ Bool) => Rep k a
```


However it raises a problem about telescope. For example, if we swap two constrains, will it still work?


```
data Rep :: forall k. (a :: k) -> Type where
    RepBool :: forall k a.  (a ~ Bool) (k ~ Type) => Rep k a 
                   -- two constrains are swapped!
```

### Coercion holes



As the constraint solver solves constraints, it generates *evidence*. For class constraints, this evidence takes the form of dictionaries that package the implementations of class methods. For equality constraints, this evidence takes the form of coercions. Because sometimes the solver has no place to bind evidence (like when we are kind-checking types, so there is no `let` or `case`), it stores coercion evidence in *coercion holes*. A coercion hole is a mutable cell, of type `IORef (Maybe Coercion)`. The cell starts out empty (`Nothing`) and then is filled in when the solver know how to build the coercion. 



These holes appear in coercions during type inference. When we're done type checking, coercion holes are zonked to be replaced by the coercion in the hole. All holes will be filled by the end -- otherwise, the program has a type error that we will have reported to the user. (There is special allowance for deferred type errors, which I won't describe here.)



None of the above changes. However, currently, coercion holes are implemented with this definition, in TyCoRep:


```
data Coercion = ...
              | CoHole CoercionHole

data CoercionHole
  = CoercionHole { ch_co_var :: CoVar
                 , ch_ref    :: IORef (Maybe Coercion)
                 }
```


By pairing the mutable cell with a `CoVar`, we get several benefits:


- The `CoVar` has a name and unique, so we can print it during debugging to track the coercion holes.
- The `CoVar` has a kind, so we know the two types that the coercion hole relates. This is necessary to be able to write `coercionKind`.
- The `CoVar` has a role (buried in its kind), necessary to implement `coercionRole`.
- The `CoVar` can be included in sets of free variables in a coercion. This is necessary so that we do not float a coercion with a hole out from an implication invalidly. See `Note [CoercionHoles and coercion free variables]` in TyConRep.


In our new world with homogeneous equality, we have a problem, though: a `CoVar` must be homogeneous. Yet, the solver will sometimes have to work with heterogeneous equality (more on that later). We thus have to remove the `CoVar`. Thus, `CoercionHole` becomes


```
data CoercionHole
  = CoercionHole { ch_types :: Pair Type
                 , ch_role  :: Role
                 , ch_name  :: Name      -- for debugging only
                 , ch_ref   :: IORef (Maybe Coercion)
                 }
```


These new coercion holes are *not* returned as free variables.


### Preventing floating



If coercion holes are no longer returned as free variables, how do we prevent bad floating? (Here, "floating" refers to the process by which a constraint inside an implication is floated out of that implication -- that is, the constraint is attempted absent any of the assumptions of the implication. This can be done only when the constraint mentions no variables bound by the implication.) Simple: don't float any constraint that mentions a coercion hole. Detecting this will require a new, simple traversal that looks for a coercion hole.



Why does this work? The hole will either be solved or it won't. If it is solved, then the constraint that was held up by the hole will no longer be, and GHC will make progress. (There is already the structure in place to make sure that such a constraint will be revisited.) If it isn't solved, then we have an unsolved constraint and will error anyway. One might worry that we need to float out a constraint to make more progress on it, which will then give us the key that unlocks solving the hole itself. I (Richard) don't think this is possible, because the constraints are already canonicalized -- there's not much progress that can be made other than outright solving.



Previously, there was a long section here about a different approach, involving levels. This has been moved to the end of this page.


### Heterogeneity in the solver



While we'd like to remove heterogeneous coercion variables from Core, they are useful in the solver.
Specifically, when we're analyzing an equality like `ty1 ~ (ty2 |> co)`, it's helpful to strip off the `co` and look at `ty1 ~ ty2`. This might discover similarities between `ty1` and `ty2` that can move solving forward. One of `ty1 ~ (ty2 |> co)` or `ty1 ~ ty2` is heterogeneous.



Currently, all constraints in the solver have types. For example, a constraint might have a type of `Eq [a]`. Equality constraints have types, too. Currently, these types use `~#`. But if we have a homogeneous `~#`, then we won't be able to express a heterogeneous equality constraint using `~#`.  In fact, a heterogeneous equality constraint won't have a type at all.   Remember the "mixed economy" of "A specification of dependent types for Haskell":


- A coercion *variable* (which has a type `t1 ~# t2`) must be homogeneous
- A *coercion* can be heterogeneous.  It does not have a type.


Equality constraints in the solver are witnessed by coercions and therefore may not have a type.



The types in the solver are useful because we make evidence bindings with those types. However, all equality Wanted constraints use coercion holes for their evidence (more on Givens later), so no binding is needed. We can thus store a heterogeneous equality constraint simply by storing a pair of types.



Note that the `TcEvDest` type stores either an evidence binding or a coercion hole. The new form of constraint (the pair of types) will always go hand-in-hand with the `HoleDest` constructor of `TcEvDest`.


### Heterogeneous given equalities



Even though we do not use evidence bindings for equality Wanteds, we still do use bindings for equality Givens. For example:


```wiki
class a ~# b => C a b where ...
f :: C a b => b -> a
```


We'll typecheck this to get


```wiki
f = /\a \(d :: C a b). \(x::b).
   let co :: a ~# b = sc_sel_1 d
   in x |> sym co
```


The superclass selection can only be done in term-land.  We cannot inline it to get


```wiki
f = /\a \(d :: C a b). \(x::b).
   x |> sym (sc_sel_1 d)
```


So at least some bindings for Given equalities must generate a real binding; we cannot use coercion holes for them. We thus need a dual of `TcEvDest`:


```
data CtEvidence
  = CtGiven    -- Truly given, not depending on subgoals
      { ctev_pred :: TcPredType
      , ctev_evar :: TcEvSource   <------ NEW!  Was EvVar!
      , ctev_loc  :: CtLoc }


  | CtWanted   -- Wanted goal
      { ctev_pred :: TcPredType
      , ctev_dest :: TcEvDest
      , ctev_nosh :: ShadowInfo
      , ctev_loc  :: CtLoc }

data TcEvSource
  = EvVarSource EvVar
  | CoercionSource Coercion
```


Just as a Wanted constraint carries with it a `TcEvDest`, a Given constraint will have to carry a `TcEvSource`. Unlike Wanteds, though, *sometimes* an Given equality will be an `EvVarSource`, if that Given equality arises from, say, a superclass selector or GADT pattern match or some such. When a `CoercionSource` given is used, we just substitute the Given into the coercion we are building (in a `CoercionHole` value). This will happen in `TcRnTypes.ctEvCoercion`, and possibly elsewhere.



**SLPJ**: can you give an example of why we need *heterogeneous* given equalities?  And also when/why we need `CoercionSource`?


### Some other details


- The three primitive equality tycons (`eqPrimTyCon`, `eqReprPrimTyCon`, and `eqPhantPrimTyCon`) all get a homogeneous kind.

- `coercionKind` does not need to change.

- Remove the now-redundant `KindCo` constructor for coercions.

- `coercionType` now works only over homogeneous coercions. We will have to audit usages of this function to make sure it doesn't get called on something heterogeneous.

- The core-spec will have to be updated.

- `~~` will have to be updated to use two `~#`s, as demonstrated above.

- Simon suggests that it is easier to have `~` refer directly to `~#`, instead of the current setup where it is defined in terms of `~~`. This is an unnecessary refactoring, but it might lead to a small performance win as there is one fewer indirection.  **SLPJ**: I did this a few weeks ago.

### Open questions


- At some point, GHC must assume that `ForAllTy`s are irrelevant. Now, however, a `ForAllTy` over a coercion variable is relevant, and must make a proper runtime function. Where is the code that has to change?

### Implementation thoughts



The following is taken from an email from Richard to Ningning about a path toward implementating homogeneous equality:


- `Note [The equality types story]` in TysPrim is a helpful primer.
- `eqPrimTyCon` defines `~#`. That's what has to change.
- `eqReprPrimTyCon` and `eqPhantPrimTyCon` should change, too, as it's best to keep these all in sync. These two tycons are equality tycons at different roles. The "Safe Zero-Cost Coercions" paper (JFP '16) documents roles carefully.
- `coercionKind` will continue to return a `Pair Type`. It doesn't have to change.
- `coercionType` currently returns the `(t1 ~# t2)` `TyConApp`. If the coercion is heterogeneous, it will have trouble in the new version. But I think it should just panic if the coercion is heterogeneous. It's not used much.

  - `eqCoercion` and `eqCoercionX` use `coercionType`. These might encounter hetero coercions. But just use `coercionKind` and compare types respectively instead of using `coercionType` here. I think that might be more efficient, anyway.
  - Any coercion stored in a `CoercionTy` is homogeneous. This is because `CoercionTy`s are arguments to promoted GADT constructors. Therefore, these coercions are used to instantiate a coercion variable. Once those are homo, then `CoercionTy`s will be, too.
  - Any coercion stored in a `CastTy` is also homogeneous, because both the source and result types have kind `Type`.
  - The use of `coercionType` in `opt_trans_rule` might see hetero coercions. Just change the check to compare the respective results of `coercionKind`.
  - The use of `coercionType` in `eqHsBang` looks like it should just use `eqCoercion` instead.
  - Use `coercionKind` instead of `coercionType` in `pprOptCo`.
  - Similarly to why `CoercionTy`s are homo, so are coercions stored in the `Coercion` constructor of `Expr`.
  - Any coercion used in a cast will be homo.
  - The instance `Eq (DeBruijn Coercion)` (in TrieMap) should also probably work via `coercionKind` instead of `coercionType`.
  - The `lkC` function will be more annoying. You'll have to refactor the `CoercionMapX` type to store a `(TypeMapG (TypeMapG (TypeMapG (TypeMapG a))))`, where you look up `k1`, `t1`, `k2`, `t2` (in that order). Basically, you have to recreate the current behavior, but without the convenience of `coercionType`.
  - You'll have to refactor `xtC` similarly.
- The change in the constraint solver starts by changing the `ctev_pred` field of the `CtEvidence` type to store something that's isomorphic to `(Either TcPredType (Pair Type))`. That will allow you to store the shape of hetero equality constraints without a hetero `~#`. Note that the `TcEvDest` field of `CtWanted` is always `HoleDest` for equality constraints, so you won't have an `EvVar` to worry about. (The `EvVar` would be trouble for a hetero constraint because we wouldn't be able to write down its type.) `CtGiven` stores an `EvVar` directly. I think this will have to become something isomorphic to `(Either EvVar Coercion)` so that we never have to make a hetero-typed `EvVar`. There'd be something nicely symmetric about that between `CtGiven` and `CtWanted`.
- Remove the `KindCo` constructor, making `mkKindCo` something like `promoteCoercion`. (I think, in fact, you'll be left with only one of those two.)

### Old conversation about using levels in the solver



This is kept here only for posterity. Don't take anything you see here as established fact.


>
>
> By tracking levels. The type checker manages a `TcLevel`, a natural number that starts at 0 and is incremented as the type checker enters scopes. Essentially, the `TcLevel` is the count of how many local scopes the type checker has entered. All type variables are assigned a `TcLevel` saying what scope they belong to. Note that there are no global type variables, so these levels start at 1. To prevent floating, all we have to do is to make sure that the maximum level of any variable in a type is not equal to (or greater than) the level of the implication. (All implications have levels, too, because implication constraints correspond to local scopes. The level in the implication is the level of the variables in the implication.) If the maximum level of any variable in a type is less than the level of the implication, floating is fine.
>
>


We already have the maximum-level checker: `TcType.tcTypeLevel`. All we need to do is add levels to coercion holes (we can use the level from `getTcLevel :: TcM TcLevel`) in the `ch_level` field and then incorporate that into `tcTypeLevel`. Then, we modify the floating-out mechanism to do a level-check instead of a free-variable check. This is done in `TcSimplify.floatEqualities`.



**RAE Question:** Currently, `floatEqualities` does a free-variable check and then *promotes* levels (reducing level numbers) of some variables. If we use the level numbers to decide what to float, when will we ever promote? This seems like it might not work. **End RAE**



**Answer from Simon**
Consider this implication constraint


```wiki
forall[2] b[2].
   forall a[3].
      ([W] alpha[2] ~ a[3],
       [W] beta[2] ~ (b[2], gamma[3]))
```


It is nested somewhere inside a constraint tree; hence this implication
has level 3.  I have put level numbers on every variable.  The greek
ones are unification variables.



This implication has two nested equalities:


- We *cannot* float `alpha[2] ~ a[3]` because it mentions the skolem `a`, which is bound by the implication.
- We *can* float `beta[2] ~ (b, gamma[2])` because it does not mention a skolem bound by this implication.


So we float out the second equality (but not the first), thus:


```wiki
forall[2] b[2].
   [W] beta[2] ~ (b[2], gamma[3]),
   forall[3] a[3]. ([W] alpha[2] ~ a[3])
```


But now that `gamma[3]` has too large a level number.  It doesn't follow the `(WantedInv)`
in `Note [TcLevel and untouchable type variables]` in `TcType`.



So we promote, creating a fresh unification variable `gamma2[2]` and setting `gamma := gamma2`.
Now we have


```wiki
forall[2] b[2].
   [W] beta[2] ~ (b[2], gamma2[2]),
   forall[3] a[3]. ([W] alpha[2] ~ a[3])
```


Currently we decide which constraints we can float by


1. Finding which variables are *bound* by the implication (see `Note [What prevents a constraint from floating]` in `TcSimplify`).
1. Finding which variables are *mentioned* by the constraint.  
1. Seeing if the two sets have an empty intersection.  If so, float.


We are considering using level numbers (which all type variables now have) to simplify this.  Thus:


1. Find the level number of the implication.  That's easy: the implication records it directly `ic_tclvl`!  In the above it's denoted `forall[lvl] skols. body`.

1. Find the maximum level number mentioned in the constraint.  Here we have to be a bit more careful.   As you can see from the example, we want to consider the level numbers of the *skolems* but ignore the *unification variables*.   But we must hunt for skolems in the *kinds* of the unification variables; e.g. if `gamma[3] :: a[3]` in the example above, we should not float.

1. See if (2) \< (1).


No sets, no intersection, so easy.



Incidentally, this level-number business would dramatically simplify this code in \`TcSimplify.floatEqualities


```wiki
       ; let seed_skols = mkVarSet skols     `unionVarSet`
                          mkVarSet given_ids `unionVarSet`
                          foldEvBindMap add_one emptyVarSet binds
             add_one bind acc = extendVarSet acc (evBindVar bind)
             -- seed_skols: See Note [What prevents a constraint from floating] (1,2,3)

             extended_skols        = transCloVarSet (extra_skols eqs) seed_skols
```


Gah!  Looking at the bindings, transitive closure... horrible.  If every coercion variable had a level number indicating which level it is bound at, we could throw all this away.



**End of answer from Simon**



**RAE:** To summarize, you propose to ignore unification variables when doing the floating-out level-check. (Presumably, we won't ignore unification variables' kinds. **SLPJ: good point; I have edited**) I'm still bothered though: we're worried about having coercion holes prevent floating. Coercion holes are very much like unification variables. If we ignore unification variables (and, by consequence, coercion holes), then do we have [\#14584](http://gitlabghc.nibbler/ghc/ghc/issues/14584) again? If we don't ignore coercion holes, then when will coercion holes ever get floated? I'm still very unconvinced here. **End RAE**


