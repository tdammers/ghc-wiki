## Integrating Class and Equality Solving


### Current main functions



Equality solver


```wiki
tcReduceEqs :: [Inst]             -- locals
            -> [Inst]             -- wanteds
            -> TcM ([Inst],       -- normalised locals (w/o equalities)
                    [Inst],       -- normalised wanteds (including equalities)
                    TcDictBinds,  -- bindings for all simplified dictionaries
                    Bool)         -- whether any flexibles where instantiated

```


It already processes both equalities and class constraints, but only to simplify the type parameters of class constraints, no attempt to simplify them is made.



Predicate solver


```wiki
reduceList :: RedEnv -> [Inst] -> Avails -> TcM Avails
```


The `[Inst]` currently includes equalities, but they are ignored.  The `Avails` passed in are free of equalities, but the outgoing ones includes equalities (which come from the `[Inst]`).  Moreover, we have the post-processing of `Avails` by


```wiki
extractResults :: Avails
	       -> [Inst]		-- Wanted
	       -> TcM (TcDictBinds, 	-- Bindings
		       [Inst],		-- The insts bound by the bindings
		       [Inst])		-- Irreducible ones
			-- Note [Reducing implication constraints]
```


The irreducible `Inst`s will include all equalities that are passed in as wanteds.


### Integration



Both the equality and class solver have a pre-processing and post-processing phase as well as the main solving phase.  (All three phases are structurally more complicated for equalities.)


#### Pre-processing



The two tasks:


- Classes: computation of the initial `Avails` value (called `init_state` in `reduceContext`).
- Equalities: normalisation of equalities and class constraints.


I'd suggest to do normalisation first (basically in the same way as it done now) and then build the initial `Avails` from the normalised constraints.


#### Main solver



We can probably leave the solving of class constraints much as it is now.  How much the solving of equalities will have to change depends on how we handle `Avails` and whether we keep local equalities in `Avails`, too.  (At the moment, the code in `TcTyFuns` doesn't use the `Avails` data type.)  We need to integrate the loops of `TcSimplify.reduceList`/`TCSimplify.reduce` with those of `TcTyFuns.propagate`.


#### Post-processing



The two tasks:


- Classes: Extract bindings, bound insts, and irreducible insts from the final `Avails` (i.e., work of `extractResult`).
- Equalities: Finalisation phase.


I'd suggest to extract results first and then finalise on the insts as usual.  That way, the code for both probably doesn't have to change much.  It's also conceptually simpler, I think.


#### Open questions


1. During solving we maintain the local equalities and local class constraints in rather different structures.  In particular, we don't use `Avails` for equalities.  How do we want to integrate this?
1. Integration of loops of `TcSimplify.reduceList`/`TCSimplify.reduce` with those of `TcTyFuns.propagate`.

### Module structure



Currently, the equality solver lives in `TcTyFuns` and the class solver lives in `TcSimplify` together with the higher-level logic.  This set up is no longer feasible when we integrate both solvers as we don't want to add a recursive module dependency.  It seems undesirable to have everything in `TcSimplify`; so, we should move the class constraint solving out.  I think, there are two options:


1. We could have a new module `TcSolver` that includes all the code currently in `TcTyFuns` plus all the class constraint solving code from `TcSimplify` (and remove `TcTyFuns`.)
1. We could have a new module `TcTyPred` that gets all the class constraint solver code from `TcSimplify`.  The code that coordinates solving of equalities with class constraints should go into `TcTyFuns` as it needs to do the preparatory normalising of types (lifting out family synonym applications) and similar things that are due to type families.  `TcTyFuns` will then invoke functions in `TcTyPred` to perform class solving steps. 


To me, the second option is more appealing as I expect it to keep interfaces smaller.


### Functional dependencies



Currently, `tcImproveOne` generates pairs of types to be unified (on the basis of the FD improvement rules) and does unify them with `unifyType` (via `unifyEqns`).  In the integrated solver, `tcImproveOne` should generate equality constraints (with identity coercions) instead.  This will get rid of the `extra_eqs` that we currently have in `reduceContext`.


---


### Additional points raised during phone conf, 3 Dec



The plan is to outline on this page, the final design we are aiming for (independent of whether we implement it in one rewrite of the source or more).


#### Treatment of variable instantiation



Instead of instantiating type variables during finalisation, we should already remove equalities of the form `co :w alpha ~> t` during normalisation or propagation and remember them as substitutions `alpha := t` in `EqConfig` (or similar) and instantiate `co := id`.  We also need to ensure that the substitution is propagated to all other constraints.



When we return to `reduceContext` and solve the implication constraint, we also got to use the substitutions as given equalities when we try to solve the implications.


#### EqConfig



`EqConfig` needs to become more general and be a general solver configuration.  Likewise `RewriteInst` needs to include a variant for class constraints, which we need to handle during propagate.


