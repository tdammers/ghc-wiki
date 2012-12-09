## Connection with GHC's Constraint Solver



The solver for the type nats is implemented as an
extra stage in GHC's constrraint solver (see `TcInteract.thePipeline`).



The following modules contain most of the code relevant for the solver:


- `TcTypeNats`:      The main solver machinery      
- `TcTypeNatsRules`: The rules used by the solver
- `TcTYpeNatsEval`:  Functions for direct evaluation on constants

## Generating Evidence



The solver produces evidence (i.e., proofs) when computing new "given"
constraints, or when solving existing "wanted" constraints.
The evidence is constructed by applications of a set of pre-defined
rules.  The rules are values of type `TypeRep.CoAxiomRule`.
Conceptually, rules have the form:


```wiki
    name :: forall tyvars. assumptions => conclusion
```


The rules have the usual logical meaning: the variables are universally
quantified, and the assumptions imply the concluson.  As a concrete example,
consider the rule for left-cancellation of addtion:


```wiki
    AddCanceL :: forall a b c d. (a + b ~ d, a + c ~ d) => b ~ c
```


The type `CoAxiomRule` also supports infinte literal-indexed families
of simple axioms using constructor `CoAxiomTyLit`.  These have the form:


```wiki
    name(l_1 .. l_n) :: conclusion
```


In this case `conclusion` is an equation that contains no type variables
but may depend on the literals in the name of the family.  For example,
the basic definitional axiom for addition, `TcTypeNatsRules.axAddDef`,
uses this mechanism:


```wiki
    AddDef(2,3) :: 2 + 3 ~ 5
```


At present, the assumptions and conclusion of all rules are equations between
types but this restriction is not important and could be lifted in the future.



The rules used by the solver are in module `TcTypeNatsRules`.


## The Solver



The entry point to the solver is `TcTypeNats.typeNatStage`.



We start by examining the constraint to see if it is obviously unsolvable
(using function `impossible`), and if so we stash it in the
constraint-solver's state and stop.  Note that there is no assumption that
`impossible` is complete, but it is important that it is sound, so
if `impossible` returns `True`, then the constraint is definitely unsolvable,
but if `impossible` returns `False`, then we don't know if the constraint
is solvable or not.



The rest of the stage proceeds depending on the type of constraint,
as follows.


### Given Constraints



Given constraints correspond to adding new assumptions that may be used
by the solver.  We start by checking if the new constraint is trivial
(using function `solve`).  A constraint is considered to be trivial
if it matches an already existing constraint or a rule that is known
to the solver.  Such given constraints are ignored because they do not
contribute new information.  If the new given is non-trivial, then it
will be recorded to the inert set as a new fact, and we proceed
to "interact" it with existing givens, in the hope of computing additional
useful facts (function `computeNewGivenWork`).



IMPORTANT: We assume that "given" constraints are processed before "wanted"
ones.  A new given constraint may be used to solve any existing
wanted, so every time we added a new given to the inert set we should
move all potentially solvable "wanted" constraint from the
inert set back to the work queue.   We DON'T do this, because it is
quite inefficient: there is no obvious way to compute which "wanted"s
might be affected, so we have to restart all of them!



The heart of the interaction is the function `interactCt`, which
performs one step of "forward" reasoning.  The idea is to compute
new constraints whose proofs are made by an application of a rule
to the new given, and some existing givens.  These new constraints are
added as new work, to be processed further on the next iteration of
GHC's constraint solver.



Aside: when we compute the new facts, we check to see if any are
obvious contradictions.  This is not strictly necessary because they
would be detected on the next iteration of the solver.  However, by doing
the check early we get slightly better error messages because
we can report the original constraint as being unsolvable (it leads
to a contradiction), which tends to be easier to relate to the original
program.  Of course, this is not completely fool-proof---it is still
possible that a contradiction is detected at a later iteration.
An alternative idea---not yet implemented---would be to examine the
proof of a contradiction and extract the original constraints that lead
to it in the first place.


### Derived Constraints



``Derived`` constraints are facts that are implied by the constraints
in the inert set.  They do not have complete proofs because
they may depend on proofs of as yet unsolved wanted constraints.
GHC does not associate any proof terms with derived constraints (to keep things simple?).
In the constraint solver, they are mostly used as "hints".  For example,
consider the wanted constraint `5 + 3 ~ x`, where `x` is a
free unification variable.  These are the steps we'll take to solve
the constraint:


```wiki
    Rules:
    Add_def(5,3) : 5 + 3 ~ 8
    Add_fun      : forall a b c1 c2. (a + b ~ c1, a + b ~ c2) => c1 ~ c2

    1. Add to inert set:
       [W] C: 5 + 3 ~ x
    2. Generate new derived:
       [D] Add_fun(C,Add_def) : x ~ 8   (proof discarded)
    3. GHC uses this hint to improve and reconsider the wanted:
       [W] C: 5 + 3 ~ 8
    4. Solved:
       [W] C = Add_def(5,3)
```


The type-nat solver processes derived constraints in a similar fashion
to given constraints (`computeNewDerivedWork`): it checks to see if they are trivially known
and, if not, then it tries to generate some additional derived constraints.
The main difference is that derived constraints can be interacted
with all existing constraints to produce new facts, while given
constraints only interact with other givens.


### Wanted Constraints



The main purpose of the solver is to discharge ``wanted`` constraints
(the purpose of processing given and derived constraints is to help
solve existing wanted goals).   When we encounter a new wanted goals
we proceed as follows:


1. Try to solve the goal, using a few different strategies:

  1. Try to see if it matches the conclusion of an iff rule (`solveIff`). Aassumptions of rule become new wanted work.
  1. Try to see if it matches an axiom exactly (`solve`)
  1. Try the ordering solver for `<=` goals (`solveLeq`)
  1. Try to use a (possibly synthesized) assumption

1. If that didn't work:

  1. Wanted is added to the inert set
  1. Check to see if any of the existing wanteds in the inert set can be solved in terms of the new goal (`reExamineWanteds`)
  1. Generate new derived facts.

#### Using IFF Rules



These rules are used to replace a wanted constraint with a collection
of logically equivalent wanted constraints.  If a wanted constraint
matches the head of one of these rules, than it is solved using the rules,
and the we generate new wanted constraints for the rule's assumptions.



The following are important properties of IFF rules:


- They need to be sound (of course!)
- The assumptions need to be logically equivalent to the conclusion (i.e., they should not result in a harder problem to solve than the original goal).
- The assumptions need to be *simpler* from the point of view of the constraint solver (i.e., we shouldn't end up with the original goal after some steps---this would lead to non-termination).


At present, IFF rules are used to define certain operators in terms of
others.  For example, this is the only rule for solving constraints about
subtraction:


```wiki
    forall a b c. (a + b ~ c) => (c - a ~ b)
```

#### Using Axioms



Basic operators are defined with an infinite family of axiom schemes.
As we can't have these written as a long list (searching might never stop!),
we have some custom code that checks to see if a constraint might be
solvable using one of the definitional axioms (see `solveWithAxiom`, `byAxiom`).


#### Using the Order Model



Constraints about the ordering of type-level numbers are kept in a datastructure
(`LeqFacts`) which forms a ``model* of the information represented by the
constraints (in a similar fashion to how substitutions form a model for a
set of equations).
*



The purpose of the model is to eliminate redundant constraints, and to make
it easy to find proofs for queries of the form `x <= y`.  In practise,
of particular interest are questions such as `1 <= x` because these appear
as assumptions on a number of rules (e.g., cancellation of multiplication).
In the future, this model could also be used to implement an interval
analysis, which would compute intervals approximating the values of
variables.



TODO At present, this model is reconstructed every time it needs to be used,
which is a bit inefficient.  Perhaps it'd be better to use this directly
as the representation of `<=` constraints in the inert set.



The model is a directed acyclic graph, as follows:


- vertices: constants or variables (of kind `Nat`)
- edges: the edge from `A` to `B` is a proof that `A <= B`.


So, to find a proof of `A <= B`, we insert `A` and `B` in the model,
and then look for a path from `A` to `B`.  The proofs on the path
can be composed using the rule for transitivity of `<=` to form the final proof.



When manipulating the model, we maintain the following "minimality"
invariant:  there should be no direct edge between two vertices `A`
and `B`, if there is a path that can already get us from `A` to \`B.
Here are some examples (with edges pointing upwards)


```wiki
    B                                            B
    |\                                          / \
    | C                                        C   D
    |/                                          \ /
    A                                            A

 Invariant does not hold                 Invariant holds
```


The purpose of the invariant is to eliminate redundant information.
Note, however, that it does not guarantee that there is a unique way
to prove a goal.


#### Using Extended Assumptions



Another way to prove a goal is to look it up in the assumptions.
If the goal matched an assumption exactly, then GHC would have
already solved it in one of its previous stages of the constraint
solver.  However,  due to the commutativity and associativity of some of the
operators, it is possible to have goal that could be solved by assumption,
only if the assumption was "massaged" a bit.



This "massaging" is implemented by the function `widenAsmps`, which
extends the set of assumption by performing a bit of forward reasoning
using a limited set of rules.   Typically, these are commutativity
an associativity rules, and the `widenAsmps` function tries to complete
the set of assumptions with respect to these operations.  For example:


```wiki
    assumptions: C: x + y ~ z
    cur. goal:   D: y + x ~ z

    extended assumptions: C: x + y ~ z, Add_Comm(C) : y + x ~ z
    solved:               D = Add_Comm(C)
```


Note that the extended assumptions are very similar to derived constraints,
except that we keep their proofs.


#### Re-examining Wanteds



If none of the strategies for solving a wanted constraint worked,
then the constraint is added to the inert set.  Since we'd like to
keep the inert set minimal, we have to see if any of the existing
wanted constraints might be solvable in terms of the new wanted
(`reExamineWanteds`).



It is good to keep the inert set minimal for the following reasons:


- Inferred types are nicer,
- It helps GHC to solve constraints by "inlining" (e.g., if we
  have only a single constraint `x + y ~ z`, then we can eliminate it
  by replacing all occurrences of `z` with `x + y`, however we can't
  do that if we ended up with two constraints \`(x + y \~ z, y + x \~ z)).


We consider each (numeric) wanted constraint in the inert set and
check if we can solve it in terms of the new wanted and all other wanteds.
If so, then it is removed from the inert set, otherwise it stays there.



Note that we can't implement this by kicking out the existing wanted
constraints and putting them back on the work queue, because this would
lead to non-termination.  Here is an example of how this might happen:


```wiki
    inert: [W] A : x <= 5
    new:   [W] B : y <= 5

    Can't solve B, add to inert, kick out A
    inert: [W] B : y <= 5
    new:   [W] A : x <= 5

    Can't solve A, add to inert, kick out B...

    ... and we are back to the beginning.
```


Perhaps there is a way around this but, for the moment, we just re-examine
the numeric wanteds locally, without going through the constraint
solver pipe-line.


