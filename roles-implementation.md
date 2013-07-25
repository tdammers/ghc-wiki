# Roles Implementation



This page describes how roles are implemented in GHC. If you're looking for how
to use roles in a Haskell program, see [Roles](roles).


## The `Role` datatype



The `Role` datatype, defined in `CoAxiom` to avoid mentioning it in an hs-boot
file, is defined thusly:


```wiki
data Role = Nominal | Representational | Phantom
  deriving (Eq, Data.Data, Data.Typeable)
```


Two types are nominally equal when they have the same name. This is the usual
equality in Haskell or Core. Two types are representationally equal when they
have the same representation. (If a type is higher-kinded, all nominally equal
instantiations lead to representationally equal types.) Any two types are
"phantomly" equal.


## Roles on `Coercion`s



Every coercion proves an equality at a certain role. There are subtle rules
governing what compositions are allowed. See
[
the core spec](http://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf?raw=true) for the details, or look at `coercionRole`. To facilitate this,
a few of the `Coercion` constructors needed to be changed:


- `Refl` now takes a role and a type, proving reflexive equality at the given role.
- `TyConAppCo` also takes a role. The choice of this role affects which roles the
  arguments must be at. If the role is nominal, all arguments must be nominal.
  If the role is phantom, all arguments must be phantom. But, if the role is
  representational, the argument roles must correspond to `tyConRoles` called
  on the tycon in the `TyConAppCo`. The idea is that different tycons have
  different requirements in order to prove representational equality. See the
  section below discussing roles with tycons. Note that, now, the interpretation
  of a `TyConAppCo` may differ from that of nested `AppCo`s.
- `CoVarCo`s extract their role from their type.
- `UnivCo` is a new "universal" coercion. It takes a role and two types and witnesses
  equality between those types at that role. It replaces the old `UnsafeCo`.
  `UnivCo` at role P is needed in `TyConAppCo`s at role P.
- The role produced by `NthCo` is essentially the inverse of the `TyConAppCo` story.
  If `NthCo`'s parameter is N or P, the result has the same role. If it's R, though,
  the result's role is determined by `tyConRoles` once again.
- `SubCo` implements sub-roling: its argument is N and it produces R.


Because coercions can be produced at any of the three roles, most functions that
produce them now take a `Role` parameter, indicating what role to produce. These,
in turn, make use of `maybeSubCo2` and `maybeSubCo`, which convert among the roles;
they are documented in the source.



The functions `mkTyConAppCo` (and, in turn `mkFunCo`) now have a bit of a delicate
requirement on their arguments: the argument types must "match" the desired role.
Thus, if the desired role is R, the arguments must have the roles indicated by
`tyConRoles`. In practice, it is not hard to ensure this precondition, but you
do have to be aware of it.


## Roles on `TcCoercion`s



The type checker operates solely on `TcCoercion`s. What roles do these have? Because
the type checker thinks only about nominal equality, it would make sense for all
`TcCoercion`s to have role N. But, sometimes the type checker needs to pass around
a coercion produced by a newtype (for example, in implicit-parameter handling).
So, they need to handle R coercions as well. But, we never lint a `TcCoercion`, so
we don't quite need to be as careful with them.



The solution is that, when desugaring to `Coercion`s, we pass in a `Role`
parameter, indicating how we should interpret the `TcCoercion`. (Casts always
use R equality.) Because we hopefully never ask for nominal equality from a
newtype axiom, this works in practice. If a problem arises, it will most likely
take the form of a `maybeSubCo2` panic.


## Roles with `TyCon`s



Every `TyCon`'s parameters are now each assigned a role. The interpretation is
this: if a parameter a of tycon T has role r, then a coercion at role r can be
lifted into a representational coercion of T a. Thus, N is the most
restrictive, and P is the most permissive. These roles are user-visible, so
they are described on the [Roles](roles) page.



Kind variables are all assigned role N. We must be careful when comparing a
tycon's roles against the role annotations, because role annotations are only
on *type* variables, never *kind* variables. So, we often have to drop
the kind-variable roles when doing the comparison.


## `eqPrimTyCon` vs `eqReprPrimTyCon`



The type of a nominal coercion is headed by `eqPrimTyCon`, spelled `~#`. In
order to support [NewtypeWrappers](newtype-wrappers), we must also have a way of storing
representational coercions. Their types are headed by `eqReprPrimTyCon`, spelled
`~R#`.


