# Type Functions: Type Checking


## Kind checking indexed type families



The workhorse of kind checking type and class declarations is `TcTyClDecls.kcTyClDecls`. It is invoked by `TcTyClDecls.tcTyClDecls` once per binding group.  It handles type synonyms different from data/newtype declarations and classes; this is as synonyms have a richer kind structure (making kind checking harder), but cannot be recursive (which makes kind checking easier).  Somehwat in contrast, we handle all flavours of family declarations in the same way as algebraic data type declarations.  More precisely, family declarations participate in the construction of the initial kind environment (as performed by `getInitialKind`).



In contrast, family instances are not processed by `TcTyClDecls.tcTyClDecls`, but by `TcInstDcls.tcInstDecls1`, which handles the heads of class instance declarations.  However, the later invokes `TcTyClDecls.tcFamInstDecl` (both directly and indirectly via `TcInstDcls.tcLocalInstDecl1`, the later for associated types).  The function `tcFamInstDecl` shares a lot of code with the `TcTyClDecls.kcTyClDecls` and `TcTyClDecls.tcTyClDecl`.


## Type checking indexed type families



Type checking in the presence of only data and newtype families is much simpler than in the presence of type synonym families as type equality remains purely syntactic (i.e., we do not need to change the unification procedure).  However, we need to check that the alternatives of a case expression inspecting an indexed data/newtype family contains only constructors of one member of the family.  (To relax this restriction, we would need a story for compiling open data types.)



However, this difference in complexity applies only to the type checking of expression whose types involve data and type synonym families, respectively.  Type checking of the declarations themselves is not that different; in fact, data family declarations require more effort as they introduce data type constructors, which need to be handled as well.  However, a lot of machinery can be re-used from vanilla algebraic data types.


### Type checking family declarations and family instances



Family declarations are handled by `TcTyClsDecls.tcTyClsDecl` together with all other type and class declarations.  Within class declarations, we invoke the function recursively to process associated types.



Family instances are type checked by `TcTyClDecls.tcFamInstDecl`; i.e., the same function that performs their kind checking.  Kind checking and type checking of family instances can be combined, as we don't need to worry as much about recursive dependencies as we have to for standard type declarations.  In particular, the kinds of families are declared by their signature and we don't have to compute any recursiveness information, as we never know whether we reached a fixed point for open types.  (Hence, we conservatively assume families and their instances to be always `Recursive`.  This is safe as they are implicit loop breakers due to implying coercions.)


### Deriving clauses at data instances



The main issue when handling `deriving` clauses for data instances in `TcDeriv` is using the representation tycon, where needed (instead of the family tycon, which appears in the actual declaration).  In particular, the list of constructors is only available from the representation tycon.  These constructors already have appropriate signatures mentioning the family tycon in the result type, so that most of the deriving machinery works out of the box.  As `TcDeriv.makeDerivEqns` extracts the set of class instances that need to be derived from the un-typechecked source form, we need the new `TcEnv.tcLookupFamInst` to look up type-checked family instances from tycon/type-indexes pairs.  (All information needed for that is already available in the type checker environment.)


## Core representation of signatures of indexed families



The function `TcTyClsDecls.tcTyClsDecls` produces `TypeRep.TyThing`s from type and class declarations.  The `TyThing`s produced from the new declaration forms are the following:


<table><tr><th>`type family`</th>
<td>
Type synonym families are represented by the standard `TyCon` variant for synonyms, namely `SynTyCon`.  They are distinguished from ordinary type synonyms by the value of the field `synTcRhs`, which is now of a new data type `SynTyConRhs`, which has a variant `OpenSynTyCon resKind` to represent families.
`data family`\`::
Data families are represented by the `TyCon` variant `AlgTyCon`, as are their non-indexed counter parts, with the difference that the field `algTcRhs` has the one of the newly introduced values `OpenDataTyCon` or `OpenNewTyCon`.
</td></tr></table>


### Synonym type constructors: `SynTyCon`



To represent type families (which do not have a fixed right hand side), the type of `synTcRhs` changed from just `Type` to `SynTyConRhs` with


```wiki
data SynTyConRhs 
  = OpenSynTyCon Kind (Maybe [Int]) -- *result* kind & positions
  | SynonymTyCon Type               -- rhs of ordinary synonym
```


Consequently, all functions that dependent on this field had to be extended.  In particular, `TcType.isTauTyCon` regards applications of type family constructors as *tau types*, which is why we need to require that the right hand side of each `type instance` declaration is also a tau type.  As a result, `BuildTyCls.buildSynTyCon`'s last argument now also takes a value of type `SynTyConRhs`.


### Associated types



Classes are represented by `Class.Class`, which we extend by a new field `classATs` of type `[TyCon]`.  The `Class` structures including embedded `TyCon`s for associated types are constructed at the end of declaration type checking by `TcTyClsDecls.tcTyClDecl1` by way of `BuildTyCl.buildClass`.



An additional complication is that the associated types of a class need already be available when type checking the super class context and the method signatures of the same class, or other things in the same type checking knot.  Hence, we need to make them available in the temporary environment constructed in the knot tied by `TcTyClsDecls.tcTyAndClassDecls`.  Special care is required as this knot tying relies on the property that the list of declarations, `alg_at_decls`, and the list of `TyThing`s produced by type checking the declarations, `rec_alg_tyclss`, match up (for zipping them together within `mkGlobalThings`).  We guarantee this by always entering the associated types of a class right after that class in the declaration list.



An important property of associated types is that their index types always coincide with the class instance arguments at those argument positions that use the same type variables in the associated family declaration.   To check that property, the right hand-sides of `AlgTyConRhs` and `SynTyConRhs` of `AlgTyCon` and `SynTyCon`, respectively, contain a field of type `Maybe [Int]` in the variant indicating an open family declaration (i.e., variant `OpenTyCon` and `OpenSynTyCon`).  This field has a value of the form `Just poss` for associated family `TyCon`s, where `poss` gives the argument position in the class head for each family argument.  For example,


```wiki
class C a b c where
  data T c b :: * -> *
```


induces a `poss` value of `[2, 1]`.  Note how `T` is 3-ary, but only the first two arguments are type indexes and correspond to the third and second class parameter, respectively.


### GHC API



The GHC API has a new predicate `isOpenTyCon` with the understanding that it is illegal to invoke `synTyConDefn`, `synTyConRhs`, and `tyConDataCons` on type constructors that fulfil `isOpenTyCon`.


## Core representation of family applications



GHC has a notion of *representation types*, implemented by `Type.repType`, that is used in the backend to look through foralls, vanilla synonyms, predicates, usage annotations, and vanilla newtypes to determine the types to be used in generated code.  It's a fundamental property of our implementation of indexed types that `repType` never looks through newtype and synonym families.  Instead, whenever the code generator needs to know the representation of an indexed type, the type checker and desugarer have to add an explicit cast from the indexed type to the representation.  The type of the cast expression will then have the desired representation.



NB: This implies that the ultimate representation type of a vanilla newtype, `TyCon.newTyConRep`, may be an indexed newtype or indexed synonym.  We make no attempt to look through them, even if we have equality axioms for the supplied type indexes in the environment.


## Core representation of family instances


### Representation of data instances



There are three (inter-linked) aspects to the representation of data/newtype instances: (1) the representation of the `TyCon` generated from an instance, (2) the representation of the `DataCon`s for the variants of the instance, and the (3) equality axiom connecting the indexed family type with the representation of Item (1).


#### The `TyCon` of an instance



When building the `TyCon` for the representation type of a `data instance`, we need to derive a new (internal) name for that representation `TyCon` from the family name.  This is done by `BuildTyCl.buildAlgTyCon`, which gets an additional argument `mb_family :: Maybe TyCon` that gives the family type constructor if we are building a `TyCon` for an instance.  In that case, `buildAlgTyCon` generates a new name with the help of `newImplicitBinder` and fills the new field `algTcParent` with type


```wiki
data AlgTyConParent = NoParentTyCon
                    | ClassTyCon    Class
                    | FamilyTyCon   TyCon     -- family tycon
                                    [Type]    -- instance types
                                    TyCon     -- representation coercion
```


which is a generalisation of the old field `algTcClass` of the internal representation for datatypes, `TyCon.AlgTyCon`.  In contrast to the old `algTcClass` field, the new field also appears in `IfaceSyn.IfaceDecl`.  However, it does so as `Maybe (IfaceTyCon, [IfaceType])` as we still do not want to represent class parent information in interfaces and we only record the family tycon and instance types in interfaces, not the coercion.  (The latter is implicitly reconstructed upon loading an interface.)  The *instance types* are the type indexes at which the data constructor has been declared; e.g., given the declaration


```wiki
data instance Map (a, b) v = MapPair (Map a (Map b v))
```


the instance types are `[(a, b), v]`.



NB: The type argument variables of the representation tycon are the free variables of the instance types; i.e., the representation data type is an ordinary data type, it is neither indexed nor open.  The only give away of its special purpose is the value in `algTcParent`.


#### The `DataCon`s of the variants of an instance



`DataCon`s of data instances are not explicitly distinguished from ordinary `DataCon`s.  However, they differ by referring to a `TyCon` and a datacon wrapper that differ from their ordinary form.  More specifically, the field `algTcParent` of the `TyCon` is of the form `FamilyTyCon (famTyCon, instTys, coe)`, where `famTyCon` is the `TyCon` of the data family to which the instance belongs, `instTys` are the instance types, and `coe` is the coercion identifying representation type and family instance.  This coercion is used by the datacon wrapper whose signature uses the family type, not the representation type.


#### The coercion identifying family instance and representation type



As each `data instance` is *represented* by its own `TyCon`, we need to be able to move between the type of the family instance and that of the representation.  We do so by an adaptation of the same method used to implement newtypes with coercions (c.f., [IntermediateTypes](intermediate-types)).  Newtypes store the coercion moving between representation and abstract type in the field `nt_co` of the `NewTyCon` variant of `TyCon.AlgTyConRhs`, whereas representation types for indexed data types use `algTcParent` (see above).  Newtype coercions are constructed by `Coercion.mkNewTypeCoercion`, whereas representation types for indexed data types use a similar function `Coercion.mkFamInstCoercion`, which is invoked by `BuildTyCl.buildAlgTyCon` iff it is passed family and instance type information.  We use the same approach in `BuildTyCl.buildSynTyCon` for `type instance`s.


### Representation of newtype instances



We handle newtype instances similar to data instances.  However, newtypes have no separate worker and wrapper, but only a hybrid that is categorised as a worker (see `MkIds.mkDataConIds`).  In particular, this worker gets the wrapper signature as well as an unfolding.  The wrapper signature ensures that the result type of the constructor mentions the family constructor (and not the instance representation constructor).  The body of an ordinary newtype applies the newtype coercion to move from abstract to concrete type.  In the case of a family instance, we compose the newtype coercion with the family coercion to directly move from the abstract family instance to the concrete type.  We don't do the same in the opposite direction - i.e., in `MkIds.unwrapNewTypeBody` - as computing the right type arguments is more complicated than a simple tycon split.  Instead, we use the same mechanism in `TcPat.tcConPat` as for pattern matching, and hence elimination, of data constructors.



NB: It is necessary to refine the `TyCon.isNewTyCon` predicate by introducing `TyCon.isClosedNewTyCon` and using it in all places where the predicate is used to determine whether a newtype can be expanded to its right hand side.  In principle, this is also possible for families, but only in dependence on the concrete type arguments (and newtype instances in scope), so it would be much harder to check.  Hence, for now, newtype families are opaque.


### Representation of type synonym instances



The basic structure of the representation of `type instance`s is the same as for data and newtypes.  Every instances is represented by a representation `TyCon.TyCon`, which in the synonym case is of the `SynTyCon` variant.  We extended `SynTyCon` by a new field `synTcParent :: TyConParent` that contains the same sort of parent information as for data types.  In particular, it refers to a coercion that moves between the family instance and the representation tycon.  This coercion is created with `Coercion.mkFamInstCoercion`.  As an example, for


```wiki
type instance T [a] Int = Maybe a
```


we get


```wiki
type R a = Maybe a
coe co a :: (T [a] Int) ~ (R a)
```


This may appear overly complicated as we could have created a coercion that has `Maybe a` as its right-hand side, avoiding a representation type `R` entirely.  However, inside GHC, the representation tycon conveniently stores all the information about the type instance (including its coercion), which the coercion by itself could not.  Moreover, we also use it to represent the instance in interface files.


## Type checking equational constraints



Constraints are turned into dictionaries by `Inst.newDictBndrs`.   For equational constraints, that is the place where *given* equalities are introduced.



**TODO** Where do we perform the detailed check of well-formedness of equalities?  In `check_pred_ty` or when adding given equalities?


## Unification in the presence of type functions



We do *not* rewrite type function applications implicitly during unification.  Instead, unifcation returns all *needed* equalities that are non-syntactic.  That has two advantages: (1) the computation of coercions is completely decoupled from unification and (2) unification does not have to know anything about equality axioms and given equalities.



**TODO** The whole extension would be a lot less invasive if we could arrange for unification to enter the needed equalities into a pool in the monad instead of returning them (as the type of the unification routines would stay the same).  Is this possible?



Needed equalities are then checked against the axioms and given equalities during context simplification, much like class predicates.


## Type checking expressions


### Pattern matching indexed data types



Pattern matching against data constructors in `TcPat.tcConPat` implements type refinement in case alternatives for GADTs; i.e., data constructors that have a non-empty `dcEqSpec`.  It might appear that we can reuse that infrastructure for type indexes, but that is unfortunately not possible.  The whole point of local type refinement is that the variable instantiations are not propagated outwards, but that is exactly what is required for type indexes.  We handle matching against data constructors of data instances by two extensions to `tcConPat`:


- we generalise `TcUnify.boxySplitTyConApp` to take type families into account and
- insert an `ExprCoFn` that is to be applied to the scrutinee in the case of a data constructor of a type family.

#### Splitting of type constructor applications



The result type of a wrapper of a data constructor of a family mentions the family type constructor, whereas the worker uses an (internal) representation type constructor.  So, when pattern matching against such a constructor, the type of the scrutinee will be the family type and we need to use `TcUnify.boxySplitTyConApp` with that family type, instead of the representation type constructor mentioned in the `dcTyCon` of the data constructor.  We achieve this by the local function `boxySplitTyConAppWithFamily` that checks for the presence of a type family before invoking `boxySplitTyConApp`.  In addition, we need to adjust the split type constructor arguments to fit the representation type constructor.  This is also taken care of by `boxySplitTyConAppWithFamily`, which matches the family arguments against the instance types using `TcUnify.boxyUnifyList`.


#### Coercing the scrutinee



The matching against the family type constructor and unification with the instance types corresponds to applying the coercion moving between the family and representation type of a data instance, which is returned by `tyConFamilyCoercion_maybe`.  To generate type correct core, this coercion needs to be applied to the scrutinee of the case expression matching on the constructor pattern.  This is done by the local function `unwrapFamInstScrutinee` whenever we match against the data constructor of a family by wrapping the result pattern into a `CoPat` containing an `ExprCoFn` with the coercion.


## Checking for overlapping and inconsistent data/newtype instances


### Overlap check of the instances in the currently compiled module



We maintain a family instance environment in the `TcRnTypes.TcGblEnv` much like that for class instances.  We add instances to this environment, with `FamInst.addLocalFamInst`, as we type check them and perform a consistency check with each addition.  This consistency check includes all the instances in the EPS and HPT, too - again just like with class instance.  Hence, We simultaneously check the instances of the current module against all imported ones, too.  (This, of course, requires that we have them all in the EPS, resp. HPT, at that point, which we guarantee by the calls to `LoadIface.loadOrphanModules` in `TcRnDriver`, reading all `imp_finsts` of the current `ImportAvails`, and by `HscTypes.hptInstances` collecting all class and family instances of imported modules in the home package, which are being used to initialised the `TcGblEnv`.)


### Overlap check for instances of imported modules



The function `FamInst.checkFamInstConsistency` checks that all family instances of the given modules (which are all the family-instance modules of the current module) are consistent.  For one-shot mode, this check is invoked in `TcRnDriver.tcRnModule`, after all imported modules (including orphans and family-instance modules) have been loaded.



For GHCi, --make, and friends it is more difficult to find an appropriate place for the check.  After all, the user may have loaded modules interactively and they must be checked for compatibility.  A simple solution is to perform this check whenever the export `AvailInfo` of a loaded module is computed by `TcRnDriver.tcGetModuleExports`.  At this point, we know that all relevant modules are loaded, we are in the type checker monad (that the check requires) and we can obtain the relevant imported modules from the interactive context.  The main disadvantage of this solution is that redundant checks are performed when multiple modules are loaded (as the consistency check is done once per loaded module).


