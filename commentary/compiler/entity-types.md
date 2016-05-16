
Video: [
Types and Classes](http://www.youtube.com/watch?v=pN9rhQHcfCo&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) (23'53")


#
Data types for Haskell entities: `Id`, `TyVar`, `TyCon`, `DataCon`, and `Class`



For each kind of Haskell entity (identifier, type variable, type constructor, data constructor, class) GHC has a data type to represent it.  Here they are:


- **Type constructors** are represented by the `TyCon` type ([compiler/types/TyCon.hs](/trac/ghc/browser/ghc/compiler/types/TyCon.hs)).
- **Classes** are represented by the `Class` type ([compiler/types/Class.hs](/trac/ghc/browser/ghc/compiler/types/Class.hs)).
- **Data constructors** are represented by the `DataCon` type ([compiler/basicTypes/DataCon.hs](/trac/ghc/browser/ghc/compiler/basicTypes/DataCon.hs)).
- **Pattern synonyms** are represented by the `PatSyn` type ([compiler/basicTypes/PatSyn.hs](/trac/ghc/browser/ghc/compiler/basicTypes/PatSyn.hs)).
- **Term variables** `Id` and **type variables** `TyVar` are both represented by the `Var` type ([compiler/basicTypes/Var.hs](/trac/ghc/browser/ghc/compiler/basicTypes/Var.hs)).


All of these entities have a `Name`, but that's about all they have in common.  However they are sometimes treated uniformly:


- A **`TyThing`** ([compiler/types/TypeRep.hs](/trac/ghc/browser/ghc/compiler/types/TypeRep.hs)) is simply the sum of all four:

  ```wiki
  data TyThing = AnId     Id
  	     | AConLike ConLike
  	     | ATyCon   TyCon
  	     | AClass   Class

  data ConLike = RealDataCon DataCon | PatSynCon PatSyn
  ```

  For example, a type environment is a map from `Name` to `TyThing`.  (The fact that a `Name` tells what name space it belongs to allow, for example, identically named values and types to  sit in a single map.)


All these data types are implemented as a big record of information that tells you everything about the entity.  For example, a `TyCon` contains a list of its data constructors; a `DataCon` contains its type (which mentions its `TyCon`); a `Class` contains the `Id`s of all its method selectors; and an `Id` contains its type (which mentions type constructors and classes).  



So you can see that the GHC data structures for entities is a *graph* not tree: everything points to everything else.  This makes it very convenient for the consumer, because there are accessor functions with simple types, such as `idType :: Id -> Type`.  But it means that there has to be some tricky almost-circular programming ("knot-tying") in the type checker, which constructs the entities. See [tying the knot](commentary/compiler/tying-the-knot) for more details on this process.


## Type variables and term variables



Type variables and term variables are represented by a single data type, `Var`, thus ([compiler/basicTypes/Var.hs](/trac/ghc/browser/ghc/compiler/basicTypes/Var.hs)):


```wiki
type Id    = Var
type TyVar = Var
```


It's incredibly convenient to use a single data type for both, rather than using one data type for term variables and one for type variables.  For example:


- Finding the free variables of a term gives a set of variables (both type and term variables): `exprFreeVars :: CoreExpr -> VarSet`.
- We only need one lambda constructor in Core: `Lam :: Var -> CoreExpr -> CoreExpr`.


The `Var` type distinguishes the two sorts of variable; indeed, it makes somewhat finer distinctions ([compiler/basicTypes/Var.hs](/trac/ghc/browser/ghc/compiler/basicTypes/Var.hs)):


```wiki
data Var
  = TyVar {
	varName    :: !Name,
	realUnique :: FastInt,		-- Key for fast comparison
	tyVarKind :: Kind,
        isCoercionVar :: Bool }

  | TcTyVar { 				-- Used only during type inference
	varName        :: !Name,
	realUnique     :: FastInt,
	tyVarKind      :: Kind,
	tcTyVarDetails :: TcTyVarDetails }

  | GlobalId { 			-- Used for imported Ids, dict selectors etc
	varName    :: !Name,	-- Always an External or WiredIn Name
	realUnique :: FastInt,
   	idType     :: Type,
	idInfo     :: IdInfo,
	gblDetails :: GlobalIdDetails }

  | LocalId { 			-- Used for locally-defined Ids (see NOTE below)
	varName    :: !Name,
	realUnique :: FastInt,
   	idType     :: Type,
	idInfo     :: IdInfo,
	lclDetails :: LocalIdDetails }
```


Every `Var` has fields `varName::Name` and a `realUnique::FastInt`. The latter is identical to the `Unique` in the former, but is cached in the `Var` for fast comparison.



Here are some per-flavour notes:


<table><tr><th>`TyVar`</th>
<td>is self explanatory.
</td></tr></table>


<table><tr><th>`TcTyVar`</th>
<td>is used during type-checking only.  Once type checking is finished, there are no more `TcTyVar`s.
</td></tr></table>


<table><tr><th>`LocalId`</th>
<td>is used for term variables bound *in the module being compiled*.   More specifically, a `LocalId` is bound either *within* an expression (lambda, case, local let), or at the top level of the module being compiled.

- The `IdInfo` of a `LocalId` may change as the simplifier repeatedly bashes on it.
- A `LocalId` carries a flag saying whether it's exported. This is useful for knowing whether we can discard it if it is not used.

  ```wiki
  data LocalIdDetails 
    = NotExported	-- Not exported; may be discarded as dead code.
    | Exported	-- Exported; keep alive
  ```

</td></tr></table>


<table><tr><th>`GlobalId`</th>
<td>is used for fixed, immutable, top-level term variables, notably ones that are imported from other modules.  This means that, for example, the optimizer won't change its properties.

- Always has an `External` or `WiredIn` [Name](commentary/compiler/name-type), and hence has a `Unique` that is globally unique across the whole of a GHC invocation.
- Always bound at top level. 
- The `IdInfo` of a `GlobalId` is completely fixed.
- All implicit Ids (data constructors, class method selectors, record selectors and the like) are are `GlobalId`s from birth, even the ones defined in the module being compiled.
- When finding the free variables of an expression (`exprFreeVars`), we only collect `LocalIds` and ignore `GlobalIds`.

</td></tr></table>



All the value bindings in the module being compiled (whether top level or not) are `LocalId`s until the CoreTidy phase. In the CoreTidy phase, all top-level bindings are made into `GlobalId`s. This is the point when a `LocalId` becomes "frozen" and becomes a fixed, immutable `GlobalId`. 


## `GlobalIdDetails` and implict Ids



`GlobalId`s are further classified by their `GlobalIdDetails`.  This type is defined in [compiler/basicTypes/IdInfo.hs](/trac/ghc/browser/ghc/compiler/basicTypes/IdInfo.hs), because it mentions other structured types such as `DataCon`. Unfortunately it is *used* in Var.hs so there's a hi-boot knot to get it there. Anyway, here's the declaration (elided a little):


```wiki
data GlobalIdDetails
  = VanillaGlobal		-- Imported from elsewhere, a default method Id.
  | RecordSelId { ... }		-- Record selector
  | DataConWorkId DataCon	-- The Id for a data constructor *worker*
  | DataConWrapId DataCon	-- The Id for a data constructor *wrapper*
  | ClassOpId Class		-- An operation of a class
  | PrimOpId PrimOp		-- The Id for a primitive operator
  | FCallId ForeignCall		-- The Id for a foreign call
  | NotGlobalId			-- Used as a convenient extra return value from globalIdDetails
```


Some `GlobalId`s are called **implicit `Id`s**. These are `Id`s that are defined by a declaration of some other entity (not just an ordinary variable binding).  For example:


- The selectors of a record type
- The method selectors of a class
- The worker and wrapper Id for a data constructor


It's easy to distinguish these Ids, because the `GlobalIdDetails` field says what kind of thing it is: `Id.isImplicitId :: Id -> Bool`.


