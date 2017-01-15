


# The data type `Type` and its friends



GHC compiles a typed programming language, and GHC's intermediate language is explicitly typed.  So the data type that GHC uses to represent types is of central importance.



The single data type `Type` is used to represent


- Types (possibly of higher kind); e.g. `[Int]`, `Maybe`
- Kinds (which classify types and coercions); e.g. `(* -> *)`, `T :=: [Int]`.  See [Commentary/Compiler/Kinds](commentary/compiler/kinds)
- Sorts (which classify types); e.g. `TY`, `CO`


GHC's use of [coercions and equality constraints](commentary/compiler/fc) is important enough to deserve its own page.



The module `TypeRep` exposes the representation because a few other modules (`Type`, `TcType`, `Unify`, etc) work directly on its representation.  However, you should not lightly pattern-match on `Type`; it is meant to be an abstract type.  Instead, try to use functions defined by `Type`, `TcType` etc.


## Views of types



Even when considering only types (not kinds, sorts, coercions) you need to know that GHC uses a *single* data type for types. You can look at the same type in different ways:


- The "typechecker view" regards the type as a Haskell type, complete with implicit parameters, class constraints, and the like.  For example:

  ```wiki
    forall a. (Eq a, ?x::Int) => a -> Int
  ```

  Functions in `TcType` take this view of types; e.g. `tcSplitSigmaTy` splits up a type into its forall'd type variables, its constraints, and the rest.

- The "core view" regards the type as a Core-language type, where class and implicit parameter constraints are treated as function arguments:

  ```wiki
    forall a. Eq a -> Int -> a -> Int
  ```

  Functions in `Type` take this view.


The data type `Type` represents type synonym applications in un-expanded form.  E.g.


```wiki
type T a = a -> a
f :: T Int
```


Here `f`'s type doesn't look like a function type, but it really is.  The function `Type.coreView :: Type -> Maybe Type` takes a type and, if it's a type synonym application, it expands the synonym and returns `Just <expanded-type>`.  Otherwise it returns `Nothing`.



Now, other functions use `coreView` to expand where necessary, thus:


```wiki
  splitFunTy_maybe :: Type -> Maybe (Type,Type)
  splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
  splitFunTy_maybe (FunTy t1 t2) = Just (t1,t2)
  splitFunTy_maybe other         = Nothing
```


Notice the first line, which uses the view, and recurses when the view 'fires'.  Since `coreView` is non-recursive, GHC will inline it, and the optimiser will ultimately produce something like:


```wiki
  splitFunTy_maybe :: Type -> Maybe (Type,Type)
  splitFunTy_maybe (PredTy p)    = splitFunTy_maybe (predTypeRep p)
  splitFunTy_maybe (NoteTy _ ty) = splitFunTy_maybe ty
  splitFunTy_maybe (FunTy t1 t2) = Just (t1,t2)
  splitFunTy_maybe other         = Nothing
```

## The representation of `Type`



Here, then is the representation of types (see [compiler/types/TyCoRep.hs](/trac/ghc/browser/ghc/compiler/types/TyCoRep.hs) for more details):


```wiki
type TyVar = Var

data Type = TyVarTy TyVar			-- Type variable
  	  | AppTy Type Type			-- Application
  	  | TyConApp TyCon [Type]		-- Type constructor application
  	  | FunTy Type Type			-- Arrow type
  	  | ForAllTy Var Type			-- Polymorphic type
  	  | LitTy TyLit 			-- Type literals

data TyLit = NumTyLit Integer			-- A number
           | StrTyLit FastString		-- A string
```


Invariant: if the head of a type application is a `TyCon`, GHC *always* uses the `TyConApp` constructor, not `AppTy`.
This invariant is maintained internally by 'smart constructors'.
A similar invariant applies to `FunTy`; `TyConApp` is never used with an arrow type.



Type variables are represented by the `TyVar` constructor of the [data type Var](commentary/compiler/entity-types).  


## Overloaded types



In Haskell we write 


```wiki
f :: forall a. Num a => a -> a
```


but in Core the `=>` is represented by an ordinary `FunTy`. So f's type looks like this:


```wiki
   ForAllTy a (TyConApp num [TyVarTy a] `FunTy` TyVarTy a `FunTy` TyVarTy a)
where
   a   :: TyVar
   num :: TyCon
```


Nevertheless, we can tell when a function argument is actually a predicate (and hence should be displayed with `=>`, etc), using 


```wiki
isPredTy :: Type -> Bool
```


The various forms of predicate can be extracted thus:


```wiki
classifyPredType :: Type -> PredTree

data PredTree = ClassPred Class [Type]   -- Class predicates e.g. (Num a)
              | EqPred Type Type         -- Equality predicates e.g. (a ~ b)
              | TuplePred [PredType]     -- Tuples of predicates e.g. (Num a, a~b)
              | IrredPred PredType       -- Higher order predicates e.g. (c a)
```


These functions are defined in module `Type`.
 


## Classifying types



GHC uses the following nomenclature for types:


<table><tr><th>**Unboxed**</th>
<td>A type is unboxed iff its representation is other than a pointer. Unboxed types are also unlifted.
</td></tr></table>


<table><tr><th>**Lifted**</th>
<td>A type is lifted iff it has bottom as an element. Closures always have lifted types:  i.e. any let-bound identifier in Core must have a lifted type.  Operationally, a lifted object is one that can be entered. Only lifted types may be unified with a type variable.
</td></tr></table>


<table><tr><th>**Data**</th>
<td>A type declared with **`data`**.  Also boxed tuples.
</td></tr></table>


<table><tr><th>**Algebraic**</th>
<td>An algebraic data type is a data type with one or more constructors, whether declared with `data` or `newtype`.   An algebraic type is one that can be deconstructed        with a case expression.  "Algebraic" is **NOT** the same as "lifted",  because unboxed (and thus unlifted) tuples count as "algebraic".
</td></tr></table>


<table><tr><th>**Primitive**</th>
<td>a type is primitive iff it is a built-in type that can't be expressed        in Haskell.
  
Currently, all primitive types are unlifted, but that's not necessarily the case.  (E.g. Int could be primitive.)
</td></tr></table>


>
>
> Some primitive types are unboxed, such as Int\#, whereas some are boxed but unlifted (such as `ByteArray#`).  The only primitive types that we classify as algebraic are the unboxed tuples.
>
>


Examples of type classifications:


<table><tr><th>          </th>
<th> **Primitive** </th>
<th>        **Boxed**        </th>
<th> **Lifted** </th>
<th> **Algebraic**  
</th></tr>
<tr><th> `Int#`        </th>
<th> Yes             </th>
<th> No        </th>
<th> No          </th>
<th> No                
</th></tr>
<tr><th> `ByteArray#`        </th>
<th> Yes             </th>
<th> Yes        </th>
<th> No          </th>
<th> No                
</th></tr>
<tr><th> `(# a, b #)`        </th>
<th> Yes             </th>
<th> No        </th>
<th> No          </th>
<th> Yes        
</th></tr>
<tr><th> `(  a, b  )`        </th>
<th> No             </th>
<th> Yes        </th>
<th> Yes          </th>
<th> Yes        
</th></tr>
<tr><th> `[a]`        </th>
<th> No             </th>
<th> Yes        </th>
<th> Yes          </th>
<th> Yes        
</th></tr></table>


