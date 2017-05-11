# Coercions in GHC's core language



Ever since coercions were introduced into GHC's Core language
I have treated


- Coercions like types
- Coercion variables like type variables


In particular, casts, coercion applications, and coercion 
abstractoins are all erased before we generate code.



I now think that this is the wrong approach.  This note describes why.


## Difficulties with the current approach



Ther are two problems with the current approach


- Equality evidence variables ("type variables") are treated differently to dictionary evidence variables ("term varaibles"). This leads to lots of tiresome non-uniformities.
- In an abstraction `/\a\x:a.e` the type variable `a` can appear in the type of a term-variable binder `x`.  In contrast `x` can't appear in the type of another binder.  Coercion binders behave exactly like term binders in this way, and quite unlike type binders.
- More seriously, we don't have a decent way to handle superclass equalities.


The last problem is the one that triggered this note, and needs a bit more explanation.  Consider


```wiki
class (F a ~ b, Num a) => C a b where
  op :: a -> b
```


The dictionary for C looks like this:


```wiki
data C a b where
  MkC :: (F a ~ b, Num a) => (a->b) -> C a b
```


Now imagine typechecking a function like this


```wiki
f :: C a b => a -> a 
f x = x + 1
```


The Core program we generate looks something like this:


```wiki
f = /\a b. \(d:C a b) (x:a).
    let (nd : Num a) = case d of { MkC _ d _ -> d }
    in (+) nd x (fromInteger nd 1)
```


The `nd` binding extracts the `Num` superclass dictionary from the
`C` dictionary; the case expression is called a *superclass selector*.



Now suppose that we needed to use the equality superclass rather than
the `Num` superclass:


```wiki
g :: C a b => [F a] -> [b]
g xs = xs
```


The obvious translation would look like this:


```wiki
g = /\ab. \(d:C a b).
    let (eq : F a ~ b) = case d of { MkC eq _ _ -> eq }
    in xs |> [eq]
```


But Core doesn't (currently) have a let-binding form that binds a coercion 
variable, and whose right-hand side is a term (in this example, a case expression)
rather than a literal coercion!  So the current plan is to generate this 
instead:


```wiki
g = /\ab. \(d:C a b).
    case d of { MkC eq _ _ -> 
    in xs |> [eq] }
```


This non-uniformity of equality and dictionary evidence 
is extremely awkward in the desugarer. Moreover, it means that we can't abstract
the superclass selector; we'd really like to have:


```wiki
g = /\ab. \(d:C a b).
    let (eq : F a ~ b) = sc_sel1 d
    in xs |> [eq]
```


And it interacts poorly with the class-op rules that GHC uses to simplify
dictinary selectors.  Imagine the call


```wiki
dIB :: C Int Bool
dIB
  g Int Bool d
```


...unfinished...


## Main proposal



Recall our basic types


```wiki
type Id    = Var   -- in Var.lhs
type TyVar = Var

data CoreExpr      -- in CoreSyn.lhs
  = Var Var 
  | Lit Lit
  | Type Type
  | Coercion Coercion
  | App CoreExpr CoreExpr
  | Lam Var CoreExpr
  | Cast CoreExpr Coercion
  | Let CoreBind CoreExpr
  | Case... | Note ...


data CoreBind = NonRec Var CoreExpr
              | Rec [(Id,CoreExpr)]

data Type          -- in TypeRep.lhs
  = TyVar TyVar
  | AppTy Type Type
  | FunTy Type Type
  | ForAllTy Var Type
  | PredTy PredType
  | TyConApp TyCon [Type]

data PredType
  = EqPred Type Type
  | ClassP Class [Type]
  | IParam Name Type 
```


Note that


- `Var` can be a type variable, coercion variable, or term variable.  You can tell which with a dynamic test (e.g. `isId :: Var -> Bool`).

- `Lam` is used for type abstractions, coercion abstractions, and value abstractions.  The `Var` can tell you which.

- Type applications (in a term) look like `(App f (Type t))`.  The `(Type t)` part must literally appear there,  with no intervening junk.  This is not statically enforced, but it turns out to be much more convenient than having a constructor `TyApp CoreExpr Type`.


OK now the new proposal is to *treat equality evidence just like any other sort of evidence*.


- A coercion variable is treated like term-level identifier, not a type-level identifier. (More on what that means below.)

- A coercion is an `CoreExpr`, of form `Coercion g`, whose type is `(s ~ t)`, of form `PredTy (EqPred s t)`.

- Unlike type applications, coercion applications are not required to have a `(Coercion g)` as the argument.  For example, suppose we have

  ```wiki
  f :: forall a. (a~Int) => a -> Int
  id :: forall b. b->b
  c :: x~Int
  ```

  Then the term `(f x (id (x~Int) c))` would be fine. Notice that the coercion argument is an appplication of the identity function.  (Yes it's a bit contrived.)  In `CoreExpr` form it would look like:

  ```wiki
    App (App (Var f) (Type x))
        (App (App (Var id) (Type (PredTy (EqPred x Int))))
             (Var c))
  ```

- Similarly a let-binding can bind a coercion

  ```wiki
    Let (NonRec c (...a coercion-valued term..)) (...body...)
  ```

- Coercion application is call-by value.  Ditto let-bindings.  You must have the evidence before calling the function.


 


- So it doesn't make sense to have recursive coercion bindings.

- If we see `Let (NonRec c (Coercion g)) e` we can substitute `(Coercion g)` for any term-level occurrences of `c` in the term `e`, and `g` for `c` in any occurrences of `c` in coercions inside `e`.  (This seems a bit messy.)
