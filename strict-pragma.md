# Strict & StrictData



This page explains the motivation, semantics, and implementation of the new language extensions `StrictData` and  `Strict`.



Related tickets: [8347](http://gitlabghc.nibbler/ghc/ghc/issues/8347), [11182](http://gitlabghc.nibbler/ghc/ghc/issues/11182), [11193](http://gitlabghc.nibbler/ghc/ghc/issues/11193)





## The Problem



High-performance Haskell code (e.g. numeric code) can sometimes be littered with bang patterns, making it harder to read. The reason is that laziness isn't the right default in this particular code, but the programmer has no way to say that except by repeatedly adding bang patterns. This page proposes two new language extensions, `StrictData` and `Strict`, that allow the programmer to switch the default on a per module basis.


# StrictData



Informally the `StrictData` language extension switches data type declarations to be strict by default allowing fields to be lazy by adding a `~` in front of the field.


## Semantics



When the user writes


```wiki
data T = C a
```


we interpret it as if she had written


```wiki
data T = C !a
```


Haskell doesn't allow for `~` patterns in data constructor definitions today: we'll add support for such definitions and have it give the current lazy behavior.



The extension only affects definitions in *this module*.


# Strict


## Semantics



Informally the `Strict` language extension switches functions, data types, and bindings to be strict by default, allowing optional laziness by adding `~` in front of a variable. This essentially reverses the present situation where laziness is default and strictness can be optionally had by adding `!` in front of a variable.



`Strict` implies `StrictData`.


- **Function definitions.**  When the user writes

  ```wiki
  f x = ...
  ```

  we interpret it as if she had written

  ```wiki
  f !x = ...
  ```

  Adding `~` in front of `x` gives the old lazy behavior.

- **Let/where bindings.**  When the user writes

  ```wiki
  let x = ...
  let pat = ...
  ```

  we interpret it as if she had written

  ```wiki
  let !x = ...
  let !pat = ...
  ```

  Adding `~` in front of `x` gives the old lazy behavior.  Notice that we do *not* put bangs on nested patterns. For example

  ```wiki
   let (p,q) = if flob then (undefined, undefined) else (True, False)
   in ...
  ```

  will behave like

  ```wiki
   let !(p,q) = if flob then (undefined, undefined) else (True, False)
  ```

  which will strictly evaluate the RHS, and bind `p` and `q` to the components of the pair.  But the pair itself is lazy (unless we also compile the Prelude with `Strict`; see "Modularity" below).  So `p` and `q` may end up bound to `undefined`.  See also "Recursive and polymorphic let bindings" below.

- **Case expressions.**  The patterns of a case expression get an implicit bang, unless disabled with `~`.  For example

  ```wiki
    case x of (a,b) -> rhs
  ```

  is interpreted as

  ```wiki
    case x of !(a,b) -> rhs
  ```

  Since the semantics of pattern matching in case expressions is strict, this usually has no effect whatsoever.  But it does make a difference in the degenerate case of variables and newtypes.  So

  ```wiki
    case x of y -> rhs
  ```

  is lazy in Haskell, but with `Strict` is interpreted as

  ```wiki
   case x of !y -> rhs
  ```

  which evalutes x.  Similarly, if `newtype Age = MkAge Int`, then

  ```wiki
   case x of MkAge i -> rhs
  ```

  is lazy in Haskell; but with `Strict` the added bang makes it strict.

- **Top level bindings** are unaffected by `Strict`.  For example:

  ```wiki
   x = factorial 20
   (y,z) = if x > 10 then True else False
  ```

  Here `x` and the pattern binding `(y,z)` remain lazy.  Reason: there is no good moment to force them, until first use.

- **Newtypes.**  There is no effect on newtypes, which simply rename existing types.  For example:

  ```wiki
  newtype T = C a
  f (C x)  = rhs1
  g !(C x) = rhs2
  ```

  In ordinary Haskell , `f` is lazy in its argument and hence in `x`; and `g` is strict in its argument and hence also strict in `x`.  With `Strict`, both become strict because `f`'s argument gets an implict bang.

### Modularity



The extension only affects definitions *in this module*. Functions and data types imported from other modules are unaffected. For example, we won't evaluate the argument to `Just` before applying the constructor. Similarly we won't evaluate the first argument to `Data.Map.findWithDefault` before applying the function.



This is crucial to preserve correctness. Entities defined in other modules might rely on laziness for correctness (whether functional or performance).



Tuples, lists, `Maybe`, and all the other types from `Prelude` continue to have their existing, lazy, semantics.


### Recursive and polymorphic let bindings



Consider a banged let-binding


```wiki
  let !pat = rhs in body
```


Bang patterns in let bindings today (GHC 7.8.3 and earlier) behave as [described in the user manual](http://www.haskell.org/ghc/docs/7.8.3/html/users_guide/bang-patterns.html):


- The binding cannot be recursive
- The variables bound by the pattern always get monomorphic types
- The complete pattern is matched before evaluation of `body` begins


The intent was that it is valid to desugar such a binding to


```wiki
  case rhs of pat -> body
```


This currently applies even if the pattern is just a single variable, so that the `case` boils down to a `seq`.



Continuing with this rule would mean that `Strict` would not allow recursive or polymoprhic pattern bindings *at all*.  So instead we propose the following revised specification for bang patterns in let bindings.


- **Static semantics.** Exactly as in Haskell, unaffected by `Strict`.  This is more permissive than the current rule for bang patterns in let bindings, because it supports bang-patterns for polymorphic and recursive bindings.

- **Dynamic semantics.** Consider the rules in the box of [
  Section 3.12 of the Haskell report](http://www.haskell.org/onlinereport/exps.html#sect3.12).  Replace these rules with the following ones, where `v` stands for a variable

  - **(FORCE)**.  Replace any binding `!p = e` with `v = e; p = v` and replace `e0` with `v `seq` e0`, where `v` is fresh.  This translation works fine if `p` is already a variable `x`, but can obviously be optimised by not introducing a fresh variable `v`.

  - **(SPLIT)**. Replace any binding `p = e`, where `p` is not a variable, with `v = e; x1 = case v of p -> x1; ...; xn = case v of p -> xn`, where `v` is fresh and `x1`..`xn` are the bound variables of `p`.  Again if `e` is a variable, you can optimised his by not introducing a fresh variable.

>
>
> The result will be a (possibly) recursive set of bindings, binding only simple variables on the LHS.  (One could go one step further, as in the Haskell Report and make the recursive bindings non-recursive using `fix`, but we do not do so in Core, and it only obfuscates matters, so we do not do so here.)
>
>


Here are some examples of how this translation works.  The first expression of each sequence is Haskell source; the subsequent ones are Core.



Here is a simple non-recursive case.


```wiki
let x :: Int     -- Non-recursive
    !x = factorial y
in body

 ===> (FORCE)
     let x = factorial y in x `seq` body

 ===> (inline seq)
     let x = factorial y in case x of x -> body

 ===> (inline x)
     case factorial y of x -> body
```


Same again, only with a pattern binding


```wiki
let !(x,y) = if blob then (factorial p, factorial q) else (0,0)
in body

 ===> (FORCE)
     let v = if blob then (factorial p, factorial q) else (0,0)
         (x,y) = v
     in v `seq` body

 ===> (SPLIT)
     let v = if blob then (factorial p, factorial q) else (0,0)
         x = case v of (x,y) -> x
         y = case v of (x,y) -> y
     in v `seq` body

 ===> (inline seq, float x,y bindings inwards)
     let v = if blob then (factorial p, factorial q) else (0,0)
     in case v of v -> let x = case v of (x,y) -> x
                           y = case v of (x,y) -> y
                       in body

 ===> (fluff up v's pattern; this is a standard Core optimisation)
     let v = if blob then (factorial p, factorial q) else (0,0)
     in case v of v@(p,q) -> let x = case v of (x,y) -> x
                                 y = case v of (x,y) -> y
                             in body

 ===> (case of known constructor)
     let v = if blob then (factorial p, factorial q) else (0,0)
     in case v of v@(p,q) -> let x = p
                                 y = q
                             in body

 ===> (inline x,y)
     let v = if blob then (factorial p, factorial q) else (0,0)
     in case v of (p,q) -> body[p/x, q/y]
```


The final form is just what we want: a simple case expression.



Here is a recursive case


```wiki
letrec xs :: [Int]  -- Recursive
       !xs = factorial y : xs
in body

 ===> (FORCE)
     letrec xs = factorial y : xs in xs `seq` body

 ===> (inline seq)
     letrec xs = factorial y : xs in case xs of xs -> body

 ===> (eliminate case of value)
     letrec xs = factorial y : xs in body
```


and a polymorphic one:


```wiki
let f :: forall a. [a] -> [a]    -- Polymorphic
    !f = fst (reverse, True)
in body

 ===> (FORCE)
     let f = /\a. fst (reverse a, True) in f `seq` body
        -- Notice that the `seq` is added only in the translation to Core
        -- If we did it in Haskell source, thus
        --    let f = ... in f `seq` body
        -- then f's polymorphic type would get intantiated, so the Core
        -- translation woudl be
        --    let f = ... in f Any `seq` body

 ===> (inline seq, inline f)
     case (/\a. fst (reverse a, True)) of f -> body
```


When overloading is involved, the results might be slightly counter intuitive:


```wiki
let f :: forall a. Eq a => a -> [a] -> Bool    -- Overloaded
    !f = fst (member, True)
in body

 ===> (FORCE)
     let f = /\a \(d::Eq a). fst (member, True) in f `seq` body

 ===> (inline seq, case of value)
     let f = /\a \(d::Eq a). fst (member, True) in body
```


Note that the bang has no effect at all in this case


### Interaction with irrefutable patterns



With `-XStrict` the `~` is used to recover ordinary patterns, to build an irrefutable pattern  `~(~pat)` is used.


## Implementation notes



Implementation notes



Consider a recursive group like this


```wiki
  letrec
     f : g = rhs[f,g]
  in <body>
```


Without `Strict`, we get a translation like this:


```wiki
  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (fm,gm)

  in let f = /\a. case t a of (fm,_) -> fm
  in let g = /\a. case t a of (_,gm) -> gm
  in <body>
```


Here `tm` is the monomorphic binding for `rhs`.



With `Strict`, we want to force `tm`, but NOT `fm` or `gm`.
Alas, `tm` isn't in scope in the `in <body>` part.



The simplest thing is to return it in the polymoprhic
tuple `t`, thus:


```wiki
  let t = /\a. letrec tm = rhs[fm,gm]
                      fm = case t of fm:_ -> fm
                      gm = case t of _:gm -> gm
                in
                (tm, fm, gm)

  in let f = /\a. case t a of (_,fm,_) -> fm
  in let g = /\a. case t a of (_,_,gm) -> gm
  in let tm = /\a. case t a of (tm,_,_) -> tm
  in tm `seq` <body>
```