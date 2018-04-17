# Applicative do-notation



This is a proposal to add support to GHC for desugaring do-notation into Applicative expressions where possible.



It's described in some detail in the paper: [
Desugaring Haskell’s do-notation Into Applicative Operations](https://www.microsoft.com/en-us/research/publication/desugaring-haskells-do-notation-into-applicative-operations/) (ICFP'16).



An implementation was merged for GHC8: [
https://github.com/ghc/ghc/commit/8ecf6d8f7dfee9e5b1844cd196f83f00f3b6b879](https://github.com/ghc/ghc/commit/8ecf6d8f7dfee9e5b1844cd196f83f00f3b6b879).



See also [RecursiveDo](recursive-do)


## Tickets



Use Keyword = `ApplicativeDo` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#10892](http://gitlabghc.nibbler/ghc/ghc/issues/10892)</th>
<td>ApplicativeDo should use \*\> and \<\*</td></tr>
<tr><th>[\#10976](http://gitlabghc.nibbler/ghc/ghc/issues/10976)</th>
<td>Applicative Comprehensions</td></tr>
<tr><th>[\#11982](http://gitlabghc.nibbler/ghc/ghc/issues/11982)</th>
<td>Typechecking fails for parallel monad comprehensions with polymorphic let (GHC 7.10.3 through 8.6.3)</td></tr>
<tr><th>[\#13309](http://gitlabghc.nibbler/ghc/ghc/issues/13309)</th>
<td>Use liftA2 in ApplicativeDo</td></tr>
<tr><th>[\#13511](http://gitlabghc.nibbler/ghc/ghc/issues/13511)</th>
<td>ApplicativeDo return case doesn't handle lets</td></tr>
<tr><th>[\#13905](http://gitlabghc.nibbler/ghc/ghc/issues/13905)</th>
<td>ApplicativeDo is too strict with newtype patterns</td></tr>
<tr><th>[\#13906](http://gitlabghc.nibbler/ghc/ghc/issues/13906)</th>
<td>ApplicativeDo doesn't handle existentials as well as it could</td></tr>
<tr><th>[\#14252](http://gitlabghc.nibbler/ghc/ghc/issues/14252)</th>
<td>ApplicativeDo: Add compiler message about irrefutable pattern matches and Monad constraints</td></tr>
<tr><th>[\#14700](http://gitlabghc.nibbler/ghc/ghc/issues/14700)</th>
<td>ApplicativeDo in MonadComprehensions</td></tr>
<tr><th>[\#15016](http://gitlabghc.nibbler/ghc/ghc/issues/15016)</th>
<td>Referencing a do-bound variable in a rec block with ApplicativeDo results in variable not in scope during type checking</td></tr>
<tr><th>[\#15100](http://gitlabghc.nibbler/ghc/ghc/issues/15100)</th>
<td>\`ApplicativeDo\` needlessly uses \`join\` too much</td></tr>
<tr><th>[\#15344](http://gitlabghc.nibbler/ghc/ghc/issues/15344)</th>
<td>ApplicativeDo seems to prevent the fail method from being used</td></tr>
<tr><th>[\#16135](http://gitlabghc.nibbler/ghc/ghc/issues/16135)</th>
<td>Panic with ExistentialQuantification and ApplicativeDo</td></tr>
<tr><th>[\#16171](http://gitlabghc.nibbler/ghc/ghc/issues/16171)</th>
<td>"ApplicativeDo" disables -Wunused-do-binds?</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#11607](http://gitlabghc.nibbler/ghc/ghc/issues/11607)</th>
<td>ApplicativeDo easily foiled with \`pure\`</td></tr>
<tr><th>[\#11612](http://gitlabghc.nibbler/ghc/ghc/issues/11612)</th>
<td>Bug in ApplicativeDo</td></tr>
<tr><th>[\#11835](http://gitlabghc.nibbler/ghc/ghc/issues/11835)</th>
<td>ApplicativeDo failed to desugar last line with pure $ \<expr\></td></tr>
<tr><th>[\#12143](http://gitlabghc.nibbler/ghc/ghc/issues/12143)</th>
<td>ApplicativeDo Fails to Desugar 'return True'</td></tr>
<tr><th>[\#12490](http://gitlabghc.nibbler/ghc/ghc/issues/12490)</th>
<td>With RebindableSyntax, ApplicativeDo should eliminate return/pure</td></tr>
<tr><th>[\#13242](http://gitlabghc.nibbler/ghc/ghc/issues/13242)</th>
<td>Panic "StgCmmEnv: variable not found" with ApplicativeDo and ExistentialQuantification</td></tr>
<tr><th>[\#13648](http://gitlabghc.nibbler/ghc/ghc/issues/13648)</th>
<td>ApplicativeDo selects "GHC.Base.Monad.return" when actions are used without patterns.</td></tr>
<tr><th>[\#13875](http://gitlabghc.nibbler/ghc/ghc/issues/13875)</th>
<td>ApplicativeDo desugaring is lazier than standard desugaring</td></tr>
<tr><th>[\#14105](http://gitlabghc.nibbler/ghc/ghc/issues/14105)</th>
<td>ApplicativeDo causes GHC panic on irrefutable list pattern match</td></tr>
<tr><th>[\#14163](http://gitlabghc.nibbler/ghc/ghc/issues/14163)</th>
<td>Stack Overflow with ApplicativeDo</td></tr>
<tr><th>[\#14249](http://gitlabghc.nibbler/ghc/ghc/issues/14249)</th>
<td>ApplicativeDo: Pattern matching on a bind forces a Monad constraint</td></tr>
<tr><th>[\#14471](http://gitlabghc.nibbler/ghc/ghc/issues/14471)</th>
<td>Certain do blocks cause TH to barf when ApplicativeDo is enabled</td></tr>
<tr><th>[\#14670](http://gitlabghc.nibbler/ghc/ghc/issues/14670)</th>
<td>-XRebindableSyntax needs return?</td></tr>
<tr><th>[\#15422](http://gitlabghc.nibbler/ghc/ghc/issues/15422)</th>
<td>GHCi debugger doesn't see free variables when using ApplicativeDo</td></tr></table>



## Summary



`ApplicativeDo` is a language extension enabled in the usual way via


```wiki
{-# LANGUAGE ApplicativeDo #-}
```


When `ApplicativeDo` is turned on, GHC will use a different method for desugaring `do`-notation, which attempts to use the `Applicative` operator `<*>` as far as possible, along with `fmap` and `join`.



`ApplicativeDo` makes it possible to use `do`-notation for types that are `Applicative` but not `Monad`.  (See examples below).



For a type that is a `Monad`, `ApplicativeDo` implements the same semantics as the standard `do`-notation desugaring, provided `<*>` = `ap` for this type.



`ApplicativeDo` respects `RebindableSyntax`: it will pick up whatever `<*>`, `fmap`, and `join` are in scope when `RebindableSyntax` is on.


## Motivation


1. Some Monads have the property that Applicative bind is more
  efficient than Monad bind.  Sometimes this is *really
  important*, such as when the Applicative bind is 
  concurrent whereas the Monad bind is sequential (c.f. [
  Haxl](https://github.com/facebook/Haxl)).  For
  these monads we would like the do-notation to desugar to
  Applicative bind where possible, to take advantage of the improved
  behaviour but without forcing the user to explicitly choose.

1. Applicative syntax can be a bit obscure and hard to write.
  Do-notation is more natural, so we would like to be able to write
  Applicative composition in do-notation where possible.  For example:

  ```wiki
  (\x y z -> x*y + y*z + z*x) <$> expr1 <*> expr2 <*> expr3
  ```

  vs.

  ```wiki
  do x <- expr1; y <- expr2; z <- expr3; return (x*y + y*z + z*x)
  ```

## Example 1


```wiki
do
  x <- a
  y <- b
  return (f x y)
```


This translates to


```wiki
(\x y -> f x y) <$> a <*> b
```


Here we noticed that the statements `x <- a` and `y <- b` are independent, so we can make an `Applicative` expression.  Note that the desugared version uses the operators `<$>` and `<*>`, so its inferred type will mention `Applicative` only rather than `Monad`.  Therefore this `do` block will work for a type that is `Applicative` but not `Monad`.


## Example 2



If the final statement does not have a `return`, then we need to use `join`:


```wiki
do
  x <- a
  y <- b
  f x y
```


Translates to


```wiki
join ((\x y -> f x y) <$> a <*> b)
```


Since `join` is a `Monad` operation, this expression requires `Monad`.


## Example 3


```wiki
  do
    x1 <- A
    x2 <- B
    x3 <- C x1
    x4 <- D x2
    return (x1,x2,x3,x4)
```


Here we can do `A` and `B` together, and `C` and `D` together.  We could do it like this:


```wiki
  do
    (x1,x2) <- (,) <$> A <*> B
    (\x3 x4 -> (x1,x2,x3,x4)) <$> C x1 <*> D x2
```


But it is slightly more elegant like this:


```wiki
   join ((\x1 x2 -> (\x3 x4 -> (x1,x2,x3,x4)) <$> C x1 <*> D x2)) <$> A <*> B)
```


because we avoid the intermediate tuple.


## Example 4


```wiki
   do
     x <- A
     y <- B x
     z <- C
     return (f x y z)
```


Now we have a dependency: `y` depends on `x`, but there is still an opportunity to use `Applicative` since `z` does not depend on `x` or `y`.  In this case we end up with:


```wiki
  (\(x,y) z -> f x y z) <$> (do x <- A; y <- B x; return (x,y)) <*> C
```


Note that we had to introduce a tuple to return both the values of `x` and `y` from the inner `do` expression



It's important that we keep the original ordering.  For example, we don't want this:


```wiki
  do 
    (x,z) <- (,) <$> A <*> C
    y <- B x
    return (f x y z)
```


because this has a different semantics from the standard 'do' desugaring; a `Monad` that cares about ordering will expose the difference.



Another wrong result would be:


```wiki
  do
    x <- A
    (\y z -> f x y z) <$> B x <*> C
```


Because this version has less parallelism than the first result, in which `A` and `B` could be performed at the same time as `C`.


## Example 5



In general, `ApplicativeDo` might have to build a complicated nested `Applicative` expression.


```wiki
do
  x1 <- a
  x2 <- b
  x3 <- c x1
  x4 <- d
  return (x2,x3,x4)
```


Here we can do `a/b/d` in parallel, but `c` depends on `x1`, which makes things a bit tricky: remember that we have to retain the semantics of standard `do` desugaring, so we can't move the call to `c` after the call to `d`.



This translates to


```wiki
(\(x2,x3) x4 -> (x2, x3, x4))
  <$> join ((\x1 x2 -> do
                         x3 <- c x1
                         return (x2,x3))
              <$> a
              <*> b)
  <*> d)
```


We can write this expression in a simpler way using `|` for applicative composition (like parallel composition) and `;` for monadic composition (like sequential composition): `((a | b) ; c) | d`.



Note that this isn't the only good way to translate this expression, this is also possible: `(a ; (b | c)) | d`.  It's not possible to know which is better.  `ApplicativeDo` makes a best-effort attempt to use parallel composition where possible while retaining the semantics of the standard 'do' desugaring.


## Syntax & spec



There's a toy implementation which includes the syntax, desugaring, transformation and some examples here: [
https://github.com/simonmar/ado/blob/52ba028cad68af578bcdfb3f1c5b905f5b9c5617/adosim.hs](https://github.com/simonmar/ado/blob/52ba028cad68af578bcdfb3f1c5b905f5b9c5617/adosim.hs)



Syntax: 


```wiki
  expr ::= ... | do {stmt_1; ..; stmt_n} expr | ...

  stmt ::= pat <- expr
         | (arg_1 | ... | arg_n)  -- applicative composition, n>=1
         | ...                    -- other kinds of statement (e.g. let)

  arg ::= pat <- expr
        | {stmt_1; ..; stmt_n} {var_1..var_n}
```


Desugaring for `do stmts`:


```wiki
dsDo {} expr = expr

dsDo {pat <- rhs; stmts} expr =
   rhs >>= \pat -> dsDo stmts expr

dsDo {(arg_1 | ... | arg_n)} (return expr) =
  (\argpat (arg_1) .. argpat(arg_n) -> expr)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)

dsDo {(arg_1 | ... | arg_n); stmts} expr =
  join (\argpat (arg_1) .. argpat(arg_n) -> dsDo stmts expr)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)
```


where


```wiki
argpat (pat <- expr)   = pat
argpat ({stmt_1; ..; stmt_n} {var_1..var_n})  = (var_1, .., var_n)

argexpr (pat <- expr)  = expr
argexpr ({stmt_1; ..; stmt_n} {var_1..var_n})  =
  dsDo {stmt_1; ..; stmt_n; return (var_1, ..., var_n)}
```

## Transformation


```wiki
ado {}    tail = tail
ado {pat <- expr} {return expr'} = (mkArg(pat <- expr)); return expr'
ado {one} tail = one : tail
ado stmts tail
  | n == 1 = ado before (ado after tail) where (before,after) = split(stmts_1)
  | n > 1  = (mkArg(stmts_1) | ... | mkArg(stmts_n)); tail
  where
    {stmts_1 .. stmts_n} = segments(stmts)

segments(stmts) =
  -- divide stmts into segments with no interdependencies

mkArg({pat <- expr}) = (pat <- expr)
mkArg({stmt_1; ...; stmt_n}) =
  {stmt_1; ...; stmt_n} {vars(stmt_1) u .. u vars(stmt_n)}

split({stmt_1; ..; stmt_n) =
  ({stmt_1; ..; stmt_i}, {stmt_i+1; ..; stmt_n})
  -- 1 <= i <= n
  -- i is a good place to insert a bind
```

## Differences from the actual implementation


1. The final expr in a "do" is a LastStmt, instead of being carried around separately.

1. there is no stripping of "return" during desugaring, it is handled earlier in the renamer instead.

1. arg has an optional "return", for the same reason as (2)


(2) and (3) are so that we can typecheck the syntax without having to desugar it first.



The syntax and desugaring rules are:


```wiki
  expr ::= ... | do {stmt_1; ..; stmt_n} | ...

  stmt ::= expr                   -- last stmt in a "do" must be this
         | pat <- expr
         | (arg_1 | ... | arg_n)
         | join (arg_1 | ... | arg_n)
         | ...

  arg ::= pat <- expr
        | {stmt_1..stmt_n} {var_1..var_n} maybe_return

  maybe_return ::= return | ()
```

```wiki
dsDo {expr} = expr

dsDo {pat <- rhs; stmts} =
   rhs >>= \pat -> dsDo stmts

dsDo {(arg_1 | ... | arg_n); stmts} =
  (\argpat (arg_1) .. argpat(arg_n) -> dsDo stmts)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)

dsDo {join (arg_1 | ... | arg_n); stmts} =
  join (\argpat (arg_1) .. argpat(arg_n) -> dsDo stmts)
     <$> argexpr(arg_1)
     <*> ...
     <*> argexpr(arg_n)
```


where


```wiki
argpat (pat <- expr)   = pat
argpat ({stmt_1..stmt_n} {var_1..var_n} _)  = (var_1, .., var_n)

argexpr (pat <- expr)  = expr
argexpr ({stmt_1..stmt_n} {var_1..var_n} ())  =
  dsDo {stmt_1; ..; stmt_n; (var_1, ..., var_n)}
argexpr ({stmt_1..stmt_n} {var_1..var_n} return)  =
  dsDo {stmt_1; ..; stmt_n; return (var_1, ..., var_n)}
```


Note that there's no matching on "return" during desugaring, the
"return" has already been removed.


---


## Related proposals


- [
  Max's proposal on haskell-cafe](http://www.haskell.org/pipermail/haskell-cafe/2011-September/095153.html)
- [
  Control.Applicative.QQ.ADo](http://hackage.haskell.org/package/applicative-quoters-0.1.0.7/docs/Control-Applicative-QQ-ADo.html)

## Implementation



The implementation is tricky, because we want to do a transformation that affects type checking (and renaming, because we might be using `RebindableSyntax`), but we still want type errors in terms of the original source code.  Therefore we calculate everything necessary to do the transformation during renaming, but leave enough information behind to reconstruct the original source code for the purposes of error messages.



See comments in [
https://phabricator.haskell.org/D729](https://phabricator.haskell.org/D729) for more details.


### Tricky case


```wiki
 do { x <- A
    ; y <- B
    ; z <- C x
    ; return (z+y) }
```


Then we could do `A ; (B | C)` or `(A | B) ; C`.  


- If `tA + (max( tB, tC )) < max( tA, tB ) + tC`, then first is best, otherwise second.


If A is smaller than B and C, first is best.  If C is smaller than A and B then second is best.


