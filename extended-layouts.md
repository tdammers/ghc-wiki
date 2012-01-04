
This page is discussing the feature request of generalized `do`-layouts, see ticket [\#5744](http://gitlabghc.nibbler/ghc/ghc/issues/5744).



For translation and typechecking rules the following notation is used:


```wiki
Expression: e last_e f
Binders:    b
Types:      t a
```


Note that `{ ; }` is **either** exactly what it is **or** the whitespace notation where those tokens are filled in automatically by the lexer. It's why you would only have to explain `do { .. ; .. }` to give a complete understanding of how the `do`-notation works.


## The Monoid Layout



The monoid layout offers whitespace notation for monoids. It is intended to offer an alternative to `do`-notation exploits where libraries have added `Monad` instances to their actually non-monadic types only to be able use the whitespace notation. Typical examples are:


- [ blaze-html](http://hackage.haskell.org/package/blaze-html)
- [
  Data.Binary.Put](http://hackage.haskell.org/packages/archive/binary/0.5.0.2/doc/html/Data-Binary-Put.html)
- [
  Data.Cereal.Put](http://hackage.haskell.org/packages/archive/cereal/0.3.5.0/doc/html/Data-Serialize-Put.html)


These examples usually have a "dead" type variable which is used exclusively for defining the monad instance and is later set to `()` for all functions using this type. To build up the desired data type either `mappend` or a variation (e.g. `Append` for blaze-html) is used. This so called "Monad" is therefor actually a hidden monoid with the clear intention to abuse the current `do`-notation. With this extension the correct `Monoid` instance can be used instead.



The proposed keyword for the beginning of a monoid layout is `be` (as in "don't `do` anything").



**Translation rules**


```wiki
be { stmts }

stmts : e      ';' stmts  =  e `mappend` stmts
      | let b  ';' stmts  =  let b in stmts
      | last_e            =  last_e
```


**Typing rules**


```wiki
be { stmts }   ::             t

stmts : e      :: Monoid t => t
      | last_e ::             t
```

## Other Layouts



Other layouts could be added to this extension. These are just ideas/suggestions of what could be done and not the main focus of this extension.


### Application Layout



This is an infix layout operator which applies every expression from the layout to the RHS function. The typechecker has to make sure that the number of arguments is correct in this case.



The keyword for this layout would be `$$` with the `infixr 0` rule (roughly equivalent to the `$` function).



**Translation rules**


```wiki
f $$ { stmts 1 }

stmts n : e_n ';' stmts (n+1)  =  e_n
      m | e_m                  =  e_m
```


Where `e_n` is the n-th expression of `m` total expressions.



** Typing rules**


```wiki
f $$ { stmts 1 }              ::  a

stmts n : e_n                 ::  t_n

f                             ::  t_ns 1 -> a

t_ns  n : t_n -> t_ns (n+1)
      m | t_m
```


Again, `e_n` is the n-th expression of `m` total expressions and `t_n` its type. The order of the arguments (expressions) is important!



** Example **



The translation/typing rules look horribly difficult for what it actually does:


```wiki
f $$
  foo a b
  bar c d

=>

f (foo a b) (bar c d)
```

### Record Layout



** Translation rules **


```wiki
TODO...
```


** Typing rules **


```wiki
TODO...
```


** Example **


```wiki
data MyRecord = MyRecord {{
  a :: Int
  b :: Int

mr :: MyRecord
mr = MyRecord {{
  a = 1
  b = 2
```