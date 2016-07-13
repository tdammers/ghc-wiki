# Overview



This page documents a proposed syntactical extension called `ArgumentDo`. The feature request is tracked at [\#10843](http://gitlabghc.nibbler/ghc/ghc/issues/10843).



This extension would allow a `do` block, a lambda, and a few other syntactic constructs to be placed directly as a function argument, without parentheses or a `$`. For example,


```
atomically do
  v <- readTVar tv
  writeTVar tv $! v + 1
```


would be equivalent to


```
atomically (do
  v <- readTVar tv
  writeTVar tv $! v + 1)
```


and


```
withForeignPtr fptr \ptr -> c_memcpy buf ptr size
```


would be equivalent to


```
withForeignPtr fptr (\ptr -> c_memcpy buf ptr size)
```

# Changes to the grammar



The Haskell report [
defines](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003) the `lexp` nonterminal thus (`*` indicates a rule of interest):


```wiki
lexp  →  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
      |  let decls in exp                  (let expression)             *
      |  if exp [;] then exp [;] else exp  (conditional)                *
      |  case exp of { alts }              (case expression)            *
      |  do { stmts }                      (do expression)              *
      |  fexp 

fexp  →  [fexp] aexp                       (function application)
 
aexp  →  qvar                              (variable)
      |  gcon                              (general constructor)
      |  literal
      |  ( exp )                           (parenthesized expression) 
      |  qcon { fbind1 … fbindn }          (labeled construction)
      |  aexp { fbind1 … fbindn }          (labelled update)
      |  …
```


which means lambda, `let`, `if`, `case`, and `do` constructs cannot be used as either LHS or RHS of a function application. GHC Haskell has a few more constructs that fall into this category, such as `mdo`, `\case` and `proc` blocks.



The `ArgumentDo` extension would allow all these constructs in argument positions. This is accomplished by moving their production rules under `aexp`:


```wiki
lexp  →  fexp 

fexp  →  [fexp] aexp                       (function application)
 
aexp  →  qvar                              (variable)
      |  gcon                              (general constructor)
      |  literal
      |  ( exp )                           (parenthesized expression) 
      |  qcon { fbind1 … fbindn }          (labeled construction)
      |  aexp { fbind1 … fbindn }          (labelled update)
      -- Here are the moved rules
      |  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
      |  let decls in exp                  (let expression)             *
      |  if exp [;] then exp [;] else exp  (conditional)                *
      |  case exp of { alts }              (case expression)            *
      |  do { stmts }                      (do expression)              *
      |  …
```


Now the `lexp` nonterminal is redundant and can be dropped from the grammar.



Note that this change relies on an existing meta-rule to resolve ambiguities:


>
>
> The grammar is ambiguous regarding the extent of lambda abstractions, let expressions, and conditionals. The ambiguity is resolved by the meta-rule that each of these constructs extends as far to the right as possible.
>
>


For example, `f \a -> a b` will be parsed as `f (\a -> a b)`, not as `f (\a -> a) b`.


# Less obvious examples


## Deleting parentheses



This extension will most often allow deletion of just one `$` operator per application. However, sometimes it does more. For example, in the following example, you can't simply replace the parentheses with a `$`:


```
pi + f (do
  ...
  )
```


With `ArgumentDo`, you would be able to write the following instead:


```
pi + f do
   ...
```

## Multiple block arguments



A function may take multiple `do` blocks:


```
f do{ x } do { y }
```


or


```
f
  do x
  do y
```

## Block as a LHS



A `do` block can be LHS of a function application:


```
do f &&& g
x
```


would just mean


```
(f &&& g） x
```

# Design space



Possible modifications to the proposal include:


- Only allow `do` in argument positions, but no other constructs. This has an advantage of making a minimal change to the grammar, while addressing the most common case.


This proposal has been extensively discussed on [
haskell-cafe](https://mail.haskell.org/pipermail/haskell-cafe/2015-September/121217.html) and on [
reddit](https://www.reddit.com/r/haskell/comments/447bnw/does_argument_do_have_a_future/).



On the mailing list I see roughly 13 people in favor of the proposal and 12 people against it. Some major opinions (mostly copied from [
bgmari's summary](https://ghc.haskell.org/trac/ghc/ticket/10843#comment:12)).


## Pros


- It's easier to read than the alternative.
- This extension removes syntactic noise.
- This makes basic do-syntax more approachable to newbies; it is a commonly asked question as to why the $ is necessary.
- This simplifies the resulting AST, potentially making it simpler for editors and other tools to do refactoring.
- It's something that belongs in the main language, and if its something we'd consider for a mythical Haskell', it has to start as an extension.
- It gets rid of some cases where using $ doesn't work because $ interacts with other infix operators being used in the same expression.
- This would make do blocks consistent with record creation, where parentheses are skipped, allowing things such as return R { x = y}
- This does not change the meaning of any old programs, only allows new ones that were previously forbidden.
- This gets rid of the need for a specially-typed $ allowing runSt $ do ... 
- It allows unparenthesized non-trivial application arguments not only as the last argument (using `$`), but for all arguments, in separate lines, when headed by `do` or another group A construct.
- It makes the language more regular by reducing the number of nonterminals by one.

## Cons


- It's harder to read than the alternative.
- Creating a language extension to get rid of a single character is overkill and unnecessary.
- You can already get rid of the $ by just adding parentheses.
- More and more syntactic "improvements" just fragment the language.
- Although this is consistent with record syntax, record syntax without parents was a mistake originally.
