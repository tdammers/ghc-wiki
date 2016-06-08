# Major proposed revision of Template Haskell



**Nota bene:** These changes have largely all been implemented in GHC 7.8 (although there are a few dangling tickets which are left.) When in doubt, look at the linked tickets.



This page explores a set of design proposals for Template Haskell.  They are inspired by discussion with Tim Sheard, Kathleen Fisher, and Jacques Carette.  It was originally triggered by several Template Haskell tickets: including [\#4230](http://gitlabghc.nibbler/ghc/ghc/issues/4230), [\#4135](http://gitlabghc.nibbler/ghc/ghc/issues/4135), [\#4128](http://gitlabghc.nibbler/ghc/ghc/issues/4128), [\#4170](http://gitlabghc.nibbler/ghc/ghc/issues/4170), [\#4125](http://gitlabghc.nibbler/ghc/ghc/issues/4125), [\#4124](http://gitlabghc.nibbler/ghc/ghc/issues/4124), [\#4364](http://gitlabghc.nibbler/ghc/ghc/issues/4364), [\#6062](http://gitlabghc.nibbler/ghc/ghc/issues/6062), [\#6089](http://gitlabghc.nibbler/ghc/ghc/issues/6089). (See also [\#7016](http://gitlabghc.nibbler/ghc/ghc/issues/7016), which work better with the suggestions below.) Taken together, these proposals would make quite a big change to TH, I think for the better.  Happily, I'm pretty sure they are relatively easy to implement.  



The page was originally [
this blog post](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal), but has moved here so that others can edit it.



There's an interesting [
critique of Template Haskell](http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell) on StackOverflow, some (but not all) of which is addressed by proposals here.



But I'd like to get the design right; hence this post.  I'm going to treat it as a working draft, and modify it in the light of comments.  So please comment.



I'm going to assume that you are more or less familiar with Template Haskell.  If not, there's lots of background on the [
Template Haskell page](http://www.haskell.org/haskellwiki/Template_Haskell).  It's a Wiki so you can help to improve it.



Here's the [implementation page](template-haskell/typed) the describes how we are going to implement this stuff.


# Some brief background



After parsing, GHC runs two completely separate passes, one after the other:


- The **renamer** resolves scopes, fixing precisely which *binding site* is connected to which *occurrence* of every variable.  For example, you write 

  ```wiki
  let x = \x -> x+1 in x
  ```

  and the renamer changes it to

  ```wiki
  let x_77 = \x_78 -> x_78 + 1 in x_77
  ```

  The renamer also performs dependency analysis, which sorts bindings (both value declarations and type declarations) into the smallest possible mutually recursive groups.  This prior sorting is required to maximise polymorphism in mutually recursive value bindings.

- The **typechecker** works on the renamed program, and typechecks it.


At the moment these two passes relate to Template Haskell as follows:


- **Quasi-quotes are run in the renamer**.  Why?  Because quasi-quotes can expand to patterns. 


Consider this, which has a quasi-quoted pattern:


```wiki
\x -> \ [pads| blah |] -> x+1
```

>
>
> Is the "x" in "x+1" bound by the outer `\x` or by the 'x' that might be brought into scope by the `[pads| blah |]` quasi-quote?  The only way to know is to run the quasi-quote, so that's what happens.
>
>

- **All other Template Haskell stuff is run in the typechecker**.  Why?  Because we try to typecheck quotations before feeding them into TH functions.  More on this below.

---


# The main issue



The big issue is this: Template Haskell is both too *weakly* typed and too *strongly* typed.


## Too weakly typed



A TH quotation has type `Q Exp`, a type that says nothing about the type of the quoted term.
For example, consider


```wiki
qnot :: Q Exp -> Q Exp
qnot x = [| not $x |]
```


Presumably the author expects `$x` to be a boolean-valued term, but
that is not checked.  For example we might write


```wiki
h = $(qnot [| "hello" |])
```


in which we pass a string-valued term to `qnot`. The splice will typecheck fine,
but the returned code will be the ill-typed `not "hello"`. There is no soundness problem
because GHC typechecks the result of the splice `$(qnot [| "hello" |])`, but 
the error is reported in code that the user never wrote.



Moreover, errors can be delayed.  For example, suppose `qnot` was like this:


```wiki
qnot :: Q Exp -> Q Exp
qnot x = [| (not $x, length $x) |]
```


This cannot possibly be right, becuase `$x` cannot be both a boolean and a list.
Yet TH will accept it, because a splice has type `forall a.a`.  The error will
only be reported to *callers* of `qnot`.



This is bad.  MetaML solves this problem by giving types to quoted terms, something
like this:


```wiki
qnot :: TExp Bool -> TExp Bool
qnot x = [| not $x |]
```


Here `TExp` (short for typed expressions) has a type parameter that expresses the
type of the quoted term.  



In TH we deliberately did not do this, because it restricts expressiveness; see
[
the original TH paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/index.htm).
We could make this choice without losing soundness because in TH, unlike in MetaML, all splicing is
done at compile time, and the result of a splice is typechecked from scratch.
But still, it's a weakness and, for some users (stand up Jacques), a very serious weakness.


## Too strongly typed



Even though TH cannot guarantee to construct only type-correct code,
every quotation is typechecked.  For example, the quotation `[| "hello" && True |]`
would be rejected because it is internally inconsistent.



But with the advent of type splices (a 
very useful feature) typechecking quotes has become hard to do. 
Consider this:


```wiki
  f :: Q Type -> Q [Dec]
  f t = [d| data T = MkT $t; 
            g (MkT x) = g+1 |]
```


This function f returns a declaration quote, declaring T and g.
You'll see that the constructor MkT takes an argument whose type is passed (as a quotation) to f -- a type splice.



The difficulty is that we can't typecheck the declaration of 'g'
until we know what $t is; and we won't know that until f is called.
In short, 


- we can't really typecheck the declaration quote at all


An analogous problem occurs in other declaration splices, for example


```wiki
  f t = [d| instance C $t where ... |]
```


Here GHC's check that an instance decl is always of form


```wiki
  instance C (T a b c)
```


falls over, again because we don't know what $t will be.
Here's another example:


```wiki
      [| let { f :: $t; f = e } in .. |]
```


We can't sensibly typecheck the term without knowing what f's type signature
is, and we can't know that without expanding the splice.



Here's a rather different example, [\#4364](http://gitlabghc.nibbler/ghc/ghc/issues/4364):


```wiki
type N0 = $( [t| Z |] )
type N1 = $( [t| Z |] )
```


Faced with a type splice


```wiki
type N0 = $(...blah...)
```


the renamer doesn't know what the splice will expand to, because splices are currently
run later, in the type checker.  So it pessimistically assumes that the
splice could expand to mention anything in scope. But that pessimistic assuption triggers
the error message


```wiki
     Cycle in type synonym declarations:
       m.hs:7:1-23: type N0 = $([t| Z |])
       m.hs:8:1-23: type N1 = $([t| Z |])  }}}
```


All this is quite annoying.  Several users have said "I'm just using a quotation as a convenient way 
to build a syntax tree.  Please don't even try to typecheck it; just wait
until it is finally spliced into a top-level splice".


---


# A proposal



TH currently embodies an uneasy compromise between being too strongly typed
and too weakly typed.  So my proposal is to move TH in both directions at once:


- Part A: Move the existing structures towards the expressive but weakly-typed end.
- Part B: Add new MetaML-style constructs for strongly-typed metaprogramming.
- Part C: Clean up and improve reification
- Part D: Quasi-quotation improvements

---


## Part A: Reduce typechecking for quotes



On the "make-it-weaker" front, here's what I propose:


- **Cease typechecking TH quotes altogether**. Instead, to use GHC's terminology, we would *rename* a quote, but not *typecheck* it.  The renaming pass ensures that the scope hygiene mechanisms would remain unchanged.  By not attempting to typecheck we avoid all the tricky problems sketched above.

- **Add pattern splices and local declaration splices**, as requested in [\#1476](http://gitlabghc.nibbler/ghc/ghc/issues/1476). For example

  ```wiki
  -- mkPat :: Q Pat -> Q Pat
  f $(mkPat [p| x |]) = x+1

  -- mkDecl :: Int -> Q [Dec]
  g x = let $(mkDecl 3) in ...
  ```

  These are not supported today because they bring new variables into scope, and hence are incompatible with running splices only after the renamer is finished; see [
  Notes on Template Haskell](http://research.microsoft.com/~simonpj/tmp/notes2.ps), section 8.  

- **Run TH splices in the renamer**, uniformly with quasi-quotes.  Of course, we must still typecheck the code we are about to run.  But there's an *existing* TH restriction that code run in top-level splices must be imported.  So we can typecheck this code even during renaming, because it can only mention imported (and hence already fully-typechecked) code.

>
>
> This solves [\#4364](http://gitlabghc.nibbler/ghc/ghc/issues/4364) because we run the splice in the renamer, so things are sorted out by the time we are checking for cycles (in the type checker).
>
>

- **Allow quoted names as patterns** as [
  requested by Conal Eliott](http://www.haskell.org/pipermail/libraries/2012-January/017449.html).  This is just a variation on allowing splices in patterns, since a quoted name `'x` is really just a simple splice


These changes would essentially implement the desires of those who say 
"I just want to generate syntax trees".  All the mentioned bug reports would be fixed.
The big loss is that quotes would not be typechecked at all.


## Lexical scoping



Consider these definitions:


```wiki
g :: Int -> Q Pat

y :: Int
y = 7

f :: Int -> Q Exp
f n = [| \ $(g n) -> y+1 |]
```


Where is the 'y' bound in the RHS of `f`?  


- Perhaps by the `y = 7` that is in scope at the definition of `f`?
- Perhaps by the pattern that `$(g n)` expands to?  
- Perhaps by a 'y' that is in scope at the splice site of `f`?
- Does it depend on whether `$(g n)` in fact binds 'y'?


A major point about TH is that we get lexical scoping (also called "hygienic").  So, to me it seems the the first of these choices is the only reasonable one.  If you want the second you can instead use explicit dynamic binding by saying


```wiki
f n = [| \ $(g n) -> $(return (VarE (mkName "n"))) + 1 |]
```


So the rule would be:


- In a quote, a variable 'v' is bound by the lexically enclosing binding of 'v', ignoring all pattern and declaration splices.


To be consistent this should apply to top level splices too.


## A variation (probably not)



A possible, rather *ad hoc*, variation would be to still typecheck quotes that are (a) top level, and (b) expression quotes. For example, we might still reject this:


```wiki
f x = [| $x + (True 'c') |]
```


because the quote is obviously ill-typed.  Only quotes nested inside top-level splices would avoid the type checker (because if the splice is run in the renamer, we can't typecheck the nested quote).  For example:


```wiki
$(f [| True 'c' |])
```


This splice would run in the renamer, and only the *result* of the splice would be typechecked. 
But what about this?


```wiki
f t = [| let g :: $t-> Int; g = e in .... |]
```


This is still very awkward to typecheck.  After all, if `$t` expands to a polymorphic type, the
result of the splice might typecheck, but it's really impossible to typecheck without 
knowing the signature.  Maybe we should just give up if there's a type splice?  The only really
simple thing is not to typecheck quotes at all.


---


## Part B: Add MetaML-style typed quotes



Template Haskell has quotes for terms, types, patterns, and
declarations.  They are all untyped, in the sense that the type of the quote
tells you nothing about the type of the quoted thing.  For example


```wiki
  [| True |] :: Q Exp
```


There's no clue that the type of the quoted expression is `Bool`.



In the case of terms (only), we know how from MetaML to
have *typed* quotations.  Here's a proposed extension to TH to add
typed term quotes: 


- **Add a new type of typed expressions** `TExp a`

- **Add a new term quotation form** `[|| e ||]`, called a *typed quote*; the type of the quote is `TExp ty`, where `ty` is the type of `e`.  In the type-system jargon, this is the "introduction form" for `TExp`.

- **Add a new splice form** `$$e`, called a *typed splice*.  The term `e` must have type `TExp ty`, and the splice `$$e` then has type `ty`.  This is the "elimination form" for `TExp`.

- **Add a constant which takes a typed quote and returns an untyped one**: `unType :: TExp a -> Q Exp`

- **Run these new typed splices in the typechecker**, not the renamer.


(The precise syntax for typed-quotes and type-splices is of course up for grabs.  But doubling the symbols seems plausible to me.)



Here's a standard example:


```wiki
power :: Int -> TExp (Int -> Int)
power n = [|| \x -> $$(go n [|| x ||]) ||]
  where
    go :: Int -> TExp Int -> TExp Int
    go 0 x = [|| 1 ||]
    go n x = [|| $$x * $$(go (n-1)) ||]
```


You could do this with existing TH but there'd be no guarantee that `power` would
return a well-typed term. With `TExp` there is.



Points to notice


- Unlike TH, the *only* way to construct a value of type `TExp` is with a quote.  You cannot drop into do-notation, nor use explicit construction of the values in the `Exp` algebraic data type.  That restriction limits expressiveness, but it enables the strong typing guarantees.

- There are no declaration, type, or pattern quotes for these typed quotes.  Only terms.

- You can't use an untyped splice in a typed quote, thus `[|| ...$(e)... ||]`. Similarly, you can't splice a type, pattern, or declaration group in a typed term quote.

- Using `unType` you can go from the typed world to the untyped one, which lets you mix the worlds.  Example:

  ```wiki
  f :: TExp Int -> Q Exp -> Q Exp
  f qt qu = [| $(unType qt) + $qu |]
  ```
- Unlike `Exp`, `TExp` is an abstract type, so you can't decompose values of type `TExp`.  All you can do with them is splice them (into a program or a larger quote).  Or you can convert to a `Q Exp` and *then* decompose, using the existing TH mechanisms.  For example

  ```wiki
  g :: TExp Int -> Q Exp
  g qt = do { tree <- unType qt
            ; case tree of
                VarE v -> ...
                ConE c -> ...
                ...etc...  }
  ```
- `TExp` is not a monad, but it is an applicative type constructor, although not quite an instance of `Applicative` class:

  ```wiki
     pure e   ===   [|| e ||]
     f <*> g   =    [|| $$f $$g ||]
  ```

  Reminder: the `Applicative` class looks like this

  ```wiki
  class Applicative f where
    pure :: a -> f a
    <*>  :: f (a->b) -> f a -> f b
  ```

  `TExp` is only "almost an instance" because `pure` isn't a function; its argument must be syntactically quoted.


**Syntax** is always a delicate point. 


- We could use some other kind of bracket, although brackets are always in short supply; eg `(| ... |)` or `{| ... |}`. 
- We could add Unicode brackets too (suggestions?); but I think we should have ASCII equivalents.
- Ian asked whether `$(...)` could accept either `Q Exp` or `TExp`.  I think not; we need to know which kind of splice it is before type checking.

---


## Part C: Reification and typechecking



The third part of this proposal concerns reification.
The Q monad gives you access to the typechecker's environment.
Notably, Template Haskell provides the function


```wiki
reify :: Name -> Q Info
```


which, given the `Name` of a variable, type, or class, looks the `Name` up in the type environment and returns what the type checker knows about it:


```wiki
data Info = TyConI       -- A type
               Dec	        -- Declaration of the type

          | ClassI       -- A class
               Dec              -- Declaration of the class
               [ClassInstance]	-- The instances of that class

          ...etc...
```

### What reify sees



A dark corner of `reify` is this: what types does `reify` see?  Consider


```wiki
f x = ($(do { xi <- reify 'x; ... }),
       x && True)
```


Here, `reify` can be used to examine the type of `x`.  But the type
of `x` isn't fully known until the type checker has seen the 
term `(x && True)`.   So in current TH it's going to be unpredicatable
what you see for `x`, which is hardly satisfactory.



It seems to me that the only decent, predictable story is to say 
that `reify` can only consult the *top-level* type environment.
More specifically, Template Haskell processes a program top-down:


```wiki
  module M where
   import ...
   f x = x
   $(th1 4)
   h y = k y y $(blah1)
   $(th2 10)
   w z = $(blah2)
```


TH processes this module as follows:


1. Typecheck down to, but not including, the first splice, `$(th1 4)`.  These declarations constitute the first *declaration group*.
1. Typecheck and run the splice, which returns a bunch of declarations D1
1. Typecheck the declarations D1 combined with the declarations down to, but not including, the second splice.  These declarations constitute the second declaration group.
1. Typecheck and run the next splice, `$(th2 10)`
1. Rinse and repeat


So the proposal is as follows. A *declaration group* is the chunk of declarations created by a top-level declaration splice, plus those following it, down to but not includig the next top-level declaration splice.  Then  **the type environment seen by `reify` includes all the declaration up to the end of the immediately preceding declaration block, but no more.**



So, in the preceding example:


- A `reify` inside the splice `$(th1 ..)` would see the definition of `f`.
- A `reify` inside the splice `$(blah)` would see the definition of `f`, but would not see any bindings created by `$(th1...)`.
- A `reify` inside the splice `$(th2..)` would see the definition of `f`, all the bindings created by `$(th1..)`, and teh definition of `h`.
- A `reify` inside the splice `$(blah2)` would see the same definitions as the splice `$(th2...)`.


This would mean that you could not say


```wiki
f x = x
g y = $(do { info <- reify 'f; ... })
```


because there is no top-level splice between the declaration of `f` and the splice. 
But that seems reasonable to me.  If you want that behaviour you can say


```wiki
f x = x
$(return [])
g y = $(do { info <- reify 'f; ... })
```

### Reifying expressions



But what about *expressions*? It would be useful (stand up Kathleen) 
to have a more elaborate reify, like this:


```wiki
typecheck :: [(Name,Type)]  -- Extend the type environment with this 
          -> Exp	    -- The expression to typecheck
          -> Q (Either String Type)
 -- Typecheck the expression, returning
 --    Left error-message     if typechecking fails
 --    Right type             if typechecking succeeds
```


(For GHCi users, `reify f` is like `:info f`, while `typecheck [] (...)` is like `:type (...)`.)



You might ask whether we *can* typeckeck an expression; remember, these `Q ty` things 
are going to be run in the renamer.  But if the type environment is that in force
just before the last top-level splice, then all is well: that stuff has been fully 
typechecked. 


---


## Part D: quasiquotation



This part is unrelated to the preceding proposals, and is responding to [\#4372](http://gitlabghc.nibbler/ghc/ghc/issues/4372) and [\#2041](http://gitlabghc.nibbler/ghc/ghc/issues/2041).


- For [\#2041](http://gitlabghc.nibbler/ghc/ghc/issues/2041), rather than the proposal made there, I think the nicest thing is for `Language.Haskell.TH` to expose a *Haskell* quasiquoter:

  ```wiki
  parseHaskell :: QuasiQuoter
  ```

  Remember that a `QuasiQuoter` is a quadruple of parsers:

  ```wiki
  data QuasiQuoter = QuasiQuoter { quoteExp  :: String -> Q Exp,
                                   quotePat  :: String -> Q Pat,
                                   quoteType :: String -> Q Type,
                                   quoteDec  :: String -> Q [Dec] }
  ```

  If TH provided such parsers, you could use them to parse antiquotes.  That seems better to than having strings in the TH syntax.


 


>
> >
> >
> > See [\#4430](http://gitlabghc.nibbler/ghc/ghc/issues/4430) for an excellent point about fixities.
> >
> >
>

- For [\#4372](http://gitlabghc.nibbler/ghc/ghc/issues/4372), I'm a bit agnostic.  There is no technical issue here; it's just about syntax.  Read the notes on the ticket.

- See [\#4429](http://gitlabghc.nibbler/ghc/ghc/issues/4429) for a suggestion about reifying `Names`.

---


## Part E: Other minor issues



This section collects other TH changes that I think should be done.


- The `InfixE` constructor of `Syntax.Exp` should only allow a `Var` in the operator position.  See Trac [\#4877](http://gitlabghc.nibbler/ghc/ghc/issues/4877)
