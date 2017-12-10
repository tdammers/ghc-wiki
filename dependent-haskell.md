# Adding dependent types to Haskell



This page is to track design and implementation ideas around adding a form of dependent types to Haskell. This work will also fix bug [\#7961](http://gitlabghc.nibbler/ghc/ghc/issues/7961). Richard Eisenberg (a.k.a. goldfire) is expecting to take on most (all?) of this work.



***Disclaimer:*** Everything below represents a research proposal. While it is my (RAE's) hope that something resembling this all will actually make it into GHC, no one should read anything too strongly into words like "will happen".


# Surface Language Design



It is possible to fix [\#7961](http://gitlabghc.nibbler/ghc/ghc/issues/7961) without any surface language changes, as that bug addresses only lifting restrictions on promotion. There is a chance that this bugfix will enter HEAD without all of the other features below, but this writeup generally will not consider fixing [\#7961](http://gitlabghc.nibbler/ghc/ghc/issues/7961) separate from adding dependent types.


## Merging Types and Kinds



Following the work in [the kind equality paper](dependent-haskell#okinds), the new Haskell will merge types and kinds into one syntactic and semantic category. Haskell would have the `* :: *` property. As a consequence, it will be easily possible to explicit quantify over kinds. In other words, the following type signature is allowed: `forall (k :: *) (a :: k). Proxy a -> Proxy a`. Furthermore, kind variables will be able to be listed explicitly when declaring datatypes and classes. Of course, if a kind variable is listed explicitly in the declaration of a type or class, then it also must be listed explicitly at the use sites. Note that this change will completely eliminate `BOX`.



There are actually two separate aspects to this change:


1. Merge the grammar of types and kinds. This is a simplification (with a sizable [caveat](dependent-haskell#)) of the current scenario and will fix the original motivation for [\#8706](http://gitlabghc.nibbler/ghc/ghc/issues/8706).
1. Add `* :: *`. Why do this? One alternative is to go the route of Coq and Agda and have an infinite tower of type universes. But, this adds a lot of complexity. These languages take this route because `* :: *` makes a language inconsistent as a logic. However, Haskell is *already* inconsistent as a logic (because of `undefined` and `GHC.Exts.Any`) and so we don't have to worry about a new source of inconsistency. Furthermore, the type safety of Haskell does not depend on its own consistency -- unlike Coq and Agda, Haskell relies on the consistency of a coercion language, which is not threatened by `* :: *`. See [the paper](dependent-haskell#okinds) for more details.

## Quantifiers



As pointed out in the [Hasochism paper](dependent-haskell#), Haskell currently enjoys a confluence of design decisions. One says that compile-time arguments are elided in runtime code. For example, when calling `map :: (a -> b) -> [a] -> [b]`, the type instantiations for `a` and `b` are properly arguments to `map` (and are passed quite explicitly in Core), but these arguments are always elided in surface Haskell. As the levels are mixing, we may want to revisit this. Along similar lines, type arguments in Haskell are always erasable -- that is, instantiations for types are never kept at runtime. While this is generally a Good Thing and powers much of Haskell's efficiency, dependent typing relies on keeping *some* types around at runtime. Here, it is even more apparent that sometimes, we want to be able to pass in values for type arguments, especially if those values can be inspected at runtime.



Haskell currently has three quantifiers: `forall`, `->`, and `=>`, as classified in the following table:


<table><tr><th>  Current Haskell  
</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> Quantifier </th>
<th> Dependent? </th>
<th> Visible? </th>
<th> Required? </th>
<th> Relevant? 
</th></tr>
<tr><th> `forall` </th>
<th> Yes </th>
<th> No (unification) </th>
<th> No (FVs) </th>
<th> No 
</th></tr>
<tr><th> `->` </th>
<th> No </th>
<th> Yes (as term) </th>
<th> Yes </th>
<th> Yes 
</th></tr>
<tr><th> `=>` </th>
<th> No </th>
<th> No (solving) </th>
<th> Yes </th>
<th> Yes 
</th></tr></table>


<table><tr><th>Dependent</th>
<td>
*Dependent* means that the quantified thing (henceforth, *quantifiee*) can appear later in the type. This is clearly true for `forall`-quantified things and clearly not true for `->`-quantified things. (That is, if we have `Int -> Bool`, we can't mention the `Int` value after the `->`!)
</td></tr></table>


<table><tr><th>Required</th>
<td>
(Not very important.) A *required* quantification is one that must textually appear in the type signature. Note that Haskell freely infers the type `a -> a` really to mean `forall a. a -> a`, by looking for free variables (abbreviated to FVs, above). Haskell currently does slightly more than analyze just free variables, though: it also quantifies over free *kind* variables that do not textually appear in a type. For example, the type `Proxy a -> Proxy a` really means (in today's Haskell) `forall (k :: BOX) (a :: k). Proxy a -> Proxy a`, even though `k` does not appear in the body of the type. Note that a *visible* quantifications impose a requirement on how a thing is used/written; *required* quantifications impose a requirement on how a thing's type is written.
</td></tr></table>


<table><tr><th>Visible</th>
<td>
*Visibility* refers to whether or not the argument must appear at **definitions** and **call sites** in the program text. If something is not visible, the table lists how GHC is to fill in the missing bit at call sites. If something is visible, we must specify how it is parsed, noting that the term- and type-level parsers are different. For example (not implemented):

```wiki
-- Invisible ----
f1 :: forall a. a -> a
f1 x = x

g1 x = f1 True

-- Visible ----
f1 :: forall a -> a -> a
f1 a x = x 

g1 x = f1 Bool True
```

Same at the type level (but this *is* implemented):

```wiki
-- Invisible ----
-- Proxy1 :: forall k. k -> *
data Proxy1 (a :: k) = P1
f1 :: Proxy1 Int -> Bool

-- Visible ----
-- Proxy2 :: forall k -> k -> *
data Proxy2 k (a :: k) = P2
f2 :: Proxy2 * Int
```

(The kind signatures for `Proxy1`/`Proxy2` are output, but can't yet be written.)
</td></tr></table>


>
>
> Similarly, type-class arguments, whose types look like `Ord a => a -> Int`, are invisible at both definition and use; but we don't have a visible form.
>
>

<table><tr><th>Relevant</th>
<td>
*Relevance* refers to how the quantifiee can be used in the term classified by the type in question. For terms and their types, a binder is relevant iff it is not erased; that is, it is needed at runtime. In GHC terms, relevant binders are `Id`s and irrelevant ones are `TyVar`s.
</td></tr></table>


>
>
> For types and their kinds, we can't talk about erasure (since the are all erased!) but the relevance idea works the same, one level up.  Example
>
>
> ```wiki
> type family Id (x::k1) (y::k2) :: (k1,k2) where
>   Id True  v = (False, v)
>   Id False v = (True,  v)
>   Id x     v = (x,     v)
> ```
>
>
> If `Id`'s kind was `forall k1 k2. k1 -> k2 -> (k1,k2)`, it looks parametric in both `k1` and `k2`.  But it isn't, because it can pattern-match on `k1`.  So `k1` is relevant, but `k2` is irrelevant.
>
>

>
>
> (This is distinct from dependence, which says how the quantifiee can be used in the *type* that follows!) `forall`-quantifiees are not relevant. While they can textually appear in the classified term, they appear only in irrelevant positions -- that is, in type annotations and type signatures. `->`- and `=>`-quantifiees, on the other hand, can be used freely. Relevance is something of a squirrely issue. It is (RAE believes) closely related to parametricity, in that if `forall`-quantifiees were relevant, Haskell would lose the parametricity property. Another way to think about this is that parametric arguments are irrelevant and non-parametric arguments are relevant. See also [this discussion](dependent-haskell#) for perhaps further intuition.
>
>


Having explained our terms with the current Haskell, the proposed set of quantifiers for dependent Haskell is below:


<table><tr><th>  Dependent Haskell  
</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> Quantifier </th>
<th> Dependent? </th>
<th> Visible? </th>
<th> Required? </th>
<th> Relevant? 
</th></tr>
<tr><th> `forall (...) .` </th>
<th> Yes </th>
<th> No (unification) </th>
<th> No (FVs) </th>
<th> No 
</th></tr>
<tr><th> `forall (...) ->` </th>
<th> Yes </th>
<th> Yes (as type) </th>
<th> Yes </th>
<th> No 
</th></tr>
<tr><th> `pi (...) .` </th>
<th> Yes </th>
<th> No (unification) </th>
<th> Yes </th>
<th> Yes 
</th></tr>
<tr><th> `pi (...) ->` </th>
<th> Yes </th>
<th> Yes (as term) </th>
<th> Yes </th>
<th> Yes 
</th></tr>
<tr><th> `->` </th>
<th> No </th>
<th> Yes (as term) </th>
<th> Yes </th>
<th> Yes 
</th></tr>
<tr><th> `=>` </th>
<th> No </th>
<th> No (solving) </th>
<th> Yes </th>
<th> Yes 
</th></tr></table>



Note that the current quantifiers remain and with their original meanings. This table adds three new quantifiers: `forall ->`, and the two `pi` quantifiers. The idea is that, currently, we always say `forall`, then some binders and then a `.`. If we replace the `.` with an `->`, then we make the quantifications *visible* but otherwise unchanged. (Quantification not mentioned in a type defaults to *invisible*, thus making the visible quantification *required*.)



The new `pi` quantifiers allow for quantifiees that are both dependent and relevant. This means that the quantifiee is named in the type and can be used within its scope in the type, and also that the quantifiee can be inspected in the term. A `pi`-bound argument is a proper dependent type. Since a `pi`-quantifiee can appear in both terms and types, its instantiations must come from a restricted subset of Haskell that makes sense at both the type and term level. This issue is addressed in [Adam Gundry's thesis](dependent-haskell#). For now, we propose that this subset include only data constructors (perhaps applied) and other `pi`-quantifiees. The subset can be expanded later. For some tantalizing ideas of how far it can go, see [this paper](dependent-haskell#), discussing promotion of Haskell terms to types.



It is tempting to treat `->` as a degenerate form of a `pi` -- something like `pi (_ :: ...) ->`. However, this is slightly misleading, in that `->` quantifies over *any* Haskell term, and `pi` quantifies over only the shared term/type subset.



Declarations given without a type signature will need to perform *relevance inference* to figure out whether quantified variables should be `forall`-bound or `pi`-bound. Relevance inference simply looks at usage sites; iff a variable is used in a relevant context (scrutinee of a pattern-match, or passed to a function expecting a relevant argument, among others) then it is relevant. It is tempting to perform relevance inference if the nature of a quantifier is omitted in a type signature, but this would make the type signature's meaning depend on the term, which is annoying. For now, we're saying "no". But, it would be good to revisit this decision in light of partial type signatures, which also make type signatures depend on terms.


### Quantifiers in kinds



The preceding discussion focuses mostly on classifying terms. How does any of this change when we think of classifying types? 


<table><tr><th>Relevance in types</th>
<td>
Relevance in a term corresponds quite closely to phase. A relevant term-level quantifiee must be kept at runtime, while an irrelevant quantifiee can be erased. But, what does relevance in a type mean? Everything in a type is (absent `pi`-quantifications) irrelevant in a term, and it all can be erased. Furthermore, it is all used in the same phase, at compile time. Yet, it seems useful to still have a notion of relevance in types. This allows programmers to reason about parametricity in their type-level functions, and it keeps the function space in types similar to the function space in terms.
</td></tr></table>


>
>
> For example, today's Haskell permits things like this:
>
>
> ```wiki
> type family F (x :: k) :: k
> type instance F True = False
> type instance F False = True
> type instance F (x :: *) = x
> ```


>
>
> Note that the behavior of `F` depends on the *kind* of its argument, `k`. This is an example of a non-parametric type function. Looking at the kind, `k -> k`, one would expect `F` to be the identity; yet, it is not.
>
>

>
>
> Thus, we would want to distinguish `pi k. k -> k` (the kind of `F`) and `forall k. k -> k` (the kind of a type-level polymorphic identity). This distinction does not affect erasure or phase, but it does affect how a quantifiee can be used. Furthermore, this keeps term classifiers more in line with type classifiers. Note that this means all current type/data families are properly classified with `pi`, not `forall`. This won't cause code breakage, though, because it is impossible to write a kind quantification (with any syntax) in today's Haskell.
>
>

>
>
> Proposed syntax for this distinction is to assume any kind variable is forall-quantified, and allow syntax like `type family pi k. F (x :: k) :: k where ...` to get pi-quantification. `forall` would be allowed in that syntax as well, but would be redundant.
>
>

<table><tr><th>Datatypes</th>
<td>
How is the kind of a datatype classified? After some debate, Stephanie and RAE thought that a poly-kinded datatype should be quantified with `pi`, not `forall`. For example, consider `data Proxy (k :: *) (a :: k) = Proxy`. Is its kind `forall (k :: *). k -> *` or `pi (k :: *). k -> *`. Let's consider the former type as if it classified a term-level function. That function would have to be a constant function, by parametricity. Yet, we do *not* want `Proxy * Bool` to be the same as `Proxy Nat Zero`. So, we choose the latter classifier.
</td></tr></table>


>
>
> This choice matters in how datatypes are used in pattern-matching situations. For example, is this possible: `type instance F (Proxy (a :: k)) = k`? The result uses `k` in a relevant context, so it must be that `k` is introduced in a relevant context, and thus that it must be pi-quantified. If we wanted to introduce forall-quantification in datatypes, then the use of these variables would be restricted during matching. Is this a good idea? Is this necessary? And, it seems that `pi` will be default in datatypes but `forall` will be default in type families. This is strange.
>
>

## Open design questions


### Parsing/namespace resolution



Parsing is a bit of a nightmare for this new language and will require some compromises.


- Merging types and kinds is almost straightforward, but for one major stumbling block: `*`. In a kind, `*` is parsed as an alphanumeric identifier would be. In a type, `*` is parsed as an infix operator. How can we merge the type- and kind-parser given this discrepancy? As an example, what is the meaning of `Foo * Int`? Is it the type `Foo` applied to `*` and `Int`? Or is it the operator `*` applied to `Foo` and `Int`? The solution to this annoyance seems to be to introduce a new identifier for `*` (say, `TYPE`) and then remove `*` from the language, allowing it to be used for multiplication, for example.

  - What name to choose for `*`? `TYPE` would appear plenty in code, and it seems a little rude. Choosing a new symbol just kicks the can down the road. Choosing `Type` would conflict with lots of code (including GHC's) that uses a type `Type`. Choosing `T` would conflict with lots of (example) code that uses `T`. The best option I'm aware of is `U`, short for universe. Mitigating this problem somewhat is that Dependent Haskell would come with kind synonyms, and whatever name we choose would be a "normal" name exported from the `Prelude` and could be hidden if desired.
  - What is our migration strategy? One proposal: introduce the new name now and keep `*` around. Then, when Dependent Haskell is ready for release, it will come with a new extension flag which will change the parsing of `*`. Only when that flag is enabled would `*` fail to work. It is unclear whether it is worth it to fully squash `*` out of the language.

>
>
> **UPDATE:** In a conversation with the denizens of UPenn's Levine 513, I came up with a nice, clean way to have our cake and eat it too. It all hinges upon the fact that the parser can't currently figure out fixities of operators. While the actual implementation may differ in the details, the parser has to treat `foo bar !# baz %% bum bimini` as just a list of tokens. Only after we rename are the operators resolved to entities with known fixities.
>
>

>
>
> With this in mind, we can just introduce a new fixity: `prefix`. During the transformation of an expression such as that above into a proper AST with correct precedence, we just treat an operator with the `prefix` fixity just like an alphanumeric identifier. Then, the kind `*` just has `prefix` fixity, and we're done with it. For backward compatibility, this `*` would have to be exported from the `Prelude`. There would be a name clash with types named `*`, but Haskellers know how to deal with name clashes already. Backward compatibility might compel us to do something clever in the renamer in the `-XNoDependentTypes` case, but that's OK.
>
>

- The type language and the term languages are more different. There are two proposals on the table to deal with types embedded in terms:

  - **Keep the parsers separate**: Since the parsers are separate and the parser certainly doesn't know anything about types, we need some indication in the code as a signal to the parser to switch to the type parser. Due to the construction of GHC's parser, this indicator would have to come *before* the phrase in question.

    - Option 1: We can use `@` to indicate that we are parsing a type. Take `id :: forall a -> a -> a`. This is just like the normal `id`, but with the type parameter explicit. Because the parser/renamer won't know the type of `id` when parsing its arguments, the first argument will have to manifestly be a type. For example, `id @Bool True`. The `@` indicates to the parser that the following thing is a *type*, not a *term*.
    - Option 2: We can use the keyword `type` to indicate that we are parsing a type.
  - **Merge the parsers**: It may be possible to merge the term/type parsers. This would make `forall` a proper keyword. `(->)` and `(=>)` are already unusable at the term level. `\` is already unusable at the type level. One possible conflict is that `'` is used in types to indicate namespace and it is used in terms to indicate Template Haskell quoting. Although it won't produce problems in code, `!` (a normal operator in terms but a strictness flag in datatype declarations) may cause trouble in parser engineering. It's conceivable to say that strictness markers must be preceded by a space and not followed by one, but this is somewhat painful. A similar situation arises with `~`, which is a normal type operator but a laziness specifier in patterns. But, the parser already deals with the `!` bang-pattern/infix operator ambiguity, so perhaps this solution can be adapted. In any case, this all seems at least possible to consider. 
    Even if we could write some kind of combined parser, the renamer would have major trouble distinguishing between data constructors and type constructors. One way or the other, programmers will likely have to specify how to parse their arguments explicitly. 

    - Option 1: Use `'` to write data constructors in types and use `^` to write type constructors in terms. The first of these is already implemented. The second is up for debate. Do these operators work only on individual identifiers? Or, can we say `f ^(...)` to make everything in the `...` be treated like a type?
    - Option 2: Use `'` to mean "switch default" -- it goes in either direction.

>
> >
> >
> > Open question: What other signals change the default renaming? In other words, if we say `f (forall x. blah)` in a term, do the bits in `blah` get renamed like a type or like a term? RAE advocates "term", but Conor has advocated "type".
> >
> >
>

- We will similarly need a syntax for type patterns embedded within term patterns. It would be ideal if the pattern syntax were identical to the expression syntax.

- The choice of `@` above is stolen from the [ExplicitTypeApplication](explicit-type-application) and [TypeApplication](type-application) proposals, neither of which have been implemented and will be subsumed by Dependent Haskell (if we get to that first). Is this the right choice, though? For example, the [ExplicitTypeApplication](explicit-type-application) page includes an example of ambiguity:

```wiki
f :: Int -> forall a. a
f n @a = ....
```

>
>
> This is ambiguous because `@`-patterns allow a space around the `@`-sign. However, common usage does *not* use any spaces around `@`, and we could use the presence/absence of a space to disambiguate between an `@`-pattern and a type pattern.
>
>

- How does this all interact with `ScopedTypeVariables`? For example:

  ```wiki
  foo :: forall x. ...
  foo @y = ...
  ```

  Here, `x` and `y` are bound to the *same* type! Is this allowed?

>
>
> One partial proposal: when `-XDependentTypes` is specified, merge the type-variable and term-variable namespaces. This would simplify some of the issues (for example, it might be possible to infer which lambda a programmer wants based on the usage of the bound variable), but it would break code. However, code breakage is likely small and can be mechanically detected and fixed. And, it only breaks code if that code now wants `-XDependentTypes`. A potential thorn if we go this route: the shape of the term-variable and type-variable namespaces are slightly different: `(##)`, for example, is a term variable, but it is a type *constant*. It's not clear what the ramifications of this problem are.
>
>

- Regardless of other choices above, simple cases might be able to remain simple. For example, `f Bool` will surely parse as a term. When the renamer can't find the data constructor `Bool`, it could be smart enough to look for a type constructor `Bool` and get on with it.

### Overriding visibility defaults



The `.`/`->` distinction in quantifiers allows programmers to specify the visibility of arguments at use sites. But, sometimes callers will want to override the defaults.


- If a visible, dependent argument is to be elided, we could allow `_` to indicate that GHC should use unification to fill in the argument. (This is similar to the approach in Coq, for example, among other languages.) Does this conflict in any way with typed holes? Perhaps a programmer wants to get an informative error message, not for GHC to plug in a value.

- Visible, non-dependent arguments cannot be inferred via unification, so `_` would not be applicable here, and would retain its current meaning of a typed hole.

- How to override an invisible, dependent type argument? This might be critical if a function call would be otherwise ambiguous. (Today's Haskell would benefit from this override occasionally, too. See [TypeApplication](type-application).) One proposal is that `@` would serve double-duty: it would override invisibility and also indicate a type argument. If this is the case, the `forall (...) ->` form would be essentially useless, as it would still require users to use `@` to indicate parsing. Thus, `forall (...) ->` seems strictly worse than `forall (...) .`.

- How to override an invisible, dependent term argument (that is, `pi (...) .`)? Using `@` would not work, because the parser wouldn't be able to deal with it. Alternatively, `pi`-bound arguments could use type-level syntax, and then `@` would work. However, this seems suboptimal, as we would more often want to use, say, a data constructor in a `pi`-bound argument, not a type constructor. Braces could possibly work, at least in expressions. These would not conflict with record-update syntax because record-updates require an `=`, but `pi`-bound arguments would not. Patterns might be problematic with braces, because record puns do not require `=`. There are no other proposals here, for the moment.

### Parametric vs. Non-parametric type families


- How does relevance inference work with the proposed syntax for quantifier choice in type families? For example:

```wiki
type family X (a :: k) :: * where
  X (a :: k) = k
```


Because, in the equation, `k` appears in a relevant context within the result, it must be pi-quantified. Yet, we said above that kind variables without explicit quantification would default to `forall`. What to do here?


# Type Inference



Figuring out type inference for this language is a bit of a challenge. While the full design won't be laid out here, interesting tidbits will be gathered here for easy retrieval.


### Inferring `pi` types



Suppose a programmer writes, without a type signature


```wiki
foo @Zero y = y
```


(Here, we are assuming `@` as the invisible-overrider.) What is `foo`'s type? It could be `pi (n :: Nat). forall (a :: *). Vec a n -> Vec a Zero`. It could also be `forall (n :: Nat) (a :: *). a -> a`. Neither is more general than the other -- we are in the same GADT type-inference problem as described in the [
OutsideIn](http://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/jfp-outsidein.pdf) paper. Thus, we reject such a `foo` that matches on an implicit parameter without a type signature.



But, what about


```wiki
foo Zero y = y
```


We are actually in the same situation here. But, backward compatibility compels us to prefer non-dependent types over dependent ones, inferring `foo :: forall (a :: *). Nat -> a -> a`. (Note that `foo :: forall (a :: *). pi (n :: Nat) -> Vec a n -> Vec a Zero` is a valid but incomparable type that we could assign.)



When do `pi`-types get inferred, if ever? Good question.


# Implementation



*Kind equalities* will be part of GHC 8. Merging commit: [
67465497](https://github.com/ghc/ghc/commit/6746549772c5cc0ac66c0fce562f297f4d4b80a2). It was developed in [
Eisenberg's nokinds tree](https://github.com/goldfirere/ghc/tree/nokinds).


# Related work



**Readers:** Please add to these lists!



There are several published works very relevant to the design:


- [
  System FC with Explicit Kind Equality](https://www.cis.upenn.edu/~justhsu/docs/nokinds.pdf). Stephanie Weirich, Justin Hsu, and Richard A. Eisenberg. ICFP 2013.
- [
  Type Inference, Haskell, and Dependent Types](http://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf). Adam Gundry. PhD Thesis, 2013.
- Eisenberg's thesis: [
  https://github.com/goldfirere/thesis](https://github.com/goldfirere/thesis)


There are also many works addressing the use of dependent types in Haskell. Here is a selection:


- [
  Dependently typed programming with singletons](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf). Richard A. Eisenberg and Stephanie Weirich. Haskell Symposium 2012.
- [
  Hasochism: The Pleasure and Pain of Dependently Typed Haskell](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf). Sam Lindley and Conor McBride. Haskell Symposium 2013.
- [
  Promoting Functions to Type Families in Haskell](http://www.cis.upenn.edu/~eir/papers/2014/promotion/promotion.pdf). Richard A. Eisenberg and Jan Stolarek. Haskell Symposium 2014.
