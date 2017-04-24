## Impredicative polymorphism



GHC does not (yet) support impredicative polymorphism, but it's a topic that comes up regularly, so this page collects the state of play.



See the new plan at [ImpredicativePolymorphism/Impredicative-2015](impredicative-polymorphism/impredicative-2015)


## Tickets



Use Keyword = `ImpredicativeTypes` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#1330](http://gitlabghc.nibbler/ghc/ghc/issues/1330)</th>
<td>Impredicativity bug: Church2 test gives a rather confusing error with the HEAD</td></tr>
<tr><th>[\#4281](http://gitlabghc.nibbler/ghc/ghc/issues/4281)</th>
<td>Make impredicativity work properly</td></tr>
<tr><th>[\#4295](http://gitlabghc.nibbler/ghc/ghc/issues/4295)</th>
<td>Review higher-rank and impredicative types</td></tr>
<tr><th>[\#7026](http://gitlabghc.nibbler/ghc/ghc/issues/7026)</th>
<td>Impredicative implicit parameters</td></tr>
<tr><th>[\#8808](http://gitlabghc.nibbler/ghc/ghc/issues/8808)</th>
<td>ImpredicativeTypes type checking fails depending on syntax of arguments</td></tr>
<tr><th>[\#9730](http://gitlabghc.nibbler/ghc/ghc/issues/9730)</th>
<td>Polymorphism and type classes</td></tr>
<tr><th>[\#10709](http://gitlabghc.nibbler/ghc/ghc/issues/10709)</th>
<td>Using ($) allows sneaky impredicativity on its left</td></tr>
<tr><th>[\#11514](http://gitlabghc.nibbler/ghc/ghc/issues/11514)</th>
<td>Impredicativity is still sneaking in</td></tr>
<tr><th>[\#14859](http://gitlabghc.nibbler/ghc/ghc/issues/14859)</th>
<td>Allow explicit impredicativity</td></tr>
<tr><th>[\#16140](http://gitlabghc.nibbler/ghc/ghc/issues/16140)</th>
<td>Cannot create type synonym for quantified constraint without ImpredicativeTypes</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#4347](http://gitlabghc.nibbler/ghc/ghc/issues/4347)</th>
<td>Bug in unification of polymorphic and not-yet-polymorphic type</td></tr>
<tr><th>[\#7264](http://gitlabghc.nibbler/ghc/ghc/issues/7264)</th>
<td>Adding GHC's inferred type signatures to a working program can make it fail with Rank2Types</td></tr>
<tr><th>[\#9420](http://gitlabghc.nibbler/ghc/ghc/issues/9420)</th>
<td>Impredicative type instantiation without -XImpredicativeTypes</td></tr>
<tr><th>[\#10619](http://gitlabghc.nibbler/ghc/ghc/issues/10619)</th>
<td>Order matters when type-checking</td></tr>
<tr><th>[\#11319](http://gitlabghc.nibbler/ghc/ghc/issues/11319)</th>
<td>ImpredicativeTypes even more broken than usual</td></tr>
<tr><th>[\#11428](http://gitlabghc.nibbler/ghc/ghc/issues/11428)</th>
<td>ImpredicativeTypes causes GHC panic with 8.0.1-rc1</td></tr>
<tr><th>[\#12557](http://gitlabghc.nibbler/ghc/ghc/issues/12557)</th>
<td>Regression in type inference with RankNTypes</td></tr>
<tr><th>[\#13895](http://gitlabghc.nibbler/ghc/ghc/issues/13895)</th>
<td>"Illegal constraint in a type" error - is it fixable?</td></tr>
<tr><th>[\#14160](http://gitlabghc.nibbler/ghc/ghc/issues/14160)</th>
<td>Type inference breaking change in GHC 8.0.2</td></tr>
<tr><th>[\#15628](http://gitlabghc.nibbler/ghc/ghc/issues/15628)</th>
<td>Higher-rank kinds</td></tr></table>



### What is impredicative polymorphism?



Consider this


```wiki
foo :: (forall a. a -> a) -> Int
bar :: forall b. Bool -> b -> b

test1 :: Bool -> Int
test1 x = foo (bar x)

test2 :: Bool -> Int
test2 = foo . bar
```


Should `test1` typecheck?  Yes: GHC can see that `foo`'s argument should have type `forall a. a->a`, and indeed `bar x` has that type.  It involves higher-rank type inference (see [
Practical type inference for higher rank types](http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/index.htm)), but GHC has supported this for ages.



What about `test2`?  After all, it's just an eta-abstracted version of `test1`.  No, `test2` is rejected.  Remember the type of `(.)`:


```wiki
(.) :: forall p q r. (q -> r) -> (p -> q) -> p -> r
```


To make this work in `test2` we must instantiate `q := forall a. a->a`, to make the type of `(.)`'s first argument match `foo`'s type.  **So we have to instantiate a polymorphic type variable `q` with a polymorphic type**.  This is called *impredicative polymorphism*, and GHC's type inference engine simply does not support it.


### Constraints also trigger impredicative polymorhism



Although it does not appear to involve instantiating any polymorphic type variables with polymorphic types, instantiating a polymorphic type variable with qualified type will also trigger the "GHC doesn't yet support impredicative polymorphism" error.



For example


```wiki
type Monadic m a = Monad m => m a
monadics :: [Monadic m a]
monadics = undefined
```


is not allowed.



A workaround for cases like this remove the constraint from the type synonym and add it at the use sites. I.e., the above example works if we change it to


```wiki
type Monadic m a = m a
monadics :: Monad m => [Monadic m a]
monadics = undefined
```

### Special case for `($)`



Consider


```wiki
runST :: (forall s. ST s a) -> a

foo = runST $ do { ...blah... }
```


Here again we need impredicative polymorphism, to instantiate `($)`'s type, very much like `(.)` above. So `foo` would be rejected.  But Haskell programmers use `($)` so much, to avoid writing parentheses, that GHC's type inference has an ad-hoc special case for `($)` that allows it to do type inference for `(e1 $ e2)`, even when impredicative polymorphism is needed.


### The internal language



All of this concerns the *source* language.  GHC's *internal language*, System FC, is fully impredicative.  This works fine because there is no type inference in System FC.  It's only type *inference* that is the problem with impredicativity.


### What about `-XImpredicativeTypes`?



We've made various attempts to support impredicativity, so there is a flag `-XImpredicativeTypes`.  But it doesn't work, and is absolutely unsupported.  If you use it, you are on your own; I make no promises about what will happen.


### Workarounds



So if you need impredicativity and `-XImpredicativeTypes` doesn't work, what can you do?  The main workaround is this: define a newtype to wrap the polymorphic function.  



For example, you can't have a list of polymorphic functions, say `[forall a. [a] -> [a]]`.  But you can wrap it like this:


```wiki
newtype ListList = LL { unLL :: forall a. [a] -> [a] }
```


Now `[ListList]` is a perfectly fine type.  The downside is that you have to wrap and unwrap, with `LL` and `unLL`, which is tiresome.


### Reading



Here are some useful papers about type inference for impredicative polymorphism;
  


- [
  FPH : First-class Polymorphism for Haskell (2008)](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/)
- [
  Boxy types: type inference for higher rank and impredicativity (2006)](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/)  We implemented this in GHC, but it was Just Too Complicated.
- [
  QML: Explicit first-class polymorphism for ML (2009)](http://research.microsoft.com/en-us/um/people/crusso/qml/) A much simpler, and less ambitious, approach.
- [
  MLF: Raising ML to the power of System F (2003)](http://gallium.inria.fr/~remy/publications.html) The other end of the spectrum from QML: a very sophisticated approach.

### The way forward



Personally I think there are two ways forward:


- Add explicit type application in some shape or form.  That is, tell GHC exactly how to instantiate those polymorphic type variables.  Explicit type application is what happens in the intermediate language, System FC, and there are many reasons for wanting it in the source language.  See the [explicit type application wiki page](explicit-type-application).

- Do something along the lines of QML.


The key is to be less ambitious than our previous attempts.  Anything in FC should be *expressible* in source Haskell, but we may have to accept relatively-intrusive type annotations to achieve it.


