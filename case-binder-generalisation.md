# Case Binder Generalisation



These are Joachim’s random scribblings in order to solve [\#9291](http://gitlabghc.nibbler/ghc/ghc/issues/9291). This is wip, but of course if you want to comment on this, let me know.


## The problem


```
fun :: Either Int String -> Either String String
fun x = case x of
    Left int -> Left (show int)
    Right str -> Right str
```


produces this (prettified) Core


```
fun :: Either Int String -> Either String String
fun = \ (x :: Either Int String) ->
    case x of b  {
      Left int ->  Left  @String @String (GHC.Show.$fShowInt_$cshow int);
      Right str -> Right @String @String str
    }
```


The `Right` branch is allocating a `Right` data constructor with a pointer to `str`. But this will be bit-for-bit identical to `x`! That is stupid! But the obvious code


```
fun :: Either Int String -> Either String String
fun = \ (x :: Either Int String) ->
    case x of b  {
      Left int ->  Left  @String @String (GHC.Show.$fShowInt_$cshow int);
      Right str -> b
    }
```


does not type-check, as `b` has a different type than the return type of `fun`.



If we would not change the type of the `Either` here, then this would be ok, and CSE would do this optimization for us.


## The approach



The observation that we make here is that it is type-safe to use `b` at a more general type inside each branches of the case. But one variable `b` cannot have different types in different scopes. So it seems we do not get around having separate case binders in each branch:


```
fun :: Either Int String -> Either String String
fun = \ (x :: Either Int String) ->
    case x of {
      bLeft  :: (forall a. Either String a) . Left int  ->  Left  @String @String (GHC.Show.$fShowInt_$cshow int);
      bRight :: (forall a. Either a String) . Right str -> Right @String @String str
    }
```


This describes what we know about the case binder in each branch more precisely.



Then CSE (which probably needs to be instrumented to do these things) could then apply the translation


```
Right @t @String str ⇝ bRight @t
```


for any type `t` in the RHS of this alternative, yielding the desired code:


```
fun :: Either Int String -> Either String String
fun = \ (x :: Either Int String) ->
    case x of {
      bLeft  :: (forall a. Either String a) . Left int ->  Left  @String @String (GHC.Show.$fShowInt_$cshow int);
      bRight :: (forall a. Either a String) . Right str -> bRight @t
    }
```


which avoid the pointless re-boxing.


## Operational semantics



Nudged by Richard, it seems that I really really should describe the operational semantics of this. In the example of `Either`, the `S_MatchData` rule (from [
the core-spec](https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf)) with this extension specializes to


```wiki
∑ ⊢ case Left @t1 @t2 e of { bLeft :: (forall a. Either String a) . Left x → u ; … }
⟶
u[bLeft  ↦ (Λa. Left @a @t2 e), x ↦ e]
```


which is equivalent to the existing rule if one ignores types (so type erasure would work), and substitutes for `bLeft` an expression of the right polymorphic type.



For the `DEFAULT` case, the case binder has the same type as the scrutinee.


## Implementation



The change to Core is slightly invasive: the `Case` data type gets simpler, but the `Alt` type synonym gets larger, and more names need to be built for these case binders.



CSE needs to get more clever, as it needs to be able to recognize expression of the shape `Right @t1 @t2 x` for \*arbitrary\* `t2` and replace them by `bRight @t2`. 


## Evaluation



Is it worth it? Hard to tell at the moment.



One could try to implement this first, as an experimente, without changing `Core` and simply using `unsafeCoerce` to “fix” the type of the (single, existing) case binder.



I do not expect great improvements in common code, and are not even confident that there is much realistic specific code that benefits a lot. So this is more driven by the desire to do the right thing, not necessarily by it being needed.


