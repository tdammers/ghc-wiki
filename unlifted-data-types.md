# Unlifted data types






This page describes the unlifted data types, i.e. algebraic data types which live in kind `#` rather than kind \*. In fact, this is a collection of standalone proposals:


1. Allow data types to be declared unlifted. (Should be easy; but has a harder subproposal to separate a new kind `Unlifted` from `#`)
1. Allow newtypes over unlifted types (giving them kind `#`). (Should be easy.)
1. Provide a built-in `Force a` which is an unlifted version of `a`, with no indirection cost. (Harder.)


See also [UnpackedSumTypes](unpacked-sum-types).


## Motivation



Bob Harper [
has written](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/):


>
>
> Haskell suffers from a paucity of types.  It is not possible in Haskell to define the type of natural numbers, nor the type of lists of natural numbers (or lists of anything else), nor any other inductive type!
>
>


The reason, of course, is that whenever you write `data Nat = Z | S !Nat`, you define a type of strict natural numbers, AS WELL AS bottom. Ensuring that an `x :: Nat` is never bottom requires all use-sites of this type to do strict pattern matching / force the value appropriately. It would be nice if there was some type-directed mechanism which specified that a value `x` was always evaluated. This would give benefits, e.g. for code generation, where we can assume that a pointer never points to a thunk or indirection.



It would be hard to justify drastically changing Haskell to allow defining a type which doesn't include bottom, but as it turns out, GHC *already* supports such non-bottom types, in the form of unlifted types of kind `#`. In particular, we already have special strict evaluation rules for unlifted types like `Int#` and `Array#`.



The fact that a pointer never points to a thunk is especially helpful in the implementation of mutable variables: without this guarantee, code which writes to this mutable variable has to first check if the thunk is fully evaluated before actually performing the memory write. For this reason, the `MutVar#` primitive (which is a GC'd object) lives in kind `#`.


## Proposal 1: Allow data types to be declared as unlifted



A data type can be declared as unlifted by writing `data unlifted`, e.g.:


```
data unlifted UBool = UTrue | UFalse
```


Such data types are always boxed, but the type does not include bottom and is operationally represented as a pointer to the value. Intuitively, if you have `x :: UBool` in scope, you are guaranteed to have `UTrue` or `UFalse`, and not bottom.


>
>
> **Iceland\_jack**: Yakshaving, to avoid new syntax we can use `GADTSyntax` and write
>
>
> ```
> data UBool :: TYPE UnliftedRep where
>   UFalse :: UBool
>   UTrue  :: UBool
> ```



The evaluation rules for unlifted data types are identical to the existing rules we have for types kinded `#`: lets are strict, cannot be recursive, and function arguments are evaluated before calls. For example:


```
unot :: UBool -> UBool
unot UTrue = UFalse
unot UFalse = UTrue

main :: IO ()
main = let y = unot (error "foo")
       in return ()
```


In this example, we get the error "foo", rather than returning `()`, because the binding of `y` must be evaluated strictly.



Just like other unlifted types, you cannot bind values of an unlifted data type at top level, or in a recursive group.  So this is illegal


```
module M where
  data unlifted UBool = UTrue | UFalse

  utrue :: UBool
  uTrue = UTrue   -- Illegal!
```


**Non-polymorphic unlifted types can directly be unpacked.** The following declaration is valid:


```
data unlifted StrictInt = StrictInt Int#
data MyInt = MyInt {-# UNPACK #-} StrictInt
```


and is representationally equivalent to `MyInt'` here:


```
data Int = Int Int#
data MyInt' = MyInt' {-# UNPACK #-} !Int
```


Of course, the constructors for `MyInt` and `MyInt'` have different types.


## Proposal 2: Polymorphism over a new Unlifted kind



Currently, we have two different kinds (ahem) of unlifted types which live in `#`: unboxed, unlifted types such as `Int#`, and boxed, unlifted types such as `Array#` and the unlifted data types we can now define.



This subproposal is to distinguish between these two kinds, calling boxed, unlifted kinds `Unlifted`. If users start defining and using unlifted types in normal code,  it is likely that they will request polymorphism over unlifted types. There are two particular cases of polymorphism we might like to support:



**Polymorphism over unlifted types in types and functions.** In data types and functions, we may want to be polymorphic over a type variable in kind `Unlifted`:


```
data unlifted UList (a :: Unlifted)
  = UNil | UCons a UList
umap :: forall (a :: Unlifted) (b :: Unlifted).
        UList a -> (a -> b) -> UList b
```


We cannot be polymorphic in `#` in general, because this includes unboxed types like `Int#` which don't have uniform representation. However, we can be polymorphic over unlifted types, which do have uniform representation.



**Boxed levity polymorphism in types (and functions with extra code generation).** In data types, we may want to have a type parameter which is polymorphic over all boxed types:


```
data BList (a :: Boxed)
  = BNil | BCons a BList
```


`BList` is representationally the same whether or not it is instantiated with a boxed lifted type, or a boxed unlifted type.



However, for levity polymorphism over functions we must generate code twice. Consider::


```
map :: forall a (b :: Boxed). (a -> b) -> BList a -> BList b
map f (BCons x xs) = BCons (f x) (map f xs)
```


We do not know if `f x` should be evaluated strictly or lazily; it depends on whether or not `b` is unlifted or lifted. This case can be handled by specializing `map` for the lifted and unlifted cases.  The fact that the semantics of the function change depending on the type is a bit questionable (though not entirely unprecedented, c.f. type classes); additionally, there isn't any reason why we couldn't also generate copies of the code for all unboxed types, dealing with `Int#`, `Float#` and `Double#`: it's unclear when to stop generating copies. (For reference, .NET does this on the fly.)


## Proposal 3: Allow newtypes over unlifted types



To allow cost-free abstraction over unlifted types, we should allow newtypes to be written over types of kind `#`, with the resulting newtype being in kind `#`. For example:


```
newtype MyInt# = MkInt# Int#
```


with `MyInt# :: #`. GHC already supports coercions over kind `#`, so this should be very simple to implement.


>
>
> **Iceland\_jack**: Use this to wrap `Int#` when used in boolean context in `GHC.Exts`, `GHC.Prim`
>
>
> ```
> newtype Bool# = Bool# Int#
>
> pattern False# :: Bool#
> pattern False# = Bool 0#
>
> pattern True# :: Bool#
> pattern True# = Bool 1#
>
> (==#)  :: Int#    -> Int#    -> Bool#
> (==##) :: Double# -> Double# -> Bool#
> ```
>
>
> and other similar cases.
>
>

## Proposal 4: Allow unlifting existing data types with no overhead



Proposal 1 requires a user to define a new data type for every unlifted type they want to define. However, for every lifted data type a user can define, there is an obvious unlifted type one might be interested in: the one without bottom. Fortunately, we can define a data type to unlift an arbitrary lifted type:


```
data Force :: * -> Unlifted where
  Force :: !a -> Force a

suspend :: Force a -> a
suspend a = a
```


`Force a` is the "head strict" version of `a`: if you have `x :: Force Int` in scope, it is guaranteed to have already been evaluated to an `Int`. We can also suspend these strict computations: `suspend (error "foo" :: Int#)` does not error until forced. Like `Box`, unlifted computations may not be lifted out of `suspend` without changing the semantics.



`Force` and `suspend` can be written purely as library code, however there is a cost of an indirection of `Force`. We might notice, however, that the value of type `Force a` only admits the value `Force a`: `undefined` is excluded by the `Unlifted` kind, and `Force undefined` is excluded by the strict field.  Thus, it would be great if we could represent `Force` on the heap simply as an unlifted pointer to `a`, which is never undefined.


>
>
> Note from David Feuer: the indirection problem could be resolved via [
> https://ghc.haskell.org/trac/ghc/wiki/NewtypeOptimizationForGADTS](https://ghc.haskell.org/trac/ghc/wiki/NewtypeOptimizationForGADTS), which would be a good thing for other reasons too.
>
>


Ideally, we would like to define the coercion `Coercible (Force a) a`, to witness the fact that `Force a` is representationally the same as `a`. However, there are two problems:


1. This coercion is ill-kinded (`Force a` has kind `Unlifted` but `a` has kind `*`), so we would need John Major equality style coercions.

1. The coercion is only valid in one direction: I can coerce from `Force a` to `a`, but not vice-versa: in the other direction, evaluation may be necessary.


My suggested implementation strategy is to bake in `Force` as a special data type, which is represented explicitly in Core, but then optimized away in STG.


## Optional extensions



**(OPTIONAL) Give some syntax for `Force`.** Instead of writing `f :: Force Int -> Force Int`, we might like to write `f :: Int! -> Int!`. We define post-fix application of bang to be a wrapping of `Force`.



**(OPTIONAL) Introduce a pattern synonym `Thunk`.** `suspend` can be generalized into the bidirectional pattern synonym `Thunk`:


```
pattern Thunk a <- x | let a = Force x
  where Thunk (Force a) = a
```

>
>
> **Iceland\_jack**: This syntax is invalid, is this what was intended?
>
>
> ```
> pattern :: Force a -> a
> pattern Thunk a        <- (Force -> a)
>   where Thunk (Force a) = a
> ```



For example:


```
let x = Thunk (error "foo" :: Force Int) :: Int
in True
```


does not error. Pattern matching over `Thunk` forces the argument (similar to bang patterns) and returns the unlifted value (unlike bang patterns):


```
let Thunk x = 3 + 5 :: Int
in x :: Force Int
```

## Dynamic semantics of unlifted types



In this section, we review the dynamic semantics of unlifted types.  These are not being added by our proposal (since they are already implemented by `#`), but since they are fairly unfamiliar to most Haskell users, I think this section will be useful.



**Case binding.** Given `case e of x1 -> e1`, where `e` is `Unlifted`, `e` is evaluated to whnf, and then the result is case-matched upon. (i.e. it is always as if it is a strict pattern match.)



**Let bindings.** Given `let x = e in e'`, where `x` is `Unlifted)`, this desugars to `let !x = e in e'` which desugars to `case e of !x -> e'`. Mutually recursive let bindings of unlifted variables are not allowed. Let bindings are evaluated bottom up (but see [\#10824](http://gitlabghc.nibbler/ghc/ghc/issues/10824)).



**Conditionals.** Given `if e then e1 else e2` where `e1` and `e2` are `Unlifted`, this desugars into the obvious case. (NB: this means `e1` and `e2` are not eagerly evaluated, as they would be for an `ifthenelse` function.)



**Constructors and function application.** Given `K e` where `e` is `Unlifted`, this desugars into `case e of !x -> K x`. NB: if `K` is in kind star, then this expression admits bottom!



Intuitively, if you have an unlifted type, anywhere you let bind it or pass it to a function, you evaluate it. Strict let bindings cannot be arbitrarily floated; you must preserve the ordering of bindings and they cannot be floated beyond an expression kinded `*`.


## FAQ



**Why do you get to use error in the unlifted examples, isn't error a bottom?** `error` is specially treated to be both lifted and unlifted. It's interpretation in an unlifted setting is that it immediately causes an exception when it is evaluated (it never goes into the heap).



**Why not `!Int` rather than `Int!` as the syntax proposal?** This syntax conflicts with strict data fields. `data S a = S !a` has a constructor of type `S :: Int -> S`, taking a lifted type and evaluating it before placing it in the constructor; `data S a = S a!` has a constructor of type `S :: Force Int -> S`, which requires the *user* to have forced the integer. Representationally, the data types are the same, but the outward behavior for clients differs dramatically.



**Is `Force (Maybe (Force Int))` allowed?** No, because `Force Int` has kind `Unlifted` but `Maybe` has kind `* -> Unlifted`. A data type declaration must be explicitly written to accept an unlifted type (`data StrictMaybe (a :: Unlifted) = SMaybe a`), or simply be strict in its field (`data StrictMaybe2 a = SMaybe2 !a`).



**How does this affect inlining?** In any CBV language, inlining doesn't always preserve semantics; unlifted types are no different. For example, this errors:


```
  let x = error "foo" :: Force Int
      y = suspend x :: Int
  in True
```


but this example does not:


```
  let y = suspend (error "foo" :: Force Int) :: Int
  in True
```


**What's the difference between `Force Int` and `Int#`?** `Force Int` is an unlifted, boxed integer; `Int#` is an unlifted, unboxed integer.



**Why aren't strict patterns enough?** A user can forget to write a strict pattern at a use-site. Putting a type in kind unlifted forces all use-sites to act as if they had strict patterns.



**Is `Force [a]` the type of strict lists?** No. It is the type of a lazy list whose head is always evaluated and non-bottom.



**Does `foo :: Force [a] -> Force [a]` force just the first constructor or the whole spine?** You can't tell; the head not withstanding, `[a]` is still a lazy list, so you would need to look at the function body to see if any extra forcing goes on. `Force` does not induce `seq`ing: it is an obligation for the call-site.


# Fully alternate proposal



This section describes a totally alternate proposal to the one(s) above. The motivation is the same, but the proposal otherwise starts from scratch.


## Proposal B1. Allow newtypes over unlifted types



Identical to Proposal 3 above.


## Proposal B2. More levity polymorphism



[NoSubKinds](no-sub-kinds) proposes the following arrangement:


```
data Levity = Lifted | Unlifted
TYPE :: Levity -> *
type * = TYPE 'Lifted
type # = TYPE 'Unlifted
```


Instead, Proposal B2 suggests the following generalization:


```
data Boxity = Boxed | Unboxed
data Levity = Lifted | Unlifted
TYPE :: Boxity -> Levity -> *
type * = TYPE 'Boxed 'Lifted
-- TYPE 'Boxed 'Unlifted is the kind of boxed, unlifted types
-- TYPE 'Unboxed 'Unlifted is the kind of unboxed, unlifted types
-- TYPE 'Unboxed 'Lifted is utterly wrong, and prohibited from ever appearing in a desugared program

type B_L   = TYPE 'Boxed   'Lifted    -- just for this wiki page
type B_UL  = TYPE 'Boxed   'Unlifted  -- just for this wiki page
type UB_UL = TYPE 'Unboxed 'Unlifted  -- just for this wiki page
```


Type variables would be allowed to have kinds `B_L` or `B_UL` but never `UB_UL`. This would allow polymorphism over boxed-but-unlifted things, by separating out the kinds. Functions could still not be properly levity-polymorphic -- GHC would default a `Boxity` metavariable to `'Boxed` and a levity metavariable to `'Lifted` during zonking. (Unless the `Levity` metavariable is paired with a boxity metavariable that is already known to be `'Unboxed`.)



This treatment is a simple extension of levity polymorphism, and it serves the same goals: to have a few cases where we really do want levity-polymorphic code (`error`, `undefined`) while providing a nicely uniform structure for kinds-of-types-with-values. I (Richard) am a little bothered by the existence of an unused spot in the structure (`TYPE 'Unboxed 'Lifted`) but not enough to back down.



**Definition:** A **valued kind** is a kind of types that have values. In released GHC, the valued kinds are `*`, `#`, and `Constraint`. Proposal B2 suggests adding `TYPE 'Boxed 'Unlifted` and `TYPE 'Unboxed 'Unlifted` to this set.



(**Extension:** Edward Kmett would recommend


```
data Constraintiness = Constraint | NotConstraint
TYPE :: Boxity -> Levity -> Constraintiness -> *
```


to allow better inference around tuple types. While RAE thinks this would actually work just fine, it feels like one bridge too far.)


## Proposal B3. Levity polymorphic data types



Proposal 1, above, allows users to make unlifted data types, separately from lifted ones. RAE is worried that this approach would lead to tons of duplication of data types (we might need **four** variants of `Maybe`, for example) and wants a more uniform way forward. Hence this Proposal B3.



The proposal will explain by way of example. (The use of GADT syntax here is incidental, but it makes the text in this proposal work out better. The normal traditional-syntax declaration would have identical behavior.)


```
data Maybe a where
  Just    :: a -> Maybe a
  Nothing :: Maybe a
```


All we know about the type variable `a` is that it is used as an argument to `->`. Functions can naturally have any valued kind as an argument. But it would be silly to allow `a :: TYPE 'Unboxed 'Unlifted`, as we'll have trouble generating code for that. Furthermore, we wish to treat **all** datatypes as levity polymorphic in their return kind (the heart of Proposal B3). So, we infer this kind for `Maybe`:


```
Maybe :: forall (v1 :: Levity) (v2 :: Levity).
         TYPE 'Boxed v1 -> TYPE 'Boxed v2
```


Of course, this is all a bit of a hack. There really must be 4 datatypes under the hood. But each of the 4 make sense to have, and would, no doubt, be wanted ere long by programmers. When zonking term-level program text, we would need to squeeze out any remaining levity polymorphism so that the back-end could figure out which of the four `Maybe` types we really want here. (The four types really make a `data family`, don't they? Using data families in the implementation might simplify things a bit.)



To reiterate: the levity polymorphism here is just to simplify the life of the programmer by using one name -- `Maybe` -- for all four sensible variants of an option type. Just like using levity polymorphism for `undefined` make the life easier for the programmer.


### A recursive example



Let's look at a recursive datatype to see how this works out:


```
data List a where
  Nil :: List a
  Cons :: a -> List a -> List a
```


After inference and with everything explicit, we would get


```
data List :: forall (v1 :: Levity) (v2 :: Levity).
             TYPE 'Boxed v1 -> TYPE 'Boxed v2 where
  Nil :: forall (v1 :: Levity) (v2 :: Levity) (a :: TYPE 'Boxed v1).
         List v1 v2 a
  Cons :: forall (v1 :: Levity) (v2 :: Levity) (a :: TYPE 'Boxed v1).
          a -> List v1 v2 a -> List v1 v2 a
```


According to this definition, `List a :: B_UL` is really a proper strict list. That is, we avoid some of the problems in the original proposal (not the "B" proposals) by avoiding issues around affecting only the head of lists. This definition affects the **whole** list.



Of course, users are free to use levity polymorphic recursion to achieve other combinations of strictness, but the default is to have either all lazy or all strict. 



**Note:** This Proposal B3 does overlap some concerns: it combines the idea of unlifted datatypes with a means for using one name for multiple entities. But I think this makes sense given the convenience of this approach.


### Design questions


1. Should levity polymorphism apply to **all** datatypes? If we think "yes", do we turn it off when a user explicitly writes `-> *` in a kind signature for a datatype?

1. It's tempting to say that the `!` prefix operator is now just an identity operation with kind `B_UL -> B_UL`. That is, it just forces the existing kind inference machinery to infer an unlifted kind. But this would seem to break existing code that expects `!` not to affect kinds.

1. Could the `Force` and `suspend` ideas from the non-B proposals help here? Perhaps. RAE doesn't fully understand `Force`.

## Proposal B4: Levity polymorphic functions



Carrying forward from Proposal B3, we could extend levity polymorphism to work on functions. For example, `map` could be inferred to have this type


```
map :: forall (v1 :: Levity) (v2 :: Levity) (v3 :: Levity) (v4 :: Levity)
              (a :: TYPE 'Boxed v1) (b :: TYPE 'Boxed v2).
       (a -> b) -> [] @v1 @v3 a -> [] @v2 @v4 b
```


Here, `v1` and `v2` are the levities of `a` and `b`, respectively, and `v3` and `v4` are the levities of the argument and result lists, respectively. As previously articulated, we can't have proper levity polymorphic functions, instead requiring specialization. This would mean **sixteen** `map`s. That's quite a high number. But I (Richard) think all of the variants make sense. I'd love for there to be some optimization to prevent the need to export 16 definitions here, but I don't see one. In any case, I do think this would work swimmingly for users. Is it worth the cost? That's up for debate.



**Conjecture:** If we take this idea to its logical extreme -- of making all functions levity polymorphic in all of the datatypes mentioned -- then RAE believes that using `!` to control kinds in datatype definitions would not break any code.


### Design questions


1. Is there a way to reduce duplication in object code? If we can't find a "yes" answer, this problem may kill the idea.

## Parametricity



Some of the examples given in the alternate proposals (B3 and B4) are not parametric polymorphism, but rather ad-hoc polymorphism of levities. For instance, the `map` example does not have one uniform implementation, but many based on the instantiations, and each case has quite different behavior.



However, there are many examples of functions that can be properly levity polymorphic, because they are just (at a low level) moving pointers around, and the levity only tracks the overall calling convention for those pointers, rather than the operation of the body of the function. The most immediate example is constructors of data types. If we have:


```
data T :: forall (l :: Levity). (a :: TYPE 'Boxed l) -> TYPE 'Boxed 'Lifted where
  C :: forall (l :: Levity) (a :: Type 'Boxed l). a -> T a
```


Then `C` is genuinely parametric in `l`. It accepts a pointer to a value that is either lifted or not, stores it, and matching on it later yields a pointer to a value with the same status. It is unclear to the present author whether `C` would also be genuinely parametric if `T` were parameterized on its levity status as well.



There are examples beyond constructors, though. All mutable cells for boxed types have the same behavior of simply storing a pointer, so put and get operations should be genuinely parametric. They simply preserve the calling conventions of things they are fed and return. Further, this area is quite rich, and enabling these parametric uses fixes problems with duplication, type safety and efficiency in primops. Right now one must choose between:


```
ArrayArray# -- not type safe
Array# (Array a) -- has extra indirection
```


whereas with levity polymorphic `Array#`, one could have `Array# (Array# a)` which is the best of both worlds (eliminating the need for duplication).



As a final thought, let's consider the `map` example:


```
map :: forall (v1 v2 v3 v4 :: Levity)
              (a :: TYPE 'Boxed v1)
              (b :: TYPE 'Boxed v2)
       (a -> b) -> [] @v1 @v3 a -> [] @v2 @v4 b
map f [] = []
map f (x:xs) =
  let y = f x
      ys = map f xs
   in y:ys
```


The author believes that `map` is parametric in `v1`, as the list stored `a` values with that calling convention, and `f` expects them with the same. However, `v2` determines whether a closure needs to be built for `y`, and unless this is somehow attributed to `f`, different code must be generated for `map`. The same seems to go for `v4`, except the fact that it's a recursive call suggests that it doesn't matter who we attribute the closure-building responsibility to, and `map` is not parametric in it. The remaining question is `v3`, but it would seem that `map` is not parametric in it, either, since it determines whether case analysis is required to ensure evaluation. So `map` is only parametric polymorphic in one of the four variables given.



It is this author's opinion that it would be unfortunate to give ad-hoc levity polymorphism the same syntax as parametric polymorphism (and also that making everything ad-hoc overloaded in levity would probably be confusing), given that there are substantial examples of the latter.


# Minimal Semantics



With regard to the quote in the Motivation section, it should be noted that it
is mostly untrue. The data type `data Nat = Z | S !Nat` is a flat domain of
natural numbers; but this is also true in strict languages like ML. The only
difference between it and `data unlifted UNat = UZ | US UNat` is the (default)
'calling convention' for operations on the type. For `Nat` the calling
convention is non-strict by default (although one can force strictness), and for
`UNat` (and the type in ML) it is (necessarily) strict. The strict calling
convention allows one to ignore the domain aspect for arguments to a function
(as opposed to expressions of a type, which coudl denote bottom).



Thus, it seems as though the semantic gap could be filled if `!Nat` and the like
were simply proper types. We could have `!Nat :: Unlifted`. It could be
represented identically to `Nat`, but bindings of that type would need to be
evaluated eagerly (and probably wouldn't be possible at the top level), and
'values' of the type could be relied upon to never be bottom. So for instance, a
function of type `!Nat -> ...` would be able to rely upon its first argument
being a non-bottom, evaluated natural number. Semantically, the values of `!Nat`
are exactly the set of natural numbers, and expressions denote the flat domain,
but the eager callling conventions ensure that we don't need to think about
bottom as a value.



In fact, this minimal addition bears a strong resemblance to the original
version of this proposal. However, it was noted that there is a problem with
the current meanting of `!a` in constructors. Given the declaration:


```
data T = C !Nat !Nat
```


The constructor `C` has type `Nat -> Nat -> T` which forces its arguments,
rather than `!Nat -> !Nat -> T`. And changing it to the latter would be a
significant, backward-incompatible change.



However, there may be a solution to this problem in the upcoming work on
impredicativity. That work proposes to add a new constraint `t <~ u`, meaning
that `t` can-be-instantiated/is-a-subtype-of `u`. The core-level witness of
`t <~ u` is simply a function `t -> u`.



It is conceivable that with this work, we could add the axiom `!a <~ a`. The
witness of this is simply an identity function that has an effect of forgetting
that its argument must already be evaluated, because the values of type `!a` are
also valid values of type `a`. Then, if the match:


```
case t of
  C x y -> ...
```


resulted in `x, y :: !Nat`, the judgments `x :: Nat` and `y :: Nat` (as the
Haskell report would specify) are valid. And also, if we have `x, y :: !Nat`,
the application `C x y` is well-typed with `C :: Nat -> Nat -> T` as a
constructor that evaluates its arguments. The only remaining piece of the puzzle
would be to have an optimization pass that eliminates the redundant evaluation
that `C` would do if called with arguments of type `!Nat` (via weakening to
`Nat`).



Assuming this all works as outlined above, it appears to introduce no redundancy
to the language, contrary to having an entirely separate data declaration form.


