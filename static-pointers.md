# The design and implementation of static pointers



This page lays out thinking about the design of the `StaticPtr` language extensions.



The basic idea is laid out in the original [
Cloud Haskell paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/remote.pdf).



See also


- [the root page for distributed Haskell](distributed-haskell).
- [
  Neil Mitchell's blog post](http://neilmitchell.blogspot.com/2017/09/existential-serialisation.html) on static pointers for existentials.

## Tickets



Use Keyword = `StaticPointers` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#12875](http://gitlabghc.nibbler/ghc/ghc/issues/12875)</th>
<td>GHC fails to link all StaticPointers-defining modules of a library in an executable</td></tr>
<tr><th>[\#13306](http://gitlabghc.nibbler/ghc/ghc/issues/13306)</th>
<td>Problems with type inference for static expressions</td></tr>
<tr><th>[\#13499](http://gitlabghc.nibbler/ghc/ghc/issues/13499)</th>
<td>"Panic: no skolem info" with StaticPointers and typed hole</td></tr>
<tr><th>[\#14770](http://gitlabghc.nibbler/ghc/ghc/issues/14770)</th>
<td>Allow static pointer expressions to have static pointer free variables</td></tr>
<tr><th>[\#14939](http://gitlabghc.nibbler/ghc/ghc/issues/14939)</th>
<td>Lint error in forall type</td></tr>
<tr><th>[\#15395](http://gitlabghc.nibbler/ghc/ghc/issues/15395)</th>
<td>Make StaticPtr (more) robust to code changes and recompilation</td></tr>
<tr><th>[\#15603](http://gitlabghc.nibbler/ghc/ghc/issues/15603)</th>
<td>ref6 example from StaticPointers documentation doesn't type check</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#10446](http://gitlabghc.nibbler/ghc/ghc/issues/10446)</th>
<td>Fix error message when variables in a static form are not in scope</td></tr>
<tr><th>[\#12000](http://gitlabghc.nibbler/ghc/ghc/issues/12000)</th>
<td>static pointer in ghci</td></tr>
<tr><th>[\#12003](http://gitlabghc.nibbler/ghc/ghc/issues/12003)</th>
<td>Improve error message about closed variables</td></tr>
<tr><th>[\#12207](http://gitlabghc.nibbler/ghc/ghc/issues/12207)</th>
<td>CgStaticPointers fails with -O</td></tr>
<tr><th>[\#13305](http://gitlabghc.nibbler/ghc/ghc/issues/13305)</th>
<td>static: check for identifiers should only consider term level variables</td></tr>
<tr><th>[\#13481](http://gitlabghc.nibbler/ghc/ghc/issues/13481)</th>
<td>T12622 fails in ghci way</td></tr>
<tr><th>[\#14204](http://gitlabghc.nibbler/ghc/ghc/issues/14204)</th>
<td>GHC bug - makeStatic: Unresolved static form at line 13, column 14.</td></tr>
<tr><th>[\#15035](http://gitlabghc.nibbler/ghc/ghc/issues/15035)</th>
<td>Panic when using StaticPointers with typed holes</td></tr></table>



## Background



We take for granted the basic design of the Cloud Haskell paper.
That is,


- A type constructor `StaticPtr :: * -> *`. Intuitively, a value of type `StaticPtr t` is represented by a static code pointer to a value of type `t`.  Note "code pointer" not "heap pointer".  That's the point!

- A language construct `static <expr>`, whose type is `StaticPtr t` if `<expr>` has type `t`.  

- In `static <expr>`, the free variables of `<expr>` must all be bound at top level. The implementation almost certainly works by giving `<expr>` a top-level definition with a new name, `static34 = <expr>`.

- A function `unStatic :: StaticPtr a -> a`, to unwrap a static pointer.

- `Static` values are serialisable.  Something like `instance Serialisable (StaticPtr a)`.  (This will turn out to be not quite right.)  Operationally this works by serialising the code pointer, or top-level name (e.g `"Foo.static34"`).


All of this is built-in.  It is OK for the implementation of `StaticPtr` to be part of the TCB.
But our goal is that *no other code need be in the TCB*.



**A red herring**.  I'm not going to address the question of how to serialise a static pointer.  One method would be to serialise a machine address, but that only works if the encoding and decoding ends are running identical binaries.  But that's easily fixed: encode a static as the *name* of the static value e.g. "function `foo` from module `M` in package `p`".  Indeed, I'll informally assume an implementation of this latter kind.



In general, I will say that what we ultimately serialise is a `StaticName`. You can think of a `StaticName` as package/module/function triple, or something like that. The implementation of `StaticName` is certainly not part of the client-visible API for `StaticPtr`; indeed, the type `StaticName` is not part of the API either.  But it gives us useful vocabulary.


---


# Serialising static pointers



We can see immediately that we cannot expect to have `instance Serialisable (Static a)`,
which is what the Cloud Haskell paper proposed.  If we had such an instance we would have


```wiki
encodeStatic :: forall a. StaticPtr a -> ByteString
decodeStatic :: forall a. ByteString -> Maybe (StaticPtr a, ByteString)
```


And it's immediately apparent that `decodeStatic` *cannot* be right. 
I could get a `ByteString` from anywhere, apply `decodeStatic` to it,
and thereby get a `StaticPtr a`.  Then use
`unStatic` and you have a value of type `a`, for, *for any type `a`*!!



Plainly, what we need is (just in the case of `cast`) to do a dynamic typecheck, thus:


```wiki
decodeStatic :: forall a. Typeable a 
                       => ByteString -> Maybe (StaticPtr a, ByteString)
```


Let's think operationally for a moment:


- GHC collects all the `StaticPtr` values in a table, the **static pointer table** or **SPT**.  Each row contains

  - The `StaticName` of the value
  - A `Dynamic` for its value (i.e. a pair of the value itself and its `TypeRep`)

- `decodeStatic` now proceeds like this:

  - Parse a `StaticName` from the `ByteString` (failure =\> `Nothing`)
  - Look it up in table (not found =\> `Nothing`)
  - Use `fromDynamic` to compare the `TypeRep` passed to `decodeStatic` (via the `Typeable a` dictionary) with the one in the table (not equal =\> `Nothing`)
  - Return the value


**Side note.** Another possibility is for `decodeStatic` not to take a `Typeable a` context but instead for `unStatic` to do so:: `unStatic :: Typeable a => StaticPtr a -> Maybe a`.  But that seems a mess.  Apart from anything else, it would mean that a value of type `StaticPtr a` might or might not point to a value of type `a`, so there's no point in having the type parameter in the first place. **End of side note.**



This design has some useful consequences that are worth calling out:


- A `StaticPtr` is serialised simply to the `StaticName`; *the serialised form does not need to contain a `TypeRep`*.  Indeed it would not even be type-safe to serialise a `StaticPtr` to a pair of a `StaticName` and a `TypeRep`, trusting that the `TypeRep` described the type of the named function. Why not?  Think back to "Background: serialisation" above, and imagine we said

  ```wiki
  decode (encode ["wibble", "wobble"]) 
    :: Typeable a => Maybe (StaticPtr a, ByteString)
  ```

  Here we create an essentially-garbage `ByteString` by encoding a `[String]`, and try to decode it.  If, by chance, we successfully parse a valid `StaticName` and `TypeRep`, there is absolutely no reason to suppose that the `TypeRep` will describe the type of the function. 

  Instead, the `TypeRep` of the static pointer lives in the SPT, securely put there when the SPT was created.  Not only is this type-safe, but it also saves bandwidth by not transmitting`TypeReps`.

- Since clients can effectively fabricate a `StaticName` (by supplying `decodeStatic` with a bogus `ByteString`, a `StaticName` is untrusted.  That gives the implementation a good deal of wiggle room for how it chooses to implement static names.  Even a simple index in the range 0..N would be type-safe! 

  The motivation for choosing a richer representation for `StaticName` (eg package/module/name) is not type-safety but rather resilience to change.  For example, the Haskell programs at the two ends could be quite different, provided only that they agreed about what to call the static pointers that they want to exchange.

## Statics and existentials



Here is something very reasonable:


```wiki
data StaticApp b where
  SA :: StaticPtr (a->b) -> StaticPtr a -> StaticApp b

unStaticApp :: StaticApp a -> a
unStaticApp (SA f a) = unStatic f (unStatic a)
```


(We might want to add more constructors, but I'm going to focus only on `SA`.)
A `SA` is just a pair of `StaticPtr`s, one for a function and one for an argument.  We can securely unwrap it with `unStaticApp`.



Now, here is the question: can we serialise `StaticApp`s?  Operationally, of course yes: to serialise a `SA`, just serialise the two `StaticPtrs` it contains, and dually for deserialisation.   But, as before, deserialisation is the hard bit.  We seek:


```wiki
decodeSA :: Typeable b => ByteString -> Maybe (StaticApp b, ByteString)
```


But how can we write `decodeSA`?  Here is the beginning of an attempt:


```wiki
decodeSA :: Typeable b => ByteString -> Maybe (StaticApp b, ByteString)
decodeSA bs
  = case decodeStatic bs :: Maybe (StaticPtr (a->b)) of
      Nothing -> Nothing
      Just (fun, bs1) -> ...
```


and you can immediately see that we are stuck.  Type variable `a` is not in scope.
More concretely, we need a `Typeable (a->b)` to pass in to `decodeStatic`, 
but we only have a `Typeable b` to hand.  



What can we do?  Tantalisingly, we know that if `decodeStatic` succeeds in parsing a static `StaticName` from `bs` then, when we look up that `StaticName` in the Static Pointer Table, we'll find a `TypeRep` for the value.  So rather than passing a `Typeable` dictionary into `decodeStatic`, we'd like to get one out!



With that in mind, here is a new type signature for `decodeStatic` that returns
both pieces:


```wiki
data DynStaticPtr where
  DSP :: TypeRepT a -> StaticPtr a -> DynStaticPtr

decodeStatic :: ByteString -> Maybe (DynStaticPtr, ByteString)
```


(The name `DynStaticPtr` comes from the fact that this data type is extremely similar to the library definition of `Dynamic`.)



Operationally, `decodeStaticK bs fail cont` works like this;


- Parse a `StaticName` from `bs` (failure =\> return Nothing)
- Look it up in the SPT (not found =\> return Nothing)
- Return the `TypeRep` and  the value found in the SPT, paired up with `DSP`. (Indeed the SPT could contain the `DynStaticPtr` values directly.)


For the construction of `DynStaticPtr` to be type-safe, we need to know that the
`TypeRep` passed really is a `TypeRep` for the value; so the construction
of the SPT is (unsurprisingly) part of the TCB.



Now we can write `decodeSA` (the monad is just the `Maybe` monad, nothing fancy):


```wiki
decodeSA :: forall b. Typeable b => ByteString -> Maybe (StaticApp b, ByteString)
decodeSA bs
  = do { (DSP (trf :: TypeRepT tfun) (fun :: StaticPtr tfun), bs1) <- decodeStatic bs
       ; (DSP (tra :: TypeRepT targ) (arg :: StaticPtr targ), bs2) <- decodeStatic bs1
            -- At this point we have 
            --     Typeable b      (from caller)
            --     Typeable tfun   (from first DSP)
            --     Typeable targ   (from second DSP)
       ; Refl <- eqTT ....

       ; fun' :: StaticPtr (targ->b) <- cast ( :: tfun :~: targ -> b) fun   
       ; return (SA fun' arg, bs2) }

cast :: (a :~: b) -> a -> Maybe b
```


The call to `cast` needs `Typeable tfun`, and `Typeable (targ->b)`. The
former is bound by the first `DSP` pattern match.  The latter is
constructed automatically from `Typeable targ` and `Typeable b`, both
of which we have.  Bingo!



Notice that *`decodeSA` is not part of the TCB*.  Clients can freely write code like `decodeSA` and be sure that it is type-safe.


# Polymorphism and serialisation



Some motivation for polymorphic static pointers can be found at [
https://ghc.haskell.org/trac/ghc/wiki/StaticPointers/NeedForPolymorphism](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers/NeedForPolymorphism) .



For this section I'll revert to the un-generalised single-parameter `StaticPtr`.


## Parametric polymorphism



Consider these definitions:


```wiki
rs1 :: Static ([Int] -> [Int])
rs1 = static reverse

rs2 :: Static ([Bool] -> [Bool])
rs2 = static reverse

rs3 :: forall a. Typeable a => Static ([a] -> [a])
rs3 = static reverse
```


The first two are clearly fine. The SPT will get one row for each of the two monomorphic calls to reverse, one with a `TypeRep` of `[Int] -> [Int]` and one with a `TypeRep` of `[Bool] -> [Bool]`.



But *both will have the same code pointer*, namely the code for the polymorpic `reverse` function.  Could we share just one `StaticName` for all instantiations of `reverse`, perhaps including `rs3` as well?



I think we can.  The story would be this:


- The SPT has a row for `reverse`, containing

  - The `StaticName` for `reverse`
  - A pointer to the code for `reverse` (or, more precisely, its static closure).
  - A function of type `TypeRep -> TypeRep` that, given the `TypeRep` for `a` returns a `TypeRep` for `[a] -> [a]`.

- When we serialise a `StaticPtr` we send 

  - The `StaticName` of the (polymorphic) function
  - A list of the `TypeRep`s of the type arguments of the function

- The rule for `static <expr>` becomes this: the free *term* variables `<expr>` must all be top level, but it may have free *type* variables, provided they are all `Typeable`.


All of this is part of the TCB, of course.


## Type-class polymorphism



Consider `static sort` where `sort :: Ord a => [a] -> [a]`.  Can we make such a `StaticPtr`.  After all, `sort` gets an implicit value argument, namely an `Ord a` dictionary.  If that dictionary can be defined at top level, well and good, so this should be OK:


```wiki
ss1 :: StaticPtr ([Int] -> [Int])
ss1 = static sort
```


But things go wrong as soon as you have polymorphism:


```wiki
ss2 :: forall a. Ord a => StaticPtr ([a] -> [a])
ss2 = static sort  -- WRONG
```


Now, clearly, the dictionary is a non-top-level free variable of the call to `sort`.



We might consider letting you write this:


```wiki
ss3 :: forall a. StaticPtr (Ord a => [a] -> [a])
ss3 = static sort   -- ???
```


so now the `static` wraps a function expeting a dictionary.  But that edges us uncomforatbly close to impredicative types, which is known to contain many dragons.



A simpler alternative is to use the Dict Trick (see Background above):


```wiki
ss4 :: forall a. StaticPtr (Dict (Ord a) -> [a] -> [a])
ss4 = static sortD

sortD :: forall a. Dict (Ord a) -> [a] -> [a]
sortD Dict xs = sort xs
```


Now, at the call side, when we unwrap the `StaticPtr`, we need to supply an explicit `Ord` dictionary, like this:


```wiki
...(unStatic ss4 Dict)....
```


For now, I propose to deal with type classes via the Dict Trick, which is entirely end-user programmable, leaving only parametric polymorphism for built-in support.


## Local bindings in the static form



See Trac [\#11656](http://gitlabghc.nibbler/ghc/ghc/issues/11656).  The static form so far required expressions whose free variables appear bound at the top level. But this is stricter than necessary. Closed local definitions can be considered static as well.



Consider the following example


```
test :: Int -> (StaticPtr ([[Int]] -> [[Int]]), Int)
test x = (static (filter hasZero), c)
  where
    hasZero = any isZero
    isZero  = (0 ==)
    c = x + 1
```


Here's a proposal to have the compiler deal with it:


1. Have the typechecker compute whether bindings are closed with the `tct_closed` flag.
1. When the typechecker finds a static form, allow the free vars to be bound at the top-level or be closed local bindings.
1. Desugar the `static e` to `StaticPtr key e`, but unlike the current implementation, don't produce a binding for it yet.
1. Run the [FloatOut](float-out) pass. If -O0 was specified, have it float things to the top level only. This should produce bindings of the form `v = StaticPtr _ _`.
1. Collect all such bindings into the static pointer table.


In our running example,


- Step (1) identifies bindings `["hasZero", "isZero"]` as closed.
- Step (2) checks that identifiers in `filter hasZero`, the body of `static`, are bound at the top-level (like `filter`) or are closed local bindings (like `hasZero`).
- Step (3) desugars the static form to produce something like:

  ```
  test :: Int -> (StaticPtr ([[Int]] -> [[Int]]), Int)
  test x = (StaticPtr "key1" (filter hasZero), c)
    where
      hasZero = any isZero
      isZero  = (0 ==)
      c = x + 1
  ```
- Step (4) runs the [FloatOut](float-out) pass that should move to the top level all needed bindings and subexpressions.

  ```
  static1 :: StaticPtr ([[Int]] -> [[Int]])
  static1 = StaticPtr "key1" (filter hasZero)

  hasZero = any isZero

  isZero  = (0 ==)

  test :: Int -> (StaticPtr ([[Int]] -> [[Int]]), Int)
  test x = (static1, c)
    where
      c = x + 1
  ```
- Step (5) finds the binding `static1` and inserts it in the SPT.


Ideally, [FloatOut](float-out) would leave bindings of the form `v = Static ...`, but it is not clear if it will add also enclosing expressions `v = ... (Static  ...) ...`. There are two ways to approach this:


1. Have the [FloatOut](float-out) pass always put `Static ...` in its own binding.
1. Have another pass do the job after [FloatOut](float-out).

### On testing closedness



Whether `hasZero` and `isZero` are given general types or not shouldn't affect the result in this case. However, constraints can be problematic:


```
test2 :: Binary a => a -> StaticPtr ByteString
test2 x = static (g x)
  where
    g = encode
```


`g` is gonna use the `Binary a` dictionary provided to `test2`, which makes the body of `g` not closed. The typechecker needs to report an error in this case. And this is why the renamer cannot check for *closedness*.


