## Type-Level Literals



Currently, we support two forms of type-level literals: natural numbers, and symbol constants.
Natural number literals are a family of types, all of which belong to the kind `Nat`.  Similarly,
symbol literals are types that belong to the kind `Symbol`:


```wiki
0, 1, 2, ...                            :: Nat
"hello", "world", "some string literal" :: Symbol
```


Both numeric and symbol literal types are empty---they have no inhabitants.  However, they may be
used as parameters to other type constructors, which makes them useful.


## Singleton Types



We use this idea to link the type-level literals to specific run-time values via *singleton types*.
The singleton types and some useful functions for working with them are defined in module `GHC.TypeLits`:


```wiki
module GHC.TypeLits where
```


A singleton type is simply a type that has only one interesting inhabitant.  We define a whole family
of singleton types, parameterized by type-level literals:


```wiki
data Sing :: a -> *
```


For example, `Sing 0`, `Sing 127`, `Sing "hello"`, and `Sing "this also"` are all
singleton types.  The intuition is that the only inhabitant of `Sing n` is the value `n`.  Notice
that `Sing` has a *polymorphic kind* because sometimes we apply it to numbers (which are of
kind `Nat`) and sometimes we apply it to symbols (which are of kind `Symbol`).



So, how do we make values of type `Sing n`?  This is done with
the special overloaded constant `sing`:


```wiki
class SingI a where
  sing :: Sing a

-- Built-in instances for all type-literals.
instance SingI 0        where sing = ... the singleton value representing 0 ...
instance SingI 1        where sing = ... the singleton value representing 1 ...
instance SingI "hello"  where sing = ... the singleton value representing "hello" ...
// ... etc.
```


Here are some examples on the GHCi prompt to get a feel of how `sing` works:


```wiki
> :set -XDataKinds
> sing :: Sing 1
> 1
> sing :: Sing "hello"
> "hello"
```


The name *SingI* is a mnemonic for the different uses of the class:


- It is the *introduction* construct for 'Sing' values,
- It is an *implicit* singleton parameter (this is discussed in more detail bellow)


It is also useful to get the actual integer or string associated with a singleton.
We can do this with the overloaded function `fromSing`.  Its type is quite general because
it can support various singleton families, but to start, consider the following two instances
of its type:


```wiki
fromSing :: Sing (a :: Nat) -> Integer
fromSing :: Sing (a :: Symbol) -> String
```


Notice that the return type of the function is determined by the *kind* of the
singleton family, not the concrete type.  For example, for any type `n` of
kind `Nat` we get an `Integer`, while for any type `s` of kind `Symbol` we get a
string.  Based on this idea, here is a more general type for `fromSing`:


```wiki
fromSing :: SingE (KindOf a) => Sing a -> Demote a
```


TODO Explain this more fully.



Notice that GHCi could display values of type `Sing`, so they have a `Show` instance.  As another example, here
is the definition of the `Show` instance:


```wiki
instance (SingE (KindOf a, Show (Demote a)) => Show (Sing a) where
  showsPrec p = showsPrec p . fromSing
```


Easy! We just convert the singleton into an ordinary value (integer or string), and use *its* `Show` instance to display it.



Next, we show two functions which make it easier to work with singleton types:


```wiki
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

singThat :: SingRep a => (Demote a -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing
```


The first function, `withSing`, is useful when we want to work with the same singleton value multiple times.
The constant `sing` is polymorphic, so every time we use it in a program, it may refer to a potentially
different singleton, so to ensure that two singleton values are the same we have to resort to
explicit type signatures, which just adds noise to a definition.  By using, `withSing` we avoid this problem
because we get an explicit (monomorphic) name for a given singleton, and so we can use the name many times
without any type signatures.  This technique is shown in the definition of the second function, `singThat`.



The function `singThat` is similar to the constant `sing` in that it defines new singleton values. However,
it allows us to specify a predicate on (the representation of) the value and it only succeeds if this predicate
holds.  Here are some examples of how that works:


```wiki
> singThat (== 1) :: Maybe (Sing 1)
> Just 1
> singThat (== 1) :: Maybe (Sing 2)
> Nothing
> singThat ("he" `isPrefixOf`) :: Maybe (Sing "hello")
> Just "hello"
```


Now, using `singThat` we can show the definition of the `Read` instance for singletons:


```wiki
instance (SingRep a, Read (Demote a), Eq (Demote a)) => Read (Sing a) where
  readsPrec p cs = do (x,ys) <- readsPrec p cs
                      case singThat (== x) of
                        Just y  -> [(y,ys)]
                        Nothing -> []
```


We use the `Read` instance of the representation for the singletons to parse a value,
and then, we use `singThat` to make sure that this was the value corresponding to
the singleton.


## Implicit vs. Explicit Parameters



There are two different styles of writing functions that use singleton types.
To illustrate the two style consider a type for working with C-style arrays:


```wiki
newtype ArrPtr (n :: Nat) a = ArrPtr (Ptr a)
```


Now consider the definition of the function `memset`, which sets all elements
of the array to a given fixed value.



One approach is to use an explicit singleton parameter.  For example:


```wiki
memset_c :: Storable a => ArrPtr n a -> a -> Sing n -> IO ()
memset_c (ArrPtr p) a size = mapM_ (\i -> pokeElemOff p i a) [ 0 .. fromIntegral (fromSing size - 1) ]
```


This style is, basically, a more typed version of what is found in many standard C libraries.
Callers of this function have to pass the size of the array explicitly, and the type system checks that the
size matches that of the array.  Note that in the implementation of `memset_c` we used `fromSing`
to get the concrete integer associated with the singleton type, so that we know how many elements
to set with the given value/



While the explicit `Sing` parameter is convenient when we define the function, it is a bit
tedious to have to provide it all the time---it is easier to let the compiler infer the value,
based on the type of the array:


```wiki
memset :: (Storable a, SingI n) => ArrPtr n a -> a -> IO ()
memset ptr a = withSing (memset_c ptr a)
```