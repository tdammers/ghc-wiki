
The only "magic" thing about `GHC.TypeLits` are the instances of `SingI`.  The rest is implemented like this:


```wiki
newtype Sing n = Sing (SingRep n)

type family SingRep a
type instance SingRep (n :: Nat) = Integer
type instance SingRep (n :: Symbol) = String

fromSing :: Sing n -> SingRep n 
fromSing (Sing n) = n
```


So, now we just need instances like these:


```wiki
instance SingI 0       where sing = Sing 0
instance SingI 1       where sing = Sing 1
instance SingI "hello" where sing = Sing "hello"
...
```


Because we cannot generate this infinite family of instances, we have
some code in GHC which can solve `SingI` predicates on the fly.



The "proof" (aka "dictionary") for `SingI n` is just the number (or string) `n`.  This is OK because:


1. GHC uses a `newtype` to represent the dictionaries for classes that have just a single method and no super-classes.  `SingI` is just such a class.
1. `Sing` is already a `newtype` for `Integer` or `String`, depending on the kind of its parameter.


Therefore, the dictionaries for class `SingI` are just integers or strings, depending on the kind of the parameter.


