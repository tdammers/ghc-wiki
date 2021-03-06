
This example illustrates some choices that come up when
using existential types in combination with singleton types.
The choices are illustrated with an example, which
defines a typed interface for working with array.
(Constructors with existential types are written in
GADT notation.)


```wiki
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

import Control.Monad(forM_)
import GHC.TypeLits
import Foreign(Ptr, pokeElemOff, Storable, mallocArray)
```

## Arrays with Statically Known Sizes



We start by defining a type for pointers to a *sequence*
of adjacent elements in memory.  The number of elements
is statically known at compile time, which is why
we add it to the type of the pointer:


```wiki
newtype ArrPtr (n :: Nat) a = ArrPtr (Ptr a)
```


Now we can define a function which sets all elements in
the array to a specific value:


```wiki
memset_c :: Storable a => ArrPtr n a -> a -> Sing n -> IO ()
memset_c (ArrPtr p) a n =
  forM_ [ 1 .. fromSing n - 1 ] $ \i ->
    pokeElemOff p (fromIntegral i) a
```


Because the size of the array is statically known, we may
define an overloaded function, that will use the correct
size automatically, based on the type:


```wiki
memset :: (Storable a, SingI n) => ArrPtr n a -> a -> IO ()
memset p a = withSing (memset_c p a)
```


Here is an example of how we might use these types:


```wiki
clearPage :: ArrPtr 4096 Word8 -> IO ()
clearPage p = memset p 0
```


Note that because of the way we wrote the code,
there is no danger of accidentally passing the
incorrect size for an array.


## Hiding the Array Size with an Existential



We may also define a type for array whose sizes
are not known statically.  Such arrays have
two components: a pointer to data, and a number
storing how many elements there are in the array.



There are two different ways to define such arrays,
and the difference between these two choices is
the central point of this example:


```wiki
data ArrayS :: * -> * where
  ArrS :: Sing n -> ArrPtr n a -> ArrayS a

data ArrayD :: * -> * where
  ArrD :: SingI n => ArrPtr n a -> ArrayD a
```


The difference between the two is how we
store the size of the array: in `ArrayS` we
are using an explicit singleton values,
while in `ArrayD` the size is stored
in an implicit *dictionary* field.



Both representations have the size of the
array, so we can use them with the functions
that we already defined for arrays of statically
known sizes:


```wiki
memsetS :: Storable a => ArrayS a -> a -> IO ()
memsetS (ArrS s p) a = memset_c p a s

memsetD :: Storable a => ArrayD a -> a -> IO ()
memsetD (ArrD p) a = memset p a
```


The interesting difference between the two
is that `ArrayD` is (in some sense) *more static*.
In particular, we can always convert 
an `ArrayD` into an `ArrayS`, but we cannot
define the inverse function:


```wiki
fromArrD :: ArrayD a -> ArrayS a
fromArrD (ArrD p) = ArrS sing p
```

## Creating Dynamically Sized Arrays


```wiki
-- Unsafe, in general.
uncheckedSing :: Integral a => a -> Sing (n :: Nat)
uncheckedSing a = Sing (toInteger a)

mallocS :: Storable a => Int -> IO (ArrayS a)
mallocS n = do p <- mallocArray n
               return (ArrS (uncheckedSing n) (ArrPtr p))

example = do arr <- mallocS 10
             memsetS arr (0 :: Int)
             return arr
```