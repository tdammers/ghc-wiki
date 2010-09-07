# Using generalized data families for selectors



A simple way of representing messages that classes respond to is using a GADT data family indexed (as a type function) by the class that responds to the messages and (as a GADT output index) by the function type equivalent to the selector (but we can omit the receiver):


```wiki
{-# LANGUAGE TypeFamilies, GADTs #-}

data family Selector :: * -> * -> *

send :: Selector r f -> r -> f
send = undefined

newtype NSObject = NSObject (Ptr ())

data instance Selector NSObject a where
  Hash :: Selector NSObject (IO Word)
  Class :: Selector NSObject (IO Class)
  IsKindOfClass_ :: Selector NSObject (Class -> IO Bool)
  -- ...
```


In practice, the `Selector` family might be an associated data type of a typeclass of objects, with `send` as a method that dispatches those selectors down to the appropriate FFI calls.



The question here is what to do about selector overlaps across classes. If we can find a nice solution for subclassing we might not need to think about overlap between methods on superclasses. Maybe wrapping the whole thing in a class would allow us to deal with the overlap, and the methods would also allow us to use camel case on the method names. As all class objects themselves will probably just be newtypes around pointers, maybe GeneralizedNewtypeDeriving can help us avoid too much boilerplate too?


