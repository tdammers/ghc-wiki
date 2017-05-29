## Examples of programs that rely on polymorphic static values



The [
static pointers discussion](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers) includes a discussion on how to support polymorphic static values. On this page we're collecting some programs that illustrate why this is needed.


## Polymorphic instances for the Static type class



Consider the following definition of a `Closure`:


```
data Closure :: * -> * where
    CPtr :: StaticPtr a -> Closure a
    CApp :: Closure (a -> b) -> Closure a -> Closure b
    CEnc :: Closure (Dict (Binary a)) -> ByteString -> Closure a

instance IsStatic Closure where
  fromStaticPtr = CPtr
```


`CPtr` allows us to lift static pointers, `CApp` allows us to apply closures of functions to closures of arguments, and finally `CEnc` allows us to lift anything serializable, as long as we have a static pointer to the corresponding `Binary` type class instance dictionary. This definition is similar to the one used in the [
distributed-closure](http://hackage.haskell.org/package/distributed-closure) package, but adjusted a little bit for the sake of clarity in the current discussion.



An example of such as a `Closure` is


```
ex1 :: Text -> Closure (IO ())
ex1 str = static T.putStrLn `CApp` CEnc (static Dict) (encode str)
```


Now since this is such a common pattern, we'd like to clean it up a bit. A *very* useful type class is the following:


```
class c => Static c where
  closureDict :: Closure (Dict c)
```


This allows us to define


```
cpure :: Static (Binary a) => a -> Closure a
cpure a = CEnc closureDict (encode a)
```


and hence


```
instance Static (Binary Text) where
  closureDict = static Dict

ex2 :: Text -> Closure (IO ())
ex2 str = static T.putStrLn `CApp` cpure str
```


In a large application we need lots of `Static C` instances, for all kinds of constraints `C`, basically alongside the standard class hierarchy. The first important point I want to make is that in order to do this in a generic way, we need polymorphic static values. For example, consider


```
dictBinaryList :: Dict (Binary a) -> Dict (Binary [a])
dictBinaryList Dict = Dict

instance (Typeable a, Static (Binary a)) => Static (Binary [a]) where
  closureDict = static dictBinaryList `CApp` closureDict
```


We can only define this `Static (Binary [a])` instance if we can define a polymorphic static value `static dictBinaryList`. Without support for polymorphic static values our ability to define generic code dealing with static pointers would be severely hindered.


## Polymorphic recursion



Consider


```
data Nat = Zero | Succ Nat

data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

closureSZero :: Closure (SNat 'Zero)
closureSZero = CPtr (static SZero)

closureSSucc :: Typeable n => Closure (SNat n -> SNat ('Succ n))
closureSSucc = CPtr (static SSucc)

closureSNat :: SNat n -> Closure (SNat n)
closureSNat SZero     = closureSZero
closureSNat (SSucc n) = (\Dict -> closureSSucc `CApp` closureSNat n)
                          (natTypeable n)

-- Auxiliary: all Nats are typeable

data Dict :: Constraint -> * where
  Dict :: c => Dict c

natTypeable :: SNat n -> Dict (Typeable n)
natTypeable SZero = Dict
natTypeable (SSucc n) = (\Dict -> Dict) (natTypeable n)
```


Here `closureSNat` calls `closureSSucc` at different types at each level through the recursion. (The need to recompute evidence for `Typeable` here is unfortunate, but is because we cannot reason "backwards" -- `Typeable` of `'Succ n` doesn't tell us anything about `Typeable` of `n`; this is an orthogonal issue though.)


