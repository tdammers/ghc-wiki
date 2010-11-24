
Here is how we can use this API to define a `Show` instance for singleton types:


```wiki
instance Show (Nat n) where
  showsPrec p n = showsPrec p (natToInteger n)
```


A more interesting example is to define a function which maps integers into singleton types:


```wiki
integerToMaybeNat :: TypeNat n => Integer -> Maybe (Nat n)
integerToMaybeNat x = check nat
  where check y = if x == natToInteger y then Just y else Nothing
```


The implementation of `integerToMaybeNat` is a little subtle: by using
the helper function `check`, we ensure that the two occurrences of
`nat` (aka `y`) both have the same type, namely `Nat n`.  There are other
ways to achieve the same, for example, by using scoped type variables,
and providing explicit type signatures.



Now, we can use `integerToNat` to provide a `Read` instance for singleton types:


```wiki
instance TypeNat n => Read (Nat n) where
  readsPrec p x       = do (x,xs) <- readsPrec p x
                           case integerToMaybeNat x of
                             Just n  -> [(n,xs)]
                             Nothing -> []
```