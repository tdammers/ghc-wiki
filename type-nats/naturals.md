## Natural Numbers



We may define the type of (value level) natural numbers in terms of singleton types.
The idea is that a natural number is, basically, an unknown singleton type.
This is why we use an existential construct in the definition:


```wiki
data Natural = forall n . Natural !(Nat n)

instance Enum Natural	 
instance Eq Natural	 
instance Integral Natural	 
instance Num Natural	 
instance Ord Natural	 
instance Read Natural	 
instance Real Natural	 
instance Show Natural	 
```


The instances make it possible to work with 'Naturals' as with any other numeric type.
Note, however, that some of the operations are partial.
For example, subtracting a larger number from a smaller one results in the undefined value of type *Natural*.



We also provide some functions for converting *Integer* values into their corresponding *Natural* ones.
We do this by using an intermediate representation for integers in terms of naturals, *NaturalInteger*.  This type
is intended to be used only for the conversion.  While, in principle, we could provide numeric instances for the type,
we chose not to, because we would be duplicating functionality provided by the type *Integer*.


```wiki
data NaturalInteger
  = Negative Natural
  | NonNegative Natural

toNaturalInteger :: Integer -> NaturalInteger

subNatural :: Natural -> Natural -> NaturalInteger
```