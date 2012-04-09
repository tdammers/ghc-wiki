
We use a family of singleton types to relate type-level naturals to runtime values.



In our design, we chose to provide as primitive an overloaded "smart" constructor and a polymorphic elimination construct:


```wiki
newtype Sing n = Sing (SingRep n)

class SingI n where
  sing :: Sing n

instance SingI 0 where sing = Sing 0
instance SingI 1 where sing = Sing 1
...

fromSing :: Sing n -> SingRep n
fromSing (Sing n) = n
```


It is also possible to make the dual choice, where we provide a polymorphic constructor and an overloaded elimination construct:


```wiki
data Sing n = Sing

class SingE n where
  fromSing :: Sing n -> SingRep n

instance NatE 0 where fromSing Sing = 0
instance NatE 1 where fromSing Sing = 1
...
```


We made this choice, at least in part, because it made the implementation simpler: with our choice the evidence for class `SingI` is just an integer or a string.  Note that our choice does not loose any generality because we can define the alternative design in terms of it:


```wiki
data Sing1 = Sing1

fromSing1 :: SingI n => Sing1 n -> SingRep n
fromSing1 = fromSing . cast
  where cast :: SingI n => Sing1 n -> Sing n
        cast Sing1 = sing
```