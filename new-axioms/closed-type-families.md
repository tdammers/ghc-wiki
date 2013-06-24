# New syntax for branched family instances



**Note**: This page is here for historical reasons. The implemented feature is described on the [NewAxioms](new-axioms) page.



This page proposes a new concrete syntax for branched family instances that seems cleaner, easier to understand and explain, and less verbose than the existing one.


## Current state of things



Currently (late May 2013), in HEAD, we can declare branched family instances like this:


```wiki
type family F a
type instance where
  F Int = Bool
  F Bool = Char
  F a = Double
```


Branched instances and unbranched instances can be freely mixed for the same type family.


## The problem



As we are revising the overlap check between family instances, it is necessary to consider how branched instances overlap. One problem with branched instances, as they are, is that they can be irregularly shaped. For example, do the following overlap?


```wiki
type family G x y
type instance where
  G Int Bool = Int
  G (Maybe a) [b] = Bool
  G (a, b) c = Char

type instance where
  G (Maybe a) (Maybe b) = Double
  G [a] b = IO a
```


No, they don't, but they hit each other's space a bit. These instances are all linear (no repeated variables), but if they weren't, it would be even harder to tell.



Furthermore, a very common case is that the programmer intends to declare a family and give all instances for it right away. The current syntax is verbose for this case.


## Proposal



Branched instances would be allowed **only** like this:


```wiki
type family F a where
  F Int = Bool
  F Bool = Char
  F a = Double
```


Standalone branched instances would be disallowed -- the `type instance where` syntax would be retired. A declaration of a branched instance would also overlap with any other instance, in effect creating a **closed type family**. In fact, we could just call them *closed type families* and dispense with the *branched instance* name.



Note that if someone needed to create a branched instance for a regular (open) type family, this could be done with a helper type family:


```wiki
type family G a
type instance G Int = Bool

-- want a branched instance:
type instance G (x,y) = G' x y
type family G' x y where
  G' Int Int = Double
  G' a b = Int
```


This case has become more verbose, but I doubt it will come up often.


