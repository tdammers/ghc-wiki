
Simple record system with selectors overloaded in explicitly-user-declared type classes. Thus one can control the scope as of any other type class.



To declare a record type, first declare classes of its selectors. Selectors can be grouped in classes by user's wish. For example:


```wiki
class Xive a where x :: a -> X;
```


For each class `C ... a`, every selector must be of type `a -> x`, where `x` is member type.



Then the semantics of a record type declaration


```wiki
data R = R { x :: X };
```


are these:

**If** `x` is in scope, part of some class `C ... a`, and of type `C ... a => a -> X`;
**then** automatic instance `C R where x (R { x = x }) = x`;
**else** type error.



If the member is polymorphic, then the semantics of a record type declaration


```wiki
data R x = R { x :: x };
```


are these:

**If** `x` is in scope, part of some class `C ... x ... a`, and of type `C ... x ... a => a -> x`;

**then** automatic instance `C ... x ... (R x) where x (R { x = x }) = x`;
**else** type error.



The semantics of the record mutation


```wiki
(r :: R) { x = x' }
```


are these:

**If** `x` is in scope, part of some class `C ... x ... a`, and of type `C ... x ... a => a -> x`,
**and** an instance `C ... x ... R` is in scope, **and** types of `x` and `x'` can be unified, i.e. `x=x'` would be valid in instance declaration of `C ... x ... a`;
**then** its value is `s` such that


```wiki
x s = x', y s = y r
```


where `y` is any other selector.


