
Occasionally there will be a type, usually a GADT, shaped exactly like this:


```
data Counter t1 ... where
  Base :: f1 -> ... -> Counter ...
  Step :: !(Counter ...) -> Counter ...
```


In code generation, any such type can be represented by a datatype with an unboxed `Word64` field and all the "base" fields.



In the simplest case where there are no base fields, this would turn a strict natural singleton into a `Word64`.



The actual reason I was thinking about this, however, is that Andres Löh had an interesting idea for defunctionalizing functions on finger trees that use a counting GADT with a non-trivial base case. That particular one is not in quite the right form, but can easily be massaged into it.



One tricky point: The representation must be fixed when the module containing the type definition is compiled, and that decision recorded. It's possible that type information will reveal later that the type has the required shape, but that's just too late. I think the easy, sensible path here is to make the call purely syntactically, ignoring any type families that could reduce to the correct recursive form.


