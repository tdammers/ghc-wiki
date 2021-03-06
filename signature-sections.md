# The `SignatureSections` Language Extension



TODO forbid `(:: forall a . a)` (any variables must be in scope), mention wildcards, explain restrictions, typing-rule == type-checking the translation, ...


## Semantics



The `SignatureSections` language pragma enables the use of the `::` type-signature operator in "type-signature sections" expressions, i.e.


>
>
> *aexp* → `(` `::` *type* `)`
>
>


where *type* must not have any free (not bound by outer context) type-variables.



**Translation:** The following identity holds


>
>
> `(` `::` *type* `)` = `\` *x* `->` *x* `::` *type*
>
>

## Applications



Like Haskell2010 expression type-signatures, type-signature sections are useful to explicitly type expressions to help resolve ambiguous typing (e.g. due to overloading).


### Examples


```
canonDouble :: String -> String
canonDouble = show . read                -- type error
canonDouble = show . (:: Double) . read  -- OK
```

```
typeRep :: proxy a -> TypeRep
data Proxy a = Proxy

-- without `SignatureSections`
a = typeRep (Proxy :: Proxy Bool)

-- with `SignatureSections
a = typeRep (:: Bool)
```

### References


- [\#10803](http://gitlabghc.nibbler/ghc/ghc/issues/10803)
- [
  http://augustss.blogspot.co.at/2014/04/a-small-haskell-extension.html](http://augustss.blogspot.co.at/2014/04/a-small-haskell-extension.html)
