## Changes to Template Haskell Library



Throughout this document, it is assumed that the current version of GHC is 7.4.1.



A proposed change will add the following constructors to TH's `Type` datatype...


```wiki
| PromotedListT [Type]    -- for types of the form '[Int, Bool]
| PromotedTupleT [Type]   -- for types of the form '(Int, 'False)
| PromotedConsT           -- ':
```


... and the following constructors to TH's `Kind` datatype:


```wiki
| ConK Name               -- for kinds of the form Bool
| VarK Name               -- k
| AppK Kind Kind          -- k1 k2
| ListK                   -- []
| TupleK Int              -- (), (,), ...
| ConstraintK             -- Constraint
```


Note that there is no `ForallK` constructor because the internal GHC representation for kinds with variables does not use this. Kinds are automatically generalized over an entire type expression.



TH will also need to support promoted constructors other than lists and tuples, but this is in fact already supported through the use of `ConT`. The namespace of defined types and of promoted types is also already kept distinct. For example, if we have the definition `data Foo = Foo`, the results of ` [t| Foo |] ` and ` [t| 'Foo |] ` are distinct (as in, `==` returns `False`). However, applying `show` to these two results produces the same string. Using the naming quote syntax, we can access promoted data constructors using the single-quote form. For example, `ConT 'False` denotes the promoted data constructor `'False`. (Note that the parsed interpretations of the `'` in these two snippets are entirely unrelated.)


## Alternatives



Here are two alternatives to the above changes. They are orthogonal to each other (i.e. either can be chosen without affecting whether or not the other is chosen).


### Simpler Types



Instead of the new types above, we could have the following:


```wiki
| PromotedTupleT Int     -- '(), '(,), ...
| PromotedNilT           -- '[]
| PromotedConsT          -- ':
```


Client code could create full tuples and lists using a combination of the above constructors with a liberal sprinkling of `AppT`s.


- Pros: Matches syntax of existing `TupleT` and `ListT`. For lists, matches forms available in surface syntax.
- Cons: Believed to be harder to use in practice. The `PromotedTupleT` construct here is not available in surface syntax. This also loses the ability to write succinct lists, while the original format proposed above allows for nil as a 0-element list.


It may be worth noting that `'(,) Int Bool` is *not* a synonym for `'(Int,Bool)`. `'(,) Int Bool` is a parse error.


### Structured Kinds



Instead of the new `ListK` and `TupleK` kinds above, we could have the following:


```wiki
| ListK Kind     -- [k]
| TupleK [Kind]  -- (k1,k2,...)
```

- Pros: Perhaps easier to use. Mirrors surface syntax.
- Cons: Does not match internal GHC representation, including what is printed in error messages and such. Different from the way `Type` works.


It may be worth noting that, in the kind language, `(,) Int Bool` is *not* a synonym for `(Int,Bool)`. `(,) Int Bool` is a parse error.


