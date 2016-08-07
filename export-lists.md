
This page gives a specification for how users should expect export lists to behave.



Specifically, we concentrate on the very complex case of type constructors exported with data constructors.


# Syntax



We are concerned with exports of the form.


```wiki
T(a_1, a_2, ..., a_n)
```


where `a_1` to `a_n` are a mixture of


1. Data Constructors
1. Record Selectors
1. Class methods
1. Type Constructors (in the case of associated types)
1. Pattern Synonyms
1. Pattern Synonym Record Selectors
1. Wildcards


We then define two cases for each category. 


1. When it is valid to include in the export list.
1. Which names are exported as a result of the inclusion.


We say that `a_1` can be \*exported with\* `T` in the case that the export is 1-valid.



Definition: 


# Semantics


1. A data constructor `D` can be exported with `T` just when `T` is the parent of `D`.
1. A record selector `f` can be exported with `T` just when `T` is the parent of `D`.
1. A type class method `foo` can be exported with `T` just when `T` is a type class which defines `foo`.
1. A type constructor `S` can be exported with `T` just when `S` is an associated type defined in the type class `T`.
1. A pattern synonym `P` can be exported with `T` in the case that either

  1. The head of the type of the scrutinee is a definite type constructor (not a type variable) `S` and `T = S`.
  1. The head of the type of the scrutinee is a type variable
1. A pattern synonym selector `p` belonging to a pattern synonym `P` can be exported with `T` in the case that `P` can be exported with `T`.
1. A wildcard can always appear in the export list. 


1-5. The thing itself is exported from the module.


1. All children of `T` are exported from the module.
