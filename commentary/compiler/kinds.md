
TODO This page is somewhat old. It needs to be updated with recent information. The followings are the latest related information.



GHC User's Guide:


- [
  Levity polymorphism](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#levity-polymorphism)
- [
  The kind \*](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#the-kind)


GHC Proposals:


- [
  Revise Levity Polymorphism](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0003-levity-polymorphism.rst)
- [
  Embrace Type :: Type](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst)
- [
  Remove the \* kind syntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0030-remove-star-kind.rst)


Papers:


- [
  System FC with Explicit Kind Equality](https://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf)
- [
  Levity Polymorphism (extended version)](https://cs.brynmawr.edu/~rae/papers/2017/levity/levity.pdf)

# Kinds



Kinds classify types.  So for example:


```wiki
   Int :: *
   Int -> Int :: *
   Maybe :: * -> *
   Int# :: #
   (# Int, Int #) :: #
```


The base kinds are these:


- "`*`" is the kind of lifted, boxed values. Things like `Int` and `Maybe Float` have kind `*`.
- "`#`" is the kind of unlifted values. Things like `Int#` have kind `#`.
- With the advent of [data type promotion and kind polymorphism](ghc-kinds) we can have a lot more kinds.
- Kinds are in flux with levity polymorphism. See [LevityPolymorphism](levity-polymorphism). See also [TypeType](commentary/compiler/type-type).


(Unboxed tuples used to have a distinct kind, but in 2012 we combined unboxed tuples with other unboxed values in a single kind "`#`".)


## Representing kinds



Kinds are represented by the data type `Type` (see [Commentary/Compiler/TypeType](commentary/compiler/type-type)):


```wiki
type Kind = Type
```


Basic kinds are 
represented using type constructors, e.g. the kind `*` is represented as


```wiki
liftedTypeKind :: Kind
liftedTypeKind = TyConApp liftedTypeKindTyCon []
```


where `liftedTypeKindTyCon` is a built-in `PrimTyCon`.  The arrow type
constructor is used as the arrow kind constructor, e.g. the kind `* -> *` 
is represented internally as


```wiki
FunTy liftedTypeKind liftedTypeKind
```


It's easy to extract the kind of a type, or the sort of a kind:


```wiki
typeKind :: Type -> Kind
```


The "sort" of a kind is always one of the
sorts: `TY` (for kinds that classify normal types) or `CO` (for kinds that
classify coercion evidence).  The coercion kind, `T1 :=: T2`, is
represented by `PredTy (EqPred T1 T2)`.


## Kind subtyping



There is a small amount of sub-typing in kinds.  Suppose you see `(t1 -> t2)`.  What kind must `t1` and `t2` have?  It could be `*` or `#`.  So we have a single kind `OpenKind`, which is a super-kind of both, with this simple lattice:



[](https://docs.google.com/drawings/pub?id=1M5yBP8iAWTgqdI3oG1UNnYihVlipnvvk2vLInAFxtNM&w=359&h=229)



(You can edit this picture [
here](https://docs.google.com/drawings/d/1M5yBP8iAWTgqdI3oG1UNnYihVlipnvvk2vLInAFxtNM/edit?hl=en_GB).)


