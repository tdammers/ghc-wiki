## Design of Kind Polymorphism



Implicit foralls are added by the renamer. Kind generalization (adding more kind foralls) is done by the type checker.


### In types



Rules:


- When there is an explicit `forall`, we don't add any foralls, which means:

  - no implicit foralls even for kind variables
  - no kind generalization, which means defaulting flexi meta kind variables to `*`
- A kind variable mentioned explicitly in a type must always be bound by an explicit `forall`, unlike type variables for which Haskell adds an implicit `forall`.
- When there is no explicit `forall`, we add an implicit forall for not-in-scope *type* variables in the renamer, and we kind generalize in the type checker.
- In the context of a function signature, an explicit forall binds its variables (type or kind) in the function equations (as it is currently the case for type variables).

```wiki
-- For the following examples, no type or kind variables are in scope

f1 :: f a -> Int                       -- forall k (f :: k -> *) (a :: k). f a -> Int

f2 :: f (a :: k) -> Int                -- Not in scope: kind variable `k'

f3 :: forall a. f a -> Int             -- Not in scope: type variable `f'

f4 :: forall f a. f a -> Int           -- forall (f :: * -> *) (a :: *). f a -> Int

f5 :: forall f (a :: k). f a -> Int    -- Not in scope: kind variable `k'
                                       -- We are a little unsure about this. Mabye we
                                       -- should kind-generalise

f6 :: forall k f a. f a -> Int         -- Warning: Unused quantified type variable `k'
                                       -- forall k (f :: * -> *) (a :: *). f a -> Int

f7 :: forall k f (a :: k). f a -> Int          -- forall k (f :: k -> *) (a :: k). f a -> Int

f8 :: forall k. forall f (a :: k). f a -> Int  -- forall k (f :: k -> *) (a :: k). f a -> Int


g1 :: (f a -> Int) -> Int                      -- forall (f :: k -> *) (a :: k). (f a -> Int) -> Int

g2 :: (forall f a. f a -> Int) -> Int          -- forall k. (forall (f :: k -> *) (a :: k). f a -> Int) -> Int

g3 :: (forall a. f a -> Int) -> Int            -- forall k (f :: k -> *). (forall (a :: k). f a -> Int) -> Int
 
g4 :: forall f. (forall a. f a -> Int) -> Int  -- forall (f :: * -> *). (forall (a :: *). f a -> Int) -> Int
 
g5 :: (forall f (a :: k). f a -> Int) -> Int   -- Not in scope: kind variable `k'


h :: forall k f (a :: k). f a -> Int
h x = ...k...f a...  -- k, f, and a are in scope
```

### In kinds



Rules:


- Any explicit kind variables are generalized over
- We kind generalize flexi meta kind variables when possible


We need a notion of large scoping, which means that a variable in the signature can bind in the where clause. Only type classes have large scoping.


#### Type classes



Additionnal rules:


- Large scoping: any type or kind variables appearing on left or right side of a `::` in the signature is brought into scope

```wiki
class C1 (f :: k1 -> *) (g :: k2 -> k1) where  -- forall k1 k2. (k1 -> *) -> (k2 -> k1) -> Constraint
  foo1 :: f (g a) -> (f :: k1 -> *) b          -- forall (a :: k2) (b :: k1). f (g a) -> f b
          -- Note that k1 and k2 scope over the type signatures
          -- just as f and g do.

class C2 f g where                             -- Same as C1
  foo2 :: f (g a) -> f b                       -- Same as foo1

class C3 (f :: k1 -> *) g where                -- Same as C1
  foo3 :: f (g a) -> f b                       -- Same as foo1
```

#### Data types


```wiki
data T1 s as where                          -- forall k. (k -> *) -> [k] -> *
  Foo1 :: s a -> T1 s as -> T1 s (a ': as)  -- forall k (s :: k -> *) (a :: k) (as :: [k]). 
                                            --   s a -> T1 k s as -> T1 k s ((':) k a as)
    -- Note that s,a do not scope over the declaration of Foo1

data T2 s (as :: [k]) where                 -- Same as T1
  Foo2 : s a -> T1 s as -> T1 s (a ': as)   -- Same as Foo1
    -- Note that s,as,k do not scope over the declaration of Foo2

data T3 s (as :: [*]) where                 -- (* -> *) -> [*] -> *
  Foo3 : s a -> T1 s as -> T1 s (a ': as)   -- forall (s :: * -> *) (a :: *) (as :: [*]).
                                            --   s a -> T1 s as -> T1 s ((':) * a as)
    -- Note that s,as do not scope over the declaration of Foo2
```

#### Type families



Last rule becomes: Any missing kind signature defaults to `*`


```wiki
-- Note (nothing to do with kind polymorphism) that we might want to say that only the Bool is an index.
type family F1 (b :: Bool) (true :: k) (false :: k) :: k  -- F1 :: forall k. Bool -> k -> k -> k

type family F2 b true false                               -- F2 :: * -> * -> * -> *

type family F3 b (true :: k) false                        -- F3 :: forall k. * -> k -> * -> *
```


For type families it just doesn't make any sense to not write the whole kind signature, since there is no inference at all.


#### Type synonyms


```wiki
type S1 f g                             -- forall k1 k2. (k1 -> *) -> (k2 -> k1) -> *
  = forall a. f (g a)                   -- = forall (a :: k2). f (g a)

type S2 (f :: k1 -> *) g                -- Same as S1
  = forall a. f (g a)                   -- Same as S1

type S3 (f :: k1 -> *) (g :: k2 -> k1)  -- Same as S1
  = forall a. f (g a)                   -- Same as S1

type S4 f (g :: * -> *)                 -- (* -> *) -> (* -> *) -> *
  = forall a. f (g a)                   -- = forall (a :: *). f (g a)
```