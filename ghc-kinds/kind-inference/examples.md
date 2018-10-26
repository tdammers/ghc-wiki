# Kind inference examples



This page is intended to collect examples of tricky cases for kind inference. Any proposed algorithm should be applied to each of these cases to see how it would behave.



More discussion is at [GhcKinds/KindInference](ghc-kinds/kind-inference).


## Associated types


```
class C1 (a :: k) where
  type F a
```


Question: What should the visibilities on `F` be?



Ryan and Richard think `F :: forall k. k -> Type`. That is, `k` is Specified, because we can always order implicit kind variables using the same ordering that appears in the class header (after kind inference).


```
class C2 (a :: k) (b :: k2) where
  type T a
```


Proposed: `T :: forall k. k -> Type`, with no mention of `b` or `k2`.


```
class C3 (a :: k) (b :: k2) hwere
  type T (z :: k3) a
```


Proposed: `T :: forall k k3. k3 -> k -> Type`. This puts `k` *before* `k3`, because class variables come before other ones (unless the user explicitly puts them later, as has been done with `a`). This rule always works because class variables cannot depend on local ones.


```
class C4 a (b :: a) where
  type T b a
```


This must be rejected, as `b` depends on `a`.


```
class C5 (a :: k) where
  type T (a :: k2)
```


Reject: `k` and `k2` are distinct skolems.


```
class C6 a (b :: a) (c :: Proxy b) where
  type T (x :: Proxy '(a, c))
```


Proposed: `T :: forall (a :: Type) (b :: a) (c :: Proxy b). Proxy '(a, c) -> Type`. Note that `b` is included here as a Specified variable. It could also be an Inferred, if we prefer.


```
class C7 a (b :: a) (c :: Proxy b) where
  type T a c
```


Proposed: `T :: forall (a :: Type) -> forall (b :: a). Proxy b -> Type`. We've inserted `b` between `a` and `c`, but `b` is Specified, not Required. Other possibilities: make `b` Inferred, or reject altogether.


## Datatypes, dependency, and polymorphic recursion



Assume


```
data Prox k (a :: k)
```

```
data Prox2 k a = MkP2 (Prox k a)
```


Question: Do we allow `k` to be dependently quantified, even if this is not lexically apparent from the declaration? This is rejected today.


```
data S2 k (a :: k) b = MkS (S2 k b a)
```


Proposed: `S2 :: forall k -> k -> k -> Type`. Note that `a` and `b` are inferred to have the same kind, as that avoid polymorphic recursion.


```
data S3 k (a :: k) b = MkS (S3 Type b a)
```


Proposed: reject as polymorphically recursive. Yet the idea in [GhcKinds/KindInference\#Simonssuggestion](ghc-kinds/kind-inference#imon's-suggestion) accepts this.


```
data Q2 k a where
  MkQ2 :: Prox k a -> Q2 k a

data Q3 k a where
  MkQ3 :: Q3 k a -> Prox k a -> Q3 k a

data Q4 k a where
  MkQ4 :: Q4 Bool False -> Prox k a -> Q4 k a

data Q5 k a where
  MkQ5 :: Q5 Bool False -> Q5 Nat 3 -> Prox k a -> Q5 k a
```


Agda accepts all of the above. It puts us to shame!


```
data Proxy2 a where
  Mk1 :: Proxy2 (a :: k)
  Mk2 :: Proxy2 (b :: j)
```


This should really be accepted. But it's challenging to arrange this, because `a`, `k`, `b`, and `j` all scope locally within their constructors. How can the kind of `Proxy2` unify with any of them?


```
data T a where
  Mk :: forall k1 k2 (a :: k1) (b :: k2). T b -> T a
```


This is polymorphically recursive. Yet hGhcKinds/KindInference\#SimonsProposedSolution accepts it. (That's what's implemented in GHC 8.6.) Richard thinks we should reject.


```
data T2 a where
  Mk :: forall (a :: k). T2 Maybe -> T2 a
```


This one is rejected, as it should be. So we don't accept *all* polymorphic recursion (how could we?). But we don't have a good specification for what we do accept and what we don't.


```
data T3 a b where
  Mk :: T3 b a -> T3 a b
```


This should be accepted with `T3 :: forall k. k -> k -> Type`; it's not polymorphically recursive. Yet, it would seem any specification which accepted `T` would also give `T3` the polymorphically recursive kind `forall k1 k2. k1 -> k2 -> Type`.


```
data T4 k (a :: k) b = MkT4 (T4 k b a)
```


Here, we have a dependent kind for `T4`. Richard thinks this should be accepted. Proposed rule: dependent variables must be fixed an unchanging at all occurrences within a mutually recursive group (otherwise, we have polymorphic recursion). That is, it would be an error to mention, say, `T4 k2` anywhere in the body of `T4`: it must be `T4 k`.


## Generalization



Contrast


```
class C8 a where
  meth :: Proxy (a :: k)
```


with


```
data V1 a where
  MkV1 :: Proxy (a :: k) -> V1 a
```


Currently (GHC 8.6) we reject `C8` while accepting `V1`. This may be just a bug, but it has to do with the fact that the type of `meth` isn't quantified over `a`, but it is over `k` (lexically).


## Dependency ordering



What if we do something simple? Like just use lexical ordering.


```
data Proxy (a :: k)
```


Then this example fails, with `k` after `a`.



Refinement: consider the RHS of `::` before the LHS.



Then this one fails:


```
data T4 a (b :: k) (x :: SameKind a b)
```


The `k` would end up between the `a` and the `b`, even though `a` depends on `k`.



Also, consider


```
data T5 a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)
```


Here, `b` needs to be between `a` and `x` somewhere. But where? Currently (GHC 8.6), this is rejected because implicitly declared variables come before explicit ones.


## And more



There are many more examples in the testsuite, of course. In particular, see the T14066 tests and the BadTelescope tests.


## Comparison to Idris



Idris accepts `T4` and `T5`, so we think Idris use some kind of ScopedSort.



However, Idris also accepts


```
data T6 :  (a:k) -> (k:Type) -> Type
```


with type `{k : Type} -> (a : k) -> (k2 : Type) -> Type`. We think both this acceptance and the inferred type are odd, as we would expect these two `k`s to be the same `k`. Then `T6` should be rejected because `a` cannot refer to a variable `k` that appears after.



Idris would reject


```
data T7 : (a : k) -> (k:Type) -> (b : k) -> SameKind a b -> Type
```


with a kind mismatch error between `a:k2` and `b:k`. We agree this should be rejected but it should be rejected with another error: `a` cannot refer to `k`.


## Difference between Specified and Inferred



The reason we want to accept `T4` but reject `T6` is because there is a difference between Specified type variable and Inferred type variable. In `T4`, `k` is Inferred; thus when we generalize its type, we can put `k` before `a`. In contrast, in `T6`, `k` is specified, and its order is indicated explicitly by the user, so `a` is not allowed to refer to it.


