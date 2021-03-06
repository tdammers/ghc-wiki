
This page collects ideas about definitions to support type-level (propositional) reasoning in Haskell programs. Much of the initial content comes from the threads "RFC: Singleton equality witnesses" and "Proxy and new-typeable" on the ghc-devs and libraries mailing lists.


## Current Proxy/Typeable Proposal (Apr 24, 2013)



This proposal was sent to ghc-devs and libraries under the subject "Proxy, new Typeable, and type-level equality" on April 3, 2013. The version below has edits that incorporate the feedback from responses to that email.


```wiki
module Data.Type.Equality where

data a :=: b where
 Refl :: a :=: a

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif for 'type-eq'
sym :: (a :=: b) -> (b :=: a)
trans :: (a :=: b) -> (b :=: c) -> (a :=: c)
coerce :: (a :=: b) -> a -> b
liftEq :: (a :=: b) -> (f a :=: f b)
liftEq2 :: (a :=: a') -> (b :=: b') -> (f a b :=: f a' b')
liftEq3 :: ...
liftEq4 :: ...
lower :: (f a :=: f b) -> a :=: b

instance Eq (a :=: b) where ...
instance Show (a :=: b) where ...
instance Read (a :=: a) where ...
instance Ord (a :=: b) where ...
instance Category (:=:) where ...
-- what other instances?

class EqT f where
 eqT :: f a -> f b -> Maybe (a :=: b)

instance EqT ((:=:) a) where ...
```

```wiki
module Data.Proxy where
-- as in Ben Gamari's version [1]
```


\[1\]: [
https://github.com/bgamari/packages-base/blob/proxy/Data/Proxy.hs](https://github.com/bgamari/packages-base/blob/proxy/Data/Proxy.hs)


```wiki
module Data.Typeable ( … , Proxy(..), (:=:)(..) ) where

...
import GHC.TypeReasoning
import {-# SOURCE #-} Data.Proxy

...
eqTypeable       :: (Typeable a, Typeable b) => Maybe    (a :=: b)
decideEqTypeable :: (Typeable a, Typeable b) => Decision (a :=: b)
-- can't use EqT and DecideEqT because Typeable is in Constraint, not *

gcast :: (Typeable a, Typeable b) => c a -> Maybe (c b) -- it is now polykinded

{-# DEPRECATED gcast1 ... #-}
{-# DEPRECATED gcast2 ... #-}
...
```

### Why are all of these in *base*?



This is a great question. The reason the Data.Type.Equality module is in base
is so that it can be used in Data.Typeable for `eqTypeable`. Does
`eqTypeable` *need* to be in *base*? No, I (Richard) don't think so.
If `gcast` is in *base*, `eqTypeable` can be implemented as `gcast Refl`. It seems a little strange to have something like `eqTypeable`
defined somewhere other than Data.Typeable, but there is no technical
restriction here. By moving `eqTypeable` out of Data.Typeable, then
it seems we can also move Data.Type.Equality out, too.



What about Data.Proxy? `Proxy` is re-exported from Data.Typeable, but
it is not used in that module. The idea here is that there should be a
canonical type to pass to `typeRep`, and `Proxy` is that canonical
type.


## Updating TypeLits/singletons



See also [TypeNats](type-nats).



Proposed update to GHC.TypeLits, using the definitions above:


```wiki
instance EqT (Sing :: Nat -> *) where
  eqT = defaultEqT
instance DecideEqT (Sing :: Nat -> *) where
  decideEqT (SNat m) (SNat n)
    | m == n    = Proved (unsafeCoerce Refl)
    | otherwise = Disproved (error "...")

-- similar for Symbol
```


With these definitions, there's an even stronger argument for keeping `(:=:)`
in *base*: these instances would have to be orphans otherwise.



Separate from the introduction of these instances, I would argue that
`unsafeSingNat` and `unsafeSingSymbol` should be moved from
GHC.TypeLits to GHC.TypeLits.Internal, which would be a new exposed module.
These two functions are very unsafe indeed, and their inclusion in
GHC.TypeLits seems to violate a design principle of separating out unsafe
code. (Note that the uses of `unsafeCoerce` in GHC.TypeLits seem benign,
and I think they should stay where they are.)


## Older proposals



Gabor Greif's proposal:


```wiki
class (kparam ~ KindParam, SingE (kparam :: OfKind k)) => SingEquality (kparam :: OfKind k) where
  type SameSing kparam :: k -> k -> *
  type SameSing kparam = (:=:)
  sameSing :: Sing a -> Sing b -> Maybe (SameSing kparam a b)

instance SingEquality (KindParam :: OfKind Nat) where
  sameSing = eqSingNat

instance SingEquality (KindParam :: OfKind Symbol) where
  sameSing = eqSingSym
```


Richard Eisenberg's proposal:


```wiki
class SingE kparam => SingEquality (kparam :: OfKind k) where
  sameSing   :: forall (a :: k) (b :: k). Sing a -> Sing b -> 
  decideSing :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :=: b)
```


I (Richard) think that using `Decision` instead of `Maybe` allows tighter programs to be written, because programmers can escape from impossible situations using `absurd`. If `sameSing` returns only a `Maybe`, then a programmer gets no information usable at the type level when the two singletons do not equal.


## Other thoughts (Richard)


- The *singletons* package contains these definitions:

```wiki
data SingInstance (a :: k) where
  SingInstance :: SingRep a => SingInstance a
class (kparam ~ KindParam) => SingKind (kparam :: OfKind k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a
```


`SingKind (KindParam :: OfKind k)` states that the kind `k` has an associated singleton. I think that's a better superclass for `SingEquality` than just `SingE`.


- I don't love having the functions `unsafeSingNat` and `unsafeSingSymbol` in `GHC.TypeLits`. I envision a future where, some day, a programmer could statically declare that they avoid the partial features of Haskell (along the lines of Safe Haskell, but stricter). Having these functions here means that this module would not be safe. (I am not bothered by various uses of `unsafeCoerce` within the module -- those uses certainly seem safe to me.) Instead, I propose that we move them to `GHC.TypeLits.Unsafe`.

- Perhaps we should move some of what we're discussing out of `GHC.TypeLits`. After all, `(:=:)` does not interact directly with singletons, and neither do some of the definitions I mentioned above. I'm at a bit of a loss for a name, though...

- I wanted the following to be in Data.Type.Equality, but I think I'm the only one, so I've removed these definitions from the original proposal. If you want them, please do shout -- I'd love company!

```wiki
data Void
-- instances as in Edward Kmett's 'void' package

absurd :: Void -> a

type Refuted a = a -> Void
data Decision a = Proved a
                | Disproved (Refuted a)

class EqT f => DecideEqT f where
 decideEqT :: f a -> f b -> Decision (a :=: b)

defaultEqT :: DecideEqT f => f a -> f b -> Maybe (a :=: b) -- for easy writing of EqT instances

instance DecideEqT ((:=:) a) where ...
```

## Other thoughts (Gabor)


- Ultimately we want GHC to derive the `SingEquality` instance (`sameSing`, `decideSing` methods) for any singleton instance. GHC libraries should be laid out in a way that GHC's deriving engine can access all vital parts. IISC, this criterion rules out a completely detached library.

## Course of Implementation



Gabor: I have created a new branch `type-reasoning` and pushed everything I have so far to the `libraries/base` repo. [
Richard's mail](http://www.haskell.org/pipermail/ghc-devs/2013-February/000304.html) summarizes what still needs to be done.



Richard (11 Feb 2013): I've just pushed a commit to the type-reasoning branch with a strawman proposal of a reorganization of these definitions. Specifically, this commit breaks TypeLits into the following five files:


- GHC.TypeEq, which contains the definitions for (:=:), Void, Refuted, etc.
- GHC.Singletons, which contains the definitions about singletons in general, such as SingI and SingEquality
- GHC.TypeLits.Unsafe, which contains just unsafeSingNat and unsafeSingSymbol
- GHC.TypeLits.Internals, which is necessary to get GHC.TypeLits.Unsafe to have access to the right internals; this module is not exported from the 'base' package
- GHC.TypeLits, which contains the definitions specific to type-level literals.


Some thoughts on this design:


- First off, why is TypeEq part of GHC?? Because we wish to write eqSingNat and eqSingSym in GHC.TypeLits, and that module rightly deserves to be part of GHC. I'm quite uncomfortable with this decision, and I even created a new git repo at \[github.com/goldfirere/type-reasoning\] to hold the definitions that eventually ended up in GHC.TypeEq. (The repo has nothing in it, now.) Perhaps the best resolution is to move eqSingNat and eqSingSym out of GHC.TypeLits and into an external package, but that seems silly in a different direction. (It is fully technically feasible, as those functions don't depend on any internals.) I would love some feedback here.
- Why is Singletons broken off? No strong reason here, but it seemed that the singletons-oriented definitions weren't solely related to type-level literals, so it seemed more natural this way.
- Making the Unsafe module was a little more principled, because those functions really are unsafe! They are quite useful, though, and should be available somewhere.
- Currently, the internals of GHC assign types like "0" the kind GHC.TypeLits.Nat, so Nat and Symbol **must** remain in the GHC.TypeLits module. Unfortunately, the plumbing around GHC.TypeLits.Unsafe want Nat and Symbol to be defined in GHC.TypeLits.Internals. So, I created a TypeLits.hs-boot file to fix the problem. This is highly unsatisfactory, and if something like what I've done here sticks around, we should change the internals of GHC to use GHC.TypeLits.Internals.Nat, getting rid of the import cycle.
- I've put in the decideSing function as discussed further up in this thread. Its implementation for Nat and Symbol must use unsafeCoerce, but that shouldn't be a surprise.


Unfortunately, the code doesn't compile now. This is because it needs SingI instances for, say, Sing 0. For a reason I have not explored, these instances are not available here, though they seem to be for code written outside of GHC. Iavor, any thoughts on this?



Please tear any of these ideas (or my whole commit) to shreds! It really is meant to be a strawman proposal, but committing these changes seemed the best way of communicating on possible set of design decisions.


## Background Material



[
Richard's blog](http://typesandkinds.wordpress.com/2012/12/01/decidable-propositional-equality-in-haskell/).



[
Edwin Brady's explanation](http://vimeo.com/61576198#t=59m40s) how this is done in Idris.



[
Gabor's article](http://heisenbug.blogspot.com/2012/12/decidable-equality.html).


