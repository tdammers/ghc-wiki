## Types-safe Distributed Haskell



This page collects thinking about mechanisms that support type-safe distribution
of Haskell programs, particularly focusing on the issues of serialisation.
The corresponding Trac ticket to track progress is [\#7015](http://gitlabghc.nibbler/ghc/ghc/issues/7015).



Several distinct layers are involved; they constitute the main payload of this design:


- [Typeable](typeable): built-in support for generating and using **run-time type representations**. The `Typeable` class has been part of (GHC) Haskell for many years, but it needs to evolve further.

- [StaticPointers](static-pointers): built-in support for so-called **static pointers**.  These were introduced in the original Cloud Haskell paper (link below), but the design was not fleshed out there.

- [DistributedClosures](distributed-closures): library support for **serialisable closures**.  These closures can be implemented in a library, `distributed-closure` building on the two layers below, but the design of that library is far from trivial.

- [StaticPointers/ImplementationPlan](static-pointers/implementation-plan): a phased approach to implementing the extension in GHC.

- [StaticPointers/TypesafeDecoding](static-pointers/typesafe-decoding): options for encoding and decoding static pointers.


Our goal is to identify the *smallest possible built-in extension to GHC*, with
the smallest possible trusted code base, that would enable
libraries like `distributed-closure` to be written in an entirely type-safe way.



See also this ticket about rationalising the runtime reflection naming structures: [\#10068](http://gitlabghc.nibbler/ghc/ghc/issues/10068).



Much of what is suggested here is implemented, in some form, in two existing projects


- **Cloud Haskell libraries** [
  distributed-static](https://hackage.haskell.org/package/distributed-static) and [
  rank1dynamic](https://hackage.haskell.org/package/rank1dynamic).  Background in the paper [
  Towards Haskell in the Cloud](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/remote.pdf).

- **HdpH libraries** [ hdph](https://hackage.haskell.org/package/hdph) and [
  hdph-closure](https://hackage.haskell.org/package/hdph-closure). Background in the paper [
  Implementing a high-level distributed-memory parallel Haskell in Haskell](http://www.dcs.gla.ac.uk/~pmaier/papers/Maier_Trinder_IFL2011_XT.pdf)


[Simon PJ's long blog post](/trac/ghc/blog/simonpj/StaticPointers) is also relevant, but mainly as background.  It should eventually be fully subsumed by the above pages.



The rest of ths page gives background, to set the scene for the main pages above.


## Introduction



In distributed programming, processes on different nodes exchange data
by asynchronously sending messages to each other. It is useful to go
beyond this model, and allow processes to send other processes to
other nodes, not just first-order data. For instance, an extremely
useful feature of distributed frameworks in Hasell (e.g.
[
distributed-process](https://hackage.haskell.org/package/distributed-process), [
HdpH](https://hackage.haskell.org/package/hdph))
and other languages (Erlang, Scala), is the ability for a process on
one node to *spawn* a process on another node.



For example, consider a simple calculator-as-a-service. It is
a process living on some node B, accepting requests of some type
`ArithRequest`, allowing to express simple arithmetic expressions.
Given a request, the calculator-as-a-service must decode it, interpret
the arithmetic expression, and return the result. But ideally, one
would like a more direct way of performing computations remotely. As
a client, a process on some node A, we would like to be able to do
something like the following instead:


```wiki
client = do
    spawn nodeB $ plus 10 2
    spawn nodeB $ mult (2^10) (3^10)
    spawn nodeB $ neg 1
```


This avoids the need for effectively defining a new DSL, and avoids
the need for an interpreter for this DSL on the other end. Expressing
computations as straight Haskell expressions allows us to reuse GHC's
syntax and type checking at little cost. The above code is similar to
what one would write in a concurrent but single-node setting, using
`forkIO` instead of spawn. Except that the above snippet implies that
`spawn` is able to serialize arbitrary Haskell values (or *closures*).
This is undesirable, because in general closures might capture all
manner of system and local resources (e.g. sockets, locks, file
descriptors) that it makes no sense to send on the wire. We instead
want to limit what can be spawned in this manner to so-called *static
closures*: values expressed using only top-level identifiers,
literals, and *serializable* locally-bound variables.



With this extension, one can write:


```wiki
client = do
    spawn nodeB $ closure $ static (plus 10 2)
    spawn nodeB $ closure $ static (mult (2^10) (3^10))
    spawn nodeB $ closure $ static (neg 1)
```

## Serialisation



I'm going to assume a a type class `Serialisable`, something like this:


```wiki
class (Typeable a, Binary a) => Serialisable a

class Binary a where
  encode :: a -> ByteString
  decode :: ByteString -> Maybe (a, ByteString)
```


(The real class `Binary` uses a `Put` and `Get` monad (with methods `put` and `get`), but I'll use the simpler form above because it introduces all issues that we need here without the clutter of the Real Thing.)
I'll use "encode" and "decode" as synonyms for "serialise" and "deserialise", because the former are easier to pronounce.



Here's an interesting question: are instances of `Binary` part of the TCB?  No, they are not.
Here is a tricky case:


```wiki
  decode (encode [True,False]) :: Maybe (Int, ByteString)
```


Here I have encode a `[Bool]` into a `ByteString`, and then decoded an `Int` from that `ByteString`.  This may
be naughty or undesirable, but it cannot seg-fault: it is type-safe in the sense above.   You can
think of it like this: a decoder is simply a parser for the bits in the `ByteString`, so a decoder
for (say) `Int` can fail to parse a full `Int` (returning `Nothing`), but it can't return a non-`Int`.



For the naughtiness, one could imagine that a Cloud Haskell library
might send fingerprints or `TypeReps` or whatnot to eliminate
potential naughtiness. But even then it is very valuable if the
type-safety of the system does not rely on the CH library.  Type
safety depends only on the correctness of the (small) TCB;
naughtiness-safety might additionally depend on the correctness of the
CH library.


## The `-XStaticPointers` language extension



The proposed `StaticPointers` language extension adds a new syntactic
construct to Haskell expressions:


```wiki
E ::= ... | static E
```


`E` is any Haskell expression, but restricted as follows: it must
contain no free variables (module-level identifiers are ok). For
technical reasons, the body `E` of a static form is further restricted
to be of unqualified type. In other words, `E` is allowed to be of
polymorphic type, but no unresolved type class or equality constraints
of any kind are allowed.



An expression of the form `static E` has type `StaticPtr T` if `E` has
type `T`. Any value of type `StaticPtr T` can be "resolved" to a value
of type `T` by the following new primitive, which can be brought into
scope by importing `GHC.StaticPtr` (so-named by symmetry with
`StablePtr`, `ForeignPtr`, etc):


```wiki
unstatic :: StaticPtr a -> a
```


This is the full extent of the impact on GHC. The above isn't
a standalone solution for remoting arbitrary computations across a set
of nodes, but the remaining support can be implemented in userland
libraries.


## The trusted code base



The implementation `Typeable` class, and its associated functions, in
GHC offers a **type-safe** abstraction, in the classic sense that
"well typed programs won't go wrong".  For example, we in `Data.Typeable` we have


```wiki
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
```


We expect `cast` to be type-safe: if `cast` returns a value `Just x` then we really do know
that `x :: b`.  Let's remind ourselves of class `Typeable`:


```wiki
class Typeable a where
  typeRep :: proxy a -> TypeRep
```


(It's not *quite* this, but close.)  The `proxy a` argument is
just a proxy for *type* argument; its value is never inspected
and you can always pass bottom.



Under the hood, `cast` uses `typeRep` to get the runtime `TypeRep` for
`a` and `b`, and compares them, thus:


```wiki
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x = if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy b)
           then Just (unsafeCoerce x)
           else Nothing
```


Although `cast` is written in Haskell, it uses `unsafeCoerce`.  For it
to truly be type-safe, it must trust the `Typeable` instances.  If the
user could write a `Typeable` instance, they could write a bogus one, and
defeat type safety.  So only GHC is allowed write `Typeable` instances.



In short, `cast` and the `Typeable` instances are part of the **trusted code base**, or **TCB**:


- The TCB should be as small as possible
- The TCB should have a small, well-defined, statically-typed API used by client code
- Client code is un-trusted; if the client code is well-typed, and the TCB is implemented correctly, nothing can go wrong

## The "Dict trick"



We refer occasionally to the "Dict Trick",
well known in Haskell folk lore:


```wiki
data Dict (c :: Constraint) where
  Dict :: forall c. c => Dict c
```


Now a value of, say, type `Dict (Typeable a)` is an ordinary value that embodies a `Typeable a` dictionary.  For example:


```wiki
f :: Dict (Typeable a) -> Dict (Typeable b) -> a -> Maybe b
f Dict Dict val = cast val
```


The pattern-matches against the `Dict` constructor brings the `Typeable` dictionaries
into scope, so they can be used to discharge the constraint arising from the call to `cast`.


