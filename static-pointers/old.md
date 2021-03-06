# Design notes for static pointers



These notes discuss the design of the language extension for static pointers as proposed in [\[1](static-pointers/old#)\] (called “static values” there). This language extension is useful for remoting computations to a distant machine. This wiki page documents the extension and its implementation, but first starts with how its meant tobe used with the help of some userland libraries (the “Cloud Haskell” section).



The corresponding Trac ticket to track progress is [\#7015](http://gitlabghc.nibbler/ghc/ghc/issues/7015).



See also [Simon PJ's long blog post](/trac/ghc/blog/simonpj/StaticPointers).


## Cloud Haskell



The `distributed-process` package [\[2](static-pointers/old#)\] implements a framework for distributed computing. This in general requires sending computations remotely, i.e. sending closures. The `distributed-static` package [\[3](static-pointers/old#)\] provides the facilities to represent closures in a way that can be shared remotely, under certain limitations. These facilities include means to represent closures, as well as combinators to build new closure representations from existing ones.



A closure is a code pointer paired with an environment. The -XStaticPointers language extension offers first class support in the compiler for creating a portable representation of the pointer part of the closure. In order to retain compatibility with previous representations of code pointers used in `distributed-static` (which have the advantage of not requiring a compiler extension), work has been put into generalizing `distributed-static` \[ [4](static-pointers/old#), [5](static-pointers/old#) \] in order to make the combinators defined there generic in the pointer representation. 


### `Closure a` values



As defined in `distributed-static`, a Closure can be a pointer to a function paired with an environment, in serialized form:


```wiki
data Closure a = Closure (Static (ByteString -> a)) ByteString
  deriving Typeable

instance Binary (Closure a) where ...
```


The `Static a` type is a representation for pointers that we will discuss later. Closures can be unwrapped with a function `unclosure` which, for the sake of this discussion, can be given the following type:


```wiki
unclosure :: Closure a -> IO (Maybe a)
```


This function will return `Nothing` in case the closure cannot be resolved for whatever implementation-dependent reasons.



A closure can be sent over the network using one of a few primitives provided by the package `distributed-process`.


```wiki
spawn :: NodeId -> Closure (Process ()) -> Process ()
send :: Serializable a => ProcessId -> a -> Process ()
sendChan :: Serializable a => SendPort a -> a -> Process ()
```


In its current implementation, all these primitives send to the peer the serialized closure plus a fingerprint of its type representation. When receiving the message, the peer will use the fingerprint to verify that the type of the closure is the expected one. This doesn't protect the program against malicious code attempting to crash the application, but it does protect the programmer from coding mistakes that could lead to crashes.


### `Static a` values



To a first approximation, a value of type `Static a` is a “label” (one could also say “reference”, “pointer”, or “name”) for a value of type `a`. Labels can be serialized, but the value they refer to need not be. Thus, a value of type `Static a` can be communicated between processes on disparate machines and dereferenced in any of them.



The dereferencing operation is called `unstatic`. It can be given the type:


```wiki
unstatic :: Static a -> IO (Maybe a)
```


The function `unstatic` either succeeds in dereferencing its argument or it returns `Nothing`. The idea is that if `unstatic` fails to find the actual value referred to, then it returns `Nothing`. 
The notion of “label” in `distributed-static` is flexible: one can construct new labels as the composition of two existing labels. The main composition operator is:


```wiki
staticApply :: Static (a -> b) -> Static a -> Static b
```


Therefore, `Static` is not in fact the datatype of labels, but given some label type `l` one can lift a label to a `Static` and compose the result:


```wiki
data Static l a where
  StaticLabel :: l -> Static l a
  StaticApply :: Static l (a -> b) -> Static l a -> Static l b

staticApply = StaticApply
```


Making `Static` parametric in the label type is necessary to keep compatibility with existing representations of code pointers. Better, it allows for arbitrary new notions of labels, such as using a per-application closed datatype representing all remotable functions (think \*defunctionalization\*). Thus, all types and functions using `Static` will also have to be parameterized by the label type, but this is a detail which does not further impact the discussion.



Note: `Static` is \*not\* the free applicative functor over labels, because only labels can be lifted into a `Static`, not arbitrary types. That is, `StaticLabel` is not the unit of a free applicative functor.


## The `StaticPointers` language extension



With `-XStaticPointers` enabled, any expression of the form `static e` where `e :: a` produces a type-tagged compiler-generated label of type `StaticPtr a`. We have


```wiki
newtype StaticPtr a = StaticPtr GlobalName
```


so we can define a lifting function for such labels:


```wiki
staticLabel :: StaticPtr a -> Static GlobalName a
```


Like `Static GlobalName a`, a reference of type `StaticPtr a` points to a value of type `a`. It also can be serialized and dereferenced in other processes with a function `deRef :: StaticPtr a -> IO (Maybe a)`. The main difference with a `Static GlobalName a` value is that a `StaticPtr a` is a type tagged label, whereas `Static a` is the type of label compositions.



Other label types are not type tagged. One might ask, why lift values of type `StaticPtr a` rather than `GlobalName` directly? The reason is that it provides better type safety. If all the compiler generates is a label with no type information, then the user needs to invent some type for it when lifting it to a `Static`. It would go something like this:


```wiki
globalNameClosure :: GlobalName -> Closure a

do spawn there (globalNameClosure (static foo) :: Closure (Process ())
```


We would essentially lose any meaningful static guarantees, since the user could easily cast to the wrong type.



Conversely, one may wonder whether it is possible to dispense entirely with the `GlobalName` datatype and instead define only `StaticPtr a`. But then there is no uniform type of compiler generated labels. Types of the form `Static GlobalName a` would have to be rewritten to `Static (StaticPtr ...) a`, but there is no good choice to fill in the ellipsis since a value of type `Static l a` may hold multiple references and each one of a different type.


### Using `-XStaticPointers` to produce `StaticPtr a` values



With `-XStaticPointers`, GHC can generate a `StaticPtr a` for any closed expression `e` of type `a`. This is denoted as `static e`. With a closed expression meaning that the free variables of `e` are only identifiers of top-level bindings. All of the following definitions are permissible:


```wiki
inc :: Int -> Int
inc x = x + 1

ref1 = static 1
ref2 = static inc
ref3 = static (inc 1)
ref4 = static ((\x -> x + 1) (1 :: Int))
ref5 y = static (let x = 1 in x)
```


While the following definitions are rejected:


```wiki
ref6 = let x = 1 in static x      -- the body of static is not closed
ref7 y = static (let x = 1 in y)  -- again the body is not closed
```


With this extension turned on, `static` is no longer a valid identifier.



The information contained in the reference is used by `deRef` to locate the values at runtime using the symbol tables in executables, libraries and object files. For this to work, symbol tables need to be made available at runtime. A simple way to ensure this is to pass the `-rdynamic` flag to GHC during linking.



Note that, because the symbol table does not contain type information about its entries, it is not possible to check that the value returned by `deRef` really is of the expected type. In an adversarial network, this would mean that some adversary could easily crash the local program by sending it a `x :: Static b` where a `Static a` is expected, disguising the `x` as a `Static a` by sending along the `TypeRep` for some `y :: Static a` along with `x` instead of the `TypeRep` for `x`. It should be noted, however, in the context of today’s Cloud Haskell, short of any authentication, there are many other ways for a remote program to abuse the local program anyways.



A `Closure` can be obtained from a `StaticPtr` as in the following example:


```wiki
client server = do
  spawn server $ staticClosure $ staticLabel $ static (say “hello”)
```


where `staticClosure` is a combinator from the package distributed-static:


```wiki
staticClosure :: Typeable a => Static a -> Closure a
```


Or we can internationalize the example:


```wiki
sayI18N translate s = say (translate “hello”)

client :: Static (String -> String) -> NodeId -> Process ProcessId
client staticTranslate server = do
  spawn server $ staticClosure $ 
    staticLabel (static sayI18N) `staticApply` staticTranslate
```

### Static semantics of `StaticPtr a` values



Informally, if we have a closed expression


```wiki
e :: forall a_1 ... a_n. t
```


the static form is of type


```wiki
static e :: forall a_1 ... a_n. StaticPtr t
```


The following definitions are valid:


```wiki
ref8 = static id :: StaticPtr (a -> a)
ref9 = static show :: StaticPtr (Int -> String)
ref10 = static return :: StaticPtr (a -> IO a)
```


Currently, the type of the body of the `static` form is constrained to have an unqualified rank-1 type. The following are therefore illegal:


```wiki
static show                    -- show has a constraint (Show a)
static Control.Monad.ST.runST  -- runST has a higher-ranked type
```


That being said, with the appropriate use of wrapper data types, the
above limitations induce no loss of generality:


```wiki
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StaticPointers #-}

import Control.Monad.ST
import GHC.Ref

data Dict c = c => Dict

g1 :: StaticPtr (Dict (Show a) -> a -> String)
g1 = static (\Dict -> show)

data Rank2Wrapper f = R2W (forall s. f s)
newtype Flip f a s = Flip { unFlip :: f s a }

g2 :: StaticPtr (Rank2Wrapper (Flip ST a) -> a)
g2 = static (\(R2W f) -> runST (unFlip f))
```


It is proposed that the `Dict` wrapper in particular, for reified dictionaries, is reexported by the distributed-static package.


### Implementation



The renamer checks that the free variables appearing in the body of the `static` forms are always identifiers of top-level bindings. This holds for both values and types.



The type-checker treats the static form mostly as if `static` were a function:


```wiki
static :: a -> StaticPtr a
```


At the very end of the type-checking phase of a module, the types of the bodies of all found `static` forms are examined to determine if they are qualified, and if so they are rejected. This needs to be done at the end of type-checking because the monomorphism restriction may affect the inferred type.



The desugarer replaces the `static` form with a `ref :: StaticPtr a` value pointing to the body of it. When the body is a single identifier, the value `ref` points to it. When not, the body is floated to a freshly generated top-level definition and the `ref` value points to it instead. 



An expression like `static return :: a -> IO a` may appear to have a single identifier in the body of `static`, but in the desugarer, it is really `return` applied to the `Monad IO` dictionary. Therefore, it will be floated.



Currently, floating of the bodies of `static` forms is implemented in the type-checker. It has been pointed out that it would be desirable to do it in the desugarer. Another task the type-checker is doing is making sure that referenced identifiers do appear in symbol tables in the produced object files via calls to the function `keepAlive :: Name -> TcM ()`. Probably, this is something that could be done in the desugarer as well.


### The `StaticPtr` datatype



The `StaticPtr` datatype is implemented in the module base:GHC.Ref.


```wiki
-- | A reference to a top-level value of type 'a'.
data StaticPtr a = StaticPtr { unStaticPtr :: GlobalName }
  deriving (Read, Show, Typeable)

-- | Global names identifying top-level values
--
-- > GlobalName package_id installed_package_id module_name value_name
--
data GlobalName = GlobalName String String String String
  deriving (Read, Show, Typeable)
```


A GlobalName holds the information about a top-level value that the desugarer fills in when replacing a `static` form. It augments the information provided by `Language.Haskell.TH.Syntax.Name` with the information in the `installed_package_id` field. This field is [
Cabal:Distribution.Package.InstalledPackageId](http://hackage.haskell.org/package/Cabal-1.20.0.2/docs/Distribution-Package.html#t:InstalledPackageId) and it would be needed to identify the package when multiple variations of it are installed.



The installed package id could be useful to locate the symbol if the dynamic dependencies of a program were not known when linking. However, the field is an empty string for all `static` forms requiring floating or pointing to identifiers in the current package. So its usefulness is very limited and we are considering to remove it.


### In GHCi



The `static` forms can be created in GHCi, but floating of expressions is not implemented there. This means that `deRef` has a chance to work in GHCi only when the body of the `static` form is a single identifier which does not depend on using any type class dictionaries. Any other kind of expression will require producing a new top-level binding and this is not done yet.


## References



\[1\] Jeff Epstein, Andrew P. Black, and Simon Peyton-Jones. Towards Haskell in the cloud. SIGPLAN Not., 46(12):118–129, September 2011. ISSN 0362-1340. [
pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf)



\[2\] [
https://hackage.haskell.org/package/distributed-process](https://hackage.haskell.org/package/distributed-process)



\[3\] [
https://hackage.haskell.org/package/distributed-static](https://hackage.haskell.org/package/distributed-static)



\[4\] [
https://github.com/tweag/distributed-static/commits/globalnames](https://github.com/tweag/distributed-static/commits/globalnames)



\[5\] [
https://github.com/tweag/distributed-process/commits/generic-static4](https://github.com/tweag/distributed-process/commits/generic-static4)


