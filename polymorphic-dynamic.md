# Polymorphic Dynamics



Haskell already provides dynamic types with `Data.Dynamic` and `Data.Typeable` but this interface limits functions to being monomorphic.  This page is to collect notes on implementing polymorphic dynamic types.


## Motivation



There are instances when an object that is not fully known at the time of compiling asks to be linked into a running program.  This is currently possible for monomorphic types:


```wiki
data Dynamic = Dynamic TypeRep Obj   -- Obj is like HValue
                                     -- GHC is inconsistent

toDyn       :: Typeable a => a -> Dynamic
fromDynamic :: Typeable a => Dynamic -> Maybe a
dynApply    :: Dynamic -> Dynamic -> Maybe Dynamic
```


Having the ability to load polymorphic (both ad-hoc and parametric) functions after the program has been executed would provide a more general mechanism.


## Use cases



I have a natural language program that uses type inference to drive a chart parser.  Each word has a type and a semantic function.  The semantic functions are actually Haskell monadic actions that may or may not be polymorphic, depending upon the particular word.  So as to be able to increase the vocabulary while running, the word definitions are looked up in a dictionary and dynamically linked at run-time.  I found cases, such as with the word `and` in which the monadic action **must** be polymorphic.  This extension would solve that problem.



The Clean manual gives as motivation: "type-safe storage, retrieval, and communication of arbitrary expressions between (independent) applications."  This includes mobile code and serialised data, the type of which is not known until the data is loaded.


## Interface to user (programmer)



Because of the introduction of polymorphism, all this work would have to be done within a particular type environment, elegantly implemented by using a monad that encapsulates the type environment.



Otherwise, the interface is very similar to that above.


```wiki
data Dynamic = Dynamic Type HValue

toDynamic   :: a -> Dyn Dynamic
fromDynamic :: Dynamic -> Dyn (Maybe a)
dynApply    :: Dynamic -> Dynamic -> Dyn (Maybe Dynamic)

dynamicLoad :: [Dflags] -- ^ compilation flags
            -> [String] -- ^ names to dynamically load
            -> String   -- ^ module
            -> Dyn [Dynamic]
```


Clean allows pattern-matching at the type-level against Dynamic types:


```wiki
transform :: Dynamic -> [Int]
transform (0 :: Int)            = []
transform (n :: Int)            = [n]
transform (f :: [Int] -> [Int]) = f [1..100]
transform _                     = []
```


(From the Clean 1.0 Reference Manual)          



Here [Runtime Data Loading](run-time-data) is a use case that would be solved by this feature.


## Design



The Dyn monad is a newtype wrapper around the GHC monad.



Currently, using the GHC API, I am able to dynamically load/compile constants and functions, including parametrically polymorphic functions, and view the result of application.



The difficulty lies with ad-hoc polymorphism, because of the requirement of a dictionary for successful linking at run-time.



A function such as `sort :: Ord a => [a] -> [a]` in a module in which it is defined is incompletely specified because the specific type of the argument is not known at the export level.



During type-checking, normally before run-time, a specific argument type will be given and so the compiler can figure which dictionary to use for the class methods.  By running the full type-checker at run time it should be possible to use the unification substitutions to determine which dictionary to use and then to dynamically link the class methods so that the function can be applied.


- `Typeable` instances are created at the programmer level.  Since this new feature needs to make use of the type-checker at run time, it seems reasonable to instead use (opaquely) the datatype for types that is used within the compiler. A primitive

  ```wiki
  typeOf :: a -> Type
  ```


can be applied to an expression to yield its type, the type can then be wrapped in a Dynamic with the value of the expression.


- When injecting a `Dynamic` back into statically type-checked code, the wrapped type must be used to ensure that the `Dynamic` is of the correct type.

- In order to facilitate branching on the type of a `Dynamic`, including pattern matching in function specifications, a new keyword `typecase` could be added:

  ```wiki
  myList :: Dynamic -> [Int]
  myList d = typecase d of
               n :: Int   -> [n]
               l :: [Int] -> l
               _            -   > error "Could not coerce dynamic value"
  ```


Where `typecase` attempts to coerce to the provided type and removes the `Just` around the value.


