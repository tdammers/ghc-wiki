# GHC Plugin Annotations


## The Problem



Plugins need to attach information to identifiers:


- When users specify PLUGIN pragmas for particular binders
- If they do their own analysis on the source code which they want to record for later passes


Information to be attached might be:
 


- Names of functions in the module
- Names of types in the module
- Plugin-specific data structures


As the annotations system is orthogonal to plugins, further development of this page will take place at [Annotations](plugins/annotations).


# Out of date speculation below


## Previously Implemented Solution


## Possible Solution 4



This is the one I'm actually going to try and implement. Annotations will look like this:


```wiki
{-# ANN f 1 #-}
f = ...

{-# ANN g Just ("foo", 1337) #-}
g = ...
```


I.e. you annotate an actual identifier with an expression. We impose the further constraint that that expression has the form of an actual literal (including data constructors). In particular, general application is disallowed, so this is not kosher:


```wiki
{-# ANN f id 1 #-}
```


You can introduce compile time compilation by using Template Haskell as usual:


```wiki
{-# ANN f $(id [| 1 |]) #-}
```


We will need another notation to be able to refer to identifiers other than data constructor and function names, something like:


```wiki
{-# ANN type Foo Just 2 #-}
data Foo = Foo
```


We also allow:


```wiki
{-# ANN module "I am an annotation" #-}
```


In your annotation, you may refer to the names of things in the modules being compiled, but not their implementations:


```wiki
data Foo = Foo ...
f = ...
g = ...

-- OK:
{-# ANN f Just ('g, ''Foo) #-}

-- Not OK:
{-# ANN f Just (f, Foo) #-}
```


Notice that this notation does not allow annotating non-top-level names or expressions, and due to its use of an Id -\> Annotation mapping may not survive inlining (this only matters to plugins, not users of annotations on exported stuff). These are annoying limitations but it does fit with our view that the annotations should be exportable and accessible by an Id -\> Annotation mapping by other modules.


## Other Considerations


- We may wish to restrict/change the language features usable within annotations:

  - Data constructors and literals should be allowed
  - Arbitrary function applications are not necessarily a good idea, but \>probably\< are
  - Might want to access (quoted) identifiers in the module being compiled and its imports
- Should plugins be able to see annotations across module boundaries?

  - If so, we need to put them in the .hi
  - It would make sense to provide a GHC API to allow users to reflect on the annotations of an arbitrary imported module (including your own module)
  - Such an expanded annotation system might find use beyond plugins:

```wiki

module MyLibraryProperties where

import QuickCheck(QCProperty(..), runTestsInModule)

{-# PLUGIN prop_f QCProperty #-}
prop_f = \xs -> f xs == f (reverse xs)

{-# PLUGIN prop_g QCProperty { timeout = 1000 } #-}
prop_g = \x -> g (x * 2) == (g x) / 2

-- This line uses the annotations system to find things with QCProperty annotations and runs them
main = runTestsInModule

```

## Possible Solution 1


```wiki

module Main where

{-# PLUGIN NVidia.GHC.GPU NVidiaGPUSettings { useTextureMemoryMb = 256 } #-}

{-# PLUGIN NVidia.GHC.GPU doSomethingExpensive NVidiaGPUFunctionSettings { maxStackDepth = 1024 } #-}
doSomethingExpensive :: Int -> ImpressiveResult
doSomethingExpensive = ....

```


In this module the user has specified one module-level annotation (NVidiaGPUSettings) and one binder-level annotation (NVidiaGPUFunctionSettings). These will be stored in a Map which is given to the plugin code so it can lookup the appropriate annotations.



A problem with this is that identifiers are somewhat unstable in Core. All non-top-level binders are destroyed and created willy-nilly by Core passes. Top-level binders are only stable if they are exported from the module. Even top level binders may vanish from an expression if e.g. we perform inlining.



This can be "solved" by marking all identifiers occuring in PLUGIN pragmas as stable (i.e. exported), and restricting such pragmas to top level identifiers only.



Another possible solution is to turn the pragmas into Notes that live on the source tree itself. However this may suffer from Notes being shuffled around by some Core passes. Additionally, this may be slightly harder to use for plugin authors.


## Possible Solution 2



The previous solution renamed NVidiaGPUSettings etc in an environment that only contained the imported NVidia.GHC.GPU module. This has the advantages that:


- A staging issue is solved as these pragmas are then unable to refer to identifiers in the module being compiled
- The module involved can still be compiled if the NVidia.GHC.GPU module is not available


However:


- This style of "implicit import statement" is inconsistent with Template Haskell which requires an explicit import for code that is run at compile time, even if that import is never used at runtime
- The required pragma is bloated by the fully qualified module name
- We cannot include references to the names of functions in the pragma, e.g using the Template Haskell quoting mechanism:

  ```wiki

  module ServerApplication where

  clientFunction :: (Request, Response) -> IO ()
  clientFunction = ...

  {-# PLUGIN HVolta.TierSplitting serverFunction HV { correspondingClientFunction = 'clientFunction } #-}
  serverFunction :: Request -> IO Response
  serverFunction = ...

  ```


An alternative that makes the imports consistent (and makes the pragma shorter) is:


```wiki

module Main where

import NVidia.GHC.GPU

{-# PLUGIN NVidiaGPUSettings { useTextureMemoryMb = 256 } #-}

{-# PLUGIN doSomethingExpensive NVidiaGPUFunctionSettings { maxStackDepth = 1024 } #-}
doSomethingExpensive :: Int -> ImpressiveResult
doSomethingExpensive = ....

```

## Possible Solution 3



This solution has all annotations being desugared to Notes that live on the Core syntax tree. The reasons for this are as follows:


- It means annotations are basically stable at all points in the compile pipeline: you won't miss one if for example some inlining happens. This is nice and predictable for plugin authors. This is especially troublesome because some things are inlined very eagerly indeed (e.g. non-exported IDs that are referenced only once).
- It means that potentially something like SCCs could be implemented outside of GHC itself, which is very useful for plugins that perform analytics, which I suspect will be an important use case.
- Allows us to attach information to non top-level binders, which I think is essentially impossible if we require a name in the annotation (making non top-level binders stable is HARD).


Disadavantages are that:


- It becomes harder to annotate types and data constructors: a similar effect can be achieved by supplying a module level annotation that references the appropriate object by quoting its name.
- Establishing relationships between objects in an annotation may become less principled if the annotation is forcibly attached to one of the objects.
- Lack of a simple Id -\> Annotation lookup facility may make plugin development harder.
- Lack of a simple Id -\> Annotation correspondence essentially rules out accessing these things externally in future.


The syntax could be used in three ways. Like a traditional annotation:


```wiki

data Stuff deriving Typeable

{-# ANN foo Stuff #-}
foo x = ...

```


The identifier does not necessarily have to be top level as this usage is immediately transformed as follows to ensure stability:


```wiki

data Stuff deriving Typeable

foo = {-# ANN Stuff #-} \x -> ...

```


And this usage may also be typed directly by the user in the expression language as with SCC annotations currently. This is the "true form" of most of the annotations we will use. The only other possibility is module level annotations:


```wiki
{-# ANN Stuff #-}

module Main where

...

```


All of these forms allow arbitrary expressions in "Stuff", which are renamed and typechecked almost as if they had been written inside a splice $() in Template Haskell but instead of having the thing inside the splice return Q Exp we require it returns a Typeable thing. This code will be executed during desugaring of the whole program to generate the actual value we will attach to the Core syntax tree in a note.



This treatment has a few nice effects:


- References to names of non top-level things are disallowed by Template Haskell semantics
- We get quoting of variable, constructor, type and phase names for free and in a uniform manner
- We get free checking that the Stuff does not try to execute something defined in the module being compiled


An interesting alternative might be to have a notion of an "annotation type" similar to the class name used by C\# / Java, and let those types e.g. implicitly add NOINLINE semantics to what they are attached to. This should help stability of the names used in annotations at the cost of reducing optimization opportunities.


