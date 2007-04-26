


# Report of the implementation of the GHCi debugger



UPDATE: We have prepared a higher level technical report about this work, available [
here](http://www.dsic.upv.es/mapa/ingles/informesinv.pl?any=2007).



During the Summer of 2006 I have been working on this project sponsorized by the[
Google SoC](http://code.google.com/soc) initiative. My mentors were Simon Marlow and David Himmelstrup (lemmih).



It has been a lot of fun, and I've learnt a huge amount of things, but the reader must be warned that I am still a beginner in many aspects, and that my knowledge of ghc is very shallow. So please take my words with a bit of perspective.



The contributions of the project have been mainly two:


- A closure viewer, capable of showing intermediate computations without forcing them, and without depending on types (and of course that excludes dependency on Show instances)
- To put the basic `breakpoint` primitive to use in a system of dynamic breakpoints for ghci.

# The closure viewer



The closure viewer functionality is provided by the following function in the GHC API:


```wiki
obtainTerm :: Session -> Bool -> Id -> IO (Maybe Term)
```


The term datatype is defined at  [compiler/ghci/RtClosureInspect.hs](/trac/ghc/browser/ghc/compiler/ghci/RtClosureInspect.hs). This datatype represents a partially evaluated Haskell value as an annotated tree:


```wiki
data Term = Term { ty        :: Type 
                 , dc        :: DataCon 
                 , val       :: HValue 
                 , subTerms  :: [Term] }

          | Prim { ty        :: Type
                 , value     :: String }

          | Suspension { ctype    :: ClosureType
                       , mb_ty    :: Maybe Type
                       , val      :: HValue
                       , bound_to :: Maybe Name   -- Does not belong here, but useful for printing
                       }
```


A few other comments on this module:


- It is not meant to be included in the stage1 compiler 
- It is imported by GHC, so in order to avoid introducing more cyclical dependencies I've tried to keep all `Session` related stuff in the GHC module.

## Implementation details



Quoting from Simon Marlow in the ghc-cvs list:


>
>
> (..)being in GHCi, we have all the compiler's information about the code to hand - including full definitions of data types.  So for a given constructor application in the heap, we can print a source-code representation of it
>
>

### DataCon recovery



UPDATE: This is now done differently. See ticket [\#1085](http://gitlabghc.nibbler/ghc/ghc/issues/1085)



(If for some reason you want to check the original solution, browse the history for this wiki page.)


### Recovering non-pointers



This happens at `RtClosureInspect.extractUnboxed` and is a bit weak, it might potentially break in some architectures.


### Compensating Wrapper Constructors



Worker and Wrapper constructors are a potential headache for two reasons, extra arguments and variations on the final type. 



The arguments list gets extended with:


- Existential Dictionaries (?)
- Type Class dictionaries
- any other ?


To compensate it suffices to drop the first (m - n) (pointed) pointers of a closure, where:


- n - \# arguments of the original constructor
- m - \# arguments of the wrapper constructor, if any, or worker constructor


In addition, the types of the arguments may change, so the closure viewer always consider the final types, not the original ones, since the closure viewer deals with the heap representation of values.


### Type Reconstruction



Type reconstruction is the process of recovering the full type of a closure which has a polymorphic dataCon. 



The problem is approached as a simplified case of type inference. The set of constraints come from the typechecker and from the concrete types of the children of the closure, if they are evaluated. Constraints are are generated and unified with the previous set, ultimately generating the desired substitution.



The only tricky issue is that newtypes are gone in the heap representation, so one needs to consider additional equations when doing unification, very similar to the coercions (:=:) of System Fc. For instance, the newtype:


```wiki
newtype MkT a = MkT [a]
```


induces an equivalence class:


```wiki
MkT a :=: [a] 
```


This can be quite tricky to solve if done in full generality, as it amounts to unification modulo a set of equations which is known to be undecidible. The closure viewer uses a set of typing rules to get around this. The details are in the paper. 



The current implementation follows the paper in everything except one thing. When doing type reconstruction for large structures, things can get very slow. For instance, :print'ing a list with 50.000 integers (which does type reconstruction) means doing around 50.000 unification steps, which takes around 4 minutes in my box. Of course, of those 50.000 we only need two. Thus the algorithm has been modified to use matching when unification is not needed. When is that ? When a type is monomorphic. This brings down the time to a few seconds, since the list structure still needs to be walked in order to construct the term. (could this be done more lazily? anyway, it is not really a problem in practice).


### Handling suspensions



The interactive ui uses `obtainTerm` from the ghc-api, which lives at  [compiler/ghci/RtClosureInspect.hs](/trac/ghc/browser/ghc/compiler/ghci/RtClosureInspect.hs), to implement the :print and :sprint command. The difference is that :print, additionally, binds suspended values.
Thus, suspensions inside semievaluated terms are bound by `:print` to \_t<sub>xx</sub> names in the interactive environment, available for the user. 
Most of this happens at `pprintClosureCommand` in  [compiler/ghci/Debugger.hs](/trac/ghc/browser/ghc/compiler/ghci/Debugger.hs).



Quirks with the current solution:


- It does not remember previous bindings. Two consecutive uses of `:print` will generate two separate bindings for the same thing, generating redundancy and potential confusion.

### Type Refinement



Often it is not possible to reconstruct the most concrete type, because children of a value are suspended. When this happens we have a value with polymorphic type, and this is a problem for two reasons. First, for the user, who cannot interact with the value in the usual way. Second, for ghci, because it does not deal well with tyvars in the type of values.



Incompletely recovered types are instantiated with "Skolem" tyvars (ask the Simons) and GHC has been modified so that they will not be affected by Haskell 98 defaulting rules. This is in order to stop them from defaulting to ()  in GHCi and causing weird things (or eventually segfaults). 



Type refinement happens when, after some forcing of closures by the user, the type reconstruction scheme is able to compute a more concrete type. In this case, the :print command updates the type of the binding in the environment with the new information. Not only that, it updates all the types in the environment with the computed substitution.



The ultimate effect of this solution is that, if there are two or more bindings in the environment whose types are related, the substitution computed after some type reconstruction step will be applied to all of them. For instance, if we are debugging a `map` function:


```wiki
map :: (a -> b) -> [a]->[b]
map f x = ...
```


recovering the type of `x` (how? after forcing and using :print, see the next section example) 
will cause the type of `f` to be updated in its first argument too.



This is a summary of how things go. The user invokes :print on some binding and `pprintClosureCommand` does: 


1. use `obtainTerm` to construct the term. This computes the most concrete possible type.
1. unify the old and new types to compute a substitution.
1. instantiate the range of the substitution with skolem tyvars
1. apply the substitution to all the types in the environment, including the old type


One more detail, newtypes need to be flattened before doing the unification step; 
type reconstruction may not have bee able to recover the newtype representation. 


#### Example



This is an example of type reconstruction/refinement 
in an (allegedly extreme) debugging session snippet:


```wiki
TODO: Update this example

Local bindings in scope:
  r :: a

Core.hs:335:35-49> :t r
r :: GHC.Base.Unknown

Core.hs:335:35-49> :p r
r = (_t1::a)

Core.hs:335:35-49> seq _t1 ()
()

Core.hs:335:35-49> :p r
r = :-> (_t2::a) (_t3::a)

Core.hs:335:35-49> :t r
r :: RuleG GHC.Base.Unknown

Core.hs:335:35-49> seq _t2 ()
()

Core.hs:335:35-49> :p r
r = :-> S (_t4::b (GT_ a b c))  (_t5::GT_ a b c)

Core.hs:335:35-49> :t r
r :: RuleG (GT_ GHC.Base.Unknown
                     GHC.Base.Unknown1
                     GHC.Base.Unknown)

Core.hs:335:35-49> seq _t4 ()
()

Core.hs:335:35-49> :p r
r = :-> (S T "+" [(_t6::GT_ a TermST b), GenVar 1]) (_t7::GT_ a TermST b)

Core.hs:335:35-49> :t r
r :: RuleG (GT_ GHC.Base.Unknown TermST GHC.Base.Unknown)
```


Note how the type of the binding `r` gets updated during the debugging session.


### Pretty printing of terms



We want to customize the printing of some stuff, such as Integers, Floats, Doubles, Lists, Tuples, Arrays, and so on.
At the  [compiler/ghci/RtClosureInspect.hs](/trac/ghc/browser/ghc/compiler/ghci/RtClosureInspect.hs) module there is some infrastructure to build a custom printer, with a basic custom printer that covers the enumerated types.



In  [compiler/ghci/Debugger.hs](/trac/ghc/browser/ghc/compiler/ghci/Debugger.hs) the function `pprintClosure` takes advantage of this and makes use of a custom printer that uses Show instances if available.


# Breakpoints



UPDATE: This is now done differently. See [NewGhciDebugger](new-ghci-debugger)



(If for some reason you want to check the original solution, browse the history for this wiki page.)


# Pending work



Tests, tests, tests. Specially exotic features including rank-2 types and GADTs.


# General Comments


## Maintaining the debugger



The **closure viewer** is a delicate piece of code, as it depends on:


- The Type System of GHC Haskell. Changes to the typechecker interface or data structures will most likely kill it.
- The runtime representation, which may vary between versions but also architectures


An extensive suite of tests should be needed to easily detect when it lags back changes in GHC. Fortunately the code itself isn't too long nor spread.


