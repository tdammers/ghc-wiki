
A Proposal for Restricted Template Haskell



Just so we aren't being so negative, we can call this Macro Template Haskell.


## Problem



The major complaints of Template Haskell are that


- it is difficult to reason about what is going on (reasoning)
- it adds complexity to the build process (building)

### Reasoning



The biggest problem for reasoning is actually a lack of visibility into the generated code. There are changes in 7.10 and 7.12 that improve this situation.



But even if one could easily view the generated code, it would be nice to have a better understanding of the Template Haskell splicing function that creates it without even delving into the source code for the function.
We have a notion of purity in Haskell. When we see a pure function, we already know a lot about it.
But we do not carry this notion into Template Haskell. Any Template Haskell splice can operate in the Q monad, which has access to `runIO`.
So we are violating referential transparency. To make matters worse, what IO means at compile time is not well-defined (for example, cross-compilation, although there is a proposal to address that).



Because TH can run arbitrary code and [
break module boundaries](https://github.com/dterei/SafeHaskellExamples/tree/master/thReify), that also makes it impossible for [SafeHaskell](safe-haskell) to deal with.


### Building



Template Haskell can make for a complex extra build phase. Recent changes to the linker have improved the situation greatly, but at a minimum it incurs a lot of overhead.


## Proposal



We can define different levels of restriction


- level 1: no runIO
- level 2: no Q Monad (I think we still need newName though). no reify, because that gets into looking outside the module making compilation more difficult and subverting [SafeHaskell](safe-haskell)
- level 3: no mkName (we could call this Pure Template Haskell)


Rather than levels, we may be able to have more flexibility by thinking in terms of [
capabilities](https://github.com/Icelandjack/Capabilities/blob/master/src/Capabilities.hs)


```wiki
reify :: Name -> Restrict (TH :+: Reify) Info
runIO :: IO a -> Restrict (TH :+: RunIO) a
```

### Module-based Resrictions for library authors



There could be separate modules defined for the various restriction levels


```wiki
module Language.Haskell.TH.Safe (
  module Language.Haskell.TH,
  reifyWithoutNameG,
 )  where
import Language.Haskell.TH hiding (runIO, reify*)
```

### Syntax at the call site



We may be out of dollar signs to use for this. Suggestions are very welcome!



My general feeling is that invocation syntax is not very important because it is not done a lot. I would rather have something cumbersome to use that doesn't take away precious syntax or names. Then if we figure out something is popular we could look for more convenient syntax.



So we could use something pretty ugly for now such as: `$pure$`, `$safe$`, and `$noIO$`.


