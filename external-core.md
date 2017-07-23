# The `ExternalCore` type



The `ExternalCore` data type is used by GHC to communicate code represented in the [Core](commentary/compiler/core-syn-type) data type with the outside world. It comes with an external syntax, a parser, a pretty printer, and code to convert between Core and External Core. Unfortunately, External Core has not been widely used, and the code has bit-rotted. The recent changes in Core to use [System FC](commentary/compiler/fc) have exacerbated the problem. This page documents the process of getting External Core and Core back in sync.



Once the process is finished, this page will just describe the design.


## Format



The current plan is to use an "extended" version of interface files for External Core, which contains unfoldings for all functions, not just functions GHC has decided to unfold. 


## Reading in External Core



The pipeline looks like:



file -\> GHC parser -\> `IfaceSyn` -\> `tcRnExtCore` -\> `ModGuts` -\> (the rest of the compiler)



This is a change from the current External Core implementation, where `HsSyn` is used to represent types from External Core files and `IfaceSyn` is used for terms. In the new implementation, `IfaceSyn` is used for both.


## Goals and questions


- Well-defined external format with stand-alone tools
- External tools will have to be maintained in order to stay in sync with the interface file format
- How external is "external"? There is a tension between re-using code from GHC, and having a truly independent file format that can be processed with completely stand-alone tools.

  - It's already possible to use the GHC API to generate Core (though not yet to read it back in), which might be enough for some users. On the other hand, the external format allows for writing tools to manipulate Core in languages other than Haskell.
- External format should be readable by humans (though perhaps only after processing it with a pretty-printing tool)
- Not too redundant (for example, only print out type information that is necessary to reconstruct types)
- Don't export information that's internal to GHC (i.e., `IdInfo` fields), since external transformations probably won't preserve it anyway

  - Corollary -- include only just enough information for external tools to be useful
- Does it still make sense to have a separate External Core datatype?
- Primitives have to be documented properly in order to write an stand-alone Core interpreter (which would eventually be desirable.)
- External toolset: typechecker, interpreter (operational semantics), ... 

  - We want to show that External Core is truly "independent", but on the other hand, maintaining these tools is a challenge.
- How likely are major changes to Core in the future?
- Should the external format look like -ddump-simpl output (as it does now), or should it be an easier-to-parse format like s-expressions (perhaps with a pretty-printer to help with debugging)?

## Relevant files



The main source files related to External Core:


- [compiler/coreSyn/ExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/ExternalCore.lhs): The definition of the External Core data type.
- [compiler/coreSyn/MkExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/MkExternalCore.lhs): Some code to convert Core to External Core.
- [compiler/coreSyn/PprExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprExternalCore.lhs): Some code to pretty-print External Core.
- [compiler/parser/LexCore.hs](/trac/ghc/browser/ghc/compiler/parser/LexCore.hs): The lexer for External Core.
- [compiler/parser/ParserCore.y](/trac/ghc/browser/ghc/compiler/parser/ParserCore.y): The parser for External Core.
- [compiler/parser/ParserCoreUtils.hs](/trac/ghc/browser/ghc/compiler/parser/ParserCoreUtils.hs): Some additional utility functions used by `ParserCore.hs`.
- [utils/ext-core/](/trac/ghc/browser/ghc/utils/ext-core/): Old code intended as an executable specification of External Core.


Other files that contain some reference to External Core or are otherwise relevant:


- [compiler/coreSyn/PprCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprCore.lhs): Some code to pretty-print the Core data type.
- [compiler/hsSyn/HsSyn.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.lhs): Top-level syntax tree representations for various things GHC can read, including External Core.
- [compiler/main/DriverPhases.hs](/trac/ghc/browser/ghc/compiler/main/DriverPhases.hs): Includes code to decide how to parse things based on file extension.
- [compiler/main/HscMain.lhs](/trac/ghc/browser/ghc/compiler/main/HscMain.lhs): The main compiler pipeline.

## Documentation


- [
  https://ghc.haskell.org/trac/ghc/wiki/ReadingList\#TypeEqualities](https://ghc.haskell.org/trac/ghc/wiki/ReadingList#TypeEqualities): Description of the System FC language which GHC now uses internally.
- [docs/ext-core/](/trac/ghc/browser/ghc/docs/ext-core/): The current documentation for External Core, which should eventually become a chapter in the [GHC User's Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html).
- [http://www.haskell.org/ghc/docs/latest/html/users\_guide/ext-core.html](http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html): What the User's Guide currently has to say about External Core.

## Design changes


- We probably want to represent all data types as GADTs, even if they can be represented in Haskell 98 form, so that we only have one representation.

## Tasks


- Complete external core type checker.
- Define an external text representation for External Core.
- Write pretty-printer for the new textual format.
- Write a parser for the new textual format.
- Convert the current External Core documentation (in LaTeX) into a chapter (in XML) in the User's Guide.

## Miscellaneous notes


- The LaTeX documentation describes PrimOps in some detail. This information is now in the library documentation, so it is probably not needed in the External Core chapter.
