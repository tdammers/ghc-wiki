# GHC Commentary: The Compiler



The compiler itself is written entirely in Haskell, and lives in the many sub-directories of the [compiler](/trac/ghc/browser/ghc/compiler) directory.  


- [Compiler Module Dependencies](module-dependencies) (deals with the arcane mutual recursions among GHC's many data types)
- [Coding guidelines](commentary/coding-style)

- [Command line arguments](commentary/compiler/command-line-args) 
- [The compilation pipeline](commentary/pipeline)

- **Compiling one module: HscMain**

  - [Overview](commentary/compiler/hsc-main) gives the big picture. 
  - Some details of the [parser](commentary/compiler/parser)
  - Some details of the [renamer](commentary/compiler/renamer)
  - Some details of the [typechecker](commentary/compiler/type-checker)
  - Some details of the [simplifier](commentary/compiler/core2-core-pipeline)
  - Some details of the [code generator](commentary/compiler/code-gen) converts STG to Cmm
  - [Backends](commentary/compiler/backends) convert Cmm to native code:

    - [C code generator](commentary/compiler/backends/ppr-c)
    - [Native code generator](commentary/compiler/backends/ncg)
    - [LLVM backend](commentary/compiler/backends/llvm)
    - [GHCi backend](commentary/compiler/backends/gh-ci)
  - A guide to the [generated assembly code](commentary/compiler/generated-code)

- [Key data types](commentary/compiler/key-data-types)

  - [The source language: HsSyn](commentary/compiler/hs-syn-type) 
  - [RdrNames, Modules, and OccNames](commentary/compiler/rdr-name-type)
  - [ModIface, ModDetails, ModGuts](commentary/compiler/module-types)
  - [Names](commentary/compiler/name-type)
  - [Entities](commentary/compiler/entity-types): variables (Var), type constructors (TyCon), data constructors (DataCon), and classes (Class).

    - [Tying the knot](commentary/compiler/tying-the-knot): how we build the circular data structure representing entities
  - Types: 

    - [Types](commentary/compiler/type-type)
    - [Kinds](commentary/compiler/kinds)
    - [Equality types and coercions](commentary/compiler/fc)
  - [The core language](commentary/compiler/core-syn-type)
  - [The STG language](commentary/compiler/stg-syn-type)
  - [The Cmm language](commentary/compiler/cmm-type)
  - [Back end types](commentary/compiler/back-end-types)


 


- [Compiling more than one module at once](commentary/compiler/driver)
- [How data type declarations are compiled](commentary/compiler/data-types)
- [The GHC API](commentary/compiler/api)
- [Symbol names and the Z-encoding](commentary/compiler/symbol-names)
- [Template Haskell](template-haskell/conversions)
- [Wired-in and known-key things](commentary/compiler/wired-in)
- [Packages](commentary/compiler/packages)
- [Recompilation Avoidance](commentary/compiler/recompilation-avoidance)


Case studies:


- [Implementation of wired-in Bool data type](commentary/compiler/case-studies/bool)

## Overall Structure



Here is a block diagram of its top-level structure:



[](/trac/ghc/attachment/wiki/Commentary/Compiler/ghc-top.png)



The part called [HscMain](commentary/compiler/hsc-main) deals with compiling a single module.  On top of this is built the **compilation manager** (in blue) that manages the compilation of multiple modules.  It exports an interface called the **GHC API**.  On top of this API are four small front ends:


- GHCi, the interactive environment, is implemented in [ghc/GHCi/UI.hs](/trac/ghc/browser/ghc/ghc/GHCi/UI.hs) and [compiler/main/InteractiveEval.hs](/trac/ghc/browser/ghc/compiler/main/InteractiveEval.hs). It sits squarely on top of the GHC API.


 


- `--make` is almost a trivial client of the GHC API, and is implemented in [compiler/main/GhcMake.hs](/trac/ghc/browser/ghc/compiler/main/GhcMake.hs). 

- `-M`, the Makefile dependency generator, is also a client of the GHC API and is implemented in [compiler/main/DriverMkDepend.hs](/trac/ghc/browser/ghc/compiler/main/DriverMkDepend.hs). 

- The "one-shot" mode, where GHC compiles each file on the command line separately (eg. `ghc -c Foo.hs`). This mode bypasses the GHC API, and is implemented
  directly on top of [HscMain](commentary/compiler/hsc-main), since it compiles only one file at a time. In fact, this is all that   
  GHC consisted of prior to version 5.00 when GHCi and `--make` were introduced.


GHC is packaged as a single binary in which all of these front-ends are present, selected by the command-line flags indicated above.  There is a single command-line interface implemented in [ghc/Main.hs](/trac/ghc/browser/ghc/ghc/Main.hs).



In addition, GHC is compiled, without its front ends, as a *library* which can be imported by any Haskell program; see [the GHC API](commentary/compiler/api).


