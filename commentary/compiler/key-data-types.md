# Key data types



The key to understanding GHC is to understand its key data types.  There are pages describing many of them here (please add new pages!).  The diagram below shows their inter-dependencies.


- [The source language: HsSyn](commentary/compiler/hs-syn-type) 
- [RdrNames, Modules, and OccNames](commentary/compiler/rdr-name-type)
- [ModIface, ModDetails, ModGuts](commentary/compiler/module-types)
- [Uniques](commentary/compiler/unique): Not drawn in the diagram, because nearly everything depends on Uniques.
- [Names](commentary/compiler/name-type)
- [Entities](commentary/compiler/entity-types): variables, type constructors, data constructors, and classes.
- Types: [Type and Kind](commentary/compiler/type-type), [equality types and coercions](commentary/compiler/fc)
- [The core language](commentary/compiler/core-syn-type)
- [The STG language](commentary/compiler/stg-syn-type)
- [The Cmm language](commentary/compiler/cmm-type)
- [Back end types](commentary/compiler/back-end-types)


[](/trac/ghc/attachment/wiki/Commentary/Compiler/KeyDataTypes/types.png)


