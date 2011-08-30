# The DPH packages



This page describes the packages (aka libraries) that form part of DPH, enumerates all the wyas in which they differ from "normal" GHC packages, and says what stuff lives where.


## What packages there are



DPH is split into the following packages:


- `dph-base`
   Contains shared debugging and tracing functions. Particularly, the flags for enabling extended sanity checking are hard-coded in the `Config.hs` module.

- `dph-prim-seq`
   Flat arrays, and sequential operators on them. Most sequential operators that we used are supplied by `Data.Vector`, but `dph-prim-seq` adds segmented operators that are only useful in the context of nested data parallelism. This package also defines the segment descriptor types that are used to implement nested arrays. Flat arrays are also referred to as "unlifted arrays", so this library is also called the "unlifted primitive library".

- `dph-prim-par`
   Flat arrays, and parallel operators on them. This package exports exactly the same types and functions as `dph-prim-seq`, except that they run in parallel. 

- `dph-prim-interface`
   Defines the common interface exposed by `dph-prim-seq` and `dph-prim-par`. The interface is defined by the `DPH_Header.h` and `DPH_Interface.h` header files, which provide the module signature and type sigs respectively. These header files are `#include`d into `dph-prim-seq` and `dph-prim-par` to ensure they really do have the same interface.

- `dph-common`
    Nested parallel arrays. This module defines the `PArray` type, and the functions used by vectorised code. The code in this package uses the interface defined by `dph-prim-interface`. This means it can be compiled against either the `dph-prim-seq` or `dph-prim-par` packages. Doing this results in the `dph-seq` and `dph-par` packages.

- `dph-seq` and `dph-par`
   These packages are produced by compiling `dph-common` against either the `dph-prim-seq` or `dph-prim-par` packages.

- `dph-common-vseg`
   Nested parallel arrays with virtual segment descriptors. This is a new version of `dph-common` currently under development. It extends the old library with a new form of segment descriptor. The new segment descriptor allows us to avoid physically replicating data in vectorised code.

- `dph-test`
   Quick check properties for the other dph packages.

- `dph-examples`
   Example programs using Data Parallel Haskell.


The DPH libraries use Template Haskell, so they can only be compiled with a stage2 compiler.


```wiki
             vectorised code
                   ^ 
                   | used-by
                   |
  dph-seq  =  dph-common  = dph-par      (nested array API)
    .              ^             ^
    . link         | used-by     . link
    .              |             .
    .      dph-prim-interface    .       (flat array API)
    .        ^            ^      .        defined by dph-prim-interface
    .        |  provides  |      .       provided by dph-prim-seq 
    .        |            |      .               and dph-prim-par
    dph-prim-seq  --->  dph-prim-par
                used-by
             
             
                dph-base                 (debugging and tracing utils)
                                           used by all packages

             dph-common-vseg             (new version of dph-common)
                                          still under development
```

## How the DPH packages are coupled to GHC



GHC knows about DPH as follows.


- The flags `-fdph-seq` and `-fdph-par` add `-package dph-seq` and `-package dph-par`. These packages contain the types and functions that vectorised code uses. They are produced by compiling `dph-common` against either the `dph-prim-seq` or `dph-prim-par` packages respectively. They also contain the array functions visible to used code via `import Data.Array.Parallel`.

- The vectoriser only generates names exported by dph-common (via either dph-par or dph-seq). It does not use the `dph-prim-interface` API directly.
