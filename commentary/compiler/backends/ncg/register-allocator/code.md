## Register Allocator Code



The register allocator code is split into two main sections, the register allocator proper and a generic graph coloring library. The graph coloring library is also used by the Stg-\>Cmm converter.


### The register allocator


- [compiler/nativeGen/RegLiveness.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegLiveness.hs) 

  Defines `LiveInstr` and `LiveCmmTop` which carry native machine instructions annotated with register liveness information. It also provides functions to annotate native code (`NatCmmTop`) with this liveness information, and to slurp out sets of register conflicts for feeding into the coloring allocator.

- [compiler/nativeGen/RegAllocColor.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocColor.hs) 

  Defines `regAlloc`, the main driver function for the graph coloring allocator. The driver accepts `LiveCmmTop`s which use virtual regs, and produces`NatCmmTops` which use real machine regs. This module also provides functions to help build and deep seq the register conflict graph.

- [compiler/nativeGen/RegAllocLinear.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocLinear.hs) 

  Defines the linear scan allocator. Its interface is identical to the coloring allocator.

- [compiler/nativeGen/RegAllocInfo.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocInfo.hs) 

  Defines the register information function, `regUsage`, which takes a set of real and virtual registers and returns the actual registers used by a particular `Instr`; register allocation is in AT&T syntax order (source, destination), in an internal function, `usage`; defines the `RegUsage` data type

- [compiler/nativeGen/RegSpillCost.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillCost.hs) 

  Defines `chooseSpill` which is responsible for selecting a virtual reg to spill to the stack when not enough real regs are available.

- [compiler/nativeGen/RegSpill.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpill.hs) 

  Defines `regSpill` which takes `LiveCmmTop`s and inserts spill/reload instructions virtual regs that wouldn't fit in real regs. `regSpill`'s strategy is to simply inserts spill/reloads for every use/def of a particular virtual reg. This inefficient code is cleaned up by the spill cleaner after allocation.


 


- [compiler/nativeGen/RegSpillClean.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillClean.hs) 

  The spill cleaner is run after real regs have been allocated. It erases spill/reload instructions inserted by `regSpill` that weren't strictly nessesary.

- [compiler/nativeGen/RegAllocStats.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocStats.hs) 

  Defines data types and pretty printers used for collecting statistics and debugging info from the coloring allocator.

### Graph coloring


- [compiler/utils/GraphBase.hs](/trac/ghc/browser/ghc/compiler/utils/GraphBase.hs) 

  Defines the basic `Graph`, `Node` and `Triv` types used by the coloring algorithm.

- [compiler/utils/GraphColor.hs](/trac/ghc/browser/ghc/compiler/utils/GraphColor.hs) 

  Defines the function `colorGraph` which is responsible for assigning colors (real regs) to nodes (virtual regs) in the register conflict graph.

- [compiler/utils/GraphOps.hs](/trac/ghc/browser/ghc/compiler/utils/GraphOps.hs) 

  Defines functions to perform basic operations on the graphs such as adding, deleting, and coalescing nodes.

- [compiler/utils/GraphPps.hs](/trac/ghc/browser/ghc/compiler/utils/GraphPps.hs) 

  Defines functions for pretty print graphs in human readable-ish and graphviz format.

### Miscellanea


- [compiler/nativeGen/RegCoalesce.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegCoalesce.hs) 

  Defines a function `regCoalesce` that does aggressive coalescing directly on `LiveCmmTops`, without using the graph. This isn't used at the moment but has been left in incase we want to rejig the allocator when the new CPS converter comes online.

- [compiler/nativeGen/RegArchBase.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchBase.hs) 

  Defines utils for calculating whether a register in the conflict graph is trivially colorable, in a generic way which handles aliasing between register classes. This module is not used directly by GHC.

- [compiler/nativeGen/RegArchX86.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchX86.hs) 

  Contains a description of the aliasing constraints between the register sets on x86. This module is not used directly by GHC.
