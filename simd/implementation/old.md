
**This page is obsolete**. Please see the [top-level SIMD project page](simd) for further details.


## Vector computing



This page is to collect notes about adding vector instructions to GHC.



In contrast to pure SIMD (single instruction multiple data) parallel computing as found in GPUs,
vector processing includes permutation of vector elements which
allows for certain efficient algorithms
(see Guy Blelloch: [
Prefix Sums and Their Applications](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/scandal/public/papers/CMU-CS-90-190.html))
that reduce computation from n scalar operations to log n vector operations using vectors of size n.
Example: Compute the cumulative sum of a 4-element vector using a shift operation
that shifts elements and clears unused elements:


```wiki
v0 = [x0, x1, x2, x3]
v1 = v0 + shiftr 1 v0  -- v1 = [x0, x0+x1, x1+x2, x2+x3]
v2 = v1 + shiftr 2 v1  -- v2 = [x0, x0+x1, x0+x1+x2, x0+x1+x2+x3]
```


A number of processors support vector operations.
These are operations that apply (arithmetic) operations on elements of fixed sized arrays simultaneously.
The X86 instructions are provided by SSE (streaming SIMD extension), SSE2, SSE3, SSE4.1, and SSE4.2.
PowerPC has the Altivec extension.


## SSE



SSE adds 8 (or 16) 128 bit registers (xmm registers) and operations over those packed data types.  The data types are packed floats (4), doubles (2), and 32 (4) and 64 (2) bit integers.  In addition to the usual mathematical and logical operations, there are also operations such as horizontal add, horizontal subtract, and dot product operations.


### Implementation


#### PrimOps



I have added the following primitive packed data types:


- `Int32Packed4#`
- `Int64Packed2#`
- `FloatPacked4#`
- `DoublePacked2#`


There are primOps for the standard operations on these datatypes (add, mul, and, or, etc).  These are relatively straightforward to add.  All that is required is that the code generating backends need to output the assembler mnemonics (e.g. `ADDPD` for "add packed double").



Conversion between packed types is also covered by the `MachineOps` in CmmExpr.hs



There remain two types of operations which are not covered by the current machine ops:


- instructions such as horizontal add which have no unpacked equivalent
- packing/unpacking instructions 


I have decided to hardcode the packing operations, e.g. `unPack1of4Float#`, in order to avoid the need to do bound checking.  In Cmm this is translated to a `MachOp`: `MO_Unpack Int`.  Another reason in favour of this method is that the specific instruction `MO_Unpack n` does not use n as a register value, rather, this translates to a specific immediate argument.  PrimOps can only take `#`-kinded arguments.



The `PackFloat4#` operation gets translated into multiple assembler lines (for each element being packed) and so I provide the `MO_Pack Int` machine operation which takes two arguments, the scalar value and the packed value and inserts the scalar value.  The specific machine operations depend on the value of the `Int` argument to the machine operation.


#### Cmm



There was a suggestion to add `CmmVector` as a data constructor to `CmmType`, unfortunately, this abstract type is not used for literals.  Thus I have added new `Width` data constructors


```wiki
Width = ...
      | W8_16 | W16_8 | W32_4 | W64_2
      ...
```

#### STG/RTS



These new operations require a new register size (128 bit), both in Cmm and in the RTS.  So I have added a `PackedReg` register type for both `Int` and `Float` data types.  This requires adding a 128 bit type to STG as well.


## Issues


- Not all processors provide these machine operations on packed datatypes and not all x86 processors provide all the SSE extensions.  I propose to add capability tests (`sse, sse2` etc.) to `System.Info` for the programmer to be able to determine whether to use these packed primOps.  Thus if Haskell code uses primOps which the CPU does not support, there will be a compile-time error.
- It would be possible to add a Cmm pass that converts operations on packed datatypes to operations on a set of scalar types, however, it is cleaner and more efficient to not use packed datatypes on machines that do not provide those vector instructions.
- On the x86\_64 architecture, there are 4 Float registers and 2 Double registers.  These actually use the `xmm` registers, so there are only two left to use as packed registers.  My current solution is to reduce the Float registers to 3 so that there are 3 `xmm` registers.
- SSE4 capable CPUs have 16 `xmm` registers, but I am unsure as to how to approach checking for these extra registers, since the registers to use are hard coded in C.

## See also


- Trac ticket [\#3557](http://gitlabghc.nibbler/ghc/ghc/issues/3557)
