# An Integrated Code Generator for GHC



We propose reworking GHC's back end into an **Integrated Code
Generator**, which will widen the interface between
machine-independent and machine-dependent parts of the back end.
We wish to **dissolve the barrier** between the current
machine-independent transformations (CPS
conversion, stack layout, etc) and the native-code generators
(instruction selection, calling conventions, register allocation --
including spilling to the C stack, etc). 
The goal is instead to have a code generator that **integrates both
machine-independent and machine-dependent components**, which will
interact through wide but well-specified interfaces.  From this
refactoring we expect the following benefits:


- **The back end will be simpler overall**, primarily because the
  refactoring will reduce or eliminate duplication of code

- **Complexity will be isolated** in two modules with well-defined
  interfaces: a dataflow engine and a register allocator

- **GHC will generate better machine code**, primarily because
  important decisions about register usage will be made at a later
  stage of translation and will exploit knowledge of the actual
  target machine. 

## Design elements



The important elements of
our design are as follows:


1. Build two big hammers, and hit as many nails as possible.  (The big hammers are the **dataflow optimization engine** and a **coalescing register allocator.** For more on their uses, see our [design philosophy](commentary/compiler/integrated-code-gen#esign-philosophy).)  The hammer itself may be big and complicated, but **using a big hammer should be easy** and should give easily predictable results.
1. Load all back ends into every instance of the compiler, and **treat every compilation as a cross-compilation.**  Despite having been used in production compilers for at least twenty years, this technique is still seen as somewhat unorthodox, but it removes many `#ifdef`s and saves significant complexity at compiler-configuration time. Removing `#ifdef`s also mitigates problems with  validating the compiler under different build configurations.

## Design philosophy



State-of-the art dataflow optimization and register allocation both
require complex implementations.  We live with this complexity because
**creating new clients is easy.**  


- **Dataflow optimization:** We can define a new
  optimization simply by defining a lattice of dataflow facts (akin
  to a specialized logic) and then writing the dataflow-transfer
  functions found in compiler textbooks.   Handing these functions to
  the dataflow engine produces a new optimization that is not only
  useful on its own, but that can easily be composed with other
  optimizations to create an integrated "superoptimization" that is
  strictly more powerful than any sequence of individual optimizations,
  no matter how many times they are re-run.
  The dataflow engine is based on 
  [
  (Lerner, Grove, and Chambers 2002)](http://citeseer.ist.psu.edu/old/lerner01composing.html);
  you can find a functional implementation of the dataflow engine presented in
  [
  (Ramsey and Dias 2005)](http://www.cs.tufts.edu/~nr/pubs/zipcfg-abstract.html).

- **Coalescing register allocator:** The back end can use fresh temporaries and register-register moves
  with abandon, knowing that a state-of-the-art register allocator
  will eliminate almost all move instructions.

- **Back ends:** Our ultimate goal is to make adding a new back end easy as well.
  In the long run, we wish to apply John Dias's dissertation work to GHC.
  In the short run, however, we
  think it more sensible to represent each target-machine instruction
  set with an algebraic datatype.  We propose to use type classes to
  define common functions such as identifying the registers read and
  written by each instruction.


     


## Proposed compilation pipeline


1. Convert from `STG` to an control flow graph `CmmGraph`:
1. Instruction selection:
1. Optimise:
1. Proc-point analysis, and transformation
1. Register allocation
1. Stack layout
1. Tidy up

### Convert from STG to control flow graph



Convert from `STG` to an control flow graph `CmmGraph` ([compiler/cmm/ZipCfg.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipCfg.hs), [compiler/cmm/ZipCfgCmmRep.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipCfgCmmRep.hs)).  This step is Simon PJ's "new code generator" from September 2007.  This conversion may introduce new variables, stack slots, and compile-time constants. 


```wiki
STG -> CmmGraph Cmm.Middle Cmm.Last
```

- Implements calling conventions for call, jump, and return instructions: all parameter passing is turned into data-movement instructions (register-to-register move, load, or store), and stack-pointer adjustments are inserted. After this point, calls, returns, and jumps are just control-transfer instructions -- the parameter passing has been compiled away.  
- How do we refer to locations on the stack when we haven't laid it out yet? The compiler names a stack slot using the idea of a "late compile-time constant," which is just a symbolic constant that will be replaced with an actual stack offset when the stack layout is chosen.One departure from the old code generator is that **we do not build a `Cmm` abstract-syntax tree;** instead we go straight to a control-flow graph.


In practice, we first generate an "abstract control flow graph", `CmmAGraph`, which makes the business of generating fresh `BlockId`s more convenient, and convert that to a `CmmGraph`.  The former is convenient for *construction* but cannot be analysed; the latter is concrete, and can be analyzed, transformed, and optimized.  


### Instruction selection



Instruction selection: each `Cmm` `Middle` and `Last` node in the control-flow graph is replaced with a new graph in which the nodes are machine instructions.


```wiki
CmmGraph Cmm.Middle Cmm.Last -> CmmGraph I386.Middle I386.Last
```


The `I386.Middle` type represents computational machine instructions; the `I386.Last` type represents control-transfer instructions.  The choice of representation is up to the author of the back end, but for continuity with the existing native code generators, we expect to begin by using algebraic data types inspired by the existing definitions in [compiler/nativeGen/MachInstrs.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachInstrs.hs).



Note that the graph still contains:


- **Variables** (ie local register that are not yet mapped to particular machine registers)
- **Stack-slot addressing modes**, which include late-bound compile-time constants, such as the offset in the frame of the a variable spill location, or BlockId stack-top-on-entry.


The invariant is that each node could be done by one machine instruction, provided each `LocalReg` maps to a (suitable) physical register; and an instruction involving a stack-slot can cope with (Sp+n).  



An **extremely important distinction** from the existing code is that we plan to eliminate `#ifdef` and instead provide multiple datatypes, e.g., in `I386.hs`, `PpcInstrs.hs`, `Sparc.hs`, and so on.  



Similarly, we expect a an instruction selector for *each* back end, so for example, we might have a transformation that maps `LGraph Cmm.Middle Cmm.Last` (with variables, stack slots, and compile-time constants) `-> LGraph I86.Middle I386.Last` (with variables, stack slots, and compile-time constants).



We expect to **abstract away from the details of these representations** by borrowing some abstractions from [
Machine SUIF](http://www.eecs.harvard.edu/hube/software/nci/overview.html).  In the longer term we would like to support RTL-based representations such as are used in gcc, vpo and Quick C--.  What this means is that `I386.Middle` (etc) is an abstract type, an instance of type class that supports the functions that the rest of the pipeline needs. For example:


```wiki
class Instr i where
  defs :: i -> [LocalReg]
  uses :: i -> [LocalReg]
  ..etc..
```


This allows us to  **make code improvements machine-independent**, by using machine-dependent functions to capture the semantics of instructions.  Figuring out precisely what the interface should be is a key step.  For example, to support copy propagation we might want an operation


```wiki
  isCopy :: i -> Maybe (LocalReg,LocalReg)
```


Similarly, to support peephole optimsation (eg transform `x = y+2; p = bits32[x]` to `p = bits32[y+2]`) we might want something like


```wiki
  availExprs :: i -> [(LocalReg,CmmExpr)]
  substExprs :: [(LocalReg,CmmExpr)] -> i -> Maybe i
```


The `substExprs` operation returns a `Just` iff a substitution took place.



Interfaces like these would require the machine-specific abstract type `i` to contain enough information to reconstruct a `LocalReg` or `CmmExpr`.  Later one, we'll need to construct SRTs too, so we must continue to track pointer-hood.



One possible implementation for `I386` or `Sparc` would be to use a generic RTL representation, together with a recogniser to maintain the machine invariant.  Our initial idea, though, is that  is an implementation choice.  It's still possible that a machine-independent optimisation could take advantage of the representation being an RTL. For example, we could provide a function in the `Instr` class


```wiki
   rtl :: i -> RTL
```


which is particularly cheap for architectures that do use `RTL` as the representation type.


### Optimisation



Optimise the code.  `LGraph Instrs` (with variables, stack slots, and compile-time constants) `-> LGraph Instrs` (with variables, stack slots, and compile-time constants), such as


- Branch chain elimination.
- Remove unreachable blocks (dead code).
- Constant propagation.
- Copy propagation.
- Lazy code motion (hoisting, sinking, partial redundancy elimination).
- Block concatenation.  branch to K; and this is the only use of K.  
- Common Block Elimination (like CSE). This essentially implements the Adams optimisation, we believe.
- Consider (sometime): block duplication.  branch to K; and K is a short block.  Branch chain elimination is just a special case of this.
- Peephole optimisation.  The difficulty of implementing a good peephole optimizer varies greatly with the representation of instructions.  We propose to postpone serious work on peephole optimization until we have a back end capable of representing machine instructions as RTLs, which makes peephole optimization trivial.

### Proc-point analysis


```wiki
analyse :: CmmGraph I386.Middle I386.Last -> [BlockId]
transform :: [BlockId] -> CmmGraph I386.Middle I386.Last -> CmmGraph I386.Middle I386.Last
```


Both input and output still have variables and stack-slot addressing modes.


- Proc points are found, and the appropriate control-transfer instructions are inserted.
- Why so early(before register allocation, stack layout)? Depending on the back end (think of C as the worst case), the proc-point analysis might have to satisfy some horrible calling convention. We want to make these requirements explicit before we get to the register allocator.  We also want to **exploit the register allocator** to make the best possible decisions about *which live variables (if any) should be in registers at a proc point*.

### Register allocation



Register allocation replaces variable references with machine register and stack slots.  This may introduce spills and reloads (to account for register shortage), which which is why we may get new stack-slot references.



That is, register allocation takes `LGraph Instrs` (with variables, stack slots) `-> LGraph Instrs` (with stack slots only).  No more variables!  



We no longer need to spill to the C stack, because we have fully allocated everything
to machine registers.


### Stack layout



Stack Layout: `LGraph Instrs` (with stack slots, and compile-time constants) `-> LGraph Instrs`


- Choose a stack layout.
- Replace references to stack slots with addresses on the stack.
- Replace compile-time constants with offsets into the stack.


No more stack-slot references.


### Tidy up


1. Proc-point splitting: `LGraph Instrs -> [LGraph Instrs]` 

  - Each proc point gets its own procedure.
1. Code layout: `LGraph Instrs -> [String]`

  - A reverse postorder depth-first traversal simultaneously converts the graph to sequential code and converts each instruction into an assembly-code string: **Assembly code ahoy**!

## Machine-dependence



A key property of the design is that the scopes of machine-dependent code and machine-dependent static types are limited as much as possible:


1. The representation of machine instructions may be machine-dependent (algebraic data type), or we may use a machine-independent representation that satisfies a machine-dependent dynamic invariant (RTLs).   The back end should be designed in such a way that most passes don't know the difference; we intend to borrow heavily from Machine SUIF.  To define the interface used to conceal the difference, Machine SUIF uses C++ classes; we will use Haskell's type classes.
1. Instruction selection is necessarily machine-dependent, and moreover, it must know the representation of machine instructions
1. Most of the optimizer need not know the representation of machine instructions.
1. Other passes, including register allocation, stack layout, and so on, should be completely machine-independent.
1. RTLs are not a new representation; they are a trivial extension of existing `Cmm` representations.
