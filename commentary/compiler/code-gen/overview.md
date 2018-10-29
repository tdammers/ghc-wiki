# Overview of GHC's code generator



The root page for codegen stuff is [Commentary/Compiler/CodeGen](commentary/compiler/code-gen).



The goal of the code generator is to convert program from [STG](commentary/compiler/generated-code) representation to [Cmm](commentary/compiler/cmm-type) representation. STG is a functional language with explicit stack. Cmm is a low-level imperative language - something between C and assembly - that is suitable for machine code generation. Note that terminology might be a bit confusing here: the term "code generator" can refer both to STG-\>Cmm pass and the whole STG-\>Cmm-\>assembly pass. The Cmm-\>assembly conversion is performed by one the backends, eg. NCG (Native Code Generator or LLVM.



The top-most entry point to the codegen is located in [compiler/main/HscMain.hs](/trac/ghc/browser/ghc/compiler/main/HscMain.hs) in the `doCodeGen` function. Code generation is done in two stages:


1. Convert STG to Cmm with implicit stack, and native Cmm calls. This whole stage lives in [compiler/codeGen](/trac/ghc/browser/ghc/compiler/codeGen) directory with the entry point being `codeGen` function in [compiler/codeGen/StgCmm.hs](/trac/ghc/browser/ghc/compiler/codeGen/StgCmm.hs) module.
1. Optimise the Cmm, and CPS-convert it to have an explicit stack, and no native calls. This lives in [compiler/cmm](/trac/ghc/browser/ghc/compiler/cmm) directory with the `cmmPipeline` function from [compiler/cmm/CmmPipeline.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmPipeline.hs) module being the entry point.


The CPS-converted Cmm is fed to one of the backends. This is done by `codeOutput` function ([compiler/main/CodeOutput.hs](/trac/ghc/browser/ghc/compiler/main/CodeOutput.hs)) called from `hscGenHardCode` after returning from `doCodeGen`.


## First stage: STG to Cmm conversion


- **Code generator** converts STG to `CmmGraph`.  Implemented in `StgCmm*` modules (in directory `codeGen`). 

  - `Cmm.CmmGraph` is pretty much a Hoopl graph of `CmmNode.CmmNode` nodes. Control transfer instructions are always the last node of a basic block.
  - Parameter passing is made explicit; the calling convention depends on the target architecture.  The key function is `CmmCallConv.assignArgumentsPos`. 

    - Parameters are passed in virtual registers R1, R2 etc. \[These map 1-1 to real registers.\] 
    - Overflow parameters are passed on the stack using explicit memory stores, to locations described abstractly using the [''Stack Area'' abstraction](commentary/compiler/stack-areas).   
    - Making the calling convention explicit includes an explicit store instruction of the return address, which is stored explicitly on the stack in the same way as overflow parameters. This is done (obscurely) in `StgCmmMonad.mkCall`.

## Second stage: the Cmm pipeline



The core of the Cmm pipeline is implemented by the `cpsTop` function in [compiler/cmm/CmmPipeline.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmPipeline.hs) module. Below is a high-level overview of the pipeline. See source code comments in respective modules for a more in-depth explanation of each pass.


- **Control Flow Optimisations**, implemented in `CmmContFlowOpt`, simplifies the control flow graph by:

  - Eliminating blocks that have only one predecessor by concatenating them with that predecessor
  - Shortcuting targets of branches and calls (see Note \[What is shortcutting\])


  


>
>
> If a block becomes unreachable because of shortcutting it is eliminated from the graph. However, **it is theoretically possible that this pass will produce unreachable blocks**. The reason is the label renaming pass performed after block concatenation has been completed.
>
>

>
>
> This pass might be optionally called for the second time at the end of the pipeline.
>
>

- **Common Block Elimination**, implemented in `CmmCommonBlockElim`, eliminates blocks that are identical (except for the label on their first node). Since this pass traverses blocks in depth-first order any unreachable blocks introduced by Control Flow Optimisations are eliminated. **This pass is optional.**

- **Determine proc-points**, implemented in `CmmProcPoint`. The idea behind the "proc-point splitting" is that we first determine proc-points, ie. blocks in the graph that can be turned into entry points of procedures, and then split a larger function into many smaller ones, each having a proc-point as its entry point. This is required for the LLVM backend. The proc-point splitting itself is done later in the pipeline, but here we only determine the set of proc-points. We first call `callProcPoints`, which assumes that entry point to a Cmm graph and every continuation of a call is a procpoint. If we are splitting proc-points we update the list of proc-points by calling `minimalProcPointSet`, which adds all blocks reachable from more than one block in the graph. The set of proc-points is required by the stack layout pass.

- **Figure out the stack layout**, implemented in `CmmStackLayout`. The job of this pass is to:

  - replace references to abstract stack Areas with fixed offsets from Sp.
  - replace the CmmHighStackMark constant used in the stack check with
    the maximum stack usage of the proc.
  - save any variables that are live across a call, and reload them as
    necessary.

>
>
> **Important**: It may happen that stack layout will invalidate the computed set of proc-points by making a proc-point unreachable. This unreachable block is eliminated by one of subsequent passes that performs depth-first traversal of a graph: sinking pass (if optimisations are enabled), proc-point analysis (if optimisations are disabled and we're doing proc-point splitting) or at the very end of the pipeline (if optimisations are disabled and we're not doing proc-point splitting). This means that starting from this point in the pipeline we have inconsistent data and subsequent steps must be prepared for it.
>
>


 


- **Sinking assignments**, implemented in `CmmSink`, performs these optimizations:

  - moves assignments closer to their uses, to reduce register pressure
  - pushes assignments into a single branch of a conditional if possible
  - inlines assignments to registers that are mentioned only once
  - discards dead assignments

>
>
> **This pass is optional.** It currently does not eliminate dead code in loops ([\#8327](http://gitlabghc.nibbler/ghc/ghc/issues/8327)) and has some other minor deficiencies (eg. [\#8336](http://gitlabghc.nibbler/ghc/ghc/issues/8336)).
>
>

- **CAF analysis**, implemented in `CmmBuildInfoTables`. Computed CAF information is returned from `cmmPipeline` and used to create Static Reference Tables (SRT). See [here](commentary/rts/storage/gc/ca-fs) for some more detail on CAFs and SRTs. This pass is implemented using Hoopl (see below).

- **Proc-point analysis and splitting** (only when splitting proc-points), implemented by `procPointAnalysis` in `CmmProcPoint`, takes a list of proc-points and for each block and determines from which proc-point the block is reachable. This is implemented using Hoopl.
  Then the call to `splitAtProcPoints` splits the Cmm graph into multiple Cmm graphs (each represents a single function) and build info tables to each of them.
  When doing this we must be prepared for the fact that a proc-point does not actually exist in the graph since it was removed by stack layout pass (see [\#8205](http://gitlabghc.nibbler/ghc/ghc/issues/8205)).

- **Attach continuations' info tables** (only when NOT splitting proc-points), implemented by `attachContInfoTables` in `CmmProcPoint` attaches info tables for the continuations of calls in the graph. *\[PLEASE WRITE MORE IF YOU KNOW WHY THIS IS DONE\]*

- **Update info tables to include stack liveness**, implemented by `setInfoTableStackMap` in `CmmLayoutStack`. Populates info tables of each Cmm function with stack usage information. Uses stack maps created by the stack layout pass.

- **Control Flow Optimisations**, same as the beginning of the pipeline, but this pass runs only with `-O1` and `-O2`. Since this pass might produce unreachable blocks it is followed by a call to `removeUnreachableBlocksProc` (also in `CmmContFlowOpt.hs`)

## Dumping and debugging Cmm



You can dump all stages of Cmm processing. This is helpful for debugging Cmm problems.



Currently we have two ways to run the Cmm pipeline:


- The `.cmm` file passed as input

>
>
> Here we get into `hscCompileFile` and run the parser first. Ouput
> from the parser can be dumped via the `-ddump-cmm-verbose` flag:
>
>

```wiki
  hscCompileFile
    |-> parseCmmFile
    |
    (dump) [-ddump-cmm-verbose]  "== Parsed Cmm =="
    |
    |-> cmmPipeline (..)
```

- The Haskell source passed as input

>
>
> The compiler reached the `HscRecomp` phase and started to produce
> hard code through the native codegen:
>
>

```wiki
  hscGenHardCode
    ....
    |-> doCodegen
    .     |-> StgCmm.codeGen
    .     |
    .     (dump) [-ddump-cmm-from-stg]  "== Cmm produced by codegen =="
    .     |
    .     |-> cmmPipeline (..)
    |
    (dump) [-ddump-cmm]         "== Output Cmm =="
    |->cmmToRawCmm
    |
    (dump) [-ddump-cmm-raw]     "== Raw Cmm =="
```

>
>
> *"Cmm produced by codegen"* is emited in `HscMain` module after converting STG to Cmm. This Cmm has not been processed in any way by the Cmm pipeline. If you see that something is incorrect in that dump it means that the problem is located in the STG-\>Cmm pass. The last section, *"Output Cmm"*, is also dumped in `HscMain` but this is done after the Cmm has been processed by the whole Cmm pipeline.
>
>


All stages of the Cmm pipeline can be dumped separately (with set of the cmm subflags) or together (when `-ddump-cmm-verbose` specified). Note, that there is still problem with output into file (see ToDo in `CmmPipeline.hs:dumpWith`).
This dump is divided into several sections:


```wiki
==================== Post control-flow optimisations ====================
...

==================== Post common block elimination ====================
...

==================== Post switch plan ====================
...

==================== Proc points ====================
...

==================== Layout Stack ====================
...

==================== Sink assignments ====================
...

==================== CAFEnv ====================
...

==================== procpoint map ====================
...

==================== Post splitting ====================
...

==================== after setInfoTableStackMap ====================
...

==================== Post control-flow optimisations ====================
...

==================== Post CPS Cmm ====================
...
```


As was mentioned you can dump only selected passes with more specific flags. For example, if you know (or suspect) that the sinking pass is performing some incorrect transformations you can make the dump shorter by adding `-ddump-cmm-sp -ddump-cmm-sink` flags. This will produce only the "Layout Stack" dump (just before sinking pass) and "Sink assignments" dump (just after the sinking pass) allowing you to focus on the changes introduced by the sinking pass.


