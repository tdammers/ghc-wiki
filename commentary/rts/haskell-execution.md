# The Haskell Execution Model



The [STG language](commentary/compiler/stg-syn-type) has a clear *operational* model, as well as having a declarative lambda-calculus reading.  The business of the [code generator](commentary/compiler/code-gen) is to translate the STG program into `C--`, and thence to machine code, but that is mere detail. From the STG program you should be able to understand:


- What functions are in the compiled program, and what their entry and return conventions are
- What heap objects are allocated, when, and what their layout is


GHC uses an eval/apply execution model, described in the paper [
How to make a fast curry: push/enter vs eval/apply](https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/).  This paper is well worth reading if you are interested in this section.



Contents:
 


- [Registers](commentary/rts/haskell-execution/registers)
- [Function Calls](commentary/rts/haskell-execution/function-calls)
- [Call and Return Conventions](commentary/rts/haskell-execution/calling-convention)
- [Heap and Stack checks](commentary/rts/haskell-execution/heap-checks)
- [Updates](commentary/rts/haskell-execution/updates)
- [Pointer Tagging](commentary/rts/haskell-execution/pointer-tagging)
