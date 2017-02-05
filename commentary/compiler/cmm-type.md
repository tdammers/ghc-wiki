
TODO This page is possibly outdated. Update to the latest information.


>
>
> See also [Code Generator](commentary/compiler/code-gen) and [compiler/cmm/cmm-notes](/trac/ghc/browser/ghc/compiler/cmm/cmm-notes).
>
>

### Note To Reader



This page was written with more detail than usual since you may need to know how to work with Cmm as a programming language.  Cmm is the basis for the future of GHC, Native Code Generation, and if you are interested in hacking Cmm at least this page might help reduce your learning curve.  As a finer detail, if you read the [Compiler pipeline](commentary/compiler/hsc-main) wiki page or glanced at the diagram there you may have noticed that whether you are working backward from an `intermediate C` (Haskell-C "HC", `.hc`) file or an Assembler file you get to Cmm before you get to the STG language, the Simplifier or anything else.  In other words, for really low-level debugging you may have an easier time if you know what Cmm is about.  Cmm also has opportunities for implementing small and easy hacks, such as little optimisations and implementing new Cmm Primitive Operations.



A portion of the [RTS](commentary/rts) is written in Cmm: [rts/Apply.cmm](/trac/ghc/browser/ghc/rts/Apply.cmm), [rts/Exception.cmm](/trac/ghc/browser/ghc/rts/Exception.cmm), [rts/HeapStackCheck.cmm](/trac/ghc/browser/ghc/rts/HeapStackCheck.cmm), [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm), [rts/StgMiscClosures.cmm](/trac/ghc/browser/ghc/rts/StgMiscClosures.cmm), [rts/StgStartup.cmm](/trac/ghc/browser/ghc/rts/StgStartup.cmm) and [rts/StgStdThunks.cmm](/trac/ghc/browser/ghc/rts/StgStdThunks.cmm).  (For notes related to `PrimOps.cmm` see the [PrimOps](commentary/prim-ops) page; for much of the rest, see the [HaskellExecution](commentary/rts/haskell-execution) page.)  Cmm is optimised before GHC outputs either HC or Assembler.  The C compiler (from HC, pretty printed by [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs)) and the [Native Code Generator](commentary/compiler/backends/ncg) (NCG) [Backends](commentary/compiler/backends) are closely tied to data representations and transformations performed in Cmm.  In GHC, Cmm roughly performs a function similar to the intermediate [
Register Transfer Language (RTL)](http://gcc.gnu.org/onlinedocs/gccint/RTL.html) in GCC.


# Table of Contents


1. [Additions in Cmm](commentary/compiler/cmm-type#dditions-in-cmm)
1. [Compiling Cmm with GHC](commentary/compiler/cmm-type#ompiling-cmm-with-ghc)
1. [Basic Cmm](commentary/compiler/cmm-type#asic-cmm)

  1. [Code Blocks in Cmm](commentary/compiler/cmm-type#ode-blocks-in-cmm)

    - [Basic Blocks and Procedures](commentary/compiler/cmm-type#asic-blocks-and-procedures)
  1. [Variables, Registers and Types](commentary/compiler/cmm-type#ariables,-registers-and-types)

    1. [Local Registers](commentary/compiler/cmm-type#ocal-registers)
    1. [Global Registers and Hints](commentary/compiler/cmm-type#lobal-registers-and-hints)
    1. [Declaration and Initialisation](commentary/compiler/cmm-type#eclaration-and-initialisation)
    1. [Memory Access](commentary/compiler/cmm-type#emory-access)
  1. [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels)

    - [Labels](commentary/compiler/cmm-type#abels)
  1. [Sections and Directives](commentary/compiler/cmm-type#ections-and-directives)

    - [Target Directive](commentary/compiler/cmm-type#arget-directive)
  1. [Expressions](commentary/compiler/cmm-type#xpressions)

    - [Quasi-operator Syntax](commentary/compiler/cmm-type#uasi-operator-syntax)
  1. [Statements and Calls](commentary/compiler/cmm-type#tatements-and-calls)

    - [Cmm Calls](commentary/compiler/cmm-type#mm-calls)
  1. [Operators and Primitive Operations](commentary/compiler/cmm-type#perators-and-primitive-operations)

    1. [Operators](commentary/compiler/cmm-type#perators)
    1. [Primitive Operations](commentary/compiler/cmm-type#rimitive-operations)
1. [Cmm Design: Observations and Areas for Potential Improvement](commentary/compiler/cmm-type#)

# The Cmm language



`Cmm` is the GHC implementation of the `C--` language; it is also the extension of Cmm source code files: `.cmm` (see [What the hell is a .cmm file?](commentary/rts/cmm)).  The GHC [Code Generator](commentary/compiler/code-gen) (`CodeGen`) compiles the STG program into `C--` code, represented by the `Cmm` data type.  This data type follows the [
definition of \`C--\`](http://www.cminusminus.org/) pretty closely but there are some remarkable differences.  For a discussion of the Cmm implementation noting most of those differences, see the [Basic Cmm](commentary/compiler/cmm-type#asic-cmm) section, below.


- [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs): the main data type definition.
- [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs): data types defining the machine operations (e.g. floating point divide) provided by `Cmm`.
- [compiler/cmm/CLabel.hs](/trac/ghc/browser/ghc/compiler/cmm/CLabel.hs): data type for top-level `Cmm` labels.

- [compiler/cmm/PprCmm.hs](/trac/ghc/browser/ghc/compiler/cmm/PprCmm.hs): pretty-printer for `Cmm`.
- [compiler/cmm/CmmUtils.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmUtils.hs): operations over `Cmm`

- [compiler/cmm/CmmLint.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmLint.hs): a consistency checker.
- [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs): an optimiser for `Cmm`.

- [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y), [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x): parser and lexer for [.cmm files](commentary/rts/cmm).

- [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs): pretty-print `Cmm` in C syntax, when compiling via C.

## Additions in Cmm



Although both Cmm and C-- allow foreign calls, the `.cmm` syntax includes the 


```wiki
foreign "C" cfunctionname(R1) [R2];
```


The \[R2\] part is the (set of) register(s) that you need to save over the call.



Other additions to C-- are noted throughout the [Basic Cmm](commentary/compiler/cmm-type#asic-cmm) section, below.


## Compiling Cmm with GHC



GHC is able to compile `.cmm` files with a minimum of user-effort.  To compile `.cmm` files, simply invoke the main GHC driver but remember to:


- add the option `-dcmm-lint` if you have handwritten Cmm code;
- add appropriate includes, especially [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) if you are using Cmm macros or GHC defines for certain types, such as `W_` for `bits32` or `bits64` (depending on the machine word size)--`Cmm.h` is in the `/includes` directory of every GHC distribution, i.e., `usr/local/lib/ghc-6.6/includes`; and,
- if you do include GHC header files, remember to pass the code through the C preprocessor by adding the `-cpp` option.


For additional fun, you may pass GHC the `-keep-s-file` option to keep the temporary assembler file in your compile directory.
For example:


```wiki
ghc -cpp -dcmm-lint -keep-s-file -c Foo.cmm -o Foo.o
```


This will only work with very basic Cmm files.  If you noticed that GHC currently provides no `-keep-cmm-file` option and `-keep-tmp-files` does not save a `.cmm` file and you are thinking about redirecting output from `-ddump-cmm`, beware. The output from `-ddump-cmm` contains equal-lines and dash-lines separating Cmm Blocks and Basic Blocks; these are unparseable.  The parser also cannot handle `const` sections.  For example, the parser will fail on the first `0` or alphabetic token after `const`:


```wiki
section "data" {
    rOG_closure:
        const rOG_info;	// parse error `rOG_info'
        const 0;	// parse error `0'
        const 0;
        const 0;
}
```


Although GHC's Cmm pretty printer outputs C-- standard parenthetical list of arguments after procedure names, i.e., `()`, the Cmm parser will fail at the `(` token.  For example:


```wiki
__stginit_Main_() {	// parse error `('
    cUX:
        Sp = Sp + 4;
        jump (I32[Sp + (-4)]);
}
```


The Cmm procedure names in [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm) are not followed by a (possibly empty) parenthetical list of arguments; all their arguments are Global (STG) Registers, anyway, see [Variables, Registers and Types](commentary/compiler/cmm-type#ariables,-registers-and-types), below.  Don't be confused by the procedure definitions in other handwritten `.cmm` files in the RTS, such as [rts/Apply.cmm](/trac/ghc/browser/ghc/rts/Apply.cmm): all-uppercase procedure invocations are special reserved tokens in [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x) and [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y).  For example, `INFO_TABLE` is parsed as one of the tokens in the Alex `info` predicate:


```wiki
info	:: { ExtFCode (CLabel, [CmmLit],[CmmLit]) }
	: 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
		-- ptrs, nptrs, closure type, description, type
		{ stdInfo $3 $5 $7 0 $9 $11 $13 }
```


GHC's Cmm parser also cannot parse nested code blocks.  For example:


```wiki
s22Q_ret() {
	s22Q_info {  	// parse error `{'
		const Main_main_srt-s22Q_info+24;
		const 0;
		const 2228227;
	}
    c23f:
	R2 = base_GHCziHandle_stdout_closure;
	R3 = 10;
	Sp = Sp + 4;    /* Stack pointer */
	jump base_GHCziIO_zdwhPutChar_info;
}
```


The C-- specification example in section 4.6.2, "Procedures as section contents" also will not parse in Cmm:


```wiki
section "data" { 
	const PROC = 3; 	// parse error `PROC' (no parse after 'const')
	bits32[] {p_end, PROC}; // parse error `[' (only bits8[] is allowed)
				// parse error `{' (no {...} variable initialisation)

	p (bits32 i) {	// parse error `{' (Cmm thinks "p (bits32 i)" is a statement)
		loop: 
			i = i-1; 
		if (i >= 0) { goto loop ; }	// no parse error 
						// (if { ... } else { ... } *is* parseable)
		return; 
	} 
	p_end: 
} 
```


Note that if `p (bits32 i) { ... }` were written as a Cmm-parseable procedure, as `p { ... }`, the parse error would occur at the closing curly bracket for the `section "data" { ... p { ... } }`\<- here.


## Basic Cmm



FIXME The links in this section are dead. But the files can be found here: [
http://www.cs.tufts.edu/\~nr/c--/index.html](http://www.cs.tufts.edu/~nr/c--/index.html). Relevant discussion about the documentations of C--: [
https://mail.haskell.org/pipermail/ghc-devs/2014-September/006301.html](https://mail.haskell.org/pipermail/ghc-devs/2014-September/006301.html)



Cmm is a high level assembler with a syntax style similar to C.  This section describes Cmm by working up from assembler--the C-- papers and specification work down from C.  At the least, you should know what a "high level" assembler is, see [
What is a High Level Assembler?](http://webster.cs.ucr.edu/AsmTools/HLA/HLADoc/HLARef/HLARef3.html#1035157).  Cmm is different than other high level assembler languages in that it was designed to be a semi-portable intermediate language for compilers; most other high level assemblers are designed to make the tedium of assembly language more convenient and intelligible to humans.  If you are completely new to C--, I highly recommend these papers listed on the [
C-- Papers](http://cminusminus.org/papers.html) page:


- [
  C--: A Portable Assembly Language that Supports Garbage Collection (1999)](http://cminusminus.org/abstracts/ppdp.html) (Paper page with Abstract)
- [
  C--: A Portable Assembly Language (1997)](http://cminusminus.org/abstracts/pal-ifl.html) (Paper page with Abstract)
- [
  A Single Intermediate Language That Supports Multiple Implementations of Exceptions (2000)](http://cminusminus.org/abstracts/c--pldi-00.html) (Paper page with Abstract)
- [
  The C-- Language Specification Version 2.0 (CVS Revision 1.128, 23 February 2005)](http://cminusminus.org/extern/man2.pdf) (PDF)


Cmm is not a stand alone C-- compiler; it is an implementation of C-- embedded in the GHC compiler.  One difference between Cmm and a C-- compiler like [
Quick C--](http://cminusminus.org/code.html) is this: Cmm uses the C preprocessor (cpp).  Cpp lets Cmm *integrate* with C code, especially the C header defines in [includes](/trac/ghc/browser/ghc/includes), and among many other consequences it makes the C-- `import` and `export` statements irrelevant; in fact, according to [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) they are ignored.  The most significant action taken by the Cmm modules in the Compiler is to optimise Cmm, through [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs).  The Cmm Optimiser generally runs a few simplification passes over primitive Cmm operations, inlines simple Cmm expressions that do not contain global registers (these would be left to one of the [Backends](commentary/compiler/backends), which currently cannot handle inlines with global registers) and performs a simple loop optimisation. 


### Code Blocks in Cmm



The Haskell representation of Cmm separates contiguous code into:


- *modules* (compilation units; a `.cmm` file); and
- *basic blocks*


Cmm modules contain static data elements (see [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels)) and [Basic Blocks](commentary/compiler/cmm-type#), collected together in `Cmm`, a type synonym for `GenCmm`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

newtype GenCmm d i = Cmm [GenCmmTop d i]

type Cmm = GenCmm CmmStatic CmmStmt


data GenCmmTop d i
  = CmmProc
     [d]	       -- Info table, may be empty
     CLabel            -- Used to generate both info & entry labels
     [LocalReg]        -- Argument locals live on entry (C-- procedure params)
     [GenBasicBlock i] -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

  -- some static data.
  | CmmData Section [d]	-- constant values only

type CmmTop = GenCmmTop CmmStatic CmmStmt
```



`CmmStmt` is described in [Statements and Calls](commentary/compiler/cmm-type#tatements-and-calls);

`Section` is described in [Sections and Directives](commentary/compiler/cmm-type#ections-and-directives);

the static data in `[d]` is \[`CmmStatic`\] from the type synonym `Cmm`;

`CmmStatic` is described in [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels).


#### Basic Blocks and Procedures



Cmm procedures are represented by the first constructor in `GenCmmTop d i`:


```

    CmmProc [d] CLabel [LocalReg] [GenBasicBlock i]
```



For a description of Cmm labels and the `CLabel` data type, see the subsection [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels), below.



Cmm Basic Blocks are labeled blocks of Cmm code ending in an explicit jump.  Sections (see [Sections and Directives](commentary/compiler/cmm-type#ections-and-directives)) have no jumps--in Cmm, Sections cannot contain nested Procedures (see, e.g., [Compiling Cmm with GHC](commentary/compiler/cmm-type#ompiling-cmm-with-ghc)).  Basic Blocks encapsulate parts of Procedures.  The data type `GenBasicBlock` and the type synonym `CmmBasicBlock` encapsulate Basic Blocks; they are defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

data GenBasicBlock i = BasicBlock BlockId [i]

type CmmBasicBlock = GenBasicBlock CmmStmt

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u
```



The `BlockId` data type simply carries a `Unique` with each Basic Block.  For descriptions of `Unique`, see 


- the [Renamer](commentary/compiler/renamer) page;
- the [Known Key Things](commentary/compiler/wired-in#nown-key-things) section of the [Wired-in and Known Key Things](commentary/compiler/wired-in) page; and, 
- the [Type variables and term variables](commentary/compiler/entity-types#ype-variables-and-term-variables) section of the [Entity Types](commentary/compiler/entity-types) page.

### Variables, Registers and Types



Like other high level assembly languages, all variables in C-- are machine registers, separated into different types according to bit length (8, 16, 32, 64, 80, 128) and register type (integral or floating point). The C-- standard specifies little more type information about a register than its bit length: there are no distinguishing types for signed or unsigned integrals, or for "pointers" (registers holding a memory address).  A C-- standard compiler supports additional information on the type of a register value through compiler *hints*.  In a foreign call, a `"signed" bits8` would be sign-extended and may be passed as a 32-bit value.  Cmm diverges from the C-- specification on this point somewhat (see below).  C-- and Cmm do not represent special registers, such as a Condition Register (`CR`) or floating point unit (FPU) status and control register (`FPSCR` on the PowerPC, `MXCSR` on Intel x86 processors), as these are a matter for the [Backends](commentary/compiler/backends).



C-- and Cmm hide the actual number of registers available on a particular machine by assuming an "infinite" supply of registers.  A backend, such as the NCG or C compiler on GHC, will later optimise the number of registers used and assign the Cmm variables to actual machine registers; the NCG temporarily stores any overflow in a small memory stack called the *spill stack*, while the C compiler relies on C's own runtime system.  Haskell handles Cmm registers with three data types: `LocalReg`, `GlobalReg` and `CmmReg`.  `LocalReg`s and `GlobalRegs` are collected together in a single `Cmm` data type:


```

data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq )
```


#### Local Registers



Local Registers exist within the scope of a Procedure:


```

data LocalReg
  = LocalReg !Unique MachRep
```



For a list of references with information on `Unique`, see the [Basic Blocks and Procedures](commentary/compiler/cmm-type#asic-blocks-and-procedures) section, above.



A `MachRep`, the type of a machine register, is defined in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs):


```

data MachRep
  = I8		-- integral type, 8 bits wide (a byte)
  | I16		-- integral type, 16 bits wide
  | I32		-- integral type, 32 bits wide
  | I64		-- integral type, 64 bits wide
  | I128	-- integral type, 128 bits wide (an integral vector register)
  | F32		-- floating point type, 32 bits wide (float)
  | F64		-- floating point type, 64 bits wide (double)
  | F80		-- extended double-precision, used in x86 native codegen only.
  deriving (Eq, Ord, Show)
```



There is currently no register for floating point vectors, such as `F128`.  The types of Cmm variables are defined in the Happy parser file [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) and the Alex lexer file [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x).  (Happy and Alex will compile these into `CmmParse.hs` and `CmmLex.hs`, respectively.)  Cmm recognises the following `C--` types as parseable tokens, listed next to their corresponding `define`s in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) and their STG types:


<table><tr><th> **Cmm Token** </th>
<th> **Cmm.h \#define** </th>
<th> **STG type** 
</th></tr>
<tr><th> `bits8` </th>
<th> `I8` </th>
<th> `StgChar` or `StgWord8` 
</th></tr>
<tr><th> `bits16` </th>
<th> `I16` </th>
<th> `StgWord16` 
</th></tr>
<tr><th> `bits32` </th>
<th> `I32`, `CInt`, `CLong` </th>
<th> `StgWord32`; `StgWord` (depending on architecture) 
</th></tr>
<tr><th> `bits64` </th>
<th> `I64`, `CInt`, `CLong`, `L_` </th>
<th> `StgWord64`; `StgWord` (depending on architecture) 
</th></tr>
<tr><th> `float32` </th>
<th> `F_` </th>
<th> `StgFloat` 
</th></tr>
<tr><th> `float64` </th>
<th> `D_` </th>
<th> `StgDouble` 
</th></tr></table>



[includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) also defines `L_` for `bits64`, so `F_`, `D_` and `L_` correspond to the `GlobalReg` data type constructors `FloatReg`, `DoubleReg` and `LongReg`.  Note that although GHC may generate other register types supported by the `MachRep` data type, such as `I128`, they are not parseable tokens.  That is, they are internal to GHC.  The special defines `CInt` and `CLong` are used for compatibility with C on the target architecture, typically for making `foreign "C"` calls.



**Note**: Even Cmm types that are not explicit variables (Cmm literals and results of Cmm expressions) have implicit `MachRep`s, in the same way as you would use temporary registers to hold labelled constants or intermediate values in assembler functions.  See:


- [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels) for information related to the Cmm literals `CmmInt` and `CmmFloat`; and,
- [Expressions](commentary/compiler/cmm-type#xpressions), regarding the `cmmExprRep` function defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs).

#### Global Registers and Hints



These are universal both to a Cmm module and to the whole compiled program.  Variables are global if they are declared at the top-level of a compilation unit (outside any procedure).  Global Variables are marked as external symbols with the `.globl` assembler directive.  In Cmm, global registers are used for special STG registers and specific registers for passing arguments and returning values.  The Haskell representation of Global Variables (Registers) is the `GlobalReg` data type, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- register number, such as R3, R11

  | FloatReg		        -- single-precision floating-point registers
	{-# UNPACK #-} !Int	-- register number

  | DoubleReg		        -- double-precision floating-point registers
	{-# UNPACK #-} !Int	-- register number

  | LongReg	                -- long int registers (64-bit, really)
	{-# UNPACK #-} !Int	-- register number

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure

  -- We keep the address of some commonly-called 
  -- functions in the register table, to keep code
  -- size down:
  | GCEnter1		-- stg_gc_enter_1
  | GCFun		-- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform  (see compiler/nativeGen/PositionIndependentCode.hs).
  | PicBaseReg
```



For a description of the `Hp` and `Sp` *virtual registers*, see [The Haskell Execution Model](commentary/rts/haskell-execution) page.  General `GlobalReg`s are clearly visible in Cmm code according to the following syntax defined in [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x):


<table><tr><th> **`GlobalReg` Constructor** </th>
<th> **Syntax** </th>
<th> **Examples** 
</th></tr>
<tr><th> `VanillaReg Int` </th>
<th> `R ++ Int` </th>
<th> `R1`, `R10` 
</th></tr>
<tr><th> `FloatReg Int` </th>
<th> `F ++ Int` </th>
<th> `F1`, `F10` 
</th></tr>
<tr><th> `DoubleReg Int` </th>
<th> `D ++ Int` </th>
<th> `D1`, `D10` 
</th></tr>
<tr><th> `LongReg Int` </th>
<th> `L ++ Int` </th>
<th> `L1`, `L10` 
</th></tr></table>



General `GlobalRegs` numbers are decimal integers, see the `parseInteger` function in [compiler/utils/StringBuffer.hs](/trac/ghc/browser/ghc/compiler/utils/StringBuffer.hs).  The remainder of the `GlobalReg` constructors, from `Sp` to `BaseReg` are lexical tokens exactly like their name in the data type; `PicBaseReg` does not have a lexical token since it is used only inside the NCG.  See [Position Independent Code and Dynamic Linking](commentary/position-independent-code) for an in-depth description of PIC implementations in the NCG.



`GlobalRegs` are a very special case in Cmm, partly because they must conform to the STG register convention and the target C calling convention.  That the Cmm parser recognises `R1` and `F3` as `GlobalRegs` is only the first step.  The main files to look at for more information on this delicate topic are:


- [compiler/codeGen/CgCallConv.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgCallConv.hs) (the section on "Register assignment")
- [includes/stg/Regs.h](/trac/ghc/browser/ghc/includes/stg/Regs.h) (defining STG registers)
- [includes/stg/MachRegs.h](/trac/ghc/browser/ghc/includes/stg/MachRegs.h) (target-specific mapping of machine registers for *registerised* builds of GHC)
- [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm) (examples of `GlobalReg` register usage for out-of-line primops)


All arguments to out-of-line PrimOps in [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm) are STG registers.



Cmm recognises all C-- syntax with regard to *hints*.  For example:


```wiki
"signed" bits32 x;  // signed or unsigned int with hint "signed"

foreign "C" labelThread(R1 "ptr", R2 "ptr") [];

"ptr" info = foreign "C" lockClosure(mvar "ptr") [];

```


Hints are represented in Haskell as `MachHint`s, defined near `MachRep` in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs):


```

data MachHint
  = NoHint	-- string: "NoHint"	Cmm syntax: [empty]
  | PtrHint	-- string: "PtrHint"    Cmm syntax: "ptr"    (C-- uses "address")
  | SignedHint	-- string: "SignedHint"	Cmm syntax: "signed"
  | FloatHint	-- string: "FloatHint"	Cmm syntax: "float" 
```



Although the C-- specification does not allow the C-- type system to statically distinguish between floats, signed ints, unsigned ints or pointers, Cmm does. Cmm `MachRep`s carry the float or int kind of a variable, either within a local block or in a global register.  `GlobalReg` includes separate constructors for `Vanilla`, `Float`, `Double` and `Long`.  Cmm still does not distinguish between signed ints, unsigned ints and pointers (addresses) at the register level, as these are given *hint* pseudo-types or their real type is determined as they run through primitive operations.  `MachHint`s still follow the C-- specification and carry kind information as an aide to the backend optimisers.  



Global Registers in Cmm currently have a problem with inlining: because neither [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs) nor the NCG are able to keep Global Registers from clashing with C argument passing registers, Cmm expressions that contain Global Registers cannot be inlined into an argument position of a foreign call.  For more thorough notes on inlining, see the comments in [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs).


#### Declaration and Initialisation



Cmm variables hold the same values registers do in assembly languages but may be declared in a similar way to variables in C.  As in C--, they may actually be declared anywhere in the scope for which they are visible (a block or file)--for Cmm, this is done by the `loopDecls` function in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y).  In [compiler/rts/PrimOps.cmm](/trac/ghc/browser/ghc/compiler/rts/PrimOps.cmm), you will see Cmm variable declarations like this one:


```wiki
W_ w, code, val;  // W_ is a cpp #define for StgWord, 
		  // a machine word (32 or 64-bit--general register size--unsigned int)
```


Remember that Cmm code is run through the C preprocessor.  `W_` will be transformed into `bits32`, `bits64` or whatever is the `bits`*size* of the machine word, as defined in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h).  In Haskell code, you may use the [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs) functions `wordRep` and `halfWordRep` to dynamically determine the machine word size.  For a description of word sizes in GHC, see the [Word](commentary/rts/word) page.



The variables `w`, `code` and `val` should be real registers. With the above declaration the variables are uninitialised.  Initialisation requires an assignment *statement*.  Cmm does not recognise C-- "`{` *literal*, ... `}`" initialisation syntax, such as `bits32{10}` or `bits32[3] {1, 2, 3}`.  Cmm does recognise initialisation with a literal:


```wiki
string_name:	bits8[] "twenty character string\n\0";

variable_num:	bits32 10::bits32;
```


The typical method seems to be to declare variables and then initialise them just before their first use.  (Remember that you may declare a variable anywhere in a procedure and use it in an expression before it is initialised but you must initialise it before using it anywhere else--statements, for example.)


#### Memory Access



If the value in `w` were the address of a memory location, you would obtain the value at that location similar to Intel assembler syntax.  In Cmm, you would write:


```wiki
code = W_[w];  // code is assigned the W_ value at memory address w
```


compare the above statement to indirect addressing in Intel assembler:


```wiki
mov	al, [eax]  ; move value in memory at indirect address in register eax, 
		   ; into register al
```


The code between the brackets (`w` in `[w]`, above) is an *expression*.  See the [Expressions](commentary/compiler/cmm-type#xpressions) section.  For now, consider the similarity between the Cmm-version of indexed memory addressing syntax, here:


```wiki
R1 = bits32[R2 + R3];	// R2 (memory address), R3 (index, offset), result: type bits32

// note: in Cmm 'R2' and 'R3' would be parsed as global registers
// this is generally bad form; instead, 
// declare a local variable and initialise it with a global, such as:
bits32 adr, ofs, res;
adr = R2;
ofs = R3;
res = bits32[adr + ofs];
R1 = res;

// using local variables will give the NCG some leeway to avoid clobbering the globals
// should you call another procedure somewhere in the same scope
```


and the corresponding Intel assembler indexed memory addressing syntax, here:


```wiki
mov	al, ebx[eax]	; ebx (base), eax (index)
; or
mov	al, [ebx + eax]
```


You will generally not see this type of syntax in either handwritten or GHC-produced Cmm code, although it is allowed; it simply shows up in macros.  C-- also allows the `*` (multiplication) operator in addressing expressions, for an approximation of *scaled* addressing (`[base * (2^n)]`); for example, `n` (the "scale") must be `0`, `1`, `2` or `4`.  C-- itself would not enforce alignment or limits on the scale.  Cmm, however, could not process it: since the NCG currently outputs GNU Assembler syntax, the Cmm or NCG optimisers would have to reduce `n` in (`* n`) to an absolute address or relative offset, or to an expression using only `+` or `-`.  This is not currently the case and would be difficult to implement where one of the operands to the `*` is a relative address not visible in the code block.  [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) defines macros to perform the calculation with a constant.  For example:


```wiki
/* Converting quantities of words to bytes */
#define WDS(n) ((n)*SIZEOF_W)  // SIZEOF_W is a constant
```


is used in:


```wiki
#define Sp(n)  W_[Sp + WDS(n)]
```


The function `cmmMachOpFold` in [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs) will reduce the resulting expression `Sp + (n * SIZEOF_W)` to `Sp + N`, where `N` is a constant.  A very large number of macros for accessing STG struct fields and the like are produced by [utils/deriveConstants](/trac/ghc/browser/ghc/utils/deriveConstants) and output into the file `includes/DerivedConstants.h` when GHC is compiled.



Of course, all this also holds true for the reverse (when an assignment is made to a memory address):


```wiki
section "data" {
	num_arr: bits32[10];
}

proc1 {
	// ...
	bits32[num_arr + (2*3)] = 5::bits32;  // in C: num_arr[(2*3)] = 5;
	// ...
}
```


or, for an example of a macro from `DerivedConstants.h`:


```wiki
StgAtomicallyFrame_code(frame) = R1;
```


this will be transformed to:


```wiki
REP_StgAtomicallyFrame_code[frame + SIZEOF_StgHeader + OFFSET_StgAtomicallyFrame_code] = R1;
// further reduces to (on Darwin PPC arch):
I32[frame + SIZEOF_StgHeader + 0] = R1;
```

### Literals and Labels



Cmm literals are exactly like C-- literals, including the Haskell-style type syntax, for example: `0x00000001::bits32`.  Cmm literals may be used for initialisation by assignment or in expressions. The `CmmLit` and `CmmStatic` data types, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs) together represent Cmm literals, static information and Cmm labels:


```

data CmmLit
  = CmmInt Integer  MachRep
	-- Interpretation: the 2's complement representation of the value
	-- is truncated to the specified size.  This is easier than trying
	-- to keep the value within range, because we don't know whether
	-- it will be used as a signed or unsigned value (the MachRep doesn't
	-- distinguish between signed & unsigned).
  | CmmFloat  Rational MachRep
  | CmmLabel    CLabel			-- Address of label
  | CmmLabelOff CLabel Int		-- Address of label + byte offset
  
        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code. 
  | CmmLabelDiffOff CLabel CLabel Int   -- label1 - label2 + offset
```



Note how the `CmmLit` constructor `CmmInt Integer MachRep` contains sign information in the `Integer`, the representation of the literal itself: this conforms to the C-- specification, where integral literals contain sign information. For an example of a function using `CmmInt` sign information, see `cmmMachOpFold` in [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs), where sign-operations are performed on the `Integer`.



The `MachRep` of a literal, such as `CmmInt Integer MachRep` or `CmmFloat Rational MachRep` may not always require the size defined by `MachRep`.  The NCG optimiser, [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs), will test a literal such as `1::bits32` (in Haskell, `CmmInt (1::Integer) I32`) for whether it would fit into the bit-size of Assembler instruction literals on that particular architecture with a function defined in [compiler/nativeGen/MachRegs.lhs](/trac/ghc/browser/ghc/compiler/nativeGen/MachRegs.lhs), such as `fits16Bits` on the PPC.  If the Integer literal fits, the function `makeImmediate` will truncate it to the specified size if possible and store it in a NCG data type, `Imm`, specifically `Maybe Imm`.  (These are also defined in [compiler/nativeGen/MachRegs.lhs](/trac/ghc/browser/ghc/compiler/nativeGen/MachRegs.lhs).)



The Haskell representation of Cmm separates unchangeable Cmm values into a separate data type, `CmmStatic`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

data CmmStatic
  = CmmStaticLit CmmLit	
	-- a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
	-- uninitialised data, N bytes long
  | CmmAlign Int
	-- align to next N-byte boundary (N must be a power of 2).
  | CmmDataLabel CLabel
	-- label the current position in this section.
  | CmmString [Word8]
	-- string of 8-bit values only, not zero terminated.
```



Note the `CmmAlign` constructor: this maps to the assembler directive `.align N` to set alignment for a data item (hopefully one you remembered to label).  This is the same as the `align` directive noted in Section 4.5 of the [
C-- specification (PDF)](http://cminusminus.org/extern/man2.pdf).  In the current implementation of Cmm the `align` directive seems superfluous because [compiler/nativeGen/PprMach.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PprMach.hs) translates `Section`s to assembler with alignment directives corresponding to the target architecture (see [Sections and Directives](commentary/compiler/cmm-type#ections-and-directives), below).


#### Labels



Remember that C--/Cmm names consist of a string where the first character is:


- ASCII alphabetic (uppercase or lowercase);
- an underscore:    `_` ;
- a period:         `.` ;
- a dollar sign:    `$` ; or,
- a commercial at:  `@` .


Cmm labels conform to the C-- specification.  C--/Cmm uses labels to refer to memory locations in code--if you use a data directive but do not give it a label, you will have no means of referring to the memory!  For `GlobalReg`s (transformed to assembler `.globl`), labels serve as both symbols and labels (in the assembler meaning of the terms).  The Haskell representation of Cmm Labels is contained in the `CmmLit` data type, see [Literals](commentary/compiler/cmm-type#) section, above.  Note how Cmm Labels are `CLabel`s with address information.  The `Clabel` data type, defined in [compiler/cmm/CLabel.hs](/trac/ghc/browser/ghc/compiler/cmm/CLabel.hs), is used throughout the Compiler for symbol information in binary files.  Here it is:


```

data CLabel
  = IdLabel	    		-- A family of labels related to the
	Name			-- definition of a particular Id or Con
	IdLabelInfo

  | DynIdLabel			-- like IdLabel, but in a separate package,
	Name			-- and might therefore need a dynamic
	IdLabelInfo		-- reference.

  | CaseLabel			-- A family of labels related to a particular
				-- case expression.
	{-# UNPACK #-} !Unique	-- Unique says which case expression
	CaseLabelInfo

  | AsmTempLabel 
	{-# UNPACK #-} !Unique

  | StringLitLabel
	{-# UNPACK #-} !Unique

  | ModuleInitLabel 
	Module			-- the module name
	String			-- its "way"
	Bool			-- True <=> is in a different package
	-- at some point we might want some kind of version number in
	-- the module init label, to guard against compiling modules in
	-- the wrong order.  We can't use the interface file version however,
	-- because we don't always recompile modules which depend on a module
	-- whose version has changed.

  | PlainModuleInitLabel	-- without the vesrion & way info
	Module
	Bool			-- True <=> is in a different package

  | ModuleRegdLabel

  | RtsLabel RtsLabelInfo

  | ForeignLabel FastString	-- a 'C' (or otherwise foreign) label
	(Maybe Int) 		-- possible '@n' suffix for stdcall functions
		-- When generating C, the '@n' suffix is omitted, but when
		-- generating assembler we must add it to the label.
	Bool			-- True <=> is dynamic

  | CC_Label  CostCentre
  | CCS_Label CostCentreStack

      -- Dynamic Linking in the NCG:
      -- generated and used inside the NCG only,
      -- see module PositionIndependentCode for details.
      
  | DynamicLinkerLabel DynamicLinkerLabelInfo CLabel
        -- special variants of a label used for dynamic linking

  | PicBaseLabel                -- a label used as a base for PIC calculations
                                -- on some platforms.
                                -- It takes the form of a local numeric
                                -- assembler label '1'; it is pretty-printed
                                -- as 1b, referring to the previous definition
                                -- of 1: in the assembler source file.

  | DeadStripPreventer CLabel
    -- label before an info table to prevent excessive dead-stripping on darwin

  | HpcTicksLabel Module       -- Per-module table of tick locations
  | HpcModuleNameLabel         -- Per-module name of the module for Hpc
  | HpcModuleOffsetLabel Module-- Per-module offset of the module for Hpc (dynamically generated)

  deriving (Eq, Ord)
```


### Sections and Directives



The Haskell representation of Cmm Section directives, in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs) as the first part of the "Static Data" section, is:


```

data Section
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
  | ReadOnlyData16	-- .rodata.cst16 on x86_64, 16-byte aligned
  | OtherSection String
```



Cmm supports the following directives, corresponding to the assembler directives pretty-printed by the `pprSectionHeader` function in [compiler/nativeGen/PprMach.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PprMach.hs):


<table><tr><th> **`Section` Constructor** </th>
<th> **Cmm section directive** </th>
<th> **Assembler Directive** 
</th></tr>
<tr><th> `Text` </th>
<th> `"text"` </th>
<th> `.text` 
</th></tr>
<tr><th> `Data` </th>
<th> `"data"` </th>
<th> `.data` 
</th></tr>
<tr><th> `ReadOnlyData` </th>
<th> `"rodata"` </th>
<th> `.rodata`
(generally; varies by arch,OS) 
</th></tr>
<tr><th> `RelocatableReadOnlyData` </th>
<th> no parse (GHC internal), output: `"relreadonly"` </th>
<th> `.const_data`
`.section .rodata`
(generally; varies by arch,OS) 
</th></tr>
<tr><th> `UninitialisedData` </th>
<th> `"bss"`, output: `"uninitialised"` </th>
<th> `.bss` 
</th></tr>
<tr><th> `ReadOnlyData16` </th>
<th> no parse (GHC internal), output: none </th>
<th> `.const`
`.section .rodata`
(generally; on x86\_64:
`.section .rodata.cst16`) 
</th></tr></table>



You probably already noticed I omitted the alignment directives (for clarity).  For example, `pprSectionHeader` would pretty-print `ReadOnlyData` as 


```wiki
.const
.align 2
```


on an i386 with the Darwin OS.  If you are really on the ball you might have noticed that the `PprMach.hs` output of "`.section .data`" and the like is really playing it safe since on most OS's, using GNU Assembler, the `.data` directive is equivalent to `.section __DATA .data`, or simply `.section .data`.  Note that `OtherSection String` is not a catch-all for the Cmm parser.  If you wrote:


```wiki
section ".const\n.align 2\n\t.section .rodata" { ... }
```


The Cmm parser (through GHC) would panic, complaining, "`PprMach.pprSectionHeader: unknown section`."  



While the C-- specification allows a bare `data` keyword directive, Cmm does not:


```wiki
// this is valid C--, not Cmm!
data { }

// all Cmm directives use this syntax:
section [Cmm section directive] { }
```


Cmm does not recognise the C-- "`stack`" declaration for allocating memory on the system stack.  



GHC-produced Cmm code is replete with `data` sections, each of which is stored in `.data` section of the binary code.  This contributes significantly to the large binary size for GHC-compiled code. 


#### Target Directive



The C-- specification defines a special `target` directive, in section 4.7.  The `target` directive is essentially a code block defining the properties of the target architecture:


```wiki
target
	memsize	N	// bit-size of the smallest addressable unit of memory
	byteorder	[big,little]	// endianness
	pointersize	N	// bit-size of the native pointer type
	wordsize	N	// bit-size of the native word type
```


This is essentially a custom-coded version of the GNU Assembler (`as`) `.machine` directive, which is essentially the same as passing the `-arch [cpu_type]` option to `as`.



Cmm does not support the `target` directive.  This is partly due GHC generally lacking cross-compiler capabilities.  Should GHC move toward adding cross-compilation capabilities, the `target` might not be a bad thing to add.  Target architecture parameters are currently handled through the [Build System](attic/building/build-system), which partly sets such architectural parameters through [utils/deriveConstants](/trac/ghc/browser/ghc/utils/deriveConstants) and [includes/ghcconfig.h](/trac/ghc/browser/ghc/includes/ghcconfig.h).


### Expressions



Expressions in Cmm follow the C-- specification.  They have:


- no side-effects; and,
- one result: 

  - a *k*-bit value
    --these expressions map to the `MachOp` data type, defined in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs), see [Operators and Primitive Operations](commentary/compiler/cmm-type#perators-and-primitive-operations), the *k*-bit value may be:

    - a Cmm literal (`CmmLit`); or,
    - a Cmm variable (`CmmReg`, see [Variables, Registers and Types](commentary/compiler/cmm-type#ariables,-registers-and-types));
      or, 
  - a boolean condition.


Cmm expressions may include 


- a literal or a name (`CmmLit` contains both, see [Literals and Labels](commentary/compiler/cmm-type#iterals-and-labels), above);
- a memory reference (`CmmLoad` and `CmmReg`, see [Memory Access](commentary/compiler/cmm-type#emory-access), above);
- an operator (a `MachOp`, in `CmmMachOp`, below); or,
- another expression (a `[CmmExpr]`, in `CmmMachOp`, below).


These are all included as constructors in the `CmmExpr` data type, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

data CmmExpr
  = CmmLit CmmLit               -- Literal or Label (name)
  | CmmLoad CmmExpr MachRep     -- Read memory location (memory reference)
  | CmmReg CmmReg		-- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- operation (+, -, *, etc.)
  | CmmRegOff CmmReg Int
```



Note that `CmmRegOff reg i` is only shorthand for a specific `CmmMachOp` application:


```

CmmMachOp (MO_Add rep) [(CmmReg reg),(CmmLit (CmmInt i rep))]
	where rep = cmmRegRep reg
```



The function `cmmRegRep` is described below.  Note: the original comment following `CmmExpr` in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs) is erroneous (cf., `mangleIndexTree` in [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs)) but makes the same point described here.  The offset, `(CmmLit (CmmInt i rep))`, is a literal (`CmmLit`), not a name (`CLabel`).  A `CmmExpr` for an offset must be reducible to a `CmmInt` *in Haskell*; in other words, offsets in Cmm expressions may not be external symbols whose addresses are not resolvable in the current context.



Boolean comparisons are not boolean conditions.  Boolean comparisons involve relational operators, such as `>`, `<` and `==`, and map to `MachOp`s that are converted to comparison followed by branch instructions.  For example, `<` would map to `MO_S_Lt` for signed operands, [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs) would transform `MO_S_Lt` into the `LTT` constructor of the `Cond` union data type defined in [compiler/nativeGen/MachInstrs.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachInstrs.hs) and [compiler/nativeGen/PprMach.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PprMach.hs) would transform `LTT` to the distinguishing comparison type for an assembler comparison instruction.  You already know that the result of a comparison instruction is actually a change in the state of the Condition Register (CR), so Cmm boolean expressions do have a kind of side-effect but that is to be expected.  In fact, it is necessary since at the least a conditional expression becomes two assembler instructions, in PPC Assembler: 


```wiki
cmplwi   r3, 0  ; condition test
blt      Lch    ; branch instruction
```


This condition mapping does have an unfortunate consequence: conditional expressions do not fold into single instructions.  In Cmm, as in C--, expressions with relational operators may evaluate to an integral (`0`, nonzero) instead of evaluating to a boolean type.  For certain cases, such as an arithmetic operation immediately followed by a comparison, extended mnemonics such as `addi.` might eliminate the comparison instruction.  See [Cmm Design: Observations and Areas for Potential Improvement](commentary/compiler/cmm-type#mm-design:-observations-and-areas-for-potential-improvement) for more discussion and potential solutions to this situation.



Boolean conditions include: `&&`, `||`, `!` and parenthetical combinations of boolean conditions.  The `if expr { }` and `if expr { } else { }` statements contain boolean conditions.  The C-- type produced by conditional expressions is `bool`, in Cmm, type `BoolExpr` in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y):


```

data BoolExpr
  = BoolExpr `BoolAnd` BoolExpr
  | BoolExpr `BoolOr`  BoolExpr
  | BoolNot BoolExpr
  | BoolTest CmmExpr
```



The type `BoolExpr` maps to the `CmmCondBranch` or `CmmBranch` constructors of type `CmmStmt`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs), see [Statements and Calls](commentary/compiler/cmm-type#tatements-and-calls).



The `CmmExpr` constructor `CmmMachOp MachOp [CmmExpr]` is the core of every operator-based expression; the key here is `MachOp`, which in turn depends on the type of `MachRep` for each operand.  See [Fundamental and PrimitiveOperators](commentary/compiler/cmm-type#).  In order to process `CmmExpr`s, the data type comes with a deconstructor function to obtain the relevant `MachRep`s, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

cmmExprRep :: CmmExpr -> MachRep
cmmExprRep (CmmLit lit)      = cmmLitRep lit
cmmExprRep (CmmLoad _ rep)   = rep
cmmExprRep (CmmReg reg)      = cmmRegRep reg
cmmExprRep (CmmMachOp op _)  = resultRepOfMachOp op
cmmExprRep (CmmRegOff reg _) = cmmRegRep reg
```



The deconstructors `cmmLitRep` and `cmmRegRep` (with its supporting deconstructor `localRegRep`) are also defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs).



In PPC Assembler you might add two 32-bit integrals by:


```wiki
add	r3, r1, r2	; r3 = r1 + r2
```


while in Cmm you might write:


```wiki
res = first + second;
```


Remember that the assignment operator, `=`, is a statement since it has the "side effect" of modifying the value in `res`.  The `+` expression in the above statement, for a 32-bit architecture, would be represented in Haskell as:


```

CmmMachOp (MO_Add I32) [CmmReg (CmmLocal uniq I32), CmmReg (CmmLocal uniq I32)]
```



The `expr` production rule in the Cmm Parser [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) maps tokens to "values", such as `+` to an addition operation, `MO_Add`.  The `mkMachOp` function in the Parser determines the `MachOp` type in `CmmMachOp MachOp [CmmExpr]` from the token value and the `MachRep` type of the `head` variable.  Notice that the simple `+` operator did not contain sign information, only the `MachRep`.  For `expr`, signed and other `MachOps`, see the `machOps` function in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y).  Here is a table of operators and the corresponding `MachOp`s recognised by Cmm (listed in order of precedence):


<table><tr><th> **Operator** </th>
<th> **`MachOp`** 
</th></tr>
<tr><th> `/` </th>
<th> `MO_U_Quot` 
</th></tr>
<tr><th> `*` </th>
<th> `MO_Mul` 
</th></tr>
<tr><th> `%` </th>
<th> `MO_U_Rem` 
</th></tr>
<tr><th> `-` </th>
<th> `MO_Sub` 
</th></tr>
<tr><th> `+` </th>
<th> `MO_Add` 
</th></tr>
<tr><th> `>>` </th>
<th> `MO_U_Shr` 
</th></tr>
<tr><th> `<<` </th>
<th> `MO_Shl` 
</th></tr>
<tr><th> `&` </th>
<th> `MO_And` 
</th></tr>
<tr><th> `^` </th>
<th> `MO_Xor` 
</th></tr>
<tr><th> `|` </th>
<th> `MO_Or` 
</th></tr>
<tr><th> `>=` </th>
<th> `MO_U_Ge` 
</th></tr>
<tr><th> `>` </th>
<th> `MO_U_Gt` 
</th></tr>
<tr><th> `<=` </th>
<th> `MO_U_Le` 
</th></tr>
<tr><th> `<` </th>
<th> `MO_U_Lt` 
</th></tr>
<tr><th> `!=` </th>
<th> `MO_Ne` 
</th></tr>
<tr><th> `==` </th>
<th> `MO_Eq` 
</th></tr>
<tr><th> `~` </th>
<th> `MO_Not` 
</th></tr>
<tr><th> `-` </th>
<th> `MO_S_Neg` 
</th></tr></table>


#### Quasi-operator Syntax



If you read to the end of `expr` in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y), in the next production rule, `expr0`, you will notice that Cmm expressions also recognise a set of name (not symbol) based operators that would probably be better understood as *quasi-operators*.  The syntax for these quasi-operators is in some cases similar to syntax for Cmm statements and generally conform to the C-- specification, sections 3.3.2 (`expr`) and 7.4.1 (syntax of primitive operators), *except that* 3. *and, by the equivalence of the two,* 1. *may return* **multiple** * arguments*. In Cmm, quasi-operators may have side effects. The syntax for quasi-operators may be:


1. `expr0` ``name`` `expr0`
  (just like infix-functions in Haskell);
1. `type[ expression ]`
  (the memory access quasi-expression described in [Memory Access](commentary/compiler/cmm-type#emory-access); the Haskell representation of this syntax is `CmmLoad CmmExpr MachRep`); 
1. `%name( exprs0 )`
  (standard prefix form, similar to C-- *statement* syntax for procedures but with the distinguishing prefix `%`; in Cmm this is *also used as statement syntax for calls, which are really built-in procedures*, see [Cmm Calls](commentary/compiler/cmm-type#mm-calls)) 


A `expr0` may be a literal (`CmmLit`) integral, floating point, string or a `CmmReg` (the production rule `reg`: a `name` for a local register (`LocalReg`) or a `GlobalReg`).



Note that the `name` in `expr0` syntax types 1. and 3. must be a known *primitive* (primitive operation), see [Operators and Primitive Operations](commentary/compiler/cmm-type#perators-and-primitive-operations).  The first and third syntax types are interchangeable:


```wiki
bits32 one, two, res;
one = 1::bits32;
two = 2::bits32;

res = one `lt` two;

// is equivalent to:

res = %lt(one, two);
```


The primitive operations allowed by Cmm are listed in the `machOps` production rule, in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y), and largely correspond to `MachOp` data type constructors, in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs), with a few additions.  The primitive operations distinguish between signed, unsigned and floating point types.



Cmm adds some expression macros that map to Haskell Cmm functions.  They are listed under `exprMacros` in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) and include:


- `ENTRY_CODE`
- `INFO_PTR`
- `STD_INFO`
- `FUN_INFO`
- `GET_ENTRY`
- `GET_STD_INFO`
- `GET_FUN_INFO`
- `INFO_TYPE`
- `INFO_PTRS`
- `INFO_NPTRS`
- `RET_VEC`

### Statements and Calls



Cmm Statements generally conform to the C-- specification, with a few exceptions noted below.  Cmm Statements implement:


- no-op; the empty statement: `;`
- C-- (C99/C++ style) comments: `// ... \n` and `/* ... */`
- the assignment operator: `=`
- store operation (assignment to a memory location): `type[expr] =`
- control flow within procedures (`goto`) and between procedures (`jump`, returns) (note: returns are *only* Cmm macros)
- foreign calls (`foreign "C" ...`) and calls to Cmm Primitive Operations (`%`)
- procedure calls and tail calls
- conditional statement (`if ... { ... } else { ... }`)
- tabled conditional (`switch`)


Cmm does not implement the C-- specification for Spans (sec. 6.1) or Continuations (sec. 6.7).

Although Cmm supports primitive operations that may have side effects (see [Primitive Operations](commentary/compiler/cmm-type#rimitive-operations), below), it does not parse the syntax `%%` form mentioned in section 6.3 of the C-- specification.  Use the `%name(arg1,arg2)` expression-syntax instead.  

Cmm does not implement the `return` statement (C-- spec, sec. 6.8.2) but provides a set of macros that return a list of tuples of a `CgRep` and a `CmmExpr`: `[(CgRep,CmmExpr)]`.  For a description of `CgRep`, see comments in [compiler/codeGen/SMRep.lhs](/trac/ghc/browser/ghc/compiler/codeGen/SMRep.lhs).  The return macros are defined at the end of the production rule `stmtMacros` in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y):


- `RET_P`
- `RET_N`
- `RET_PP`
- `RET_NN`
- `RET_NP`
- `RET_PPP`
- `RET_NNP`
- `RET_NNNP`
- `RET_NPNP`


In the above macros, `P` stands for `PtrArg` and `N` stands for `NonPtrArg`; both are `CgRep` constructors.  These return macros provide greater control for the [CodeGen](commentary/compiler/code-gen) and integrate with the RTS but limit the number and type of return arguments in Cmm: you may only return according to these macros!  The returns are processed by the `emitRetUT` function in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y), which in turn calls several functions from [compiler/codeGen/CgMonad.lhs](/trac/ghc/browser/ghc/compiler/codeGen/CgMonad.lhs), notably `emitStmts`, which is the core Code Generator function for emitting `CmmStmt` data.



The Haskell representation of Cmm Statements is the data type `CmmStmt`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):


```

data CmmStmt
  = CmmNop
  | CmmComment FastString

  | CmmAssign CmmReg CmmExpr	 -- Assign to register

  | CmmStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | CmmCall	 		 -- A foreign call, with 
     CmmCallTarget
     [(CmmReg,MachHint)]	 -- zero or more results
     [(CmmExpr,MachHint)]	 -- zero or more arguments
     (Maybe [GlobalReg])	 -- Global regs that may need to be saved
				 -- if they will be clobbered by the call.
				 -- Nothing <=> save *all* globals that
				 -- might be clobbered.

  | CmmBranch BlockId             -- branch to another BB in this fn

  | CmmCondBranch CmmExpr BlockId -- conditional branch

  | CmmSwitch CmmExpr [Maybe BlockId]   -- Table branch
	-- The scrutinee is zero-based; 
	--	zero -> first block
	--	one  -> second block etc
	-- Undefined outside range, and when there's a Nothing

  | CmmJump CmmExpr [LocalReg]    -- Jump to another function, with these 
				  -- parameters.
```



Note how the constructor `CmmJump` contains `[LocalReg]`: this is the Cmm implementation of the C-- `jump` statement for calling another procedure where the parameters are the arguments passed to the other procedure. None of the parameters contain the address--in assembler, a label--of the caller, to return control to the caller.  The `CmmCall` constructor also lacks a parameter to store the caller's address.  Cmm implements C-- jump nesting and matching returns by *tail calls*, as described in section 6.8 of the C-- specification.  Tail calls are managed through the [CodeGen](commentary/compiler/code-gen), see [compiler/codeGen/CgTailCall.lhs](/trac/ghc/browser/ghc/compiler/codeGen/CgTailCall.lhs).  You may have already noticed that the call target of the `CmmJump` is a `CmmExpr`: this is the Cmm implementation of computed procedure addresses, for example:


```wiki
proc1 {
...

 jump (bits32[x+4])( ... );

}
```


The computed procedure address, in this case `(bits32[x+4])`, should always be the first instruction of a `Cmm` procedure.  You cannot obtain the address of a code block *within* a procedure and `jump` to it, as an alternative way of computing a *continuation*.  



`CmmBranch BlockId` represents an unconditional branch to another [Basic Block](commentary/compiler/cmm-type#asic-blocks-and-procedures) in the same procedure.  There are two unconditional branches in Cmm/C--:


1. `goto` statement; and
1. a branch from the `else` portion of an `if-then-else` statement.


`CmmCondBranch CmmExpr BlockId` represents a conditional branch to another [Basic Block](commentary/compiler/cmm-type#asic-blocks-and-procedures) in the same procedure.  This is the `if expr` statement where `expr` is a `CmmExpr`, used in both the unary `if` and `if-then-else` statements.  `CmmCondBranch` maps to more complex Assembler instruction sets or HC code ([compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs)).  For assembler, labels are created for each new Basic Block.  During parsing, conditional statements map to the `BoolExpr` data type which guides the encoding of assembler instruction sets.



`CmmSwitch` represents the `switch` statement.  It is parsed and created as with the `doSwitch` function in [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) or created from `case` expressions with the `emitSwitch` and `mk_switch` functions in [compiler/codeGen/CgUtils.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgUtils.hs).  In the NCG, a `CmmSwitch` is generated as a jump table using the `genSwitch` function in [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs).  There is currently no implementation of any optimisations, such as a cascade of comparisons for switches with a wide deviation in values or binary search for very wide value ranges--for output to HC, earlier versions of GCC could not handle large if-trees, anyway.


#### Cmm Calls



Cmm calls include both calls to foreign functions and calls to Cmm quasi-operators using expression syntax (see [Quasi-operator Syntax](commentary/compiler/cmm-type#uasi-operator-syntax)). Although Cmm does not implement any of the control flow statements of C-- specification (section 6.8.1), foreign calls from Cmm are one of the most complex components of the system due to various differences between the Cmm and C calling conventions.



The data type, `CmmCallTarget` is defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs) as:


```

data CmmCallTarget
  = CmmForeignCall		-- Call to a foreign function
	CmmExpr 		-- literal label <=> static call
				-- other expression <=> dynamic call
	CCallConv		-- The calling convention

  | CmmPrim			-- Call to a "primitive" (eg. sin, cos)
	CallishMachOp		-- These might be implemented as inline
				-- code by the backend.
```



`CCallConv` is defined in [compiler/prelude/ForeignCall.hs](/trac/ghc/browser/ghc/compiler/prelude/ForeignCall.hs); for information on register assignments, see comments in [compiler/codeGen/CgCallConv.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgCallConv.hs).



`CallishMachOp` is defined in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs); see, also, below [Primitive Operations](commentary/compiler/cmm-type#rimitive-operations).  `CallishMachOp`s are generally used for floating point computations (without implementing any floating point exceptions).  Here is an example of using a `CallishMachOp` (not yet implemented):


```wiki
  add, carry = %addWithCarry(x, y);
```

### Operators and Primitive Operations



Cmm generally conforms to the C-- specification for operators and "primitive operations".  The C-- specification, in section 7.4, refers to both of these as "primitive operations" but there are really two different types: 


- *operators*, as I refer to them, are: 

  - parseable tokens, such as `+`,`-`,`*` or `/`; 
  - generally map to a single machine instruction or part of a machine instruction;
  - have no side effects; and, 
  - are represented in Haskell using the `MachOp` data type; 
- *primitive operations* (Cmm *quasi-operators*) are special, usually inlined, procedures, represented in Haskell using the `CallishMachOp` data type; primitive operations may have side effects.


The `MachOp` and `CallishMachOp` data types are defined in [compiler/cmm/CmmMachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmMachOp.hs).



Both Cmm Operators and Primitive Operations are handled in Haskell as [Inline PrimOps](commentary/prim-ops#), though what I am calling Cmm *primitive operations* may be implemented as out-of-line foreign calls.


#### Operators


```

data MachOp

  -- Integer operations
  = MO_Add    MachRep
  | MO_Sub    MachRep
  | MO_Eq     MachRep
  | MO_Ne     MachRep
  | MO_Mul    MachRep		-- low word of multiply
  | MO_S_MulMayOflo MachRep 	-- nonzero if signed multiply overflows
  | MO_S_Quot MachRep		-- signed / (same semantics as IntQuotOp)
  | MO_S_Rem  MachRep		-- signed % (same semantics as IntRemOp)
  | MO_S_Neg  MachRep		-- unary -
  | MO_U_MulMayOflo MachRep	-- nonzero if unsigned multiply overflows
  | MO_U_Quot MachRep		-- unsigned / (same semantics as WordQuotOp)
  | MO_U_Rem  MachRep		-- unsigned % (same semantics as WordRemOp)

  -- Signed comparisons (floating-point comparisons also use these)
  | MO_S_Ge MachRep
  | MO_S_Le MachRep
  | MO_S_Gt MachRep
  | MO_S_Lt MachRep

  -- Unsigned comparisons
  | MO_U_Ge MachRep
  | MO_U_Le MachRep
  | MO_U_Gt MachRep
  | MO_U_Lt MachRep

  -- Bitwise operations.  Not all of these may be supported at all sizes,
  -- and only integral MachReps are valid.
  | MO_And   MachRep
  | MO_Or    MachRep
  | MO_Xor   MachRep
  | MO_Not   MachRep
  | MO_Shl   MachRep
  | MO_U_Shr MachRep	-- unsigned shift right
  | MO_S_Shr MachRep	-- signed shift right

  -- Conversions.  Some of these will be NOPs.
  -- Floating-point conversions use the signed variant.
  | MO_S_Conv MachRep{-from-} MachRep{-to-}	-- signed conversion
  | MO_U_Conv MachRep{-from-} MachRep{-to-}	-- unsigned conversion
```



Each `MachOp` generally corresponds to a machine instruction but may have its value precomputed in the Cmm, NCG or HC optimisers.  


#### Primitive Operations



Primitive Operations generally involve more than one machine instruction and may not always be inlined.  


```

-- These MachOps tend to be implemented by foreign calls in some backends,
-- so we separate them out.  In Cmm, these can only occur in a
-- statement position, in contrast to an ordinary MachOp which can occur
-- anywhere in an expression.
data CallishMachOp
  = MO_F64_Pwr
  | MO_F64_Sin
  | MO_F64_Cos
  | MO_F64_Tan
  | MO_F64_Sinh
  | MO_F64_Cosh
  | MO_F64_Tanh
  | MO_F64_Asin
  | MO_F64_Acos
  | MO_F64_Atan
  | MO_F64_Log
  | MO_F64_Exp
  | MO_F64_Sqrt
  | MO_F32_Pwr
  | MO_F32_Sin
  | MO_F32_Cos
  | MO_F32_Tan
  | MO_F32_Sinh
  | MO_F32_Cosh
  | MO_F32_Tanh
  | MO_F32_Asin
  | MO_F32_Acos
  | MO_F32_Atan
  | MO_F32_Log
  | MO_F32_Exp
  | MO_F32_Sqrt
  | MO_WriteBarrier
  | MO_Touch         -- Keep variables live (when using interior pointers)
  
  -- Note that these three MachOps all take 1 extra parameter than the
  -- standard C lib versions. The extra (last) parameter contains
  -- alignment of the pointers. Used for optimisation in backends.
  | MO_Memcpy
  | MO_Memset
  | MO_Memmove
```



For an example, the floating point sine function, `sinFloat#` in [compiler/prelude/primops.txt.pp](/trac/ghc/browser/ghc/compiler/prelude/primops.txt.pp) is piped through the `callishOp` function in [compiler/codeGen/CgPrimOp.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgPrimOp.hs) to become `Just MO_F32_Sin`.  The `CallishMachOp` constructor `MO_F32_Sin` is piped through a platform specific function such as [compiler/nativeGen/X86/CodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/X86/CodeGen.hs) on X86, where the function `genCCall` will call `outOfLineFloatOp` to issue a call to a C function such as `sin`.


## Cmm Design: Observations and Areas for Potential Improvement



"If the application of a primitive operator causes a system exception, such as division by zero, this is an unchecked run-time error. (A future version of this specification may provide a way for a program to recover from such an exception.)" C-- spec, Section 7.4.  Cmm may be able to implement a partial solution to this problem, following the paper: [
A Single Intermediate Language That Supports Multiple Implementations of Exceptions (2000)](http://cminusminus.org/abstracts/c--pldi-00.html).  (TODO write notes to wiki and test fix.)



The IEEE 754 specification for floating point numbers defines exceptions for certain floating point operations, including: 


- range violation (overflow, underflow); 
- rounding errors (inexact); 
- invalid operation (invalid operand, such as comparison with a `NaN` value, the square root of a negative number or division of zero by zero); and,
- zero divide (a special case of an invalid operation).  


Many architectures support floating point exceptions by including a special register as an addition to other exception handling registers.  The IBM PPC includes the `FPSCR` ("Floating Point Status Control Register"); the Intel x86 processors use the `MXCSR` register.  When the PPC performs a floating point operation it checks for possible errors and sets the `FPSCR`.  Some processors allow a flag in the Foating-Point Unit (FPU) status and control register to be set that will disable some exceptions or the entire FPU exception handling facility.  Some processors disable the FPU after an exception has occurred while others, notably Intel's x86 and x87 processors, continue to perform FPU operations.  Depending on whether quiet NaNs (QNaNs) or signaling NaNs (SNaNs) are used by the software, an FPU exception may signal an interrupt for the software to pass to its own exception handler.  



Some higher level languages provide facilities to handle these exceptions, including Ada, Fortran (F90 and later), C++ and C (C99, fenv.h, float.h on certain compilers); others may handle such exceptions without exposing a low-level interface.  There are three reasons to handle FPU exceptions, and these reasons apply similarly to other exceptions: 


- the facilities provide greater control; 
- the facilities are efficient--more efficient than a higher-level software solution; and, 
- FPU exceptions may be unavoidable, especially if several FPU operations are serially performed at the machine level so the higher level software has no opportunity to check the results in between operations. 


A potential solution to the problem of implementing Cmm exceptions, especially for floating point operations, is at [Cmm: Implementing Exception Handling](commentary/cmm-exceptions).  



The C-- Language Specification mentions over 75 primitive operators.  The Specification lists separate operators for integral and floating point (signed) arithmetic (including carry, borrow and overflow checking), logical comparisons and conversions (from one size float to another, from float to integral and vice versa, etc.).  C-- also includes special operators for floating point number values, such as `NaN`, `mzero`*k* and `pzero`*k*, and rounding modes; integral kinds also include bitwise operators, unsigned variants, and bit extraction for width changing and sign or zero-extension.  A C-- implementation may conveniently map each of these operators to a machine instruction, or to a simulated operation on architectures that do not support a single instruction.  There seem to be two main problems with the current GHC-implementation of Cmm:


1. not enough operators
1. no implementation of vector (SIMD) registers (though there is a `I128` `MachRep`)


If a particular architecture supports it, assembler includes instructions such as mnemonics with the `.` ("dot") suffix (`add., fsub.`), which set the Condition Register (CR) thereby saving you at least one instruction.  (Extended mnemonics can save you even more.)  Extended mnemonics with side effects may be implemented as new `CallishMachOps`, see [Primitive Operations](commentary/compiler/cmm-type#rimitive-operations) and [Cmm Calls](commentary/compiler/cmm-type#mm-calls).  Assembler also supports machine exceptions, especially exceptions for floating-point operations, invalid storage access or misalignment (effective address alignment).  The current implementation of Cmm cannot model such exceptions through flow control because no flow control is implemented, see [Cmm Calls](commentary/compiler/cmm-type#mm-calls).



Hiding the kinds of registers on a machine eliminates the ability to handle floating point exceptions at the Cmm level and to explicitly vectorize (use SIMD extensions).  The argument for exposing vector types may be a special case since such low-level operations are exposed at the C-level, as new types of variables or "intrinsics," that are C-language extensions provided by special header files and compiler support (`vector unsigned int` or `__m128i`, `vector float` or `__m128`) and operations (`vec_add()`, `+` (with at least one vector operand), `_mm_add_epi32()`).  


