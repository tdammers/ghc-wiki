## Why?



To make arithmetic safer: [
http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103](http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103)


## What's already there?


- `Data.SafeInt` uses these primops, which only work on `Int`s: `addIntC#`, `subIntC#`, `mulIntMayOflo#`.
- `plusWord2#` and `timesWord2#` in `compiler/prelude/primops.txt.pp`, but no primop for subtraction (see below).
- `maxInt#` and `minInt#` exist in `libraries/base/GHC/Base.hs`, but not `maxWord#` or `maxInt64#`, etc.
- In `libraries/integer-gmp/src/GHC/Integer/Type.hs`, there's `subWordC# :: Word# -> Word# -> (# Word#, Int# #)`
  defined as a helper, which should be replaced by a proper primop.

## TODO


- Add `subWordC#` as a primop.  DONE [
  https://github.com/ghc/ghc/commit/8160f42b8dad33e47b4c73ed3f9bf889462e7bfe](https://github.com/ghc/ghc/commit/8160f42b8dad33e47b4c73ed3f9bf889462e7bfe)
- Add overflow-aware `absInt#` (for the `abs minBound` case).
- Add overflow-aware shifts for `Int#` and `Word#`.
- Maybe look at the bottom of `libraries/integer-gmp/src/GHC/Integer/Type.hs` for inspiration.

## How do I add a new primop?



See the guide at [
https://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps\#AddinganewPrimOp](https://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps#AddinganewPrimOp)



I need to add *inline* primops since `addIntC#` & co. are all inline.
(Just use the same attributes, but make sure to read on each of them.)
So, I only need to touch these files:


- `compiler/prelude/primops.txt.pp`
- `compiler/codeGen/StgCmmPrim.hs`


There's also a tutorial on adding an *out-of-line* primop, but some
bits of it may be useful (e.g., building GHC after making changes):
[
https://ghc.haskell.org/trac/ghc/wiki/AddingNewPrimitiveOperations](https://ghc.haskell.org/trac/ghc/wiki/AddingNewPrimitiveOperations)


## Where is C-- generated?



For `addIntC#`, it's done in the already mentioned
`compiler/codeGen/StgCmmPrim.hs` file:


```wiki
callishPrimOpSupported :: DynFlags -> PrimOp -> Either CallishMachOp GenericOp
callishPrimOpSupported dflags op
  = case op of
      ...
      IntAddCOp      | (ncg && x86ish)
                         || llvm      -> Left (MO_AddIntC    (wordWidth dflags))
                     | otherwise      -> Right genericIntAddCOp
```

```wiki
-- XXX: Modules required for pretty-printing.
import BlockId
import UniqSupply
import System.IO.Unsafe


emitPrimOp dflags results op args
   = case callishPrimOpSupported dflags op of
          Left op   -> -- emit $ mkUnsafeCall (PrimTarget op) results args
                       -- XXX: Always print C-- when compiling.
                       emit $
                         pprTrace
                           "emitPrimOp:"
                           (ppr $
                              labelAGraph
                                (mkBlockId $ uniqFromSupply $ unsafePerformIO $ mkSplitUniqSupply 'a')
                                ( mkUnsafeCall (PrimTarget op) results args
                                , GlobalScope ) )
                           (mkUnsafeCall (PrimTarget op) results args)
          Right gen -> gen results args
```


Test module:


```wiki
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Test where

import GHC.Prim

f x = addIntC# 2# x
```


Output:


```wiki
$ ghc/inplace/bin/ghc-stage2 --make Test.hs
[1 of 1] Compiling Test             ( Test.hs, Test.o )
emitPrimOp:
  {offset
    avf:
        (_cvc::I64, _cvd::I64) = call MO_AddIntC W64(2, _sv7::I64);
        goto avf;
  }
```


Another option is to use `-ddump-cmm`, which doesn't require touching
the compiler, but it generates more code.


## Where is asm generated?



For x86, it's `compiler/nativeGen/X86/CodeGen.hs`:


```wiki
genCCall _ is32Bit target dest_regs args = do
    ...
    (PrimTarget (MO_AddIntC width), [res_r, res_c]) ->
        addSubIntC platform ADD_CC (Just . ADD_CC) width res_r res_c args
    ...
  where
        addSubIntC platform instr mrevinstr width res_r res_c [arg_x, arg_y]
            = do let format = intFormat width
                 rCode <- anyReg =<< trivialCode width (instr format)
                                       (mrevinstr format) arg_x arg_y
                 reg_tmp <- getNewRegNat II8
                 let reg_c = getRegisterReg platform True (CmmLocal res_c)
                     reg_r = getRegisterReg platform True (CmmLocal res_r)
                     code = rCode reg_r `snocOL`
                            SETCC OFLO (OpReg reg_tmp) `snocOL`
                            MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)
                 return code
        addSubIntC _ _ _ _ _ _ _
            = panic "genCCall: Wrong number of arguments/results for addSubIntC"
```


Testing on the same file (some things are omitted):


```wiki
$ ghc --make -ddump-asm Test.hs
[1 of 1] Compiling Test             ( Test.hs, Test.o )

==================== Asm code ====================
.text
	.align 8
	.quad	4294967300
	.quad	0
	.quad	15
.globl Test.f_info
.type Test.f_info, @object
Test.f_info:
_cuF:
_cuH:
	addq $2,%r14
	seto %al
	movzbl %al,%eax
	movq %r14,%rbx
	movq %rax,%r14
	jmp *(%rbp)
	.size Test.f_info, .-Test.f_info
```

## What do I need to know about GHC types?



The "The word size story" section in `primops.txt.pp` provides a good overview.


## Where are the tests for the `Int` primops?



`grep -rniI --exclude=*.html addIntC testsuite`


## How do I expose my new primops as ordinary functions?



Just define `Num` instances in the safeint package as it's done for `SafeInt`.
(There could be an alternative class for things returning an `Either`, but first I'll just add a version that fails at runtime.)


## Asm from `subWordC#` vs. asm from a user-defined overflow-checking function


### `subWordC#`



Test module:


```wiki
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module SubWordC where

import GHC.Base

foo x = subWordC# 1##
```


x86\_64:


```wiki
[1 of 1] Compiling SubWordC         ( SubWordC.hs, SubWordC.o )

==================== Asm code ====================
.data
        .align 8
.align 1
.globl __stginit_main@main:SubWordC
.type __stginit_main@main:SubWordC, @object
__stginit_main@main:SubWordC:



==================== Asm code ====================
.data
        .align 8
.align 1
.globl SubWordC.foo_closure
.type SubWordC.foo_closure, @object
SubWordC.foo_closure:
        .quad   SubWordC.foo_info



==================== Asm code ====================
.text
        .align 8
        .quad   8589934606
        .quad   0
        .quad   15
.globl SubWordC.foo_info
.type SubWordC.foo_info, @object
SubWordC.foo_info:
_cv7:
_cv9:
        movl $1,%eax
        subq %rsi,%rax
        setc %bl
        movzbl %bl,%ebx
        movq %rbx,%r14
        movq %rax,%rbx
        jmp *(%rbp)
        .size SubWordC.foo_info, .-SubWordC.foo_info



==================== Asm code ====================
.section .data
        .align 8
.align 1
Svn_srt:


```


x86:


```wiki
[1 of 1] Compiling SubWordC         ( SubWordC.hs, SubWordC.o )

==================== Asm code ====================
.data
        .align 4
.align 1
.globl __stginit_main@main:SubWordC
.type __stginit_main@main:SubWordC, @object
__stginit_main@main:SubWordC:



==================== Asm code ====================
.data
        .align 4
.align 1
.globl SubWordC.foo_closure
.type SubWordC.foo_closure, @object
SubWordC.foo_closure:
        .long   SubWordC.foo_info



==================== Asm code ====================
.text
        .align 4,0x90
        .long   131086
        .long   0
        .long   15
.globl SubWordC.foo_info
.type SubWordC.foo_info, @object
SubWordC.foo_info:
_cvj:
        movl (%ebp),%eax
        movl 4(%ebp),%eax
_cvl:
        movl $1,%ecx
        subl %eax,%ecx
        setc %al
        movzbl %al,%eax
        movl %ecx,%esi
        movl %eax,4(%ebp)
        addl $4,%ebp
        jmp *4(%ebp)
        .size SubWordC.foo_info, .-SubWordC.foo_info



==================== Asm code ====================
.section .data
        .align 4
.align 1
Svz_srt:


```

### User-defined overflow-checking function



Test modules:



MySubWordC.hs:


```wiki
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module MySubWordC where

import GHC.Base
import GHC.Prim

subWordC :: Word# -> Word# -> (# Word#, Int# #)
subWordC x y = (# minusWord# x y, gtWord# y x #)
```


MySubWordCTest.hs:


```wiki
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module MySubWordCTest where

import MySubWordC

foo x = subWordC 1##
```


x86\_64:


```wiki
[1 of 2] Compiling MySubWordC       ( MySubWordC.hs, MySubWordC.o )

==================== Asm code ====================
.data
        .align 8
.align 1
.globl __stginit_main@main:MySubWordC
.type __stginit_main@main:MySubWordC, @object
__stginit_main@main:MySubWordC:



==================== Asm code ====================
.data
        .align 8
.align 1
.globl MySubWordC.subWordC_closure
.type MySubWordC.subWordC_closure, @object
MySubWordC.subWordC_closure:
        .quad   MySubWordC.subWordC_info



==================== Asm code ====================
.text
        .align 8
        .quad   8589934604
        .quad   0
        .quad   15
.globl MySubWordC.subWordC_info
.type MySubWordC.subWordC_info, @object
MySubWordC.subWordC_info:
_cv9:
_cvb:
        cmpq %r14,%rsi
        seta %al
        movzbl %al,%eax
        subq %rsi,%r14
        movq %r14,%rbx
        movq %rax,%r14
        jmp *(%rbp)
        .size MySubWordC.subWordC_info, .-MySubWordC.subWordC_info



==================== Asm code ====================
.section .data
        .align 8
.align 1
Svl_srt:


[2 of 2] Compiling MySubWordCTest   ( MySubWordCTest.hs, MySubWordCTest.o )

==================== Asm code ====================
.data
        .align 8
.align 1
.globl __stginit_main@main:MySubWordCTest
.type __stginit_main@main:MySubWordCTest, @object
__stginit_main@main:MySubWordCTest:



==================== Asm code ====================
.data
        .align 8
.align 1
.globl MySubWordCTest.foo_closure
.type MySubWordCTest.foo_closure, @object
MySubWordCTest.foo_closure:
        .quad   MySubWordCTest.foo_info
        .quad   0



==================== Asm code ====================
.text
        .align 8
        .long   SvL_srt-(MySubWordCTest.foo_info)+0
        .long   0
        .quad   4294967301
        .quad   0
        .quad   4294967311
.globl MySubWordCTest.foo_info
.type MySubWordCTest.foo_info, @object
MySubWordCTest.foo_info:
_cvI:
_cvK:
        movl $1,%r14d
        movl $MySubWordC.subWordC_closure,%ebx
        jmp stg_ap_n_fast
        .size MySubWordCTest.foo_info, .-MySubWordCTest.foo_info



==================== Asm code ====================
.section .data
        .align 8
.align 1
SvL_srt:
        .quad   MySubWordC.subWordC_closure


```


x86:


```wiki
[1 of 2] Compiling MySubWordC       ( MySubWordC.hs, MySubWordC.o )

==================== Asm code ====================
.data
        .align 4
.align 1
.globl __stginit_main@main:MySubWordC
.type __stginit_main@main:MySubWordC, @object
__stginit_main@main:MySubWordC:



==================== Asm code ====================
.data
        .align 4
.align 1
.globl MySubWordC.subWordC_closure
.type MySubWordC.subWordC_closure, @object
MySubWordC.subWordC_closure:
        .long   MySubWordC.subWordC_info



==================== Asm code ====================
.text
        .align 4,0x90
        .long   131084
        .long   0
        .long   15
.globl MySubWordC.subWordC_info
.type MySubWordC.subWordC_info, @object
MySubWordC.subWordC_info:
_cvl:
        movl (%ebp),%eax
        movl 4(%ebp),%ecx
_cvn:
        cmpl %eax,%ecx
        seta %dl
        movzbl %dl,%edx
        subl %ecx,%eax
        movl %eax,%esi
        movl %edx,4(%ebp)
        addl $4,%ebp
        jmp *4(%ebp)
        .size MySubWordC.subWordC_info, .-MySubWordC.subWordC_info



==================== Asm code ====================
.section .data
        .align 4
.align 1
Svx_srt:


[2 of 2] Compiling MySubWordCTest   ( MySubWordCTest.hs, MySubWordCTest.o )

==================== Asm code ====================
.data
        .align 4
.align 1
.globl __stginit_main@main:MySubWordCTest
.type __stginit_main@main:MySubWordCTest, @object
__stginit_main@main:MySubWordCTest:



==================== Asm code ====================
.data
        .align 4
.align 1
.globl MySubWordCTest.foo_closure
.type MySubWordCTest.foo_closure, @object
MySubWordCTest.foo_closure:
        .long   MySubWordCTest.foo_info
        .long   0



==================== Asm code ====================
.text
        .align 4,0x90
        .long   SvX_srt-(MySubWordCTest.foo_info)+0
        .long   65541
        .long   0
        .long   65551
.globl MySubWordCTest.foo_info
.type MySubWordCTest.foo_info, @object
MySubWordCTest.foo_info:
_cvU:
        movl (%ebp),%eax
_cvW:
        movl $MySubWordC.subWordC_closure,%esi
        movl $1,(%ebp)
        jmp stg_ap_n_fast
        .size MySubWordCTest.foo_info, .-MySubWordCTest.foo_info



==================== Asm code ====================
.section .data
        .align 4
.align 1
SvX_srt:
        .long   MySubWordC.subWordC_closure


```