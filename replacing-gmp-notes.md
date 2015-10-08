# Replacing GMP: Bignum libraries, Licensing and Implementation


#### Table of Contents



On this page:


1. [Current Status](replacing-gmp-notes#urrent-status)
1. [Introduction](replacing-gmp-notes#ntroduction)
1. [Reasons for Replacing GMP as the Bignum library](replacing-gmp-notes#easons-for-replacing-gmp-as-the-bignum-library)
1. [Files related to GMP in the GHC Compiler Source Code](replacing-gmp-notes#iles-related-to-gmp-in-the-ghc-compiler-source-code)
1. [Optimisation Opportunities](replacing-gmp-notes#ptimisation-opportunities)
1. [Binary Drop in Replacement for GMP](replacing-gmp-notes#inary-drop-in-replacement-for-gmp)


Other pages


- [Notes about replacing GMP with a native Haskell library](replacing-gmp-notes/haskell-library)
- [The Current GMP Implementation](replacing-gmp-notes/the-current-gmp-implementation)
  A detailed description of the current interface and interaction between GHC's [Runtime System (RTS)](commentary/rts) and GMP.
- [Performance Measurements of other Multi-Precision Libraries](replacing-gmp-notes/performance-measurements)
- Miscellaneous GMP Discussion?
- Design Discussion?
- [Required Integer Functions](replacing-gmp-notes/required-integer-functions)
- Integer Function Design (C library)?
- Replacement Library Integration?

### Current Status



The `Integer` type is now provided by a separate `integer` package, which provides an API that hides the implementation details. By default this is `integer-gmp`. To change it, set `INTEGER_LIBRARY=integer-foo` in `mk/build.mk`.



There is an alternative implementation [
integer-simple](http://git.haskell.org/packages/integer-simple.git), although as we don't regularly test builds with it you may need to make a few tweaks to get it to work. `integer-simple` is intended to be easily understood, entirely Haskell code that is *fast enough*. For serious number crunching one of the highly tuned big integer libraries will be needed, but hopefully `integer-simple` will suffice for normal use. In order to test this, we need to do some testing, e.g. nofib runs.



It would also be interesting to separate out the `J#/S#` wrapper from the GMP `Integer`, and to compare all 4 combinations: `GMP`, `GMP+J#/S#`, `simple`, `simple+S#/J#`.



If `integer-simple` is indeed fast enough, then I think that it solves all of the problems with `integer-gmp`. We would also have packages like `gmp` for those who want to use the fast C implementations.


### Introduction



This task was started following [
Task \#601](http://hackage.haskell.org/trac/ghc/ticket/601), while these Notes were requested by [
Simon Peyton-Jones](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010676.html).



GHC currently implements the Integer and Fractional types by using the [
The GNU MP Bignum Library](http://swox.com/gmp/) (GMP) which supports arbitrary precision mathematical calculations.  GMP is fast, memory efficient, and offers many high level signed integer functions (140 of them), as well as many rational and floating point arithmetic functions.  The current GHC implementation only uses those functions necessary for the Prelude.  



GMP memory is integrated with the [RunTime System's](commentary/rts) (RTS's) [Storage Manager](commentary/rts/storage) (SM)--the RTS's Garbage Collector (GC).  GMP memory is allocated from the GC heap, so values produced by GMP are under the control of the RTS and its GC.  The current implementation is memory efficient while allowing the RTS and its GC to maintain control of GMP evaluations.



If you want to help with replacing GMP or do it yourself, you will have to work with the GC and RTS system.  The parts you will have to modify are written in C and C--, with configuration and assembly done through the Makefiles.  You should have an understanding of:


- how the GC works and how memory from GMP is integrated with it;
- some C--/Cmm (this is fairly basic if you know C well, though the same adage for knowing C well holds for C--: if you know Assembler well enough to  understand and debug C in it you will be much better off), the only real documentation on C-- itself is in the [
  C-- manual (PDF)](http://cminusminus.org/extern/man2.pdf), from cminusminus.org; the implementation of C-- for GHC is performed by several Haskell modules in the directory [compiler/cmm/](/trac/ghc/browser/ghc/compiler/cmm/) of the HEAD branch, see [
  http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc)), and see [the new Commentary Cmm page](commentary/compiler/cmm-type); and,
- makefiles and configuration scripts.


A guide to GHC primitives is available (in an unformatted version) in [/compiler/prelude/primops.txt.pp](/trac/ghc/browser/ghc//compiler/prelude/primops.txt.pp); there is a formatted version (from the latest build) at [http://www.haskell.org/ghc/dist/current/docs/libraries/base/GHC-Prim.html](http://www.haskell.org/ghc/dist/current/docs/libraries/base/GHC-Prim.html).  (See [The (new) GHC Commentary](commentary) [PrimOps](commentary/prim-ops) page for an excellent description of how primitive operations are implemented.  A highly recommended introduction directly related to GMP is [AddingNewPrimitiveOperations](adding-new-primitive-operations).) In primops.txt.pp--better yet, [GHC.Prim](http://www.haskell.org/ghc/dist/current/docs/libraries/base/GHC-Prim.html)--you might want to search for the text `"section "The word size story.""`, and especially the text `"section "Integer#""` or just go to [The word size story](http://www.haskell.org/ghc/dist/current/docs/libraries/base/GHC-Prim.html#1) and [Integer](http://www.haskell.org/ghc/dist/current/docs/libraries/base/GHC-Prim.html#8).   The Haskell definition of the Integer data type is in [
/packages/base/GHC/Num.lhs](http://darcs.haskell.org/packages/base/GHC/Num.lhs).



Other basic recommended reading is:


- [
  The (old) GHC Commentary](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/): [
  The Native Code Generator](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/the-beast/ncg.html); and,
- [
  The (old) GHC Commentary](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/): [
  Style Guidelines for RTS C code](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/rts-libs/coding-style.html) or [The (new) GHC Commentary](commentary): [Style Conventions for RTS C Code](commentary/rts/conventions).

#### *Caveat*


>
>
> Beware!  The main interest here is replacing GMP--GHC is still belongs to the University of Glasgow and those in charge still retain the purview to accept or reject a proposed solution.
>
>

### Reasons for Replacing GMP as the Bignum library



There are several problems with the current GMP implementation:


1. Licensing

>
>
> GMP is licensed under the [
> GNU Lesser General Public License](http://www.gnu.org/copyleft/lesser.html) (LGPL), a kind of "copyleft" license.  According to the terms of the LGPL, paragraph 5, you may distribute a program that is designed to be compiled and dynamically linked with the library under the terms of your choice (i.e., commercially) but if your program incorporates portions of the library, if it is linked statically, then your program is a "derivative"--a "work based on the library"--and according to paragraph 2, section c, you "must cause the whole of the work to be licensed" *under the terms of the LGPL* (including for free).  
>
>

>
>
> The LGPL licensing for GMP is a problem for the overall licensing of binary programs compiled with GHC because most distributions (and builds) of GHC use static libraries.  (Dynamic libraries are currently distributed only for OS X.)  The LGPL licensing situation may be worse: even though [
> The Glasgow Haskell Compiler License](http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/LICENSE?rev=1.1.26.1;content-type=text%2Fplain) is essentially a "free software" license (BSD3), according to paragraph 2 of the LGPL, GHC must be distributed under the terms of the LGPL!
>
>

1. Memory Structure; Simultaneous Access to GMP by Foreign (C) code in the Same Binary

>
>
> In the current GMP implementation, GMP is configured to use GHC's GC memory and GMP can only have one allocator for memory.  Since any single binary containing Haskell code compiled with GHC contains the RTS and GMP, C code--including foreign calls to GMP from Haskell code (say you need a GMP function that is not a primitive)--in the same binary cannot use GMP.  This problem was noted in [
> bug Ticket \#311](http://hackage.haskell.org/trac/ghc/ticket/311).  The Simon Peyton-Jones suggested that a simple renaming of GHC-GMP functions would solve this problem and Bulat Ziganshin suggested simply using an automated tool to do this.  See [
> Replacement for GMP](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010679.html).  Different function names would make GMP into a separate, custom GHC library leaving the C part of the program free to use GMP.
>
>

>
>
> GHC does not have a custom-modified version of GMP (in fact, GHC uses the system build of GMP if that is available).  The memory configuration of GMP uses GMP's [
> Custom Allocation](http://swox.com/gmp/manual/Custom-Allocation.html#Custom-Allocation) routines.  Alternative libraries may not have this facility built in.
>
>

1. Other Improvements to Integer

>
>
> Most of the suggestions in this section come from discussions in the glasgow-haskell-users list thread [
> returning to Cost of Integer](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010654.html).  In particular, [
> John Meacham's suggestion](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010660.html) to use a ForeignPtr to data held by the normal GMP system library and store the value in an unboxed Int if the number of significant digits in Integer could fit into the size of an Int.
>
>

>
>
> The current GMP implementation of Integer, defined in [libraries/base/GHC/Num.lhs](/trac/ghc/browser/ghc/libraries/base/GHC/Num.lhs), is:
>
>
> ```
> data Integer
>    = S# Int#              -- small integers
> #ifndef ILX
>    | J# Int# ByteArray#   -- large integers
> #else
>    | J# Void BigInteger   -- .NET big ints
> ```
>
>
>
> where the Int\# counts the number of [
> limbs](http://swox.com/gmp/manual/Nomenclature-and-Types.html#Nomenclature-and-Types) (a GMP term referring to parts of a multi-precision number that fit into a 32 or 64 bit word, depending on the machine) and the ByteArr\# is the actual array in RTS-GC memory holding the limbs.  The sign of the Int\# is used to indicate the sign of the number represented by the ByteArr\#.  
>
>

>
>
> This current implementation of Integer means that there are two separate constructors for small and large Integers (S\# Int\# and J\# Int\# ByteArr\#).  The suggestion discussed by [
> John Meacham](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010670.html), [
> Lennart Augustsson](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010664.html), [
> Simon Marlow](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010677.html) and [
> Bulat Ziganshin](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010687.html) was to change the representation of Integer so the Int\# does the work of S\# and J\#: the Int\# could be either a pointer to the Bignum library array of limbs or, if the number of significant digits could fit into say, 31 bits, to use the extra bit as an indicator of that fact and hold the entire value in the Int\#, thereby saving the memory from S\# and J\#.  
>
>

>
>
> [
> Bulat Ziganshin and John Meacham](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010688.html) noted a few problems with a 30bit Int: 
>
>

- interoperability between Haskell and other languages, especially C, would be more difficult so you would have to define a new primitive, say \#Int30 for the representation; and,
- representing a Haskell constructor (the Int\#) inside a pointer--a bit-size constructor--would limit the number of constructors you would be able to have (depending on the size of a pointer object, say the C99 uintptr\_t, on a particular machine).

### Files related to GMP in the GHC Compiler Source Code



Note: references are relative to the main directory of the source distribution; links below are to the darcs repository at [
http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc), created with the `[[GhcFile(path/to/file)]]` script (see [Commentary](commentary)).


- [configure.ac](/trac/ghc/browser/ghc/configure.ac) (*Modify*: remove GMP related material; replace with MP library requirements)

- [compiler/prelude/primops.txt.pp](/trac/ghc/browser/ghc/compiler/prelude/primops.txt.pp)   (*Modify*: Integer material)
- [compiler/prelude/PrelNames.lhs](/trac/ghc/browser/ghc/compiler/prelude/PrelNames.lhs) (*Reference*: integerTyConName and similar)
- \[\[GhcFile(compiler/prelude/TysPrim.lhs)\]   (*Reference*)

- [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) (*Modify*: cpp test for `#if SIZEOF_mp_limb_t != SIZEOF_VOID_P `)
- [includes/MachRegs.h](/trac/ghc/browser/ghc/includes/MachRegs.h) (*Reference*: general; unrelated to GMP: may be starting point for vectorized Cmm (currently only -fvia-c allows auto-vectorization))
- [includes/mkDerivedConstants.c](/trac/ghc/browser/ghc/includes/mkDerivedConstants.c) (*Modify*: references to GMP `__mpz_struct`: `struct_size(MP_INT)`, `struct_field(MP_INT,_mp_alloc)`, `struct_field(MP_INT,_mp_size)`, `struct_field(MP_INT,_mp_d)` and `ctype(mp_limb_t)`.  Note: mp\_limb\_t generally == unsigned long)
- [includes/Regs.h](/trac/ghc/browser/ghc/includes/Regs.h) (*Modify*: references to MP\_INT, `#include "gmp.h"`; Reference: Stg registers, etc.)
- [includes/Rts.h](/trac/ghc/browser/ghc/includes/Rts.h) (*Modify*: reference to `#include "gmp.h"`, `extern` declarations to `__decodeDouble` and `__decodeFloat`; References to various Stg types and macros)
- [includes/StgMiscClosures.h](/trac/ghc/browser/ghc/includes/StgMiscClosures.h) (*Modify*: references to `RTS_FUN(...Integer)` PrimOps; *Reference*: Weak Pointers, other Stg closures)

- [rts/Linker.c](/trac/ghc/browser/ghc/rts/Linker.c) (*Modify*: `SymX(__gmpn...)` and related GMP functions)
- [rts/Makefile](/trac/ghc/browser/ghc/rts/Makefile) (*Modify*: building GMP library)
- [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm) (*Modify*: remove GMP references; NOTE: optimisation of `/* ToDo: this is shockingly inefficient */`, see discussion below)
- [rts/StgPrimFloat.c](/trac/ghc/browser/ghc/rts/StgPrimFloat.c) (*Modify*: `__encodeDouble`, `__encodeFloat` and `decode` versions defined here refer to GMP; might optimise with bitwise conversion instead of union; conversion depends on whether replacement MP library uses floating point, etc.)
- [rts/sm/Storage.c](/trac/ghc/browser/ghc/rts/sm/Storage.c) (*Modify*: `stgAllocForGMP`, `stgReallocForGMP` and `stgDeallocForGMP`; `mp_set_memory_functions(...)`; functions on lines 811, 833, 835, 848; may use as reference for implementation if replacement MP library uses GHC-garbage collected memory)
- [rts/gmp/](/trac/ghc/browser/ghc/rts/gmp/) (directory) (*Modify*: recommended to remove entirely, i.e., do not add conditional compilation for users who want to keep on using GMP)

### Optimisation Opportunities



(1) The "shockingly inefficient" operation of this code:


```wiki
/* ToDo: this is shockingly inefficient */

#ifndef THREADED_RTS
section "bss" {           /* "bss" = UninitialisedData, see CmmParse.y:427 */
  mp_tmp1:
    bits8 [SIZEOF_MP_INT];/* SIZEOF_MP_INT created by includes/mkDerivedConstants.c:43-48 */
}

section "bss" {
  mp_tmp2:
    bits8 [SIZEOF_MP_INT];
}

section "bss" {
  mp_result1:
    bits8 [SIZEOF_MP_INT];
}

section "bss" {
  mp_result2:
    bits8 [SIZEOF_MP_INT];
}
#endif

/* skip some code */


#ifdef THREADED_RTS
#define FETCH_MP_TEMP(X) \
W_ X; \
X = BaseReg + (OFFSET_StgRegTable_r ## X);
#else
#define FETCH_MP_TEMP(X) /* Nothing */
#endif

#define GMP_TAKE2_RET1(name,mp_fun)                                     \
name                                                                    \
{                                                                       \
  CInt s1, s2;                                                          \
  W_ d1, d2;                                                            \
  FETCH_MP_TEMP(mp_tmp1);                                               \
  FETCH_MP_TEMP(mp_tmp2);                                               \
  FETCH_MP_TEMP(mp_result1)                                             \
  FETCH_MP_TEMP(mp_result2);                                            \
                                                                        \
  /* call doYouWantToGC() */                                            \
  MAYBE_GC(R2_PTR & R4_PTR, name);                                      \
                                                                        \
  s1 = W_TO_INT(R1);                                                    \
  d1 = R2;                                                              \
  s2 = W_TO_INT(R3);                                                    \
  d2 = R4;                                                              \
                                                                        \
  MP_INT__mp_alloc(mp_tmp1) = W_TO_INT(StgArrWords_words(d1));          \
  MP_INT__mp_size(mp_tmp1)  = (s1);                                     \
  MP_INT__mp_d(mp_tmp1)	    = BYTE_ARR_CTS(d1);                         \
  MP_INT__mp_alloc(mp_tmp2) = W_TO_INT(StgArrWords_words(d2));          \
  MP_INT__mp_size(mp_tmp2)  = (s2);                                     \
  MP_INT__mp_d(mp_tmp2)	    = BYTE_ARR_CTS(d2);                         \
                                                                        \
  /* This actually initialises GMP as well as mp_result1 */             \
  /* mp_result1 must subsequently grow to size */                       \
  foreign "C" __gmpz_init(mp_result1 "ptr") [];                         \
                                                                        \
  /* Perform the operation */                                           \
  foreign "C" mp_fun(mp_result1 "ptr",mp_tmp1  "ptr",mp_tmp2  "ptr") [];\
                                                                        \
  RET_NP(TO_W_(MP_INT__mp_size(mp_result1)),                            \
         MP_INT__mp_d(mp_result1) - SIZEOF_StgArrWords);                \
}
```


results from initialising each struct (`mp_tmp2`, etc.) on each call, in order to convert the data from the `J# Int# ByteArray#` in the RTS to the GMP structure before passing it to GMP.  There are at least two possible alternatives to this:


>
>
> (a) wrap the replacement MP-library array/structure for arbitrary precision integers in a closure so you do not have to rebuild the struct from on each MP-library call; or
>
>

>
>
> (b) use ForeignPtr (in Cmm, Weak Pointers--difficult to implement) to foreign threads holding the the struct/array
>
>


(2) Primitive Operations in [compiler/codeGen/CgPrimOp.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgPrimOp.hs)



Related to replacing GMP, some operations in CgPrimOP.hs such as IntAddCOp may benefit from operations defined in a replacement MP library (or, more generally, simple optimisation).  For example:


```
emitPrimOp [res_r,res_c] IntAddCOp [aa,bb] live
{-                                                                              
   With some bit-twiddling, we can define int{Add,Sub}Czh portably in           
   C, and without needing any comparisons.  This may not be the                 
   fastest way to do it - if you have better code, please send it! --SDM        
                                                                                
   Return : r = a + b,  c = 0 if no overflow, 1 on overflow.                    
                                                                                
   We currently don't make use of the r value if c is != 0 (i.e.                
   overflow), we just convert to big integers and try again.  This              
   could be improved by making r and c the correct values for                   
   plugging into a new J#.                                                      
                                                                                
        { r = ((I_)(a)) + ((I_)(b));\                                           
        c = ((StgWord)(~(((I_)(a))^((I_)(b))) & (((I_)(a))^r)))\                
        >> (BITS_IN (I_) - 1);\                                                 
   }                                                                            
   Wading through the mass of bracketry, it seems to reduce to:                 
   c = ( (~(a^b)) & (a^r) ) >>unsigned (BITS_IN(I_)-1)                          
                                                                                
-}
   = stmtsC [
        CmmAssign res_r (CmmMachOp mo_wordAdd [aa,bb]),
        CmmAssign res_c $
          CmmMachOp mo_wordUShr [
                        CmmMachOp mo_wordAnd [
                          CmmMachOp mo_wordNot [CmmMachOp mo_wordXor [aa,bb]],
                          CmmMachOp mo_wordXor [aa, CmmReg res_r]
                                             ], 
                        CmmLit (mkIntCLit (wORD_SIZE_IN_BITS - 1))
                                ]
           ]
```



If an integer add were to overflow here, the addition operation would be performed *twice*; even if the integer add did not overflow one extra operation is performed.  Is this an acceptable price for no comparisons?


### Binary Drop in Replacement for GMP



One approach which would only address the license issue would be to develop doing your own linking, without gmp. Each time you come to a linker error having to do with a missing function, you can add the function to a gmp replacement which exports the same interface as gmp as you go... Over time, we'd re-implement as much of GMP as is required by any haskell program... Getting somethign working can take priority.  



Test suites can be developed and perhaps even borrowed from the gmp development team since we should be binary compatable...



This approach has been started already here: [
http://hackage.haskell.org/trac/ghc/attachment/ticket/601/jmp.c](http://hackage.haskell.org/trac/ghc/attachment/ticket/601/jmp.c)


