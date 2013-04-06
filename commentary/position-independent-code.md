# Position-Independent Code and Dynamic Linking



We need to generate position-independent code on most platforms when we want our code to go into dynamic libraries (also referred to as shared libraries or DLLs). On some platforms (AIX, powerpc64-linux, x86\_64-darwin), PIC is required for all code.



To access things defined in a dynamic library, we might need to do special things, such as look up the address of the imported thing in a table of pointers, depending on what platform we are on.


## How to access symbols



A C compiler is in an unfortunate position when generating PIC code, as it does not have any hints, whether an accessed symbol ends up in the same dynamic library or if it is truely an external symbol (from the dynamic library point of view). It can only generate non-PIC access for symbols generated within the same object file. In Haskell, we can do better as we assume all package code to end up in a single dynamic library. Hence, all intra-package symbol accesses can be generated as code that does direct access. For all inter-package accesses (package haskell98 accessing symbols in package base, e.g.), we have to generate PIC code. For the following we establish the following: 


- *object-local symbols*, symbols within the same object file. Always generate direct access. 
- *package-local symbols*, symbols within the same Haskell package. The NCG can generate direct access code, C compilers can't.
- *local symbols*, either object-local or package-local.
- *global symbols*, symbol in different libraries/packages. Always generate PIC.

## CLabel.labelDynamic



On most platforms, we can access any global symbol as if it was imported from a dynamic library; this usually means a small performance hit (an extra pointer dereference), but it is otherwise harmless. On some platforms, we have to access all global symbols this way. On Windows, we must know exactly which symbols are DLL-imported and which aren't.



Module `CLabel` contains a function `labelDynamic :: CLabel -> Bool` which is supposed to know whether a `CLabel` is imported from a dynamic library. On Windows, this function needs to be exact; everywhere else, we don't mind the occasional false positive.


## Info Tables



Info tables are in the text segment, which is supposed to be read-only and position-independent. Therefore, an info table *must not* contain any absolute address; instead, all addresses in info tables are instead encoded as relative offsets from the info label.



Note that this is done even when we are generating code that is otherwise position-dependent, in order to preserve binary compatibility between PIC and non-PIC.



It is not possible to generate those relative references from C code, so for the via-C compilation route, we pretty-print these relative references (`CmmLabelDiffOff` in cmm) as absolute references and have the mangler convert them to relative references again.


## Imported labels in SRTs (Windows)



Windows doesn't support references to imported labels in the data segment; on other platforms, the dynamic linker will just relocate the pointers in the SRTs to point to the right symbols. There is a hack in the code that tries to work around it; it might be bitrotted, and it might have been made unnecessary by the GNU linker's new auto-import on Windows.


## PIC and dynamic linking support in the NCG



The module `PositionIndependentCode` lies at the heart of PIC and dynamic linking support in the native code generator.



The basic idea is to call a function `cmmMakeDynamicReference` for all labels accessed from the code during the cmm-to-cmm transformation phase. This function will decide on the appropriate way to access the given label for the current platform and the current combination of -fPIC and -dynamic flags.



We extend Cmm and the `CLabel` module by a few things to allow us to express all the different things that occur on different platforms:



The `Cmm.GlobalReg` datatype has a constructor `PicBaseReg`. This PIC base register is the register relative to which position-independent references are calculated. This can be a general-purpose register that is allocated on a per-CmmProc basis, or it can be a dedicated register, like the instruction pointer `%rip` on x86\_64.


## How things are done on different platforms



This section is a survey of how PIC and dynamic linking works on different platforms. There are small snippets of assembly code for several platforms, platforms that are similar to other platforms are left out (e.g. powerpc-darwin is left out, because the logic is the same as for i386-darwin). I hope the reader will not be too confused by irrelevant differences between the platforms, such as the fact that Darwin and Windows prefix all symbols with an underscore, and Linux doesn't.


### Position dependent code



In the absence of PIC and dynamic linking, things are simple; when we use a label in assembly code, the linker will make sure it points to the right place.


```wiki
# i386-linux without PIC and without dynamic linking
# i386-mingw32 and i386-darwin without dynamic linking
#         are the same with leading underscores.
# get the address of variable bar:
    movl $bar, %eax
# read a 4-byte-variable bar:
    movl bar, %eax
# call function foo:
    call foo
# tail-call foo_info:
    jmp foo_info
```


Now, to access a symbol `xfoo` that has been imported from a dynamic library, we do not want to mention the address of `xfoo` in the text section, because it would need to be modified at load-time.



One solution is to allocate a pointer to the imported symbol in a writable section and have the dynamic linker fill in this pointer table. The pointer table itself resides at a statically known address. The imp\* symbols on Windows are automatically generated by the linker.


```wiki
# i386-mingw32, accessing imported symbols
# get the address of imported symbol xbar:
    movl __imp__xbar, %eax
# read a 4-byte-variable xbar:
    movl __imp__xbar, %eax
    movl (%eax), %eax
# call imported function xfoo:
    call *__imp__xfoo
# tail-call imported xfoo_info:
    jmp *__imp__xfoo_info
```


On Mac OS X, the same system is used for data imports, but this time we have to define the symbol pointers ourselves. For references to code, there is an additional mechanism available; we can jump to a small piece of stub code that will resolve the symbol the first time it is used, in order to reduce application load times. Unfortunately, everything on Mac OS X requires 16-byte stack alignment, even the dynamic linker, so we cannot use this for a tail call.


```wiki
# i386-darwin, accessing imported symbols
# get the address of imported symbol xbar:
    movl L_xbar$non_lazy_ptr, %eax
# read a 4-byte-variable xbar:
    movl L_xbar$non_lazy_ptr, %eax
    movl (%eax), %eax
# call imported function xfoo:
    call L_xfoo$stub
# tail-call imported xfoo_info:
    jmp *L_xfoo$non_lazy_ptr

# And now we need to define those L_*$* things:

    .section __IMPORT,__pointers,non_lazy_symbol_pointers
L_xbar$non_lazy_ptr:
    .indirect_symbol _xbar
    .long   0
L_xfoo$non_lazy_ptr:
    .indirect_symbol _xfoo
    .long   0

    .section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_foo$stub:
    .indirect_symbol _foo
    hlt ; hlt ; hlt ; hlt ; hlt
    # The linker will insert a jmp instruction instead of those hlts
```


In theory, dynamic linking is transparent to position-dependent code on Linux, i.e. the code for accessing imported labels should look exactly the same as for non-imported labels. Unfortunately, things just don't work as they should for strange stuff like info tables.



When the ELF static linker finds a jump or call to an imported symbol, it automatically redirects the jump or call to a linker generated code stub (in the so-called procedure linkage table, or PLT). The linker then considers the label to be a code label and redirects all further references to the label to the code stub, even if they are data references. If this ever happens to an info label, our program will crash, as there is no info table in front of the code stub.



When the ELF static linker finds a data reference to an imported symbol (that it doesn't consider a code label), it allocates space for that symbol in the executable's data section and issues an `R_COPY` relocation, which instructs the dynamic linker to copy the (initial) contents of the symbol to its new place in the executable's image. All references to the symbol from the dynamic library are relocated to point to the symbol's new location, instead.



If `R_COPY` is ever used for an info label, our program will also crash, because the data we're interested in is \*before\* the info label and is not copied to the symbol's new home.



Fortunately, if the static linker finds a pointer to an imported symbol in a writable section, it just instructs the dynamic linker to update that pointer to the symbols address, without doing anything "funny". We can therefore work around these problems.



The workaround is inspired by the position-independent code that GCC generates for powerpc-linux, a platform that is amazingly broken.


```wiki
# i386-linux, accessing imported symbols
# get the address of imported variable xbar:
    movl $xbar, %eax
# read a 4-byte-variable xbar:
    movl xbar, %eax
# call an imported function xfoo:
    call xfoo
    
# Up to here, everything was fine
# (assuming that xbar and xfoo are conventional variables and functions, 
#  as we would find them in foreign code)
# From here on, we have to use a workaround:
    
# tail-call imported xfoo_info:
    jmp *.LC_xfoo_info
# get the address of an imported info table xfoo_info:
    movl .LC_xfoo_info, %eax
    
.section ".got2", "aw"
.LC_xfoo_info:
    .long xfoo_info
```


Things look pretty much the same on x86\_64-linux, powerpc-linux and powerpc-darwin; PowerPC has the added handicap that it takes two instructions to load a 32 bit quantity into a register. On x86\_64-darwin, powerpc64-linux and all versions of AIX, PIC is *required*.


### Position independent code



First, let it be said that there is no such thing as position-independent code on Windows. The dynamic linker will just patiently relocate all dynamic libraries that are not loaded at their preferred base address. On all other platforms, PIC is at least strongly recommended for dynamic libraries.



In an ideal world, there would be assembler instructions for referring to things via an offset from the current instruction pointer. Jump instructions are ip-relative on all platforms that GHC runs on, but for data accesses, only x86\_64 is this ideal world.



On x86\_64, on both Linux and Mac OS X, we can use `foo(%rip)` to encode an instruction pointer relative data reference to `foo`, and `foo@GOTPCREL(%rip)` to encode an instruction pointer relative referece to a linker-generated symbol pointer for symbol `foo`.
A linker-generated code stub for imported code can be accessed by appending `@PLT` to the label on Linux, and is used implicitly when necessary on Mac OS X.



Again, we have to avoid the code stubs for tail-calls and use the symbol pointer instead, because there is a stack alignment requirement.


```wiki
# x86_64-linux, -fPIC
# x86_64-darwin is almost the same,
#   .. but with leading underscores and no @PLT suffixes

# get the address of variable bar:
    leaq bar(%rip), %rax
# read a 4-byte-variable bar:
    movl bar(%rip), %eax
# call function foo:
    call foo
# tail-call foo_info:
    jmp foo_info

# get the address of imported symbol xbar:
    movq xbar@GOTPCREL(%rip), %rax
# read a 4-byte-variable xbar:
    movq xbar@GOTPCREL(%rip), %rax
    movl (%rax), %eax
# call imported function xfoo:
    call xfoo@PLT
# tail-call imported xfoo_info:
    jmp *xfoo_info@GOTPCREL(%rip)
```


Other platforms are not nearly as nice; i386 and powerpc\[64\] do not have a way of accessing the current instruction pointer or referring to data relative to it. The \*only\* way to get at the current instruction pointer is to issue a call instruction. To generate PIC code, we have to do just that at the beginning of each function.



On Darwin, things are relatively straightforward:


```wiki
# i386-darwin, -fPIC
    
# first, initialise PIC:
    call    1f
1:  pop     %ebx
    # now, %ebx contains the address of local label 1
    # (Note: local label 1 is referred to as "1f" before its definition,
    #        and as "1b" after its definition)

# get the address of variable bar:
    leal _bar-1b(%ebx), %eax
# read a 4-byte-variable bar:
    movl _bar-1b(%ebx), %eax
# call function foo:
    call foo
# tail-call foo_info:
    jmp foo_info

# get the address of imported symbol xbar:
    movl L_xbar$non_lazy_ptr-1b(%ebx), %eax
# read a 4-byte-variable xbar:
    movl L_xbar$non_lazy_ptr-1b(%ebx), %eax
    movl (%eax), %eax
# call imported function xfoo:
    call L_xfoo$stub
# tail-call imported xfoo_info:
    jmp *L_xfoo$non_lazy_ptr-1b(%ebx)

# And now we need to define those L_*$* things:

    .section __IMPORT,__pointers,non_lazy_symbol_pointers
L_xbar$non_lazy_ptr:
    .indirect_symbol _xbar
    .long   0
L_xfoo$non_lazy_ptr:
    .indirect_symbol _xfoo
    .long   0

    .section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_foo$stub:
    .indirect_symbol _foo
    hlt ; hlt ; hlt ; hlt ; hlt
    # The linker will insert a jmp instruction instead of those hlts
```


There is one more small additional complication on Darwin. The assembler doesn't support label difference expressions involving labels not defined in the same source file, so we have to treat all symbols not defined in the same source file as dynamically imported.



On Linux, we need to first calculate the address of the Global Offset Table (GOT) and then use `bar@GOT` to refer to symbol pointers and `bar@GOTOFF` to refer to a local symbol relative to the GOT.
Also, the linker-generated code-stubs (`xfoo@PLT`) require the address of the GOT to be in register `%ebx` when they are invoked. The NCG currently doesn't do this, so we avoid code stubs altogether on i386.


```wiki
# i386-linux, -fPIC
    
# first, initialise PIC:
    call    1f
1:  pop     %ebx
    # now, %ebx contains the address of local label 1
    addl    $_GLOBAL_OFFSET_TABLE_+(.-1b), %ebx
    # now, %ebx contains the address of the GOT

# get the address of variable bar:
    leal bar@GOTOFF(%ebx), %eax
# read a 4-byte-variable bar:
    movl bar@GOTOFF(%ebx), %eax
# call function foo:
    call foo
# tail-call foo_info:
    jmp foo_info

# get the address of imported symbol xbar:
    movl xbar@GOT(%ebx), %eax
# read a 4-byte-variable xbar:
    movl xbar@GOT(%ebx), %eax
    movl (%eax), %eax
# call imported function xfoo:
        
    # using the PLT would work here, because we happened to use %ebx,
    # but the NCG won't do it right now:
    # call xfoo@PLT
    
    # Instead, we use the symbol pointer:
    call *xfoo@GOT(%ebx)

# tail-call imported xfoo_info:
    jmp *xfoo@GOT(%ebx)
```


**To be done:** powerpc-linux, AIX/powerpc64-linux


## Linking on ELF



To generate a DSO on ELF platform, we use GNU ld. Except for `-Bsymbolic`, ld is invoked regularly with the `-shared` option, and `-o` pointing to the output DSO file followed objects that in its sum compose an entire package. In Haskell, we assume that there is a one-to-one mapping from packages to DSOs. So, all parts of the base package will end up in a libHSbase.so. As intra-package references are not generated as PIC code, we have to supply all objects that make up a package, so that ld is able to resolve these references before writing a (.text) relocation free DSO library file. To enable these cross-object relocations GNU ld needs `-Bsymbolic`.


## Mangling dynamic library names



As Haskell DSOs might end up in standard library paths, and as they might not be compatible among compilers and compiler version, we need to mangle their names to include the compiler and its version.



The scheme is libHS*\<package\>*-*\<package-version\>*-*\<compiler\>\<compilerversion\>*.so. E.g. libHSbase-2.1-ghc6.6.so


