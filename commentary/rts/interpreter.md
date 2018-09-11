# GHC Commentary: The byte-code interpreter and dynamic linker


## Linker



The linker lives in `rts/Linker.c` and is responsible for handling runtime loading of code into a Haskell process.  This is something of a big blob of unpleasant code, and see [DynamicGhcPrograms](dynamic-ghc-programs) for information about efforts to reduce our dependence on this linker.



Nevertheless, GHC's linker certainly adds functionality, and this has been enough to earn its keep (for now). In particular, the linker knows how to **relocate static libraries** (e.g. `.o` and `.a` libraries).  This is a pretty rare feature to find: ordinarily, libraries that are to be loaded at runtime are compiled as position independent code (-fPIC), which allows the same physical code pages to be shared between processes, reducing physical memory usage. At runtime, GHC rewrites the relocations, meaning that the resulting page cannot be shared across processes, but that the result is just as efficient as if the code had been statically linked to begin with.



Implementation of the linker cuts three axes: object file format (ELF, Mach-O, PEi386), operating system (Linux, MingW, Darwin, etc), and architecture (i386, x86\_64, powerpc, arm), and there are corresponding sets of macros for fiddling with each (`OBJFORMAT_*`, `*_HOST_OS` and `*_HOST_ARCH`). A large part of the unpleasantness of the current linker is the fact that all of these different concerns are jumbled in one file; refactoring these out to separate files would be a very nice service.



(write more here)


## Bytecode Interpreter


---



[CategoryStub](category-stub)


