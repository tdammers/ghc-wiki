# GHC Commentary: Storage



GHC's storage manager is designed to be quite flexible: there are a large number of tunable parameters in the garbage collector, and partly the reason for this was because we wanted to experiment with tweaking these settings in the context of Haskell.



[](/trac/ghc/attachment/wiki/Commentary/Rts/Storage/sm-top.png)


- [Layout of Heap Objects](commentary/rts/storage/heap-objects)
- [Layout of the Stack](commentary/rts/storage/stack)
- [Slop](commentary/rts/storage/slop)
- [The Block Allocator](commentary/rts/storage/block-alloc)
- [The Garbage Collector](commentary/rts/storage/gc)
- [The HEAP\_ALLOCED() macro](commentary/rts/storage/heap-alloced)


See also:


- [Pointer tagging](commentary/rts/haskell-execution/pointer-tagging)
