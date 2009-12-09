# Pinned Objects



The GC does not support pinning arbitrary objects.  Only objects that have no pointer fields can be pinned.  Nevertheless, this is a useful case, because we often want to allocate garbage-collectable memory that can be passed to foreign functions via the FFI, and we want to be able to run the GC while the foreign function is still executing (for a `safe` foreign call).  Hence, the memory we allocated must not move.



Bytestrings are currently allocated as pinned memory, so that the bytestring contents can be passed to FFI calls if necessary.



The RTS provides an API for allocating pinned memory, in [includes/rts/storage/GC.h](/trac/ghc/browser/ghc/includes/rts/storage/GC.h):


```wiki
StgPtr  allocatePinned  ( Capability *cap, lnat n );
```


This allocates memory from the given Capability's nursery.



Pinned objects work in the GC as follows:


- Pinned objects are allocated into a block of their own, not mixed up with unpinned objects.
- The block containing pinned objects is marked as a *large block*, i.e. the `BF_LARGE` bit is set in `bd->flags`.
- When encountering a live object in a `BF_LARGE` block, the GC never copies the object, instead it just re-links the whole block onto the `large_objects` list of the destination generation.
- The GC doesn't have to scavenge the pinned object, since it does not contain any pointers.  This is just as well, because we cannot scan blocks for live pinned objects, due to [slop](commentary/rts/storage/slop).  Hence the restriction that pinned objects do not contain pointers.


This means that using pinned objects may lead to memory fragmentation, since a single pinned object keeps alive the whole block in which it resides.  If we were to implement a non-moving collector such as mark-region?, then we would be able to reduce the impact of fragmentation due to pinned objects.


