# Slop



Slop is unused memory between objects in the heap.


<table><tr><th> Object1 </th>
<th> ... Slop ... </th>
<th> Object2 
</th></tr></table>


## Why do we want to avoid slop?



Slop makes it difficult to traverse an area of memory linearly, visiting all the objects, because we can't tell where `Object2` starts in the above diagram.  We need to do linear traversals for two reasons, currently:


- Heap profiling? needs to perform a census on the whole heap.
- [Sanity checking](commentary/rts/sanity) needs to ensure that all the pointers in the heap
  point to valid objects.


Additionally, linear traversals are useful for the mark phase of the [compacting garbage collector](commentary/rts/storage), and would be useful if we were to allow objects to be pinned arbitrarily (currently pinned objects cannot contain pointers, which means they don't need to be scavenged by the GC).


## How does slop arise?



Slop can arise for two reasons:


- The compiled code allocates too much memory, and only fills part of it with objects.  For example,
  when compiling code for a function like this:

  ```wiki
  f = \x -> case x of
              True  -> e1
              False -> e2
  ```

  the code generator takes the maximum of the heap requirements of e1 and e2 and aggregates it into
  the heap check at the beginning of the function `f` (to avoid doing too many heap checks).  
  Unfortunately that means either `e1` or `e2` has too much heap allocated to it, leaving some slop.
  We solve this problem by moving the heap pointer *backwards* before making a tail-call if
  there is any heap slop.

- When an object is overwritten with a smaller object.  This happens in two ways:
  [Updates](commentary/rts/haskell-execution/updates) and [Black Holes](commentary/rts/storage/heap-objects#).

## What do we do about it?



We avoid the problem for heap profiling? by arranging that we only ever do a census on a newly garbage-collected heap, which has no slop in it (the garbage collector never leaves slop between objects in the heap).



Slop does arise due to updates and black holes during normal execution, and GHC does not attempt to avoid it (because avoiding or filling slop during an update is costly).  However, if we're doing [sanity checking](commentary/rts/sanity), then we need to arrange that slop is clearly marked: so in a `DEBUG` version of the RTS (see [RTS configurations](commentary/rts/config))  the update code and the blackhole code both arrange to fill slop with zeros: see the `FILL_SLOP` macro in [rts/Updates.h](/trac/ghc/browser/ghc/rts/Updates.h).  Hence sanity checking only works with a `DEBUG` version of the RTS.


