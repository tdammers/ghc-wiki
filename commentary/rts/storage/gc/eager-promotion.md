# Eager Promotion



Eager promotion is a technique we use in GHC to improve the performance of generational GC.  It is somewhat specific to the characteristics of lazy evaluation, since it takes advantage of the fact that we have some objects that are mutated just once (i.e. thunks).



The key observation is this: when an object P contains a pointer to an object Q in a younger generation, and P is not mutable, then we know that Q cannot be garbage collected until the generation in which P resides is collected.  Hence, we might as well promote Q to this generation immediately, rather than [aging](commentary/rts/storage/gc/aging) it or promoting it to an intermediate generation.  Furthermore, if eager promotion is successful, then the object containing the old-to-new pointers will no longer need to be in the  [remembered set](commentary/rts/storage/gc/remembered-sets) for the generation it resides in.



We gave some performance results for this technique in [
Runtime Support for Multicore Haskell](http://simonmar.github.io/bib/papers/multicore-ghc.pdf); the upshot is that it's worth 10% or so.



Eager promotion works like this.  To do eager promtion, the scavenger sets the flag `gct->eager_promotion` (it can leave the flag set when scavenging multiple objects, this is the usual way), and `gct->evac_gen` is set to the generation to which to eagerly promote objects.  The `evacuate` function will try to move each live object into `gct->evac_gen` or a higher generation if possible, and set `gct->failed_to_evac` if it fails (see [Commentary/Rts/Storage/GC/RememberedSets](commentary/rts/storage/gc/remembered-sets)).  It may fail if the target object has already been moved: we can't move an object twice during GC, because there may be other pointers already updated to point to the new location.  It may also fail if the object is in a generation that is not being collected during this cycle.



Objects which are repeatedly mutable should not be subject to eager promotion, because the object may be mutated again, so eagerly promoting the objects it points to may lead to retaining garbage unnecessarily.  Hence, when we are scavenging a mutable object (see [rts/sm/Scav.c](/trac/ghc/browser/ghc/rts/sm/Scav.c)), we temporarily turn off `gct->eager_promotion`.


