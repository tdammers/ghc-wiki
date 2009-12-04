# Aging in the generational GC



Aging is an important technique in generational GC: the idea is that objects that have only recently been allocated have not had sufficient chance to die, and so promoting them immediately to the next generation may lead to retention of unnecessary data.  The problem is amplified if the prematurely promoted objects are thunks that are subsequently updated, leading to retention of an arbitrary amount of live data until the next collection of the old generation, which may be a long time coming.



The idea is that instead of promoting live objects directly from generation 0 into generation 1, they stay in generation 0 for a "while", and if they live long enough, they get promoted.  The simplest way is to segment the objects in generation 0 by the number of collections they have survived, up to a maximum.  GHC 6.12 used to do this: each generation had a tunable number of *steps*. Objects were initially promoted to step 0, copied through each subsequent step on following GC cycles, and then eventually promoted to the next generation.



Measurement we made showed that the optimal number of steps was somewhere between 1 and 3 (2 was almost always better than either 1 or 3).  In priniciple it is possible to have a fractional number of steps, although GHC 6.12 only supported integral numbers.



In GHC 6.13 and later, we made the following change: each block now points to the generation to which objects in that block will be copied in the next GC (the `dest` field of `bdescr`).  This lets us decide on a block-by-block basis which objects to promote and which to retain in a generation, and lets us implement fractional numbers of steps.  At the same time, we dropped the notion of explicit steps, so each generation just has a single list of blocks.  This means that we can no longer do aging of more than 2 GC cycles, but since the measurements showed that this was unlikely to be beneficial, and the new structure is much simpler, we felt it was worthwhile.



Blocks in the nursery have a `dest` field pointing to generation 0, and blocks of live objects in generation 0 have a `dest` field pointing to generation 1.  This gives us the same effect as 2 steps did in the GHC 6.12, except that intermediate generations (e.g. gen 1 in a 3-gen setup) now only have one step rather than 2.  We could implement aging in the intermediate generations too if that turns out to be beneficial (more than 2 generations is rarely better than 2, according to our measurements).


