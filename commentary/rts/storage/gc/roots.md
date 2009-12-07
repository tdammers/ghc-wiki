# Garbage Collection Roots



The "roots" are the set of pointers that the GC starts traversing from, i.e. the roots of the live object graph.



Most roots belong to a particular Capability.  Traversing the roots of a capbility is done by `markSomeCapabilities()` in [rts/Capability.c](/trac/ghc/browser/ghc/rts/Capability.c).  The roots of a Capability are:


- The run queue (head and tail)
- The wakeup queue (head and tail)
- For each Task on the `suspended_ccalling_tasks` list, the TSO for that Task
- The Spark Pool
- Only for the non-threaded RTS: The blocked queue (head and tail), and the sleeping queue


In addition, each Capability has a [remembered set](commentary/rts/storage/gc/remembered-sets) for each generation.  A remembered set is a source of roots if that generation is *not* being collected during this cycle; otherwise the remembered set is discarded.  During GC, all remembered sets are discarded and new ones will be constructed for each generation and Capability; see `scavenge_capability_mut_lists()` in [rts/sm/Scav.c](/trac/ghc/browser/ghc/rts/sm/Scav.c).



There are also roots from other parts of the system:


- Signal handlers (only in the non-threaded RTS; in the threaded RTS signal handlers are maintained by the IO manager in `GHC.Conc` rather than the RTS).
- [Weak pointers](commentary/rts/storage/gc/weak)
- Stable pointers?
