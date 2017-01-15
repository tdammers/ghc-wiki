# Sanity Checking



Source code: [rts/sm/Sanity.c](/trac/ghc/browser/ghc/rts/sm/Sanity.c), [rts/sm/Sanity.h](/trac/ghc/browser/ghc/rts/sm/Sanity.h).



The purpose of sanity checking is to catch bugs in the RTS as early as possible; if the program is going to crash, we want it to crash as soon as possible after the error occurred.  The problem with debugging the RTS is that heap corruption can go unnoticed through several GC cycles, making it particularly difficult to trace back to the erroneous code.



Sanity checking is turned on by the `+RTS -DS` option.  We treat it like an expensive assertion: normal assertions are allowed to take a few extra percent of run time, so we don't mind having them on all the time in a `DEBUG` RTS, but sanity checking may double the run time of the program or worse.  So the rule of thumb is that expensive assertions go into sanity checking, cheap assertions are on in `DEBUG`, or possibly even on all the time.



Sanity checking does a complete traversal of the heap after each GC to look for dangling pointers (see `checkHeap` in [rts/sm/Sanity.c](/trac/ghc/browser/ghc/rts/sm/Sanity.c)).  For this it needs to ensure that there is no [slop](commentary/rts/storage/slop), which is why we can only do this in a `DEBUG` runtime: the slop-avoiding machinery is only on with `DEBUG`.



Sanity checking also turns on some other expensive checks: for example in the [generic apply](commentary/rts/haskell-execution#) code we check that the arguments point to valid closures.


