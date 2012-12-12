# RTS Configurations



The RTS can be built in several different ways, corresponding to global CPP defines.  The flavour of the RTS is chosen by GHC when compiling a Haskell program, in response to certain command-line options: `-prof`, `-threaded`, etc.



The CPP symbols and their corresponding command-line flags are:


<table><tr><th>`PROFILING`</th>
<td>
Enables profiling.

GHC option: `-prof`

RTS suffix: `p`
</td></tr></table>


<table><tr><th>`THREADED_RTS`</th>
<td>
Enables multithreading in the RTS, bound threads, and SMP execution.

GHC option: `-threaded`

RTS suffix: `thr`
</td></tr></table>


<table><tr><th>`DEBUG`</th>
<td>
Enables extra debugging code, assertions, traces, and the `+RTS -D` options.

GHC option: `-debug`

RTS suffix: `debug`
</td></tr></table>


<table><tr><th>`TRACING`</th>
<td>
Enables RTS tracing and event logging, see [rts/Trace.c](/trac/ghc/browser/ghc/rts/Trace.c).  Implied by `DEBUG`.

GHC option: `-eventlog`

RTS suffix: `l`
</td></tr></table>



So for example, `libHSrts_thr_debug.a` is the version of the runtime compiled with `THREADED_RTS` and `DEBUG`, and will be linked in if you use the `-threaded` and `-debug` options to GHC.



The ways that the RTS is built in are controlled by the `GhcRTSWays` Makefile variable.  


## Combinations



All combinations are allowed.  Only some are built by default though; see [mk/config.mk.in](/trac/ghc/browser/mk/config.mk.in)[](/trac/ghc/export/HEAD/ghc/mk/config.mk.in) to see how the `GhcRTSWays` variable is set.


## Other configuration options


<table><tr><th>`NO_REGS`</th>
<td>
Disabled the use of hardware registers for the stack pointer (`Sp`), heap pointer (`Hp`), etc.  This is
enabled when building "unregisterised" code, which is controlled by the `GhcUnregisterised` build option.
Typically this is necessary when building GHC on a platform for which there is no native code generator
and LLVM does not have a GHC calling convention.
</td></tr></table>


<table><tr><th>`USE_MINIINTERPRETER`</th>
<td>
Enables the use of the RTS "mini-interpreter", which simulates tail-calls.  Again, this is enabled by
`GhcUnregisterised` in the build system.
</td></tr></table>


<table><tr><th>`TABLES_NEXT_TO_CODE`</th>
<td>
Controls whether the info table is placed directly before the entry code for a closure or return continuation.
This is normally turned on if the platform supports it, but is turned off by `GhcUnregisterised`.
</td></tr></table>


