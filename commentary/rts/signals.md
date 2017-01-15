# The GHC Commentary: Signals



This section describes how the RTS interacts with the OS signal facilities.  Throughout we use the term "signal" to refer to both POSIX-style signals and Windows *ConsoleEvents*.



Signal handling differs between the *threaded* version of the runtime and the non-threaded version (see [Commentary/Rts/Config](commentary/rts/config)).  Here we discuss only the threaded version, since we expect that to become the standard version in due course.



Source files:


- POSIX signal handling:

  - [rts/posix/Signals.h](/trac/ghc/browser/ghc/rts/posix/Signals.h), [rts/posix/Signals.c](/trac/ghc/browser/ghc/rts/posix/Signals.c)
- Windows console events:

  - [rts/win32/ConsoleHandler.h](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.h), [rts/win32/ConsoleHandler.c](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.c)

## Signal handling in the RTS



The RTS is interested in two signals: a timer signal, and an interrupt signal.


### The timer signal



 
The timer signal is used for several things:


- To cause the [scheduler](commentary/rts/scheduler) to context switch
- Sampling for [time profiling](commentary/profiling)
- To detect deadlock (see [Commentary/Rts/Scheduler](commentary/rts/scheduler))


Source files:


- The timer interrupt handler, and starting/stopping the timer:

  - [rts/Timer.h](/trac/ghc/browser/ghc/rts/Timer.h), [rts/Timer.c](/trac/ghc/browser/ghc/rts/Timer.c)
- Platform-independent ticker interface, used by the timer:

  - [rts/Ticker.h](/trac/ghc/browser/ghc/rts/Ticker.h)
- Posix implementation of ticker:

  - [rts/posix/Itimer.c](/trac/ghc/browser/ghc/rts/posix/Itimer.c)
- Windows implementation of ticker:

  - [rts/win32/Ticker.c](/trac/ghc/browser/ghc/rts/win32/Ticker.c)


On Posix, the timer signal is implemented by calling `timer_create()` to generate regular `SIGVTALRM` signals (this was changed from SIGALRM in [\#850](http://gitlabghc.nibbler/ghc/ghc/issues/850)).



On Windows, we spawn a new thread that repeatedly sleeps for the timer interval and then executes the timer interrupt handler.


## The interrupt signal



The interrupt signal is `SIGINT` on POSIX systems or `CTRL_C_EVENT/CTRL_BREAK_EVENT`on Windows, and is normally sent to the process when the user hits Control-C.   By default, interrupts are handled by the runtime.  They can be caught and handled by Haskell code instead, using `System.Posix.Signals` on POSIX systems or `GHC.ConsoleHandler` on Windows systems.  For example, [GHCi](commentary/compiler/backends/gh-ci) hooks the interrupt signal so that it can abort the current interpreted computation and return to the prompt, rather than terminating the whole GHCi process.



When the interrupt signal is received, the default behaviour of the runtime is to attempt to shut down the Haskell program gracefully.  It does this by calling `interruptStgRts()` in [rts/Schedule.c](/trac/ghc/browser/ghc/rts/Schedule.c) (see [Commentary/Rts/Scheduler](commentary/rts/scheduler#)).  If a second interrupt signal is received, then we terminate the process immediately; this is just in case the normal shutdown procedure failed or hung for some reason, the user is always able to stop the process with two control-C keystrokes.


## Signal handling in Haskell code



Source files:


- POSIX: [rts/posix/Signals.h](/trac/ghc/browser/ghc/rts/posix/Signals.h), [rts/posix/Signals.c](/trac/ghc/browser/ghc/rts/posix/Signals.c)
- Windows: [rts/win32/ConsoleHandler.h](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.h), [rts/win32/ConsoleHandler.c](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.c)


A Haskell program can ask to install signal handlers, via the `System.Posix.Signals` API, or `GHC.ConsoleHandler` on Windows.  When a signal arrives that has a Haskell handler, it is the job of the runtime to create a new Haskell thread to run the signal handler and place the new thread on the run queue of a suitable [Capability](commentary/rts/scheduler#apabilities).



When the runtime is idle, the OS threads will all be waiting inside `yieldCapability()`, waiting for some work to arrive.  We want a signal to be able to create a new Haskell thread and wake up one of these OS threads to run it, but unfortunately the range of operations that can be performed inside a POSIX signal handler is extremely limited, and doesn't include any inter-thread synchronisation (because the signal handler might be running on the same stack as the OS thread it is communicating with).



The solution we use, on both Windows and POSIX systems, is to pass all signals that arrive to the [IO Manager](commentary/rts/io-manager) thread.  On POSIX this works by sending the signal number down a pipe, on Windows it works by storing the signal number in a buffer and signaling the IO Manager's `Event` object to wake it up.  The IO Manager thread then wakes up and creates a new thread for the signal handler, before going back to sleep again.


## RTS Alarm Signals and Foreign Libraries



When using foreign libraries through the Haskell FFI, it is important
to ensure that the foreign code is capable of dealing with system call
interrupts due to alarm signals GHC is generating.



For example, in this `strace` output
a `select` call is interrupted, but the foreign C code interprets the
interrupt as an application error and closes a critical file
descriptor:


```wiki
[pid 22338] send(7, "\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0
    \0\0\0\0\0\0\0\0\0\0\0"..., 116, MSG_NOSIGNAL) = 116
[pid 22338] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 22338] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 22338] sigreturn()                 = ? (mask now [])
[pid 22338] gettimeofday({1267656511, 467069}, NULL) = 0
[pid 22338] stat64("/etc/localtime", {st_mode=S_IFREG|0644, st_size=3519, ...}) = 0
[pid 22338] write(6, "Communication failed in RPC"..., 176) = 176
[pid 22338] close(7)                    = 0
```


Once the C code was modified to deal with the interrupt properly, it
proceeded correctly (note that foreign call is restarted 3 times before it succeeds).


```wiki
[pid 23967] send(7, "\f\0\0\0\244\1\0\0\0\0\0\0B\4\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"..., 536, MSG_NOSIGNAL <unfinished ...>
[pid 23968] <... select resumed> )      = ? ERESTARTNOHAND (To be restarted)
[pid 23968] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23968] sigreturn()                 = ? (mask now [])
[pid 23968] futex(0x9b52a88, FUTEX_WAIT_PRIVATE, 7, NULL <unfinished ...>
[pid 23967] <... send resumed> )        = 536
[pid 23967] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 23967] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23967] sigreturn()                 = ? (mask now [])
[pid 23967] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 23967] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23967] sigreturn()                 = ? (mask now [])
[pid 23967] select(8, [7], NULL, NULL, NULL) = 1 (in [7])
[pid 23967] recv(7, "\7\2\0\0\0\0\0\0\0\0\0\0\0\0\0\0\200\0\0\0\244]\0\0\0\0\0\0\0\0\0\0"..., 116, 0) = 116
```