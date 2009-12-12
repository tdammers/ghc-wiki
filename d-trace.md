# Using DTrace with GHC



GHC 6.13 includes DTrace probes in the runtime system.  Currently, these probes correspond to the events of the [EventLog](event-log) framework and are only available on Mac OS X (from Leopard onwards).



It is straight forward to extend the current implementation with additional probes, and due to the lightweight nature of DTrace, new probes could inspect the runtime system and running Haskell program in an even more fine-grained manner.


## About DTrace



DTrace and the associated D programming language are described in detail in the [
Solaris Dynamic Tracing Guide](http://wikis.sun.com/display/DTrace/Documentation).  The Mac OS X specific GUI frontend, Instruments, is explained in the [
Instruments User Guide](http://developer.apple.com/mac/library/DOCUMENTATION/DeveloperTools/Conceptual/InstrumentsUserGuide/Introduction/Introduction.html) and the Mac version of the command line tool `dtrace` is documented by the [
Mac OS X dtrace man page](http://developer.apple.com/mac/library/documentation/Darwin/Reference/ManPages/man1/dtrace.1.html).


## Using DTrace with GHC



To make effective use of DTrace probes, we need D scripts and custom instruments for Instruments app.  Until then, the available probes can be queried as follows:


1. Start `ghci` in a terminal
1. Invoke the following command at another command line prompt: `sudo dtrace -P 'HaskellEvent*' -l`


To trace a Haskell program:


1. Execute `sudo dtrace -P 'HaskellEvent*' -Z` on the command line
1. Then, invoke your Haskell program (the terminal running `dtrace` will dump the trace)


If you only want to be notified of the start and end of garbage collections, invoke dtrace as follows: 


```wiki
sudo dtrace -n 'HaskellEvent*:::gc-start' -n 'HaskellEvent*:::gc-end' -Z
```


Further details on the available probes are below.



**IMPORTANT:** Do not use `dtrace` with the `-c` option to start and trace a Haskell program in a single invocation.  On Mac OS X, this doesn't seem to work for reasons that I don't understand.  (To be precise, it does seem to work for other programs, but probes in the Haskell runtime appear not to be detected.)


## Probe description



The following probes are being made available by the `HaskellEvent` provider:


<table><tr><th>`create-thread (capability, tid)`</th>
<td>
Triggered when a new runtime thread is created.  Reports the capability on which the thread is created and the new thread's thread id.
</td></tr>
<tr><th>`run-thread (capability, tid)`</th>
<td>
Indicates that the given thread starts to run on the specified capability.
</td></tr>
<tr><th>`stop-thread (capability, tid)`</th>
<td>
The identified thread stops execution on the given capability.
</td></tr>
<tr><th>`thread-runnable (capability, tid)`</th>
<td>
The given thread has just been appended to the run queue of the specified capability.
</td></tr>
<tr><th>`migrate-thread (cap, tid, new_cap)`</th>
<td>
The specified thread has just been moved from capability `cap` to `new_cap`, either because `cap` is sharing its run queue with `new_cap` or because the migration was explicitly requested.
</td></tr>
<tr><th>`run-spark (capability, tid)`</th>
<td>
We are about to convert a spark into a new parallel thread.  The capability and thread are those determining the spark and converting it, **not** the thread id of the new spark.
</td></tr>
<tr><th>`steal-spark (cap, tid, victim_cap)`</th>
<td>
We are about to convert a spark from a different capability, namely `victim_cap`, into a new parallel thread.  Again, the capability and thread are those determining the spark and converting it, **not** the thread id of the new spark.
</td></tr>
<tr><th>`shutdown (cap)`</th>
<td>
The specified capability is about to disappear; its run queue and spare worker lists are already empty.
</td></tr>
<tr><th>`thread-wakeup (cap, tid, other_cap)`</th>
<td>
We just unblocked the specified thread on capability `other_cap`.  (The capability `cap` is the one which performed the unblocking.)
</td></tr>
<tr><th>`gc-start (cap)`</th>
<td>
The specified capability gets ready for a garbage collection.
</td></tr>
<tr><th>`gc-end (cap)`</th>
<td>
The specified capability completed a garbage collection.
</td></tr>
<tr><th>`gc-request-seq-gc (cap)`</th>
<td>
We are about to perform a single-threaded garbage collection (meaning that we will grab all capabilities, and then, perform the GC on the specified capability).
</td></tr>
<tr><th>`gc-request-par-gc (cap)`</th>
<td>
We are about to perform a parallel garbage collection (this still means all mutator threads need to stop).  We might need to wait for the other capabilities to donate a worker thread each.
</td></tr>
<tr><th>`create-spark-thread (cap, tid)`</th>
<td>
We just turned a spark into the specified thread on the given capability.
</td></tr>
<tr><th>`startup (num_caps)`</th>
<td>
Initialising the runtime system with the given number of capabilities (that's the value passed with `+RTS -N`).
</td></tr>
<tr><th>`user-msg (cap, msg)`</th>
<td>
The given user message (a string that you need to copy with `copyinstr()`) was emitted on the given capability; this happens when a call to `traceEvent` is being made, passing the message as an argument.
</td></tr>
<tr><th>`gc-idle (cap)`</th>
<td>
The GC worker thread of the specified capability just became idle.
</td></tr>
<tr><th>`gc-work (cap)`</th>
<td>
The GC worker thread of the specified capability is about to do some GC work.
</td></tr>
<tr><th>`gc-done (cap)`</th>
<td>
The GC worker thread of the specified capability finished doing GC.
</td></tr></table>


## Probe definition



The provider is defined as follows:


```wiki
provider HaskellEvent {

  // scheduler events
  probe create__thread (EventCapNo, EventThreadID);
  probe run__thread (EventCapNo, EventThreadID);
  probe stop__thread (EventCapNo, EventThreadID, EventThreadStatus);
  probe thread__runnable (EventCapNo, EventThreadID);
  probe migrate__thread (EventCapNo, EventThreadID, EventCapNo);
  probe run__spark (EventCapNo, EventThreadID);
  probe steal__spark (EventCapNo, EventThreadID, EventCapNo);
  probe shutdown (EventCapNo);
  probe thread_wakeup (EventCapNo, EventThreadID, EventCapNo);
  probe gc__start (EventCapNo);
  probe gc__end (EventCapNo);
  probe request__seq__gc (EventCapNo);
  probe request__par__gc (EventCapNo);
  probe create__spark__thread (EventCapNo, EventThreadID);

  // other events
  probe startup (EventCapNo);
  probe user__msg (EventCapNo, char *);
  probe gc__idle (EventCapNo);
  probe gc__work (EventCapNo);
  probe gc__done (EventCapNo);

};
```


where we have


```wiki
typedef uint32_t EventThreadID;
typedef uint16_t EventCapNo;
typedef uint16_t EventThreadStatus;
```


The two events `EVENT_LOG_MSG` and `EVENT_BLOCK_MARKER` are not supported.  The former doesn't appear to be used and the latter appears to be an artefact of the event log file format.


## Portability



User-space DTrace probes are implemented differently on Mac OS X than in the original DTrace implementation; see under the heading *BUILDING CODE CONTAINING USDT PROBES* in the [
Mac OS X dtrace man page](http://developer.apple.com/mac/library/documentation/Darwin/Reference/ManPages/man1/dtrace.1.html).  Nevertheless, it shouldn't be too hard to enable these probes on other platforms, too.


