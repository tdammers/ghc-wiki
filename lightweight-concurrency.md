


# Lightweight Concurrency in GHC



This page documents the effort to move GHC's concurrency support from its current location in the C part of the runtime system (RTS) to Haskell. This works builds on Peng Li's earlier work ([
http://simonmar.github.io/bib/papers/conc-substrate.pdf](http://simonmar.github.io/bib/papers/conc-substrate.pdf)). This page contains information about the design, implementation, problems and potential solutions for building user-level concurrency primitives in GHC. Currently, the focus is on user-level implementation of non-deterministic parallelism in GHC ([Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html)).



Lightweight concurrency implementation resides in the [
ghc-lwc2](https://github.com/ghc/ghc/commits/ghc-lwc2) branch in the git repo.


## Introduction



GHC has a rich support for concurrency (forkIO, MVars, STM, Asynchronous exceptions, bound threads, safe FFI, transparent scaling on multicores, etc.) and a fast and robust runtime system. However, the concurrency support is implemented in C and baked into the RTS. The concurrency primitives non-trivially interact among each other, and along with the lightweight thread scheduler, through a cascade of locks and condition variables. Often times, the invariants on which RTS fields can be accessed when are expressed as comments, and enforced through assertions (See [
here](https://github.com/ghc/ghc/blob/master/rts/Task.h#L37-L46) for one such fascinating example). This policy of enforcing through assertions keeps the overheads low, but makes the task of modifying and extending the runtime cumbersome.



But, why would we be interested in modifying GHC's concurrency environment? There are several good reasons to believe that a particular concurrent programming model, or a scheduling policy would not suit every application. With the emergence of many-core processors, we see NUMA effects becoming more prominent, and applications might benefit from NUMA aware scheduling and load balancing policies. Moreover, an application might have a better knowledge of the scheduling requirements -- a thread involved in user-interaction is expected to be given more priority over threads performing background processing. We might want to experiment with various work-stealing or work-sharing policies. More ambitiously, we might choose to build X10 style async-finish or Cilk style spawn-sync task parallel abstractions. Ideally, we would like allow the programmer to write an application that can  seamlessly combine all of these different programming abstractions, with pluggable scheduling and load balancing policies.



While we want to provide flexibility to the Haskell programmer, this should not come at a cost of added complexity and decreased performance. This idea reflects in the synchronization abstractions exposed to the programmer - [Primitive Transactional Memory(PTM)](lightweight-concurrency#tm)), and our decision to keep certain pieces of the concurrency puzzle in the RTS ([Safe Foreign Calls](lightweight-concurrency#afe-foreign-calls),[Blackholes](lightweight-concurrency#)). One would think lifting parts of the runtime system to Haskell, and retaining other parts in C, would complicate the interactions between the concurrency primitives and schedulers. We abstract the scheduler interface using PTM monads, which simplifies the interactions. The figure below captures the key design principles of the proposed system.



[](/trac/ghc/attachment/wiki/LightweightConcurrency/GHC_LWC_Key.jpg)



Although implementing concurrency primitives as a library is hardly a novel idea, the aim of this work is to bring it to the GHC programmer, without having to give up any of the existing concurrency features in return.


## Background - GHC's Concurrency RTS



For a high-level design of the current scheduler, see [
Scheduler](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler).


## Concurrency Substrate



The idea of the concurrency substrate is to provide a minimal set of primitives over which a variety of user-level concurrency libraries can be implemented. As such, the concurrency substrate must provide a way to create threads, a way to schedule them, and a synchronization mechanism in a multiprocessor context. Whereas, the Creation and maintenance of schedulers and concurrent data structures is the task of the concurrency library. Concurrency substrate resides at [
libraries/base/LwConc/Substrate.hs](https://github.com/ghc/ghc/blob/ghc-lwc2/libraries/base/LwConc/Substrate.hs).


### PTM



The only synchronization mechanism exposed by the concurrency substrate is a primitive transactional memory (PTM). Locks and condition variables can be notoriously difficult to use, especially in an environment with user-level threads, where they tend to interfere with the scheduler. Moreover, they do not compose elegantly with lazy evaluation. PTM interface is shown below:


```wiki
data PTM a
data PVar a
instance Monad PTM

newPVar    :: a -> PTM (PVar a)
readPVar   :: PVar a -> PTM a
writePVar  :: PVar a -> a -> PTM ()
atomically :: PTM a -> IO a
retry      :: PTM a
```


A PTM transaction may allocate, read and write transactional variables of type `PVar a`. Transaction is atomically executed using the `atomically` primitive. It is important to notice that PTM provides a blocking `retry` mechanism which needs to interact with the scheduler, to block the current thread and resume another thread. We will see [later](lightweight-concurrency#tm-retry) how we allow such interactions while not imposing any restriction on the structure of the schedulers.


### One-shot Continuations



The concurrency substrate enables creation and scheduling of I/O-performing computations through *one-shot continuations*. An SCont (stack continuation) is a suspended I/O computation, which is in fact just a reference to a TSO object. Capturing the current continuation is just getting a reference to the current TSO, and hence is very fast. In the following discussion, we use SConts when referring to the Haskell object and threads when referring to its corresponding RTS version. SCont interface is shown below:


```wiki
data SCont
newSCont         :: IO () -> IO SCont
switch           :: (SCont -> PTM SCont) -> IO ()
getCurrentSCont  :: PTM SCont
switchTo         :: SCont -> PTM ()
```


Given an I/O-performing computation `newSCont` returns a reference to an SCont which when scheduled, will perform the I/O action. The switch primitive is a bit unique. Switch takes a function which is applied to the current continuation. The result of the function is PTM transaction of type PTM SCont. This transaction can encapsulate the actions necessary for appending the current SCont to the scheduler and fetching the next SCont to switch to. The switch primitive performs this transaction atomically, and switches control to the resultant SCont. 



Performing the body of switch atomically in a transaction avoids the nasty race conditions usually seen in multicore runtimes where one-shot continuations are used for modelling schedulers. In such systems, there are often cases where before the switch primitive has had a chance to return, another processor picks up the current continuation (appended to the scheduler) and tries to switch to it. It becomes necessary to go for complicated solutions such as releasing the scheduler locks after the target thread resumes execution to prevent races. In our case, PTM eliminates the need for such a mechanism - the other processor would not be able to access the current SCont, unless the transaction has committed and control has switched to the target SCont. Primitive `getCurrentSCont` returns a reference to the current SCont under PTM. Primitive `switchTo` commits the current PTM transaction and switches to the given SCont. As we will see, these two primitives are necessary for [abstracting the scheduler](lightweight-concurrency#bstracting-the-scheduler). 


#### Return value of a switching transaction



Since switchTo eagerly commits the transaction, the code that follows switchTo is not evaluated. This is a problem if the transaction that contains switchTo has a type different than PTM (). Consider the following code:


```wiki
s :: String <- atomically $ do {
  sc <- getCurrentSCont;
  -- save the current SCont somewhere
  switchTo someSCont;
  return "This is never evaluated!"
}
print s
```


The type of the transaction that contains switchTo is PTM string, and atomically performing the transaction is expected to return a String value. But the value returned when the current SCont resumes execution (after switchTo) is a (). Our solution is to make switchTo return a `error "Attempting to use return value of a switched transaction"`, and any attempt to use the return value of a switching transaction throws a runtime error.


### SCont Status



Of course, care must be taken to ensure that the control does not switch to an SCont that is either running, blocked on an MVar, or completed. But how do we know whether the given SCont is ready to run? We expect the scheduler writer or library implementer to indicate the status of SCont before switching. SCont status API is show below.


```wiki
data ResumeToken

data SContStatus = SContRunning | 
                     -- SCont is currently running
                   SContKilled  |           
                     -- SCont was killed by an (asynchronous) exception
	           SContSwitched SContSwitchReason
data SContSwitchReason = Yielded |          
                           -- SCont has yielded, but runnable
                         BlockedInHaskell ResumeToken | 
                           -- SCont is blocked on a user-level concurrent 
                           -- data structure (MVars and such)
                         BlockedInRTS |     
                           -- SCont is blocked on a foreign call, blackhole, etc,.
                         Completed
                           -- SCont has run to completion

setSContSwitchReason :: SCont -> SContSwitchReason -> PTM ()
getSContStatus       :: SCont -> PTM SContStatus
```


Any attempt to switch to an SCont with status other than `SContSwitched Yielded` throws an exception. Primitive `setSContSwitchReason` updates the status of SCont. Since setSContSwitchReason is a PTM action, the effect of updating the status takes place when the transaction commits and the control has switched to another SCont. This avoids any race conditions that might be involved in reading the status of an SCont before it has switched. 



Before a switch operation, we expect the programmer to indicate the reason for switching through setScontSwitchReason. Exception is raised by the switch primitives if a switch reason has not been provided. When a switched SCont resumes execution, its status is automatically updated to `SContRunning`.



Resume tokens are utilized for supporting asynchronous exceptions. Resume tokens are discussed along with the [discussion on asynchronous exceptions](lightweight-concurrency#synchronous-exceptions).


### SCont-Local Storage



SCont-local storage (SLS) provides a solution for associating arbitrary state with an SCont. Each SCont has a single slot with type [Dynamic](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Dynamic.html). SLS interface is give below:


```wiki
setSLS :: SCont -> Dynamic -> IO ()
getSLS :: SCont -> PTM Dynamic
```


`Data.Dynamic` provides a way for safely casting between any arbitrary data type and `Dynamic` type. This allows SLS to be generic as well as type-safe. Moreover, SLS is GC'ed along with the SCont. 


## Abstracting the Scheduler



Concurrency substrate does not impose any structure on the user-level schedulers. The programmer might choose to implement a single scheduler for the entire system or a scheduler per capability. The schedulers might also be hierarchical, with pluggable load-balancing policies. However, we need a uniform interface such that the concurrency libraries, STM, asynchronous exceptions, safe-foreign calls, blackholes and other such mechanisms can interact with the user-level scheduler. For this purpose, we introduce the notion of `scheduler actions`, which is expected for every SCont. The substrate interface for scheduler actions is shown below:


```wiki
------ Schedule SCont Action :: SCont -> PTM () ------

getScheduleSContAction :: PTM (SCont -> PTM ())
setScheduleSContAction :: SCont -> (SCont -> PTM ()) -> PTM ()

----------- Yield Control Action :: PTM () -----------

getYieldControlAction :: PTM (PTM ())
setYieldControlAction :: SCont -> PTM () -> PTM ()
```


Abstractly, given an SCont, the scheduleSContAction appends the SCont to a scheduler. The yieldControlAction picks an SCont from a scheduler and switches to it. The `get*` functions will fetch the scheduler actions of the current SCont. In order to make the ideas more concrete, let us assume that we have a very simple round-robin scheduler, implemented as a `PVar[SCont]`. One possible implementation of scheduler actions for this scheduler is given below.


```wiki
scheduleSContAction :: SCont -> PTM () 
scheduleSContAction sc = do
  sched :: PVar [SCont] <- -- get sched 
  contents :: [SCont] <- readPVar sched 
  setSContSwitchReason sc Yielded -- sc is ready to be run
  writePVar $ contents ++ [sc]


yieldControlAction :: PTM () 
yieldControlAction = do
  sched :: PVar [SCont] <- -- get sched 
  contents :: [SCont] <- readPVar sched 
  case contents of
    x:tail -> do { 
      writePVar $ contents tail; 
      switchTo x -- DOES NOT RETURN
    } 
    otherwise -> ...
```


The implementation is pretty straight-forward; scheduleSContAction appends the given scont to the back of the list, and yieldControlAction picks an SCont from the front of the list and switches to it. The `otherwise` case of yieldControlAction is chosen if the there are no available SConts to switch to. This will be discussed later under [Sleep Capability](lightweight-concurrency#leep-capability). Having the scheduler actions as PTM actions ensures that the operations on the scheduler are always properly synchronized. Notice that scheduleSContAction returns while yieldControlAction does not. We expect every user-level thread (SCont) to be associated with a scheduler. The scheduler actions are saved as fields in the SCont's TSO structure so that the RTS can access them. Typically, when a new SCont is created, it is immediately associated with a scheduler. 


## User-level Concurrency



Now that we have defined an abstract scheduler interface, lets look at how to construct user-level concurrency primitives using the scheduler actions. 


### Schedulers



Since our first goal is to implement GHC's concurrency support in Haskell, let us start with`forkIO` and `yield`. These two primitives are sufficient for a simple cooperatively scheduled lightweight thread system. In order to make the presentation cleaner, assume that we have the following helper functions.


```wiki
getSSA = getScheduleSContAction
setSSA = setScheduleSContAction
getYCA = getYieldControlAction
setYCA = setYieldControlAction
```


Primitive `yield` appends the current SCont to the scheduler, picks the next SCont from the scheduler and switches to it. We utilize the scheduler actions to achieve this. The implementation of yield is shown below. It is important to notice that yield does not assume anything about the implementation of the scheduler except for the scheduler actions. Hence, there is no need to re-implement yield primitive for every scheduler, thus minimizing the overhead of implementing new schedulers.


```wiki
yield :: IO ()
yield = atomically $ do
  -- Append current SCont to scheduler
  ssa <- getSSA
  ssa a
  -- Switch to next SCont from scheduler
  switchToNext :: PTM () <- getYCA
  switchToNext
```


Primitive `forkIO` also follows the strategy of utilizing scheduler actions. The implementation of forkIO is shown below. 


```wiki
forkIO :: IO () -> IO SCont
forkIO f = do
  -- Switch to next thread after completion
  let epilogue = atomically $ do {
    sc <- getCurrentSCont;
    setSContSwitchReason sc Completed;
    switchToNext <- getYCA;
    switchToNext
  }
  ns <- newSCont (f >> epilogue)  
  atomically $ do {
    -- Initialize scheduler actions
    ssa <- getSSA;
    setSSA ns ssa;
    yca <- getYCA;
    setYCA ns yca;
    -- Append the new SCont to current SCont's scheduler
    ssa ns
  }
  return ns
```


Here, the thread that invokes forkIO initializes the new SCont (`ns`) with its own scheduler actions, and appends it to the scheduler. After the newly created SCont finishes execution, the control must switch to another thread in the scheduler. This is captured by the `epilogue`.



A full implementation of a round-robin scheduler can be found [
here](https://github.com/ghc/ghc/blob/ghc-lwc2/libraries/lwconc/LwConc/ConcurrentList.hs). This scheduler has one queue per capability. Work is shared among the capabilities by spawning threads in a round-robin fashion on the capabilities.


### MVars



[MVars](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html) are one of the basic synchronization mechanisms exposed by GHC's concurrency library. A simple user-level implementation of MVar might look like:


```wiki
newtype MVar a = MVar (PVar (State a))
data State a = Full a [(a, PTM ())] | Empty [(PVar a, PTM ())]
```


MVar is either empty with a list of pending takers, or full with a value and a list of pending putters. The PTM () action in the full and empty list represents the logic necessary for waking up the pending putters and takers. The following snippet shows the implementation of `takeMVar`.


```wiki
takeMVar :: MVar a -> IO a 
takeMVar (MVar ref) = do
  hole <- atomically $ newPVar undefined 
  atomically $ do
    st <- readPVar ref 
    case st of
      Empty ts -> do 
        s <- getCurrentSCont 
        ssa <- getSSA
        let wakeup = ssa s 
        writePVar ref $ v
          where v = Empty $ ts++[(hole, wakeup)] 
        switchToNext <- getYCA
        setSContSwitchReason s $ BlockedInHaskell ...
        switchToNext
      Full x ((x', wakeup):ts) -> do 
        writePVar hole x 
        writePVar ref $ Full x' ts 
        wakeup
      otherwise -> ... 
  atomically $ readPVar hole
```


Primitive `takeMVar` first creates a hole, which will contain the result. If the MVar happens to be empty, we fetch the scheduleSContAction for the current thread, and append append it along with the hole to the end of the queue. This enqueued PTM action, when executed, will append the current thread to its scheduler. We indicate the reason for switching to be `BlockedInHaskell`. Finally, the control switches to the next runnable thread using the yieldControlAction. All of these actions occur atomically within the same transaction.



If the MVar is full with a pending writer, we first fill the hole with the value. Then, MVar's status is updated with the enqueued value and the rest of the writers. Finally, we execute the dequeued PTM action to place the writer into its corresponding scheduler.



Notice that just like yield and forkIO, takeMVar is scheduler agnostic; the MVar implementation is cleanly separated from the scheduler implementation. Moreover, the same MVar might be shared between threads from different schedulers since they utilize the uniform scheduler interface. Since the scheduler actions are PTM actions, actions from different schedulers can be composed together elegantly and simplifies reasoning about synchronization. An implementation of a MVar can be found [
here](https://github.com/ghc/ghc/blob/ghc-lwc2/libraries/lwconc/LwConc/MVarList.hs).



As an aside, the race condition in [swapMVar](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent-MVar.html#v%3AswapMVar) can be [
eliminated](https://github.com/ghc/ghc/blob/ghc-lwc2/libraries/lwconc/LwConc/MVarList.hs#L110) with the help of PTM abstraction. Thus, PTM abstraction makes it easy to construct correct concurrent data-structures. 


## System Threads and Parallelism



We retain the task model of the current runtime system. There is a one-to-one mapping between tasks and system threads. Tasks are not exposed to the programmer and is transparently managed by the RTS. Any task that is not performing a safe-foreign call needs to acquire a `capability` to run. The number of capabilities represents the number of SConts that can run in parallel. Just like in the current system, the rts parameter `-N` controls the maximum number of capabilities. Cores are system resources and hence, the control over their allocation to different processes should be a property of the context under which the programs are run. Hence, we believe it is important to have non-programmatic control over parallelism.



A program always boots up on 1 core running the `main` function. Additional capabilities can be created on demand using the following primitives.


```wiki
getNumCapabilities   :: IO Int
getCurrentCapability :: PTM Int
newCapability :: SCont -> IO ()
```


Primitive `newCapability` runs the given SCont on a free capability. If there are no free capabilities, a runtime error is raised. A typical, initial task spawned on another core would pull work from the scheduler and switch to it. For example,


```wiki
initialTask :: IO ()
initialTask = atomically $ do
  s <- getCurrentSCont
  yca <- getYCA
  setSContSwitchReason s Completed
  yca
```


When a program boots up with `N` capabilities, the programmer can choose to create `N-1` additional capabilities using the primitive `newCapability` which run `initialTask`.


### Sleep Capability



In a multicore setting, runnable SConts might not always be available on a capability. In this case, the capability must wait for SConts to be added to the scheduler. In our system, this must be handled under `yieldControlAction`, where it is expected that the control switches to another SCont. The concurrency substrate provides 


```wiki
sleepCapability :: PTM ()
```


primitive that aborts the current transaction and blocks the current capability. The capability is implicitly woken up when one of the PVars that it has read from has been updated. Then, the original transaction is re-executed. Under yieldControlAction, one of the PVars read before sleeping will be the scheduler data structure. Hence, the capability is woken up when the scheduler data structure is updated. The complete implementation of yieldControlAction example introduced [earlier](lightweight-concurrency#bstracting-the-scheduler) is given below.


```wiki
yieldControlAction :: PTM () 
yieldControlAction = do
  sched :: PVar [SCont] <- -- get sched 
  contents :: [SCont] <- readPVar sched 
  case contents of
    x:tail -> do { 
      writePVar $ contents tail; 
      switchTo x -- DOES NOT RETURN
    } 
    otherwise -> sleepCapability
```

### SCont Affinity



Every SCont is bound to a particular capability and only that capability is capable of running the SCont. Switching to an SCont that is not bound to the current capability raises a runtime error. SCont affinity interface is shown below.


```wiki
setSContCapability   :: SCont -> Int -> IO ()
getSContCapability   :: SCont -> PTM Int
```


A newly created SCont is bound to the current capability. Primitive `setSContCapability` is used to change the affinity of an SCont that belongs to the current capability. Trying to change the affinity of an SCont that belongs to a different capability throws a runtime error. 


### Bound SCont



Similar to [bound threads](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html#g:9) concurrency substrate supports bound SConts. The interface is shown below.


```wiki
newBoundSCont          :: IO () -> IO SCont
isCurrentSContBound    :: IO Bool
rtsSupportsBoundSConts :: Bool
```


Creating a bound SCont creates a new task, which is the only task capable of running the bound SCont. When switching to a bound SCont, the RTS transparently switches to the corresponding bound task. Similarly, when switching away from a bound SCont, the RTS suspends the current bound task, and switches to another appropriate task. However, an unbounded SCont (created through `newSCont` primitive) might be run on any unbounded task (referred to as worker tasks). New worker tasks might be created by the RTS on demand.


## Scheduler Interaction with RTS



We retain certain components of GHC's concurrency support that interact with the scheduler in the C part of the runtime system (RTS). Some of these interactions such as non-termination detection and finalizers become clear only in the RTS. Other interactions like safe-foreign calls and asynchronous exceptions, which can potentially be implemented in Haskell, are retained in the RTS for performance and simplicity. Furthermore, there are issues like [black-holes](lightweight-concurrency#), which are complicated enough that they are best handled transparently from the programmer's point of view.



We observe that our [scheduler actions](lightweight-concurrency#bstracting-the-scheduler) are sufficient to capture the interaction of user-level scheduler and RTS. As mentioned earlier, the scheduler actions are saved as fields in the TSO structure. In order to invoke the scheduler actions from the RTS (*upcalls*), we need a container thread. We associate with every capability an *upcall thread* and an *upcall queue*. 



Whenever a scheduler action needs to be invoked from the RTS, the scheduler action is added to the upcall queue. During every iteration of the RTS `schedule()` loop, we check for pending upcalls. If there are pending upcalls, we save the current thread, switch to the upcall thread, execute every upcall to completion, and finally switch to the original thread.



Next, we shall look at various RTS interaction with the user-level scheduler and how scheduler actions enable them.


### Blocked Indefinitely


#### Unreachable Concurrent Datastructure



The [
BlockedIndefinitelyOnMVar](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception-Base.html#t:BlockedIndefinitelyOnMVar) is raised on a thread that is blocked on an MVar, but the MVar has become unreachable. This is performed at the end of a garbage collection, just before resuming execution of the Haskell threads. In the vanilla RTS, after raising BlockedIndefinitelyOnMVar exception on a blocked thread, the thread is added back to the run queue. However, in the lightweight concurrency (LWC) implementation, this is not so straightforward. In particular, 


- How do we know whether the SCont is blocked on a concurrent data structure in Haskell? 
- How do we safely add the thread to the Haskell scheduler?


We know that any SCont blocked with status `SContSwitched BlockedInHaskell t` is blocked on a concurrent data structure. For an SCont that is blocked on a concurrent data structure which has become unreachable, we raise `BlockedIndefinitelyOnConcDS` exception. Subsequently, we utilize the SCont's scheduleSContAction to put the SCont back into its corresponding scheduler. Importantly, since the scheduler actions are PTM actions, the necessary synchronization is taken care of by the PTM layer. 


#### Unreachable Scheduler



Unlike the vanilla RTS, schedulers can become unreachable in the LWC implementation. Concurrency substrate allows the programmer to install finalizers with the following primitive.


```wiki
setFinalizer :: SCont -> IO () -> IO()
```


For the given thread, `setFinalizer` installs the given IO () as the finalizer. If an SCont is blocked with status `SContSwitched Yielded` has become unreachable, we run the SCont's finalizer, if installed.


### Preemptive Scheduling



GHC's concurrency library supports preemptive scheduling of threads. In the LWC implementation, we utilize the scheduler actions to preempt the thread; on a timer interrupt, we execute the current thread's schedulerSContAction followed by yieldControlAction. This is similar to the implementation of the `yield` primitive described [earlier](lightweight-concurrency#chedulers).


### Safe Foreign Calls



A [
safe foreign call](http://simonmar.github.io/bib/papers/conc-ffi.pdf) does not impede the execution of other Haskell threads on the same scheduler, if the foreign call blocks. Before performing the foreign call, the task, say `T1`, releases the capability that it currently owns. This might wake up other tasks which are waiting to acquire a free capability. After the foreign call has completed, `T1` tries to reacquire the last owned capability. In the fast path, the foreign call quickly completes and `T1` reacquires the capability. In the slow path, some other task, say `T2`, acquires the capability.


#### Slow-path in Vanilla RTS



In the vanilla RTS, `T2` will pick the next available thread from the current capability's run queue and resume execution. When `T1` eventually completes the foreign call, it tries to reacquire the capability. Thus, performing a safe-foreign call does not block all the threads on that capability. 


#### Slow-path in LWC RTS



The fast path in the LWC implementation is the same as vanilla implementation. However, in the slow path, we need a way for `T2` to resume the scheduler, and a way for `T1` to join the scheduler when the foreign call execution eventually completes. Assume that the Haskell thread that is running on the task `T1` is `t1`. We utilize the yieldContrlAction of `t1` to enable `T2` to resume execution of other threads on the scheduler. When `T1` eventually resumes execution after the foreign call, it finds that it has lost the race to acquire the capability to T2. At this point, `T1` executes `t1`'s scheduleSContAction to join the scheduler.


### PTM retry



GHC's STM module provides a `retry` primitive, which blocks the thread invoking retry to block until one of the TVars it has read has been written to. After a thread blocks on the STM, the next available thread from the capability's run queue is resumed. Eventually, the blocked thread is added back to the run queue when one of the TVars in its read set has been written to. 



For `retry` under PTM, while waiting on a PVar is still an RTS mechanism, interaction with the scheduler is implemented using the scheduler actions. After blocking the thread on the PTM, RTS executes the blocking thread's yieldControlAction to resume the scheduler. When the thread is eventually unblocked, its scheduleSContAction is executed to put the thread back into its user-level scheduler data structure.


### Black-hole Handling



Long lived thunks may be *blackholed* to avoid duplication of work. A blackholed thunk is owned by the thread which blackholed it.  When a thread encounters a blackhole owned by some other thread, the vanilla GHC suspends the thread until the thunk finishes evaluation. This requires interaction with the scheduler. In general, any thunk evaluation may encounter a blackhole. 



For the LWC implementation, can we utilize the scheduler actions to yield control to another thread from the user-level scheduler, similar to the solutions above? The simple answer is no. Since the scheduler actions themselves are implemented in Haskell code, they can also encounter blackholes. Hence, we might encounter situations where the user-level scheduler becomes blocked on a thread that it is scheduling, resulting in a deadlock. 



Since thunks (usually) represent pure computation, can we not duplicate thunk evaluation when we detect a deadlocked scheduler? Unfortunately, this is not so straightforward. The closure that represents a thunk is lost when the thunk is blackholed. Moreover, the thread evaluating the blackholed thunk (blackhole owner) might be running on the same or a different capability than the thread entering the blackhole. Correspondingly, the blackhole owner thread might either not be schedulable or running. This complicates the problem of potentially forcing a blackholed thunk's evaluation on a thread other than the blackhole owner. It is for these reasons we handle blackholes transparently from the programmer's perspective in the LWC implementation. 



When a thread enters a blackhole, there are essentially 3 parameters that we need to consider:


1. **PTM : Is the current thread manipulating a scheduler?** since schedulers are implemented in Haskell code, there isn't a clear distinction between the scheduler and the rest of the program. As an approximation, we assume that whenever a thread is in the middle of a PTM transaction, it is potentially manipulating the scheduler.
1. **UPT : Is the current thread an upcall thread?** In the common case, when a thread enters a blackhole, we utilize its scheduler actions to block it on the blackhole's blocking queue. But an upcall thread, evaluating scheduler actions from the RTS is not associated with any scheduler and hence does not have scheduler actions. This case must be handled separately.
1. **CCAP : Is the blackhole owner on the current capability?** If the blackhole owner is on the current capability, then the blackhole owner is currently suspended. Otherwise, thunk evaluation is potentially progressing on another capability.


Since each of these conditions can either be true or false, we have 8 cases to consider. 


- **(1, 2) PTM(F)   UPT(F)   CCAP(T/F)** - This is the typical case when a thread blocks on a blackhole. Here, we enque the thread on the blackhole's blocked thread queue and perform the yieldControlAction to switch to another thread. When the thunk finishes evaluation, we examine the blocked thread queue. If a blocked thread is not an upcall thread, we know it has a scheduleSContAction, which is executed to resume the blocked thread.
- **(3, 4) PTM(F)   UPT(T)   CCAP(T/F)** - This case cannot happen. Upcall threads only execute PTM actions.
- **(5, 6) PTM(T)   UPT(T/F) CCAP(T)**   - We are under PTM and potentially manipulating the scheduler. The blackhole is owned by a thread on current capability and is suspended. Hence, the only option is to force evaluation of the thunk. This is achieved by creating a closure (AP\_STACK) that contains all of the frames from the blackhole owner thread until the update frame that corresponds to the blackholed thunk. Blackhole owner's stack is modified such that when it resumes, it evaluates the newly created closure instead of resuming the original thunk evaluation. Current thread evaluates the newly created thunk to force evaluation of the thunk. Here, the current thread is said to have `inherited` the thunk.
- **(7)    PTM(T)   UPT(F)   CCAP(F)**   - A user-level thread under PTM has blocked on a blackhole owned by a thread on a different capability. We cannot inherit the computation. The solution is similar to (1). 
- **(8)    PTM(T)   UPT(T)   CCAP(F)**   - This is a tricky case. Upcall thread blocks on a blackhole, which is owned by a thread on a different capability. We need to put the capability to sleep and wake-up when the blackholed thunk finishes evaluation. Here, we enque the upcall thread on the blackhole's blocked thread queue. Now, the current capability does not have any runnable threads. Hence, it goes to sleep. When the thunk finishes evaluation, we examine the blocked thread queue. If a blocked thread is an upcall thread, we push it on its owning capability. This implicitly wakes up the capability, which resumes execution.  

#### RTS Messaging Layer



Since thunk evaluation and blackholing is a critical for good performance, we would like the common case - thunk finishes evaluation without being blackholed - to be fast. Hence, we retain the RTS messaging layer between the capabilities for blocking on a blackhole. When a thread enters a blackhole whose owner thread resides on another capability, a block request message is sent to the corresponding capability. Notice that the [association](lightweight-concurrency#cont-affinity) between SConts (threads) and capabilities is essential for identifying which capability to send the block request message to. During every iteration of the RTS Schedule loop, a capability checks its inbox for pending messages, and if any, processes the messages. Hence, no synchronization is necessary for replacing a thunk with a value. 


### Exceptions Escaping SConts



Every SCont has a top-level exception handler, which catches all exceptions and executes the SCont's yieldControlAction in the exception handler. If an exception escapes the computation spawned as an SCont, we mark the SCont's status as `SContKilled`, and switch to the next available SCont from the scheduler. This ensures that schedulers are not lost if an SCont is killed.


### Asynchronous Exceptions



The substrate exposes 


```wiki
throwTo :: Exception e => SCont -> e -> IO ()
```


primitive which raises an arbitrary exception on the given SCont. The masking semantics is exactly the same as [throwTo under Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html). Under the hood, RTS messaging layer is used to raised exceptions on SConts belonging to other capabilities. This is necessary since raising asynchronous exceptions involves modifying the stack, and hence, is safe only if performed on the capability to which the target SCont belongs to. If the calling SCont blocks on throwTo, we utilize the scheduler actions to resume other SConts that might be available on the scheduler. 



If an exception is raised on an SCont that is blocked on an blackhole, STM, or user-level concurrent data structure, we remove the SCont from any blocking queues, raise the exception, and utilize the SCont's scheduleSContAction to enqueue it back to the scheduler. If an exception is raised on a SCont suspended on a scheduler, we simply raise the exception.



For blocking actions in the RTS, such as STM, and blackholes, RTS knows how to remove the SCont from the corresponding queue. However, if an SCont happens to be blocked on a user-level data structure such as an MVar, how do we asynchronously remove the thread from the MVar data structure? Once could envision a model where a SCont blocking on a concurrent data structure would provide a `unblockSCont :: PTM()` which can be used to remove the blocked SCont from the user-level blocking queue. In the RTS, blocking queues are implemented as doubly-linked lists such that removing an element from the middle of the list is fast. However, implementing an efficient unblockSCont action for every user-level data structure can be cumbersome and complicated, and defeats the purpose of lifting the concurrency library to Haskell.



Alternatively, instead of eagerly removing the SCont from the user-level blocking queue, we can defer it until the SCont is about to be unblocked from the blocking queue. In this case, on receiving the asynchronous exception, we will raise the exception on the SCont, eagerly append it to the scheduler, and mark the blocked action as invalid. The invalidation is achieved through resume tokens.


```wiki
data ResumeToken

newResumeToken     :: PTM ResumeToken
isResumeTokenValid :: ResumeToken -> PTM Bool

data SContSwitchReason = BlockedInHaskell ResumeToken | ...
```


Primitive `newResumeToken` allocates a new, valid resume token. The validity of a resume token can be queried using the primitive `isResumeTokenValid`. Whenever an SCont blocks on a user-level data structure (i.e. updating switch reason to `BlockedInHaskell`), it is expected that it is provided a new, valid resume token. If an asynchronous exception is raised on this blocked SCont, the resume token is transparently invalidated. Eventually, when the SCont is about to be unblocked from the concurrent data-structure, the resume token can be queried for validity. If the resume token is invalid, then the blocked SCont has been resumed already and hence it should not be resumed again. The following snippet shows the implementation of `takeMVar` primitive that can tolerate asynchronous exceptions. The only change is to the `wakeup` function.


```wiki
takeMVar :: MVar a -> IO a 
takeMVar (MVar ref) = do
  hole <- atomically $ newPVar undefined 
  atomically $ do
    st <- readPVar ref 
    case st of
      Empty ts -> do 
        s <- getCurrentSCont 
        ssa <- getSSA
        token <- newResumeToken
        let wakeup = do {
          v <- isResumeTokenValid token;
          if v then
            ssa s
          else
            return ()
        }
        writePVar ref $ v
          where v = Empty $ ts++[(hole, wakeup)] 
        switchToNext <- getYCA
        setSContSwitchReason s $ BlockedInHaskell ...
        switchToNext
      Full x ((x', wakeup):ts) -> do 
        writePVar hole x 
        writePVar ref $ Full x' ts 
        wakeup
      otherwise -> ... 
  atomically $ readPVar hole
```


Thus, except for resume tokens, asynchronous exceptions are transparently handled by the runtime system.


## Related Work


- [Concurrent Programming in GHC](ghc-concurrency)
- [
  Lightweight Concurrent Primitives for GHC](http://simonmar.github.io/bib/papers/conc-substrate.pdf)
- [
  Tackling the awkward squad](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/)
- [
  Runtime Support for Multicore Haskell](http://simonmar.github.io/bib/papers/multicore-ghc.pdf)
- [
  Composable Scheduler Activations for Haskell](http://kcsrk.info/papers/schedact_jfp15.pdf)
