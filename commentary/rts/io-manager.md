# The I/O Manager



This page describes the internals of the I/O manager, the latest version of which can be found in [
GHC.Event](http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Event.html). The I/O manager's job is to to provide a blocking I/O API to the user without forcing the RTS to create one operating system thread per Haskell thread. We here focus on the *threaded* RTS on non-Windows platforms.



ezyang: **WARNING: some of this information may be out of date**



The RTS keeps a global list of pending events, unsuprising called `pendingEvents`, containing a elements of the following data type:


```wiki
data IOReq
  = Read   {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())
  | Write  {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())
```


When a thread wants to read from a file descriptor `fd` it calls `threadWaitRead` which in turn calls `waitForReadEvent`.


```wiki
waitForReadEvent :: Fd -> IO ()
waitForReadEvent fd = do
  m <- newEmptyMVar
  atomicModifyIORef pendingEvents (\xs -> (Read fd m : xs, ()))
  prodServiceThread
  takeMVar m
```


`waitForReadEvent` creates a new `MVar`, adds it to `pendingEvents` and finally blocks on it. `pendingEvents` gets read by the I/O manager thread which runs the event loop, in GHC called `service_loop`. It roughly performs these steps:


1. Pick up new I/O requests from `pendingRequests` and set the variable to the empty list.
1. Create data structures appropriate for calling `select`.
1. For each `Read` request in `pendingEvents` check if the file descriptor is in the ready set returned by `select`. If so perform a `putMVar` on the `MVar` associated with that request to wake up the blocked thread.
1. Repeat from step 1.
