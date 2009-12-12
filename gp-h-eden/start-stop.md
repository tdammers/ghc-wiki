## Startup / Shutdown support in the RTS



A common property of all our parallel variants is, the runtime system starts in several coordinated instances on different machines, and also needs to coordinate the clean shutdown.


- We added a suitable (deliberately simple) message passing layer (== PVM \| MPI \| home brewn, corresponding to `parpvm | mpi | cp`)


`rts/parallel/MPSystem.h` is the interface  `rts/parallel/PVMComm.c|MPIComm.c|CpComm.c` implement the methods (TODO `CpComm.c` are only stubs for now)


- Managing the startup of several machines (parsed by start script)


`rts/RtsStartup.c` calls `rts/parallel/ParInit.c::startupParallelSystem|synchroniseSystem`, which call the suitable functions in the message passing layer


- Each machine is running an own (empty) scheduler, started by a function added to `rts/Schedule.c`


*Historic: the first version simply used `workerStart` from the threaded RTS way to run the empty scheduler. This released the capability after finishing (scheduler returns) and caused an assertion failure when releasing it again (by rts\_unlock) in RtsMain. Now we have our own (tiny) method to start the scheduler.*


- Shutdown is triggered via system message PP\_FINISH from node 1 to the others.


In a first version, the empty scheduler instances blocked on a call to MP\_recv in `getRemoteWork`. Now, we handle it inside a more complex message processing function, when we have more types of messages between the machines).



[--\> back to GpHEden](gp-h-eden)


