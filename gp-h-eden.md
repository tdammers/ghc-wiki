# Glasgow Parallel Haskell (GpH) and Eden documentation



This page will describe the GpH and Eden parallel variants of GHC.  This is work in progress.



St Andrews has hosted the [HackPar](hack-par) parallel hackathon in December 2009.



Heriot-Watt will host a [HackSprint](hack-sprint) meeting 12-14th December 2010. 



Heriot-Watt will host the [
http://saints.sqpn.com/blessed-clement-of-dunblane/ Blessed Clement of Dunblane](http://saints.sqpn.com/blessed-clement-of-dunblane/ Blessed Clement of Dunblane) [HackDay](gp-h-eden/hack-day_-mar13) on Tue March 19th, 2013.



A series of [Parallel Hacker Meetings](gp-h-eden/meetings) has been launched.


---



What follows is an entry page to describe technical documentation of the Eden/GpH implementation in parallel systems with distributed memory.



The different aspects are covered by separate pages reachable from here.


## Modifications to implement Eden (and parallel RTS instances in general)


- [Compiler Ways](gp-h-eden/compiler-ways) for parallelism
- [Startup and Shutdown](gp-h-eden/start-stop) of the parallel system
- [Packing and Unpacking Heap Structures](gp-h-eden/packing)
- [Primitive Operations for Coordination Control](gp-h-eden/primitives)
- [Placeholders, Garbage Collection, other Aspects](gp-h-eden/placeholders-and-gc)


Future work


- event logging for parallel Haskell
- GUM variant of packing code, using global addressing

## People



Here is a list of hackers involved in the development of GpH/Eden


- [ Jost Berthold (JB)](http://www.mathematik.uni-marburg.de/~berthold/)
- [ Vladimir Janjic (VJ)](http://www.cs.st-andrews.ac.uk/~jv), 
- [ Hans-Wolfgang Loidl (HWL)](http://www.macs.hw.ac.uk/~hwloidl/)
- [ Kevin Hammond (KH)](http://www.cs.st-andrews.ac.uk/~kh/) 
- [ Mustafa Aswad (MKA)](http://www.macs.hw.ac.uk/~mka19/) 
- [ Henrique Ferreiro (HE)](http://www.madsgroup.org/staff/henrique/)
- [ Philip W Trinder (PWT)](http://www.macs.hw.ac.uk/~trinder/)
- [ Patrick Maier (PM)](http://www.macs.hw.ac.uk/~pm175)
- [ Abyd Al Zain (AAZ)](http://www.macs.hw.ac.uk/~ceeatia/)
- [ Mischa Dieterle (MD)](http://www.mathematik.uni-marburg.de/~dieterle)
- Thomas Horstmeyer (TH) 

## Communication



 
A mailing list for parallel Haskell implementors has been set up at HWU: 
[ hackpar](http://www.macs.hw.ac.uk/mailman/listinfo.cgi/hackpar)


## Tasks



This list of tasks is based on the discussion during the parallel hackathon, Dec 2009.
Each task has a maintainer associated with it, who looks after the software.


- GpH/Eden Code maintenance: (HWL to oversee)

  - Scheduling: Per-PE scheduling and load distribution policies (VJ)
  - Packing/Serialisation: Turning a graph into a write-/send-able format (JB)
  - Distributed Shared Memory: Abstraction layer over distribution of heap (HWL)
  - Communication: Sending and receiving work and data (MKA)
  - Monitoring/Tracing: MD/TH
- Visualisation Tools: Visualising parallel execution (MD/TH)
- Parallel NoFib Suite: Parallel benchmark programs (HE)
- Testsuite: Programs testing the parallel RTS
- Packaging and building: ???
- Darcs repo maintenance: Main repos in Marburg (JB)
- GpH/Eden tracs ticketing: Bug reports (HWL)


 
   


