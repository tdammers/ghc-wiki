# Parallel Haskell Hack Day


## Organisation



Dates : 19.3.2013, starting 10:30



Location : Heriot-Watt University, Edinburgh, Earl Mountbatten Buildings, EM 1.70



It's building 2 on [
http://www.macs.hw.ac.uk/\~hwloidl/hw\_map\_colour.pdf this map](http://www.macs.hw.ac.uk/~hwloidl/hw_map_colour.pdf this map), with entrance from Boundary Road North.
 


## Participants



So far: Hans-Wolfgang Loidl (HWL), Jost Berthold (JB), Vladimir Janjic (VJ), Vladimir Komendantsky (VK), Malak Aljabri (MSA), Majed Al-Saeed (MMAA), Robert Stewart (RS), Evgenij Belikov (EB), Prabhat Totoo (PT)


## Agenda


<table><tr><th> 10:30 </th>
<th> Welcome and Setup 
</th></tr>
<tr><th> 10:45 </th>
<th> Exchange of ideas and brainstorm 
</th></tr>
<tr><th> 11:00 </th>
<th> Pair programming on selected topics 
</th></tr>
<tr><th>       </th>
<th> 
</th></tr>
<tr><th> 13:00 </th>
<th> Lunch 
</th></tr>
<tr><th> 14:00 </th>
<th> Short round of summaries and discussion 
</th></tr>
<tr><th> 14:15 </th>
<th> TBC (pair programming, short talks...) 
</th></tr>
<tr><th> X:00  </th>
<th> Summary Reports and Future Work
</th></tr></table>


## Groups


- RTS group (MSA,EB,JB,HWL)
- Algorithms (PT,JB,HWL)
- Serialisation (VK,JB,HWL)
- Visualisation (MMAA,PWT,JB,HWL) 

## General Info



[
http://james.mathematik.uni-marburg.de:8080/gitweb/ Eden gitweb](http://james.mathematik.uni-marburg.de:8080/gitweb/ Eden gitweb)
[
http://www.macs.hw.ac.uk/\~hwloidl/hackspace/IDX IDX file](http://www.macs.hw.ac.uk/~hwloidl/hackspace/IDX IDX file)


## Potential Topics


- GHC parallel RTS: general house-keeping and fixes  (MSA,EB,VJ,HWL,JB)

  - Cleaning up non-main PEs in parcp way
  - Behaviour of primops in non-parallel ways
  - Start script optimisations
  - eliminating start script in parcp way (i.e. in-RTS log archiving)
  - change PrimOps.cmm to work with LLVM-way
  - debugging issues:

    - role of THUNK\_SELECTOR in packing code
    - RTTables in GUM execution
    - Eden- vs GUM-style execution
    - PAP un/packing (GUM)
    - clean-up issue
    - usage of GhostTSOs (GUM)
  - ParTicky profiling (GUM)

    - fix \#fetches issue
    - fix cmd line parsing (-N) + stats output (-qPg)
    - discuss futher parameters to monitor
  - code integration:

    - migration
    - bounded sparks
    - sharks
- Light-weight concurrency

  - See [
    http://hackage.haskell.org/trac/ghc/wiki/LightweightConcurrency this Wiki page](http://hackage.haskell.org/trac/ghc/wiki/LightweightConcurrency this Wiki page) and checkout the ghc-lwc2 branch in the main GHC repo
- Eden/GpH programming (nbody, paraffins, sp mat mult etc) (PT,RS,EB,HWL,JB)

  - nbody using Eden and GpH skeletons
  - seq and par sp mat mult optimisation (-threaded Array/AssocList vs QuadTrees)
  - move forward wrt. test suite
- Serialisation: (VK,RS,HWL,JB)

  - Factor out from PARALLEL\_RTS
  - review API (for Par Sci usage)
  - make good use of it 
- Eventlog and visualisation (MMAA,JB)
