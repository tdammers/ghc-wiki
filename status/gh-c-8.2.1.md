# GHC plans for 8.2.1






This page is our road-map for what will be in 8.2.  



We expect GHC 8.2 to be principally a consolidation release, in which we settle down and flesh out existing features, rather than introduce major new features.  In particular, we would like to work on performance, especially of the compiler itself.


- We hope to incorporate all the "Landed" and "In-flight" stuff under "Release highlights" below.

- We'll include (or at least review) all patches in tickets in "Status: patch" below.

- We will address all the tickets under "Status: new" below with "highest" or "high" priority.  We love help to do more, but there are far too many "normal" tickets to make any promises.


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!


## Current status



As of March 27 all of the major changes have landed in the tree. However, there are a variety of smaller tasks, including some late-breaking regression fixes, which need to be resolved.


## Dates



Very tentative: Release candidate by **late March 2017**. Release in **July 2017**.


## Libraries Status



See [Libraries](status/gh-c-8.2.1/libraries)


## Release highlights



Below are the major highlights of 8.2.


### Compile time improvements


- A variety of compile-time improvements.

#### Front-end changes


- **Indexed `Typeable` representations** [Typeable/BenGamari](typeable/ben-gamari) (Ben Gamari, Simon Peyton Jones, et al). While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

>
>
> GHC 8.2 will address this by introducing indexed type representations, leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [
> paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/) for an description of the proposal and the [
> Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari) for the current status of the implementation.
>
>

- **Deriving**:

  - Deriving strategies (Ryan Scott): grant users the ability to choose explicitly how a class should be `derived` (using a built-in algorithm, `GeneralizedNewtypeDeriving`, `DeriveAnyClass`, or otherwise), addressing [\#10598](http://gitlabghc.nibbler/ghc/ghc/issues/10598).

- **Levity polymorphism**.  Richard E is actively involved in consolidating the implementation of levity polymorphism; see [LevityPolymorphism](levity-polymorphism).  This will not include resolving the `Constraint` vs `*` debate [\#11715](http://gitlabghc.nibbler/ghc/ghc/issues/11715).

- **Backpack** (Edward Z Yang) has been merged into GHC 8.2. However, at this point, it has only really been seriously used by Edward, and so it really needs testing! For up-to-date links to documentation and reference material, see [Backpack](backpack)

- **Exhaustiveness checking**

  - Exhaustiveness checking for `EmptyCase`s ([
    Phab:D2105](https://phabricator.haskell.org/D2105)), addressing [\#10746](http://gitlabghc.nibbler/ghc/ghc/issues/10746). 
  - Exhaustiveness checking for pattern synonyms, including the ability to define sets of synonyms that constitute a complete match, addressing [\#8779](http://gitlabghc.nibbler/ghc/ghc/issues/8779) (Matthew Pickering),

- **Overloaded record fields** (Adam Gundry, [
  Phab:D2708](https://phabricator.haskell.org/D2708), [
  Phab:D3144](https://phabricator.haskell.org/D3144)): Changes to `IsLabel` class used by `OverloadedLabels` extension, plus the addition of a `HasField` class supporting polymorphism over record field selectors, but no `OverloadedRecordFields` extension or `IsLabel x (r -> a)` instance. See [
  the proposal and discussion](https://github.com/ghc-proposals/ghc-proposals/pull/6) for more details.

- **Unpacked sum types** (Ömer Sinan Ağacan, [wiki](unpacked-sum-types)): Allowing for efficient, unpacked representation of sum data types.

#### Back-end and runtime system


- **Compact regions** (Giovanni Campagna, Edward Yang, [
  Phab:D1264](https://phabricator.haskell.org/D1264), [
  paper](http://ezyang.com/papers/ezyang15-cnf.pdf)). This runtime system feature allows a referentially "closed" set of heap objects to be collected into a "compact region", allowing cheaper garbage collection, heap-object sharing between processes, and the possibility of inexpensive serialization. Use the [
  compact](https://hackage.haskell.org/package/compact) library to get access to this feature!

- **Refactoring and improvements to the cost-center profiler** (Ben Gamari, [
  Phab:D1722](https://phabricator.haskell.org/D1722)): Allow heap profiler samples to be directed to the GHC eventlog, allowing correlation with other program events, enabling easier analysis by tooling and eventual removal of the old, rather crufty `.hp` profile format. 

- **Support for NUMA systems** (Simon Marlow, [
  in-progress](https://github.com/simonmar/ghc/tree/numa)).  The aim is to reduce the number of remote memory accesses for multi-socket systems that have a mixture of local and remote memory.

- **Experimental changes to the scheduler** (Simon Marlow, [
  in progress](https://github.com/simonmar/ghc/commit/7e05ec18b4eda8d97e37015d415e627353de6b50)) that enable the number of threads used for GC to be lower than the `-N` setting.

- **Improved idle CPU usage**: A long-standing regression ([\#11965](http://gitlabghc.nibbler/ghc/ghc/issues/11965)) resulting in unnecessary wake-ups in an otherwise idle program was fixed. This should lower CPU utilization and improve power consumption for some programs.

- **Live streaming of event-log data** ([
  Phab:2934](https://phabricator.haskell.org/2934))

- **Further improvements to debugging information** (Ben Gamari): There are still a number of outstanding issues with GHC's DWARF implementation, some of which even carry the potential to crash the runtime system during stacktrace collection. GHC 8.2 will hopefully have these issues resolved, allowing debugging   - TODO information to be used by end-user code in production.

>
>
> With stable stack unwinding support comes a number of opportunities for new serial and parallel performance analysis tools (e.g. statistical profiling) and debugging. As GHC's debugging information improves, we expect to see tooling developed to support these applications. See the [
> DWARF status page](https://ghc.haskell.org/trac/ghc/wiki/DWARF/80Status) for further information.
>
>

### Optimiser and intermediate language


- [Implement join points](sequent-core) (Luke Maurer). This makes the compiler's join points optimization more robust, resulting in significantly better code generation in some cases.

#### Library changes


- Merge `Bifoldable` and `Bitraversable` into `base`, addressing [\#10448](http://gitlabghc.nibbler/ghc/ghc/issues/10448) (Edward Kmett, Ryan Scott)

#### Build system and miscellaneous changes


- Deterministic builds [DeterministicBuilds](deterministic-builds). Given the same environment, file and flags produce ABI compatible binaries. (Bartosz Nitka, in-progress)

## Tickets marked merge with no milestone




  
  
  
  
  
    

## Status: merge (1 match)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, milestone: , group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, milestone: , group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: merge, milestone: , group: status, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#16094](http://gitlabghc.nibbler/ghc/ghc/issues/16094)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [panic! (the 'impossible' happened): for powerpc-unknown-linux getRegister(ppc): I64\[I32\[BaseReg + 812\] + 64\]](http://gitlabghc.nibbler/ghc/ghc/issues/16094)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      trommler
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



## Tickets slated for 8.2.1


### merge/patch/upstream




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Differential Rev(s) (Ticket query: status: merge, status: patch,
status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type,
col: summary, col: priority, col: differential, col: owner, order: differential)
      </th>
<th>
        
        Owner (Ticket query: status: merge, status: patch, status: upstream,
milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary,
col: priority, col: differential, col: owner, order: owner)
      </th>
<td>
    </td></tr>
<tr><td>
          </td>
<th>
            No tickets found
          </th>
<td>
        </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



### new




  
  
  
  
  
    

## Status: new (5 matches)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: new, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: new, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: type)
      </th>
<th>
        
        Summary (Ticket query: status: new, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: new, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1,
order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: new, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#8040](http://gitlabghc.nibbler/ghc/ghc/issues/8040)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [installed include/HsVersions.h  wants to \#include "../includes/ghcautoconf.h"](http://gitlabghc.nibbler/ghc/ghc/issues/8040)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10010](http://gitlabghc.nibbler/ghc/ghc/issues/10010)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [LLVM/optimized code for sqrt incorrect for negative values](http://gitlabghc.nibbler/ghc/ghc/issues/10010)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10249](http://gitlabghc.nibbler/ghc/ghc/issues/10249)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [GHCi leaky abstraction: error message mentions \`ghciStepIO\`](http://gitlabghc.nibbler/ghc/ghc/issues/10249)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13194](http://gitlabghc.nibbler/ghc/ghc/issues/13194)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Concurrent modifications of package.cache are not safe](http://gitlabghc.nibbler/ghc/ghc/issues/13194)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#14503](http://gitlabghc.nibbler/ghc/ghc/issues/14503)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Killing a thread will block if there is another process reading from a handle](http://gitlabghc.nibbler/ghc/ghc/issues/14503)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



### infoneeded




  
  
  
  
  
    

## Status: infoneeded (1 match)


  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: infoneeded, milestone: 8.2.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: status: infoneeded, milestone: 8.2.1, group: status,
max: 0, col: id, col: type, col: summary, col: priority, col: owner,
order: type)
      </th>
<th>
        
        Summary (Ticket query: status: infoneeded, milestone: 8.2.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: status: infoneeded, milestone: 8.2.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Owner (Ticket query: status: infoneeded, milestone: 8.2.1,
group: status, max: 0, col: id, col: type, col: summary, col: priority,
col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13739](http://gitlabghc.nibbler/ghc/ghc/issues/13739)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [very slow linking of executables with ld.bfd \< 2.27](http://gitlabghc.nibbler/ghc/ghc/issues/13739)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



