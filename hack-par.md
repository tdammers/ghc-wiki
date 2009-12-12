# Parallel Haskell Hackathon


## Organisation



Dates : 10-12 December



Location : St Andrews


## Participants



Vladimir Janjic (Host), [
Hans-Wolfgang Loidl](http://www.macs.hw.ac.uk/~hwloidl/), [
Kevin Hammond](http://www.cs.st-andrews.ac.uk/~kh/), Mustafa Aswad, [
Henrique Ferreiro](http://www.madsgroup.org/staff/henrique/), [
Philip W Trinder](http://www.macs.hw.ac.uk/~trinder/), Patrick Maier, Abyd Al Zain, Mischa Dieterle, Thomas Horstmeyer, [
Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold/), Christoph Herrmann, [
Simon Marlow](http://www.haskell.org/~simonmar/) (virtual),


## Source code



Important: if you can, please prepare a setup of the latest sources in advance.
A darcs repository has been set up on the server in Marburg, please read here how to get a working setup: [
Repository Briefing](http://james.mathematik.uni-marburg.de:8080/EdenWiki/DarcsRepoCheatSheet).



You should contact Mischa (dieterle\@mathematik...) in order to get access, or check out read-only via http (which is of course not what we want later).
If you have questions or problems, edit the page or mail Jost (berthold\@mathematik...)


## Agenda



An [Agenda Proposal](agenda-proposal) is now on a separate page.


#### Topics to cover in the Hackathon:


- Short overview sessions on implementation internals (KH: these should not all be on the first afternoon!):

  - [ GUM](http://www.macs.hw.ac.uk/~dsg/gph/) (Mustafa)
  - [ Eden](http://www.mathematik.uni-marburg.de/~eden/) (Jost)
  - [ Globus](http://www.globus.org/) (Abyd)
  - Migration/Load Balancing (Vladimir)
  - [ ghc](http://haskell.org/ghc) HEAD developments (Simon)
  - GUM/Eden Compilation and Debugging (Vladimir?)
- GUM-6 port
- Merging GUM and Eden implementations

  - Packing code
  - Scheduler
  - GC interface
  - Tagging
- Usage of new tracing infrastructure
- Development infrastructure

  - Unified revision control (darcs)
  - Debugging infrastructure
  - Packaging
  - Setup for automatic tests
- Parallel nofib-suite
- Planned extensions and applications

  - Integration with GHC/SMP
  - Integrating migration
  - Globus interface
  - pre-SCSCP [ GAP](http://www.gap-system.org/) interface
  - Micro-kernel/substrate approach to the RTS
- Nominating people in charge of sub-projects

  - parallel nofib suite
  - Eden/GUM code maintenance
  - testing framework
- Prize for the most awkward bug fixed in the Hackaton. <sub>~~Jost: I am non-competitive in this contest, since my task is to introduce them ;)~~</sub> <sub>~~HWL: Clearly you are at an disadvantage: you first have to introduce the bug; we don't have that problem;-)~~</sub>

## Expected Outcomes



Concrete Deliverables


- Tracing mechanism
- Unified repository, containing Eden and GUM code
- Common test platform - we can talk to Bodo Scholz about unibench
- List of people in charge of sub-projects
- Longer Term: Eden&GUM in GHC HEAD


Sub-tasks in Eden/GUM/GHC integration:


- Merge Eden/GUM repository
- Merge GUM into current Eden version
- Adapt for per-CPU/Core GC
- Integrating Vladimirs work (load balancing, migration)


Longer term system issues


- Manycore
- Hierarchies
- HPC
- Grid
- Cloud
- GPGPUs
- Artcop
- Revisit design decisions, e.g. cycles at the global level


Others


- Update plan btw Eden/GUM and GHC-HEAD
- Community creation
- PhD Topic for Henrique
- Benchmarks/Parallel NoFib suite - we need to be able to classify expected parallel behaviour as well as detecting e.g. space leaks
- Launch GpH book
- Long Term: Build Franchise
- Agree on new Strategies module (rewrite applications)
- Visualisation tools (which?)
- Revisit design decisions for Eden/GUM

## Discussions



SM introduced pointer tagging; it will be necessary to follow indirections, and maintain tags on
exported closures  we need to study evac.c looking at UNTAG\_CLOSURE.


## Decisions



We prioritised setting up a common repository and Eden/GUM integration.  Benchmarking and packaging
were seen as longer term issues.



We agreed to use the GHC ticketing system, with some specific identification for Eden/GUM issues.
We agreed to use a common darcs repository for Eden/GUM.



We agreed that VJ's GUM 4.06 changes should be integrated with the combined system.
The priority is to integrate thread migration.


## Community Building



We need to find common ways of working to maximise our effort.



darcs for repositories



skype for communication



do we want dedicated mailing lists/IRC channels?


## Who Does What



We agreed to setup hacker teams to work on different issues as follows


### Repository: VJ + JB



Set up a common repository to hold GUM and Eden.  Later, we should try to integrate this with the main
GHC repository.


### GUM/Eden Integration


- Integrating basic GUM code into Eden version 6.13: HWL?


\[IntegratingProgress Progress\]


- Packing code (incl. tags): HWL+ MKA + JB


\[PackingProgress Progress\]


- Scheduler: HWL + HF + PM + PWT


\[SchedulerProgress Progress\]


- GC Interface + Global Addresses (with tags): HWL + VJ


Do we retain FETCHME closures or e.g. use a table



\[GarbageCollectionProgress Progress\]


- Tracing (agree format): KH + JB + MD + TH + HWL


We need to agree a format!  We want tools to work on all formats.  We agreed with SM to extend the threadscope event format,
adding in new event types for distributed/more detailed events.



\[TracingProgress Progress\]


- Thread Migration (packing TSOs, dealing with black holes, plus shark etc mechanisms): PWT + VJ


\[ThreadMigrationProgress Progress\]


### Visualisation Tools: HF + JB + KH + MKA + PWT



The visualisation tools need to work with the agreed format.  We may need more detailed views
and/or interactive versus publication tools.



[Progress](visualisation-progress)


### Benchmarking: HF + MD + JB  + everyone



We agreed to hold a discussion on benchmarking on Friday.



\[BenchmarkingProgress Progress\]


### Packaging and Builds



This is important in the long-term.  For now we left it open until we had made progress on an integrated system.



We need to consider MPI support.



\[PackagingProgress Progress\]


## Taking Things Forward


### Organisation



Information: Haskell Wiki, GHC Wiki



Meeting: once per year at e.g. IFL in Utrecht + private meeting



Repository: where to live?  darcs at Marburg plus bleeding edge on Ladybanks at St Andrews



Experimental/Development machines: ladybanks at St Andrews generally open



Ticketing: use GHC TRAC - HWL to organise



Communication: skype (share account names), mailing list (common for parallel haskell - Patrick to organise)



KH: I have created a new twitter account - [
parallelhaskell](http://twitter.com/parallelhaskell)


### What we did and what is still to do



[--\> back to GpHEden](gp-h-eden)


