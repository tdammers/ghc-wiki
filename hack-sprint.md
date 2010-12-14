# Parallel Haskell [HackSprint](hack-sprint)


## Organisation



Dates : 12-14 December 2010



Location : Heriot-Watt


## Participants



So far:
[
Hans-Wolfgang Loidl](http://www.macs.hw.ac.uk/~hwloidl/) (Host), Mischa Dieterle, Thomas Horstmeyer, [
Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold/), [
Henrique Ferreiro](http://www.madsgroup.org/staff/henrique/), David Castro Pérez



To be confirmed:
Vladimir Janjic, [
Kevin Hammond](http://www.cs.st-andrews.ac.uk/~kh/), Chris Brown, Mustafa Aswad, Malak Aljabri, [
Philip W Trinder](http://www.macs.hw.ac.uk/~trinder/), Patrick Maier, Robert Stewart


## Agenda


- Reports on recent software releases
- Updates on work at each of the sites
- Tests & benchmarks

  - Repo
  - System-tests
  - Benchmarks
  - Larger applications
- Interfaces to on-going projects (SCIEnce, HPC-GAP)

  - CASH
  - SC skeletons
  - fault-tolerance
- Packaging
- Roadmap for future development

  - Following [
    ghc HEAD](http://james.mathematik.uni-marburg.de:8080/darcs/darcsweb.cgi??r=ghc-HEAD): status report, coordination and planning
  - Status of/ Plans for existing branches 

    - migration, 
    - tracing, 
    - data locality
    - ...
  - Planned extensions and applications

    - Integration with GHC/SMP
    - Integrating migration
    - Globus interface
    - pre-SCSCP [ GAP](http://www.gap-system.org/) interface
    - Micro-kernel/substrate approach to the RTS

## Eventlog



Sunday sessions:


- Tracing: 

  - overview (TH,MD)
  - integration into GUM build (MKA,MD)
- Test programs: systemtests (HWL)
- MultiCore Challenge application in GpH and Eden (TH,MSA)


Tuesday session:


- Reports and updates on activities
- Hacking Activities

  - CAF bug fixed in eden-6.12 (MD,TH)
  - problem in ghc-HEAD (JB)
  - systemtests on GUM build (HWL)

    - systemtests for GPH
    - Tests Eden for Eden
  - changes in packing code (HWL,JB) 
- Eden usage for Symb Comp (CB,JB)

  - CASH
  - SC skeletons 
- Packaging 

  - configure script for CASH; TODO for edenmodules
- Integration

  - migration, (VJ)
  - tracing, (MKA)
  - data locality (MKA)
- Strategic issues

  - Plans on ghc-HEAD integration

    - Milestones: ghc-6.12-eden-gum
    - Longer term: 

      - scans of ghc-HEAD changes
      - 
    - Marburg Wiki for main tech info exchange
    - Skype calls (1-to-1)
    - regular annual meetings (Challenge)
  - Usage in projects

    - SGP   (CA interfacing)

      - minor release soon, with separate Eden modules
      - next year major release, using Eden package
    - SGPII (fault tolerance, GdH)

      - look at Edi.hs
  - Priorities for new developments
- Benchmarks:

  - nofib/parallel
  - nofib/parallel-eden
  - Strategies examples: push to nofib/parallel
  - Other new programs (karatsuba): put to parfib/


 
[
Docu on latest problems running ghc-HEAD](http://james.mathematik.uni-marburg.de:8080/EdenWiki/BlackholingAndParallelRTS)



[
Marburg Wiki for tech info exchange, up-to-date info with ghc-HEAD and the like](http://james.mathematik.uni-marburg.de:8080/EdenWiki/CategoryTechDocs)


