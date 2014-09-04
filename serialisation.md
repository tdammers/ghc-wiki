# Serialisation in Haskell



This page collects together the various approaches under
discussion for serialising Haskell values, when communicating
between distributed Haskell processes.



The four main approaches are:


1. Serialisation support in the RTS.
1. Serialisation as a library.
1. Serialisation using template Haskell.
1. Serialisation with a GHC extension for static values.


The following implementations exist.


## Jost Berthold (i.e. Mr Eden)



Jost Berthold has done work on serialisation in the Eden RTS


- RTS support & user API in [
  his IFL 2010 paper](http://www.diku.dk/~berthold/papers/mainIFL10-withCopyright.pdf)

- Jost's [
  HIW 2013 slides on his API design ideas](http://www.haskell.org/wikiupload/2/28/HIW2013PackingAPI.pdf)

## Jost Berthold: packman



Jost has also implemented serialisation as a [
library called packman](https://github.com/jberthold/packman).



Simon PJ asks: is the Haddock'd documentation available anywhere?


## Cloud Haskell



CloudHaskell implements closures with Template Haskell. Closures are
monomorphic in the first implementation. Edsko de Vries made
significant changes, and the hackage document claims it supports static
polymorphic values: [
distributed-static library](http://hackage.haskell.org/package/distributed-static).



Closures are not first class, which makes the template haskell boilerplate code makes code difficult to read for serialising recursive functions. I've documented this, see Figure 2 in 
[
Rob Stewart's MsC project](http://www.macs.hw.ac.uk/~hwloidl/MScProjects/FirstClass-HdpH-Serialisation.pdf)


## HdpH



HdpH also uses Template Haskell for closure creation. The differences in
closure representation between HdpH and CloudHaskell is described in
section 3.2 of [
Patrick Maier's IFL'11 paper](http://www.dcs.gla.ac.uk/~pmaier/papers/Maier_Trinder_IFL2011_XT.pdf)


## Mathieu Boespflug at TweagIO



Mathieu and Facundo Domínguez have implemented a GHC extension called [StaticPointers](static-pointers).



Here's the [
user manual](https://github.com/tweag/ghc/commit/105929e0280f20f2a0f153e380c40cdb2bd9c79c)



Here are [
all the commits](https://github.com/tweag/ghc/pull/1) (to be merged with GHC?):



Mathieu is going to be talking about his serialisation approach on
Saturday at HIW. At least Jost, Patrick and myself will be there.


