# Class Framework for Collections : Draft



This page describes a proposal for a class framework for collection types. Related ticket: [\#666](http://gitlabghc.nibbler/ghc/ghc/issues/666)



Please give feedback on libraries at haskell.org.
I'm especially interested in cases where this framework does not fit your application.


## Goals, Non-Goals and Working Hypotheses


- Focus on practical usage. No design in the abstract; what's proposed here shall be usable, and used.
- Provide a clean separation between Collection and Data classes.
- Reduce the amount of explicit module-qualification that is currently required for collection types usage.
- Allow algorithms to be parameterized over collection types.

- No attempt to fit updatable/state-monad-based collections. 
  It looked like a bad idea to mix both updatable/non-updatable collections in a single classes framework.
  However, when updatable/state-monad-based collections are added, they should adopt much of the look-n-feel so as to avoid the IArray/MArray different-name-for-same-functions problem. 
- No attempt to fit 100% of the existing functions into classes. 
  Some of them are just best left in the modules to be accessed qualified.

- The classes are "implementation-based" (in opposition to the Edison idea).

  1. It seemed like a bad idea to do the same thing as something that already existed.
  1. Collections are (mostly) about performance anyway (otherwise everyone would use standard linked-lists).
  1. The type give information about the programmer's expected behaviour of the collection. 
- This deliberately uses many symbols already in the prelude. The intent is that the user hides whatever clashes from Prelude, however it's also possible to import this module qualified.
- This uses functional dependencies. This is motivated by the fact that it gives a lot more expressive power without much inconvenience for the users. (note: We should consider porting this to Associated Types, since it's an alternative for FD in next version of Haskell.)
- A single module is currently proposed, for ease of testing. The final version should of course be properly spilt into modules.
- Most names of classes are only tentative and should be all reconsidered. Good ideas welcome.

## References


- SPJ's Bulk Types with Class : [
  http://research.microsoft.com/%7Esimonpj/Papers/collections.ps.gz](http://research.microsoft.com/%7Esimonpj/Papers/collections.ps.gz)
- The Edison implementation of Okasaki's Functional Data Structures : [
  http://www.eecs.tufts.edu/\~rdocki01/edison.html](http://www.eecs.tufts.edu/~rdocki01/edison.html);
- Java's implementation : [
  http://java.sun.com/j2se/1.5.0/docs/api/java/util/package-summary.html](http://java.sun.com/j2se/1.5.0/docs/api/java/util/package-summary.html)
- C++ STL: [
  http://www.cppreference.com/cppstl.html](http://www.cppreference.com/cppstl.html)

## TODO


- Quite some functions are missing, they are omitted for the sake of brievety,
  until a consensus is reached on the main issues.
  They should be trival to add though. (eg. partition)
- write instances for the new Seq type, following List.\[\]

## Note



Attachment below is outdated, but the library can be found on Hackage:



[
http://hackage.haskell.org/cgi-bin/hackage-scripts/package/collections-0.3](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/collections-0.3)


