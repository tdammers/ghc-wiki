
In GHC 6.5 and subsequently you can import GHC as a Haskell library, which lets you write a Haskell program that has access to all of GHC. 



This page is a place for everyone to add


- Notes about how to get it working
- Comments about the API
- Suggestions for improvement


and so on.


# Getting started



You'll need to get a version of GHC that supports the GHC API.  Either download ghc from [CVS](http://www.haskell.org/ghc/docs/latest/html/building/sec-cvs.html) or use darcs: `darcs get --partial http://darcs.haskell.org/ghc`.  There are also [nightly snapshot distributions](http://www.haskell.org/ghc/dist/current/dist) available.



To use the GHC API you say simply 


```wiki
  import GHC
```


Doing this imports the module `GHC` from the package `ghc`, which comes with GHC 6.5 and subsequent.  The module GHC exports the "GHC API", which is still in a state of flux.  Currently it's not even Haddock-documented.  You can see the source code (which is somewhat documented) here 
[
http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/compiler/main/GHC.hs](http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/compiler/main/GHC.hs)



Here's an example main program that does it [Main.hs](/trac/ghc/attachment/wiki/UsingGhcAsLibrary/Main.hs)[](/trac/ghc/raw-attachment/wiki/UsingGhcAsLibrary/Main.hs)


