# Hackage Testing



A great way to test changes to GHC (and Cabal) is to compile all packages on Hackage and compare the results to other runs that use different options or versions of GHC.



We have an automated program for performing this, allowing all of Hackage to act as a regression test. At the moment we just compare which packages build to which ones don't under two different runs. In the future we could also run a packages testsuite and/or benchmark suite to get more information and also track performance regressions.



This tool is now hosted on [
GitHub](https://github.com/dterei/Hackager) and on [
Hackage](http://hackage.haskell.org/package/hackager).


```wiki
$ cabal install hackager
$ ...
$ hackager --help
```