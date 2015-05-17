# Developer’s Infrastructure



There are a number of services and tools that GHC developers should be aware of.


## Official tools


- Our [git repositories](repositories), which are best used following the [
  Working Conventions](https://ghc.haskell.org/trac/ghc/wiki/)
- The trac instance you are just using, which hosts the [wiki](/trac/ghc/wiki) and the [bug tracker](/trac/ghc/report)
- Our [mailing lists](mailing-lists-and-irc)
- For code review, we use a [Phabricator](phabricator) instance at [
  https://phabricator.haskell.org/](https://phabricator.haskell.org/)
- We use [Travis](travis) to validate each new commit: see [
  https://travis-ci.org/ghc/ghc/builds](https://travis-ci.org/ghc/ghc/builds) for the current status
- We also validate new commits using [Harbormaster](phabricator/harbormaster): see [
  https://phabricator.haskell.org/diffusion/GHC/history/](https://phabricator.haskell.org/diffusion/GHC/history/) for the current status
- The evolution of the performance of our master branch is tracked on [
  http://perf.haskell.org/ghc](http://perf.haskell.org/ghc)
- We also use dedicated [builders](builder) to do nightly [
  builds](http://haskell.inf.elte.hu/builders/) of GHC and the [
  user documentation](http://haskell.inf.elte.hu/docs/). See the [ builder summary page](builder-summary) for more information. 

## External tools



There are a bunch of external services that might be of interest to GHC developers:


- Source code statistics on Ohloh: [
  https://www.ohloh.net/p/ghc](https://www.ohloh.net/p/ghc)
- Similar statistics, on GitHub: [
  https://github.com/ghc/ghc/graphs](https://github.com/ghc/ghc/graphs)
