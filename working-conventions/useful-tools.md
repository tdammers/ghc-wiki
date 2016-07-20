# Useful tools



This page collects tools which may be useful to humans working on GHC.


- Ben Gamari has a bunch of handy, albeit sometimes poorly documented, tools in his [
  ghc-utils](http://github.com/bgamari/ghc-utils) repository. These include the tools Ben uses for release management, day-to-day maintenance, performance characterization, and general GHC development 
- Joachim Breitner has developed the lovely [
  gipeda](http://github.com/nomeata/gipeda) web interface for comparing performance metrics between commits. This powers [
  perf.haskell.org](http://perf.haskell.org/) (with data from Joachim's [
  ghc-speed-logs](http://github.com/nomeata/ghc-speed-logs) repository)
- Ben Gamari has developed a set of [
  tools](http://github.com/bgamari/ghc-perf-import) for slurping the `ghc-speed-logs` data into a PostgreSQL database and do interesting timeseries analyses on it. See [
  http://home.smart-cactus.org/\~ben/ghc-perf-import/web/](http://home.smart-cactus.org/~ben/ghc-perf-import/web/).
- There is a [script](working-conventions/bisection) for running automated bisections
