## Cloud Haskell



Cloud Haskell is a distributed computing framework for Haskell, implemented in Haskell. It's a tool for writing applications that coordinate their work on a cluster of commodity computers or virtual machines. This is useful for providing highly reliable, redundant, long-running services, as well as for building compute-intensive applications that can benefit from lots of hardware. It has two interfaces:


- The *process layer* (aka [ErlangInHaskell](erlang-in-haskell)): an interface based on message-passing between distributed processes.
- The *task layer* (aka SkywritingInHaskell): a fault-tolerant data-centric interface.


Here are some resources relevant to this project:


- [
  Jeff Epstein, Andrew P. Black, Simon Peyton-Jones: Towards Haskell in the Cloud](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf)
- [ Project webpage](http://haskell-distributed.github.io/)
- [
  The source repository](https://github.com/haskell-distributed/distributed-process)
- [
  Haskell Wiki page](http://www.haskell.org/haskellwiki/GHC/CloudAndHPCHaskell)
- [Design notes](static-values) for a language extension to support the `static` form.
