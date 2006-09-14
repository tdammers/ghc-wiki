# Overall organisation of GHC



Start at the [
GHC home page](http://haskell.org/ghc).  The most important links are
in the left-hand column:


- [
  Documentation](http://haskell.org/haskellwiki/GHC).  This is the *user* documentation, aimed at people who use GHC, but don't care how it works.  It's on the Haskell Wiki (powered by MediaWiki), and we strongly encourage people to edit and improve it.

- [
  Developers](http://hackage.haskell.org/trac/ghc).  This link takes you to the home page for *developers*; that is, people interested in hacking on GHC itself (i.e. you).  It's a Wiki too, but powered by Trac, and includes bug-tracking etc.  There is a big section called Developer Documentation: **please help us to improve it**.

- [Download](http://www.haskell.org/ghc/download.html).  At any moment, GHC has a **STABLE branch** and the **HEAD**, both of which you can download from this page.

  - The STABLE branch is the current released version.  It has an even version number (e.g. 6.4, 6.6), with an extra suffix for patch-level release (e.g. 6.4.2).  Patch-level releses fix bugs; they do not change any APIs.

  - The HEAD is simply the latest, greatest version that we are working on; it may be broken on any given day, although you are encouraged not to break it gratuitiously.  The HEAD has an odd version numbers (e.g 6.5, 6.7).  Every night we build the HEAD, and dump the result on the download site under "Development snapshots", with a version number that encodes the date (e..g 6.5.20060831).

>
> >
> >
> > A very useful link on the download page is the [documentation for the HEAD](http://www.haskell.org/ghc/dist/current/docs/) (under Development snapshots).  Useful because typesetting the documentation uses DocBook, which easy to install on every platform.
> >
> >
>

# GHC source code



GHC's source code is several Darcs repositories.  The important ones are:



[
http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc):: All of GHC: compiler, run-time system, support utilities.



[
http://darcs.hasekll.org/packages/pkg](http://darcs.hasekll.org/packages/pkg):: A library package *pkg*.  A certain number of packages are essential to build GHC. They are listed in `libraries/core-packages` and currently comprise: `base`, 
`Cabal`,
`haskell98`,
`readline`,
`regex-base`,
`regex-compat`,
`regex-posix`,
`parsec`,
`stm`,
`template-haskell`,
`unix`,
`Win32`.



[
http://darcs.haskell.org/testsuite](http://darcs.haskell.org/testsuite):: GHC's test suite.


