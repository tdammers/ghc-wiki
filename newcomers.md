# Resources for newcomers to GHC



This page is intended to serve as the first stop for those people who say, "I want to contribute to GHC, but I don't know quite where to begin." Begin here. While the [building guide](building), [working conventions](working-conventions), [commentary](commentary) and [debugging](debugging) pages (always linked from the left sidebar) have great information that can come in handy while you're working on your first, or first several patches, this page is intended to have the details you will need to get rolling.



If you have any questions along the way don't hesitate to reach out to the community. There are people on the [mailing lists and IRC](mailing-lists-and-irc) who will gladly help you (although you may need to be patient). Don't forget that all GHC developers are still learning; your question is never too silly to ask.


## First steps



[Prepare](building/preparation) your machine, [clone](building/getting-the-sources) the git repo, and [Building/QuickStart](building/quick-start) GHC. For the short, short version, which may or may not work for your machine, you can try this (note that [
building older versions of GHC may require having an older version of GHC on your path](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools)):


```
# needed only once, URL rewrite rule is persisted in ${HOME}/.gitconfig
git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/
# (if you already cloned using ssh, you'll need this rule instead to make submodules work:)
# git config --global url."git@github.com:ghc/packages-".insteadOf git@github.com:ghc/packages/

# clone GHC's main Git repository (creates './ghc' folder in CWD)
git clone --recursive git://github.com/ghc/ghc
cd ghc/

# configure build
cp mk/build.mk.sample mk/build.mk

## edit mk/build.mk to remove the comment marker # on the line "BuildFlavour = devel2"

./boot
./configure
# NOTE: On Windows you need to download some binary distributables before being able to build
# This only has to be done once and can be done by adding a flag to the call to configure:
./configure --enable-tarballs-autodownload

# build GHC
make -j8 # parallelize to at most 8 parallel jobs; adapt to actual number of cpu cores
```

>
>
> If your machine has all the prerequisites, this might just work. Expect it all to take roughly 30 minutes.
>
>

- While you are waiting for your build to finish, orient yourself to the general architecture of GHC. This [
  article](http://www.aosabook.org/en/ghc.html) is written by two of the chief architects of GHC, Simon Marlow and Simon Peyton-Jones, is excellent and current (2012).

- After a successful build, you should have your brand new compiler in `./inplace/bin/ghc-stage2`. (GHCi is launched with `./inplace/bin/ghc-stage2 --interactive`). Try it out.

## Fast rebuilding



There are 4 things to remember:


1. Select `BuildFlavour = devel2` in your `mk/build.mk` file (copy `mk/build.mk.sample` to `mk/build.mk` first), to
  [make GHC build more quickly](building/using#ow-to-make-ghc-build-quickly).

1. Don't run `make` directly in the ghc root directory (unless you just pulled in changes from others). Instead, first
  change to the directory (`compiler`, `utils`, `ghc` or `libraries`) where you're making your changes.
  See [Building a single sub-component](building/using#).


 


1. Set `stage=2` in your `mk/build.mk` file, to
  [freeze the stage 1 compiler](building/using#reezing-stage-1).
  This makes sure that only the
  [stage-2](building/architecture/idiom/stages) compiler will be
  rebuilt after this.

1. While in the sub-component directory, use `make fast`
  [skip dependency building](building/using#kip-dependency-building) (except after pulling in changes from others).


A good first sanity check is to twiddle some error message in the code, just to see that changed error message pop up when you compile a file. Write some Haskell code with an error in it, and look at the error message. Search through the code for that error message. Change the message, rebuild ghc (run `make fast` in the `ghc` directory), and recompile your file again with `./inplace/bin/ghc-stage2`. If you see the changed message, you're good to go.


## Finding a ticket



Now that you can build GHC, let's get hacking. But first, you'll need to identify a goal. GHC's Trac tickets are a great place to find starting points. You are encouraged to ask for a starting point on IRC or the `ghc-devs` [mailing list](mailing-lists-and-irc). There someone familiar with the process can help you find a ticket that matches your expertise and help you when you get stuck.



If you want to get a taste for possible starting tasks, below is a list of tickets that appear to be "low-hanging fruit" -- things that might be reasonable for a newcomer to GHC hacking. Of course, we can't ever be sure of how hard a task is before doing it, so apologies if one of these is too hard.



You can add tickets to this list by giving them the `newcomer` Trac keyword.



**Bugs:**

<table><tr><th>[\#8316](http://gitlabghc.nibbler/ghc/ghc/issues/8316)</th>
<td>GHCi debugger panics when trying force a certain variable</td></tr>
<tr><th>[\#10346](http://gitlabghc.nibbler/ghc/ghc/issues/10346)</th>
<td>Cross-module SpecConstr</td></tr>
<tr><th>[\#11004](http://gitlabghc.nibbler/ghc/ghc/issues/11004)</th>
<td>hsc2hs does not handle single quotes properly</td></tr>
<tr><th>[\#11068](http://gitlabghc.nibbler/ghc/ghc/issues/11068)</th>
<td>Make Generic/Generic1 methods inlinable</td></tr>
<tr><th>[\#12488](http://gitlabghc.nibbler/ghc/ghc/issues/12488)</th>
<td>Explicit namespaces doesn't enforce namespaces</td></tr>
<tr><th>[\#12576](http://gitlabghc.nibbler/ghc/ghc/issues/12576)</th>
<td>Large Address space is not supported on Windows</td></tr>
<tr><th>[\#12636](http://gitlabghc.nibbler/ghc/ghc/issues/12636)</th>
<td>ProfHeap's printf modifiers are incorrect</td></tr>
<tr><th>[\#13165](http://gitlabghc.nibbler/ghc/ghc/issues/13165)</th>
<td>Speed up the RTS hash table</td></tr>
<tr><th>[\#13193](http://gitlabghc.nibbler/ghc/ghc/issues/13193)</th>
<td>Integer (gmp) performance regression?</td></tr>
<tr><th>[\#13452](http://gitlabghc.nibbler/ghc/ghc/issues/13452)</th>
<td>Lock .tix file</td></tr>
<tr><th>[\#13624](http://gitlabghc.nibbler/ghc/ghc/issues/13624)</th>
<td>loadObj() does not respect alignment</td></tr>
<tr><th>[\#13795](http://gitlabghc.nibbler/ghc/ghc/issues/13795)</th>
<td>:kind! is not expanding type synonyms anymore</td></tr>
<tr><th>[\#14069](http://gitlabghc.nibbler/ghc/ghc/issues/14069)</th>
<td>RTS linker maps code as writable</td></tr>
<tr><th>[\#14899](http://gitlabghc.nibbler/ghc/ghc/issues/14899)</th>
<td>Significant compilation time regression between 8.4 and HEAD due to coverage checking</td></tr>
<tr><th>[\#15227](http://gitlabghc.nibbler/ghc/ghc/issues/15227)</th>
<td>Add PrelRules for par\#</td></tr>
<tr><th>[\#15252](http://gitlabghc.nibbler/ghc/ghc/issues/15252)</th>
<td>syn\_arg\_wraps and syn\_res\_wrap are only populated after typechecking</td></tr>
<tr><th>[\#15402](http://gitlabghc.nibbler/ghc/ghc/issues/15402)</th>
<td>The settings and behaviour of idle GC are very confusing</td></tr>
<tr><th>[\#15540](http://gitlabghc.nibbler/ghc/ghc/issues/15540)</th>
<td>GHCi does not follow the XDG Base Directory Specification</td></tr>
<tr><th>[\#15603](http://gitlabghc.nibbler/ghc/ghc/issues/15603)</th>
<td>ref6 example from StaticPointers documentation doesn't type check</td></tr>
<tr><th>[\#15660](http://gitlabghc.nibbler/ghc/ghc/issues/15660)</th>
<td>source file modify race leads to inconsistent error message</td></tr>
<tr><th>[\#15784](http://gitlabghc.nibbler/ghc/ghc/issues/15784)</th>
<td>:doc shouldn't report \<has no documentation\> for a data constructor when it can show docs for the type constructor of the same name and type</td></tr>
<tr><th>[\#15820](http://gitlabghc.nibbler/ghc/ghc/issues/15820)</th>
<td>Document the proposals process in the GHC manual</td></tr>
<tr><th>[\#15836](http://gitlabghc.nibbler/ghc/ghc/issues/15836)</th>
<td>ghc-in-ghci script fails when there is a Main.hs in the top-level directory</td></tr>
<tr><th>[\#15839](http://gitlabghc.nibbler/ghc/ghc/issues/15839)</th>
<td>DerivingStrategies defaulting warning has no associated enable/suppress flag</td></tr>
<tr><th>[\#15843](http://gitlabghc.nibbler/ghc/ghc/issues/15843)</th>
<td>Tuple sections can't be quoted</td></tr>
<tr><th>[\#15849](http://gitlabghc.nibbler/ghc/ghc/issues/15849)</th>
<td>Error message: "Perhaps you need a let in a do block", when there is no do block.</td></tr>
<tr><th>[\#15932](http://gitlabghc.nibbler/ghc/ghc/issues/15932)</th>
<td>DeriveFunctor and GeneralizedNewtypeDeriving instances never reporting as covered</td></tr>
<tr><th>[\#15935](http://gitlabghc.nibbler/ghc/ghc/issues/15935)</th>
<td>TYPE is not generated by genprimops</td></tr>
<tr><th>[\#15963](http://gitlabghc.nibbler/ghc/ghc/issues/15963)</th>
<td>Test suite should report timeouts as timeouts</td></tr>
<tr><th>[\#15973](http://gitlabghc.nibbler/ghc/ghc/issues/15973)</th>
<td>Int used to represent target integer literals</td></tr>
<tr><th>[\#16167](http://gitlabghc.nibbler/ghc/ghc/issues/16167)</th>
<td>-ddump-json doesn't work with -e</td></tr>
<tr><th>[\#16168](http://gitlabghc.nibbler/ghc/ghc/issues/16168)</th>
<td>Prelude docs for Integer mention J\#</td></tr>
<tr><th>[\#16196](http://gitlabghc.nibbler/ghc/ghc/issues/16196)</th>
<td>Update README.md to reflect gitlab</td></tr>
<tr><th>[\#16235](http://gitlabghc.nibbler/ghc/ghc/issues/16235)</th>
<td>Hadrian devel2 builds Haddock</td></tr></table>


**Feature requests:**

<table><tr><th>[\#7275](http://gitlabghc.nibbler/ghc/ghc/issues/7275)</th>
<td>Give more detailed information about PINNED data in a heap profile</td></tr>
<tr><th>[\#8109](http://gitlabghc.nibbler/ghc/ghc/issues/8109)</th>
<td>Type family patterns should support as-patterns.</td></tr>
<tr><th>[\#12178](http://gitlabghc.nibbler/ghc/ghc/issues/12178)</th>
<td>Allow inline pragmas on pattern synonyms</td></tr>
<tr><th>[\#12982](http://gitlabghc.nibbler/ghc/ghc/issues/12982)</th>
<td>Missed constant folding oportunities</td></tr>
<tr><th>[\#15461](http://gitlabghc.nibbler/ghc/ghc/issues/15461)</th>
<td>Machine accessible interface to GHCi</td></tr>
<tr><th>[\#15483](http://gitlabghc.nibbler/ghc/ghc/issues/15483)</th>
<td>ghc -M requires -dep-suffix for no good reason</td></tr>
<tr><th>[\#16155](http://gitlabghc.nibbler/ghc/ghc/issues/16155)</th>
<td>Pattern Synonym for Ratio</td></tr>
<tr><th>[\#16164](http://gitlabghc.nibbler/ghc/ghc/issues/16164)</th>
<td>Provide bitreverse primop</td></tr></table>


**Tasks:**

<table><tr><th>[\#4960](http://gitlabghc.nibbler/ghc/ghc/issues/4960)</th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th>[\#10068](http://gitlabghc.nibbler/ghc/ghc/issues/10068)</th>
<td>Make the runtime reflection API for names, modules, locations more systematic</td></tr>
<tr><th>[\#11610](http://gitlabghc.nibbler/ghc/ghc/issues/11610)</th>
<td>Remove IEThingAll constructor from IE datatype</td></tr>
<tr><th>[\#12619](http://gitlabghc.nibbler/ghc/ghc/issues/12619)</th>
<td>Allow users guide to be built independently from GHC</td></tr>
<tr><th>[\#12687](http://gitlabghc.nibbler/ghc/ghc/issues/12687)</th>
<td>Add a flag to control constraint solving trace</td></tr>
<tr><th>[\#12822](http://gitlabghc.nibbler/ghc/ghc/issues/12822)</th>
<td>Cleanup GHC verbosity flags</td></tr>
<tr><th>[\#13698](http://gitlabghc.nibbler/ghc/ghc/issues/13698)</th>
<td>Add a more complete example for the special SPEC argument to the user guide</td></tr>
<tr><th>[\#13892](http://gitlabghc.nibbler/ghc/ghc/issues/13892)</th>
<td>Add some benchmarks to nofib from Andras Kovac's Eff benchmarks</td></tr>
<tr><th>[\#13923](http://gitlabghc.nibbler/ghc/ghc/issues/13923)</th>
<td>Add a suppression flag to stop Typeable bindings being emitted with -ddump-simpl</td></tr>
<tr><th>[\#14023](http://gitlabghc.nibbler/ghc/ghc/issues/14023)</th>
<td>Split up glasgow\_exts.rst</td></tr>
<tr><th>[\#14099](http://gitlabghc.nibbler/ghc/ghc/issues/14099)</th>
<td>Document fundeps</td></tr>
<tr><th>[\#15651](http://gitlabghc.nibbler/ghc/ghc/issues/15651)</th>
<td>Check if some auto apply code is dead and remove if appropriate.</td></tr>
<tr><th>[\#15821](http://gitlabghc.nibbler/ghc/ghc/issues/15821)</th>
<td>Implement more constant folding for Naturals</td></tr>
<tr><th>[\#15929](http://gitlabghc.nibbler/ghc/ghc/issues/15929)</th>
<td>Explore whether adding XRay attributes to LLVM IR is worthwhile</td></tr>
<tr><th>[\#16052](http://gitlabghc.nibbler/ghc/ghc/issues/16052)</th>
<td>Core optimizations for memset on a small range</td></tr>
<tr><th>[\#16062](http://gitlabghc.nibbler/ghc/ghc/issues/16062)</th>
<td>Improve -dynamic-too progress messages</td></tr>
<tr><th>[\#16126](http://gitlabghc.nibbler/ghc/ghc/issues/16126)</th>
<td>Make -threaded the default</td></tr></table>



## Practical advice


- Read up on the steps you are expected to take for [contributing a patch to GHC](working-conventions/fixing-bugs).

- See also code reviews in [
  Phabricator](https://phabricator.haskell.org/differential/). You can refer to how they have been fixed.

## Less practical advice


- Don't get scared. GHC is a big codebase, but it makes sense when you stare at it long enough!

- Don't hesitate to ask questions. We have all been beginners at some point and understand that diving in to GHC can be a challenge. Asking questions will help you make better use of your hacking time.

- Be forewarned that many pages on the GHC Wiki are somewhat out-of-date. Always check the last modification date. Email `ghc-devs` if you're not sure.

- You may want to look at these "how it went for me" blog posts.

  - [
    Hacking on GHC (is not that hard)](http://rawgit.com/gibiansky/4c54f767bf21a6954b23/raw/67c62c5555f40c6fb67b124307725df168201361/exp.html) by Andrew Gibiansky
  - [
    Contributing to GHC](http://anniecherkaev.com/projects/contributing-to-ghc) by Annie Cherkaev
  - [
    Contributing to GHC via Phabricator](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147) by Moritz Angermann

- There is a blog post series by Stephen Diehl that provides an overview of many important data structures and contains links to other sources of information: [
  Dive into GHC](http://www.stephendiehl.com/posts/ghc_01.html)

## Need help?



You can email the [
ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) list, or ask on IRC in `#ghc`.



Happy hacking!


