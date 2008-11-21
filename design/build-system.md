# Re-structuring the GHC build system



This page describes our current plan for making GHC's build system more
malleable. The new build system will be developed in a branch here:

[
http://darcs.haskell.org/ghc-new-build-system/](http://darcs.haskell.org/ghc-new-build-system/) 

but if you want to grab it then you would probably prefer to start from a
snapshot tarball:

[
http://darcs.haskell.org/ghc-new-build-system-2008-11-20-ghc-corelibs-testsuite.tar.bz2](http://darcs.haskell.org/ghc-new-build-system-2008-11-20-ghc-corelibs-testsuite.tar.bz2)


## Design goals


- The build system should not only be easy to use for *users* (does
  the right thing with the minimum of intervention) but also for
  *developers*.  This means:

  - when something changes, it is easy to bring the build up to date
    quickly to test the change, with the option of just updating the
    part of the system being changed (library module, GHC module,
    tool, etc.)  In all cases a simple 'make' should bring the part
    of the system in the current directory up to date, or complain
    if that can't be done for some reason.
  - easy to build individual modules, and add extra flags
    (e.g. -v, -ddump-simpl)
  - easy to change build parameters from the command line.  e.g. we 
    often want to build something using a different compiler, `make HC=ghc`,
    to see how a different compiler behaves.
  - easy to modify the build system.  If you modify some part of the
    build system, it should take immediate effect: no having to
    remove bits and rebuild them manually to get changes to take
    effect.
  - as little "state" in the build as possible: e.g. avoid stamp
    files unless they are just join points for dependencies.  Stamp
    files should be invisible to the developer if they are needed at
    all.
  - The build system should be tractable: if it doesn't do what you
    expect, there should be a clear path to understanding why and
    how to fix it.  Extra make rules are sometimes good for this:
    e.g. we currently have 'make show VALUE=VAR' which is a godsend.

- The build should be as parallel as possible

- The build system should support bootstrapping from HC files,
  something that hasn't worked since 6.6.1.

- The build should not emit any warnings unless something is actually
  wrong (they cause concerned users).

- The build system should be well documented on the wiki.  We should
  pay special attention to the documentation for building on Windows:
  it's currently far too verbose, complex, and out-of-date.  Ideally
  there should be a short section on how to prepare a Windows system
  for building GHC (in the section on pre-requisites), and perhaps
  short Windows-specific notes throughout the rest of the docs.
  Putting it all in one place risks duplication and things getting
  outdated.

- The build system should clearly report what it's doing (and
  sometimes why), without being too verbose.  It should emit actual
  command lines as much as possible, so that they can be inspected
  and cut & pasted.

- We should express as many dependencies as possible in the build
  system, but occasionally removing a few edges is prudent.  For
  example, if we have rebuilt some libraries, we might not want that
  to immediately invalidate the whole stage 2 GHC build, since the
  binary still works (as long as we're using static libraries...).
  However, if we need to rebuild *any* module in stage 2 and a
  library has changed, it's probably a good idea to rebuild them all
  at that point, because otherwise the resulting binary will likely
  be broken.  GHC will probably say "compilation IS NOT required" for
  most modules anyway.

- The build system should report clearly at configure-time the status of each optional feature (e.g. editline support).

## Basic plan


- Use our own build system for the work of actually building packages
  from source to .a/.so, including preprocessing (hsc2hs et. al.). 

- Use Cabal to preprocess the .cabal file and generate metadata in the
  form of Makefile bindings for our build system to use.  We plan to
  generate **only** Makefile bindings this way; we will not generate Makefile
  **rules** or shell commands.  All Makefile code and shell commands will be
  in plain honest-to-goodness Makefiles.

- Use Cabal to generate the InstalledPackageInfo.

- We do registration and installation using Makefile rules.

- Use Cabal for Haddocking, and anything else we need to do. 


The advantages of this are:


- The critical parts of the build system are under our control, and are
  easily modifiable. 

- Modifying the build system does not require modifying Cabal. We rely
  on a stable, slowly-varying version of Cabal, not on the leading edge.
  That take pressure off the Cabal developers, and means that GHC can use
  a version of Cabal that has survived quite a bit of testing. 

- Development is easier, because 'make' will preprocess files too. Right
  now if you modify a .y or .hsc file, you need to tell Cabal to
  preprocess again before saying 'make' (this is a regression from
  pre-Cabal). 

- We can make improvements that would be hard in Cabal, such as making
  libraries depend on each other. 

- It ought to be easier to reinstate HC bootstrapping, since we rely
  less on Cabal to get us to a .a file. 

- Compared to the pre-Cabal build system, we're not duplicating the
  package metadata or the code that processes it, only the build rules. 

## Avoiding recursive makefiles



Currently, with recursive make, this means we jump around between
Makefiles a lot, which isn't good for parallelism in the build.  For example, the current build order looks like this:


- With bootstrapping compiler:

  - Build libraries/{filepath,Cabal}
  - Build utils/ghc-cabal
- With bootstrapping compiler and ghc-cabal:

  - Build utils/hsc2hs
  - Build libraries/hpc
  - Build compiler (stage 1)
- With stage 1:

  - Build libraries/\*
  - Build utils/\* (except haddock)
  - Build compiler (stage 2)
- With stage 2:

  - Build utils/haddock
  - Build compiler (stage 3)
- With haddock:

  - libraries/\*
  - compiler


Instead, following [
Recursive make considered harmful](http://miller.emu.id.au/pmiller/books/rmch/?ref=DDiyet.Com), we propose to move all the logic and dependencies into the root
Makefile (or files that get included into it) so that make sees all of
it together.  Advantages:


- We get to specify dependencies between different parts of the tree much
  more easily and precisely.  BIG WIN.  Have you noticed how often you
  need to `make distclean` in a GHC tree to make sure everything is up to
  date? (well I rarely do this, because I have the dependencies in my
  head and I can rebuild manually, but I imagine this isn't the case for
  most people!)

- We get more parallelism, more easily.  I'd argue this is a big win too,
  right now a validate only uses about 1.3 out of 2 cores.


We don't lose modularity: different parts of the build system are still in
different files, it's just that make sees them all at once.  Right now
every make invocation already reads thousands of lines of boilerplate
Makefile code, and we'd be invoking make only once rather than many times.



So we will probably have to worry about efficiency.  For example, it takes
tens of seconds on Windows for make to discover that compiler/ is up to
date.  We don't want that happening every time you rebuild some small part
of the tree, so we plan to cut a few dependencies on purpose.  But I'm
hoping it'll be necessary to do this in a few well-defined places only, and
only when building in subdirectories.  For example, If you say 'make' in
libraries/base, then we won't try to rebuild the stage1 compiler, we'll
just fail if it does't exist.



We still want "make" to work in subdirectories, so for example the
Makefile (actually GNUmakefile, to avoid colliding with Makefile in
libraries like Cabal) in libraries/base might look like


```wiki
.NOTPARALLEL

.PHONY: default
default: dist/build/libbase.a
    @:

# Note that this rule also generates ghc.mk if it doesn't exist
%:
    $(MAKE) -C ../.. libraries/base/$@
```


(ghc.mk is discussed later). In actual fact, GNUmakefile will want to
be more complicated, to handle "make way=v", "make way=p", "make doc",
etc. Where possible, the make code will be "include"d in, rather than
generated, so as to make it easier to deal with.



We need the .NOTPARALLEL or if you say "make foo bar" then the two
recursive make calls might both make "quux" (a dependency of foo and
bar) at the same time. The main Makefile will be able to do work in
parallel when building each of foo and bar, though. The common case,
where you only specify 0 or 1 targets, doesn't lose any parallelism.


## Detailed plan


- Rename cabal-bin.hs to ghc-cabal.hs, and move it into utils/ghc-cabal/

- The version of Cabal used by ghc-cabal.hs does not need to be an
  up-to-the-minute bleeding-edge version. It should be stable and vary
  slowly. We suck a new version of Cabal into the GHC build system
  manually, rather than mirroring the Cabal HEAD. 

- Rather than installing things in-place all over the build tree, we
  will have a single inplace directory at the root of the tree. The
  structure inside this directory will match that of the normal install,
  which will simplify various things. There are two slight wrinkles:

  - The tree will not be complete; for example, the libraries will be
    registered in-place in their dist directories
  - Rather than inplace/bin/ghc, we will have inplace/bin/ghc-stage\[123\]
    Tools like genprimopcode, genapply etc. will probably also go into
    inplace/bin, in order to make the makefiles more consistent.

- In e.g. utils/ghc-pkg, the default target will be

  ```wiki
  default:
      $(MAKE) -C ../.. inplace/bin/ghc-pkg
  ```

  and inplace/bin/ghc-pkg will in turn depend on something like
  utils/ghc-pkg/dist-inplace/build/ghc-pkg. It would be possible to
  put the binary in inplace/bin directly, but at the cost of diverging
  from Cabal's filesystem layout. Also, it would be a little odd to
  install from inplace/. On \*nix machines we can make the inplace files
  symlinks.

- It should be possible to "make distclean" without configuring first.
  Distcleaning should be much simpler now: Just remove

  - all the generated makefiles
  - all the dist directories
  - the inplace directory.
  - files generated by configure
  - generated makefiles
  - and doubtless a few other bits and pieces

- There are a number of tools, for example ghc and ghc-pkg, which we
  build with both the bootstrapping compiler (for use during the build)
  and the in-tree compiler (to be installed). When you run "make" in the
  tool's directory, only the earliest version will be built by default.

- The rules (in ghc.mk) for actually building the foo library (which
  depends on the bar library) will look something like:

```wiki
all: libraries/foo/dist/build/foo.a
all: libraries/foo/dist/doc/foo.haddock

LIBRARY_foo_HS_FILES = libraries/foo/dist/build/Bar.hs \
                       libraries/foo/dist/build/Quux.lhs
LIBRARY_foo_v_O_FILES = libraries/foo/dist/build/Bar.o \
                        libraries/foo/dist/build/Quux.o
LIBRARY_foo_v_HI_FILES = libraries/foo/dist/build/Bar.hi \
                         libraries/foo/dist/build/Quux.hi
LIBRARY_foo_v_A_FILE = libraries/foo/dist/build/foo.a

libraries/foo/dist/build/Quux.o libraries/foo/dist/build/Quux.hi: \
    libraries/foo/dist/build/Bar.hi

libraries/foo/dist/build/%.hs: libraries/foo/%.hs
    cp $< $@
# and a duplicate rule for .lhs. We can use ln instead of cp on
# *nix.

libraries/foo/dist/build/%.hs: libraries/foo/%.y inplace/bin/happy
    inplace/bin/happy ...
# and alex, hsc2hs, etc

libraries/foo/dist/build/%.o: libraries/foo/dist/build/%.hs \
                              $(LIBRARY_bar_v_HI_FILES) \
                              inplace/bin/ghc-stage1
    inplace/bin/ghc-stage1 -c ^< -o ^@ # -hidir etc
# and a duplicate rule for .lhs. This rule can probably be
# generalised to handle building of all libraries .o files,
# and perhaps even all .o files.

.DELETE_ON_ERROR: $(LIBRARY_foo_v_A_FILE)

$(LIBRARY_foo_v_A_FILE): inplace/bin/ghc-pkg \
                         libraries/foo/dist/inplace-config \
                         $(LIBRARY_foo_v_O_FILES) \
                         $(LIBRARY_bar_v_A_FILE)
    ghc $(LIBRARY_foo_v_O_FILES) -package bar -o $@
    inplace/bin/ghc-cabal --in-dir libraries/foo register

libraries/foo/dist/doc/foo.haddock: inplace/bin/haddock
    inplace/bin/ghc-cabal --in-dir libraries/foo haddock
```

>
> >
> >
> > and the rules for building tools and GHC will look similar.
> > There will also need to be rules for profiling libraries etc.
> > For the tools, there will be copies of the rules for the "in-place"
> > and "install" dist directories (or perhaps we have ghc-inplace.mk
> > and ghc-install.mk).
> >
> >
>

- The above makefile gets generated by something like

```wiki
include libraries/foo/ghc.mk

libraries/%/ghc.mk: inplace/bin/ghc-cabal
    inplace/bin/ghc-cabal --in-dir $* generate $@
```

>
> >
> >
> > in the top-level Makefile.
> >
> >
>

- There are two ways that we could handle the generation. We could
  generate the complete rules, as above, but that makes them harder to
  edit (as you'd actually have to edit some Haskell source that generates
  them). Alternatively, we could generate them with a little `$(eval ...)`
  in the makefile. the downside of that is that it requires make \>= 3.80,
  which msys doesn't come with. However, it looks like it is easy to
  upgrade an msys installation, so that's the route that we plan to take.

- We don't want to require that the libraries ship part of the GHC build
  system in their tarballs, so instead we will generate the GNUmakefile's
  during ./configure.

- The "Setup makefile" command can be removed from Cabal.

- ghc-pkg will be extended to be able to register libraries compiled
  different ways separately. As well as making the dependencies workable
  in the makefiles, this will also allow cabal-install to work better,
  at the risk of allowing library variants to desync more easily.

- Does ghc-pkg currently cope with being called twice simultaneously?
  We should move to using a directory of package.conf files too.
