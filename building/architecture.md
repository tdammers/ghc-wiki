


# The GHC Build System Architecture



This section contains information you need to know in order to
understand and modify the GHC build system.  The build system is
non-standard in various ways (to be explained shortly), and is
decidedly non-trivial: do not attempt to modify it without having a
grasp of the concepts that follow!



It's difficult to document a system that is full of details and subject to constant change.  The approach we've adopted here is to split the documentation in two:


- The high-level architectural design, the stuff that is less likely to change, is documented here.  Occasionally we'll include direct links to source files to illustrate the details.

- The low-level technical details, such as the order of arguments to a macro, and the names of
  specific variables, are documented as comments in the build-system code.  Hopefully this way the
  documentation is more likely to stay up to date.


**Historical note**: this is the third major revision of the GHC build
system.  The first incarnation was based on `jmake`, a derivative of
X11's own `imake`, which is based on using the C preprocessor to add macro
capabilities and `#include` to plain **make**.  The second incarnation
used GNU **make**'s extensions for including makefiles (but lost the
ability to use macros, since at the time GNU **make** didn't have support
for general macros).  In this third revision, we use even more of GNU
**make**'s extensions, and we make a fundamental change to the design, as
described in the next section. Another (fourth) revision based on `Shake` is
currently being attempted; see [Building/Shake](building/shake) for more details.


## Overall structure and important files



The following are a few of the most important files in the build system.  For a more complete overview of the source-tree layout, see [Commentary/SourceTree](commentary/source-tree).


<table><tr><th>[ghc.mk](/trac/ghc/browser/ghc/ghc.mk)[](/trac/ghc/export/HEAD/ghc/ghc.mk)</th>
<td>
This is where you should start reading: `ghc.mk` is the main file in
the build system which ties together all the other build-system
files.  It uses **make**'s `include` directive to include all the
files in `mk/*.mk`, `rules/*.mk`, and all the other `ghc.mk` files
elsewhere in the tree.
</td></tr></table>


<table><tr><th>[Makefile](/trac/ghc/browser/ghc/Makefile)[](/trac/ghc/export/HEAD/ghc/Makefile)</th>
<td>
The top-level `Makefile`, recursively invokes `make` on `ghc.mk`
according to the [phase ordering idiom](building/architecture/idiom/phase-ordering).
</td></tr></table>


<table><tr><th>`rules/*.mk`</th>
<td>
Each `.mk` file in the `rules` directory corresponds to a single
macro that can be called using **make**'s `$(call ...)`
expression.  For example, the `build-package` macro is in
`rules/build-package.mk`.
</td></tr></table>


<table><tr><th>[mk/config.mk.in](/trac/ghc/browser/mk/config.mk.in)[](/trac/ghc/export/HEAD/ghc/mk/config.mk.in)</th>
<td>
The configuration information for the build system, processed by
`configure` to produce `mk/config.mk`.  Settings can be overriden by
creating a local file `mk/build.mk` (see
[Build configuration](building/using#)).
</td></tr></table>


<table><tr><th>[compiler/ghc.mk](/trac/ghc/browser/compiler/ghc.mk)[](/trac/ghc/export/HEAD/ghc/compiler/ghc.mk), [rts/ghc.mk](/trac/ghc/browser/rts/ghc.mk)[](/trac/ghc/export/HEAD/ghc/rts/ghc.mk), etc.</th>
<td>
Most subdirectories of the source tree have a `ghc.mk` file which
contains the instructions for building the components in that
directory.  Note: these `ghc.mk` files cannot be invoked
individually, they should only be included by the top-level
`ghc.mk`.
</td></tr></table>


## Idioms



Each of the following subsections describes one of the ``idioms`` that
we use in the build system.  There are a handful of such idioms, and
when you've understood them all you'll be able to understand most of
the code you'll find in the build system.  We'll describe the idioms
first, and then get on to the specifics of how we build GHC.


- [Non-recursive make](building/architecture/idiom/non-recursive-make)
- [Stub makefiles](building/architecture/idiom/stub-makefiles)
- [Standard targets (all, clean etc.)](building/architecture/idiom/standard-targets)
- [Stages](building/architecture/idiom/stages)
- [Distdir](building/architecture/idiom/distdir)
- [Interaction with Cabal](building/architecture/idiom/cabal)
- [Variable names](building/architecture/idiom/variable-names)
- [Macros](building/architecture/idiom/macros)
- [Phase ordering](building/architecture/idiom/phase-ordering)
- [No double-colon rules](building/architecture/idiom/double-colon)
- [The vanilla way](building/architecture/idiom/vanilla-way)
- [Whitespace](building/architecture/idiom/whitespace)
- [Platform names (build, host, target)](building/architecture/idiom/platform-names)
- [Directories](building/architecture/idiom/directories)
