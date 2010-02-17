


# The (new) GHC Build System



(this page will eventually replace [Attic/Building/BuildSystem](attic/building/build-system) when the new build system is merged in)



This section contains everything you need to know in order to
understand and modify the GHC build system.  The build system is
non-standard in various ways (to be explained shortly), and is
decidedly non-trivial: do not attempt to modify it without having a
grasp of the concepts that follow!



Each of the following subsections describes one of the ``idioms`` that
we use in the build system.  There are a handful of such idioms, and
when you've understood them all you'll be able to understand most of
the code you'll find in the build system.  We'll describe the idioms
first, and then get on to the specifics of how we build GHC.



**Historical note**: this is the third major revision of the GHC build
system.  The first incarnation was based on "jmake", a derivative of
X11's "imake", which is based on using the C preprocessor to add macro
capabilities and `#include` to plain **make**.  The second incarnation
used GNU **make**'s extensions for including makefiles (but lost the
ability to use macros, since at the time GNU **make** didn't have support
for general macros).  In this third revision, we use even more of GNU
**make**'s extensions, and we make a fundamental change to the design, as
described in the next section.


## Idiom: non-recursive make



Build systems for large projects often use the technique commonly
known as "recursive make", where there is a separate `Makefile` in
each directory that is capable of building that part of the system.
The `Makefile`s may share some common infrastructure and configuration
by using GNU **make**'s `include` directive; this is exactly what the
previous GHC build system did.  However, this design has a number of
flaws, as described in Peter Miller's
[
Recursive Make Considered Harmful](http://miller.emu.id.au/pmiller/books/rmch/).  



The GHC build system adopts the non-recursive **make** idiom.  That is, we
never invoke **make** from inside a `Makefile`, and the whole build system
is effectively a single giant `Makefile`.



This gives us the following advantages:


- Specifying dependencies between different parts of the tree is
  easy.  In this way, we can accurately specify many dependencies
  that we could not in the old recursive-make system.  This makes it much more likely that when you say "make"
  after modifying parts of the tree or pulling new patches,
  the build system will bring everything up-to-date in the correct order, and leave you with a working
  system.

- More parallelism: dependencies are more fine-grained, and there
  is no need to build separate parts of the system in sequence, so
  the overall effect is that we have more parallelism in the build.


Doesn't this sacrifice modularity?  No - we can still split the build
system into separate files, using GNU **make**'s `include`.



Specific notes related to this idiom:


- Individual directories usually have a `ghc.mk` file which
  contains the build instructions for that directory.

- Other parts of the build system are in `mk/*.mk` and `rules/*.mk`.

- The top-level `ghc.mk` file includes all the other `*.mk` files in
  the tree.  The top-level `Makefile` invokes **make** on `ghc.mk`
  (this is the only recursive invocation of **make**; see the "phase
  ordering" idiom below).

## Idiom: stub makefiles



It's all very well having a single giant `Makefile` that knows how to
build everything in the right order, but sometimes you want to build
just part of the system.  When working on GHC itself, we might want to
build just the compiler, for example.  In the recursive **make** system we
would do `cd ghc` and then `make`.  In the non-recursive system we can
still achieve this by specifying the target with something like \`make
ghc/stage1/build/ghc\`, but that's not so convenient.



Our second idiom therefore supports the `cd ghc; make` idiom, just as
with recursive make. To achieve this we put tiny stub `Makefile` in each
directory whose job it is to invoke the main `Makefile` specifying the
appropriate target(s) for that directory.  These stub `Makefiles`
follow a simple pattern:


```wiki
dir = libraries/base
TOP = ../..
include $(TOP)/mk/sub-makefile.mk
```


where `mk/sub-makefile.mk` knows how to recursively invoke the giant top-level **make**.


## Idiom: standard targets (all, clean, etc.)



We want an `all` target that builds everything, but we also want a way to build individual components (say, everything in `rts/`).  This is achieved by having a separate "all" target for each directory, named `all_`*directory*.  For example in `rts/ghc.mk` we might have this:


```wiki
all : all_rts
.PHONY all_rts
all_rts : ...dependencies...
```


When the top level **make** includes all these `ghc.mk` files, it will see that target `all` depends on `all_rts, all_ghc, ...etc...`; so `make all` will make all of these.  But the individual targets are still available.  In particular, you can say


- `make all_rts` (anywhere) to build everything in the RTS directory
- `make all` (anywhere) to build everything
- `make`, with no explicit target, makes the default target in the current directory's stub `Makefile`, which in turn makes the target `all_`*dir*, where *dir* is the current directory.


Other standard targets such as `clean`, `install`, and so on use the same technique.  There are pre-canned macros to define your "all" and "clean" targets, take a look in `rules/all-target.mk` and `rules/clean-target.mk`.


## Idiom: stages



What do we use to compile GHC?  GHC itself, of course.  In a complete build we actually build GHC twice: once using the GHC version that is installed, and then again using the GHC we just built.  To be clear about which GHC we are talking about, we number them:


- **Stage 0** is the GHC you have installed.  The "GHC you have installed" is also called "the bootstrap compiler".
- **Stage 1** is the first GHC we build, using stage 0.  Stage 1 is then used to build the packages.
- **Stage 2** is the second GHC we build, using stage 1.  This is the one we normally install when you say `make install`.
- **Stage 3** is optional, but is sometimes built to test stage 2.


Stage 1 does not support interactive execution (GHCi) and Template Haskell.  The reason being that when running byte code we must dynamically link the packages, and only in stage 2 and later can we guarantee that the packages we dynamically link are compatible with those that GHC was built against (because they are the very same packages).


## Idiom: distdir



Often we want to build a component multiple times in different ways.  For example:


- certain libraries (e.g. Cabal) are required by GHC, so we build them once with the
  bootstrapping compiler, and again with stage 1 once that is built.

- GHC itself is built multiple times (stage 1, stage 2, maybe stage 3)

- some tools (e.g. ghc-pkg) are also built once with the bootstrapping compiler,
  and then again using stage 1 later.


In order to support multiple builds in a directory, we place all generated files in a subdirectory, called the "distdir".  The distdir can be anything at all; for example in `compiler/` we name our distdirs after the stage (`stage1`, `stage2` etc.).  When there is only a single build in a directory, by convention we usually call the distdir simply "dist".



There is a related concept called *ways*, which includes profiling and dynamic-linking.  Multiple ways are currently part of the same "build" and use the same distdir, but in the future we might unify these concepts and give each way its own distdir.


## Idiom: interaction with Cabal



Many of the components of the GHC build system are also Cabal
packages, with package metadata defined in a `foo.cabal` file. For the
GHC build system we need to extract that metadata and use it to build
the package. This is done by the program `ghc-cabal` (in `utils/ghc-cabal`
in the GHC source tree). This program reads `foo.cabal` and produces
`package-data.mk` containing the package metadata in the form of
makefile bindings that we can use directly.



We adhere to the following rule: **`ghc-cabal` generates only
makefile variable bindings**, such as


```wiki
  HS_SRCS = Foo.hs Bar.hs
```


`ghc-cabal` never generates makefile rules, macro, macro invocations etc. 
All the makefile code is therefore contained in fixed, editable 
`.mk` files.


## Idiom: variable names



Now that our build system is one giant `Makefile`, all our variables
share the same namespace.  Where previously we might have had a
variable that contained a list of the Haskell source files called
`HS_SRCS`, now we have one of these for each directory (and indeed each build, or distdir) in the source tree,
so we have to give them all different names.



The idiom that we use for distinguishing variable names is to prepend
the directory name and the distdir to the variable.  So for example the list of
Haskell sources in the directory `utils/hsc2hs` would be in the
variable `utils/hsc2hs_dist_HS_SRCS` (**make** doesn't mind slashes in variable
names).  The pattern is: *directory*\_*distdir*\_*variable*.


## Idiom: macros



The build system makes extensive use of Gnu **make** **macros**.  A macro is defined in
GNU **make** using `define`, e.g.


```wiki
define build-package
# args: $1 = directory, $2 = distdir
... makefile code to build a package ...
endef
```


(for example, see `rules/build-package`), and is invoked like this:


```wiki
$(eval $(call build-package,libraries/base,dist))
```


(this invocation would be in `libraries/base/ghc.mk`).



Note that `eval` works like this: its argument is expended as normal,
and then the result is interpreted by **make** as makefile code.  This
means the body of the `define` gets expanded *twice*.  Typically
this means we need to use `$$` instead of `$` everywhere in the body of
`define`.



Now, the `build-package` macro may need to define **local variables**.
There is no support for local variables in macros, but we can define
variables which are guaranteed to not clash with other variables by
preceding their names with a string that is unique to this macro call.
A convenient unique string to use is *directory*\_*distdir*\_; this is unique as long as we only call each macro with a given directory/build pair once.  Most macros in
the GHC build system take the directory and build as the first two
arguments for exactly this reason.  For example, here's an excerpt
from the `build-prog` macro:


```wiki
define build-prog
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$1_$2_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
...
```


So if `build-prog` is called with `utils/hsc2hs` and `dist` for the
first two arguments, after expansion **make** would see this:


```wiki
utils/hsc2hs_dist_INPLACE = $(INPLACE_BIN)/$(utils/hsc2hs_dist_PROG)
```


The idiom of `$$($1_$2_VAR)` is very common throughout the build
system - get used to reading it!  Note that the only time we use a
single `$` in the body of `define` is to refer to the parameters `$1`,
`$2`, and so on.


## Idiom: phase ordering



NB. you need to understand this section if either (a) you are modifying parts of the build system that include automatically-generated `Makefile` code, or (b) you need to understand why we have a top-level `Makefile` that recursively invokes **make**.



The main hitch with non-recursive **make** arises when parts of the build
system are automatically-generated.  The automatically-generated parts
of our build system fall into two main categories:


- Dependencies: we use `ghc -M` to generate make-dependencies for 
  Haskell source files, and similarly `gcc -M` to do the same for
  C files.  The dependencies are normally generated into a file
  `.depend`, which is included as normal.

- Makefile binding generated from `.cabal` package descriptions.  See
  "Idiom: interaction with Cabal".


Now, we also want to be able to use `make` to build these files, since
they have complex dependencies themselves.  For example, in order to build
`package-data.mk` we need to first build `ghc-cabal` etc.; similarly,
a `.depend` file needs to be re-generated if any of the source files have changed.



GNU **make** has a clever strategy for handling this kind of scenario.  It
first reads all the included Makefiles, and then tries to build each
one if it is out-of-date, using the rules in the Makefiles themselves.
When it has brought all the included Makefiles up-to-date, it restarts itself
to read the newly-generated Makefiles.



This works fine, unless there are dependencies *between* the
Makefiles.  For example in the GHC build, the `.depend` file for a
package cannot be generated until `package-data.mk` has been generated
and **make** has been restarted to read in its contents, because it is the
`package-data.mk` file that tells us which modules are in the package.
But **make** always makes **all** the included `Makefiles` before restarting - it
doesn't know how to restart itself earlier when there is a dependency
between included `Makefiles`.



Consider the following Makefile:


```wiki
all :

include inc1.mk

inc1.mk : Makefile
	echo "X = C" >$@

include inc2.mk

inc2.mk : inc1.mk
	echo "Y = $(X)" >$@
```


Now try it:


```wiki
$ make -f fail.mk
fail.mk:3: inc1.mk: No such file or directory
fail.mk:8: inc2.mk: No such file or directory
echo "X = C" >inc1.mk
echo "Y = " >inc2.mk
make: Nothing to be done for `all'.
```


**make** built both `inc1.mk` and `inc2.mk` without restarting itself
between the two (even though we added a dependency on `inc1.mk` from
`inc2.mk`).



The solution we adopt in the GHC build system is as follows.  We have
two Makefiles, the first a wrapper around the second.


```wiki
# top-level Makefile
% :
        $(MAKE) -f inc.mk PHASE=0 just-makefiles
        $(MAKE) -f inc.mk $<
```

```wiki
# inc.mk

include inc1.mk

ifeq "$(PHASE)" "0"

inc1.mk : inc.mk
	echo "X = C" >$@

else

include inc2.mk

inc2.mk : inc1.mk
	echo "Y = $(X)" >$@

endif

just-makefiles:
        @: # do nothing

clean :
	rm -f inc1.mk inc2.mk
```


Each time **make** is invoked, we recursively invoke **make** in several
*phases*:


- **Phase 0**: invoke `inc.mk` with `PHASE=0`.  This brings `inc1.mk` 
  up-to-date (and *only* `inc1.mk`).  

- **Final phase**: invoke `inc.mk` again (with `PHASE` unset).  Now we can be sure 
  that `inc1.mk` is up-to-date and proceed to generate `inc2.mk`.  
  If this changes `inc2.mk`, then **make** automatically re-invokes itself,
  repeating the final phase.


We could instead have abandoned **make**'s automatic re-invocation mechanism altogether,
and used three explicit phases (0, 1, and final), but in practice it's very convenient to use the automatic
re-invocation when there are no problematic dependencies.



Note that the `inc1.mk` rule is *only* enabled in phase 0, so that if we accidentally call `inc.mk` without first performing phase 0, we will either get a failure (if `inc1.mk` doesn't exist), or otherwise **make** will not update `inc1.mk` if it is out-of-date.



In the case of the GHC build system we need 4 such phases, see the
comments in the top-level `ghc.mk` for details.



This approach is not at all pretty, and
re-invoking **make** every time is slow, but we don't know of a better
workaround for this problem.


## Idiom: no double-colon rules



**Make** has a special type of rule of the form `target :: prerequisites`,
with the behaviour that all double-colon rules for a given target are
executed if the target needs to be rebuilt.  This style was popular
for things like "all" and "clean" targets in the past, but it's not
really necessary - see the "all" idiom above - and this means there's one fewer makeism you need to know about.


## Idiom: the vanilla way



Libraries can be built in several different "ways", for example
"profiling" and "dynamic" are two ways.  Each way has a short tag
associated with it; "p" and "dyn" are the tags for profiling and
dynamic respectively.  In previous GHC build systems, the "normal" way
didn't have a name, it was just always built.  Now we explicitly call
it the "vanilla" way and use the tag "v" to refer to it.  



This means that the `GhcLibWays` variable, which lists the ways in
which the libraries are built, must include "v" if you want the
vanilla way to be built (this is included in the default setup, of
course).


## Whitespace



make has a rather ad-hoc approach to whitespace. Most of the time it ignores it, e.g.


```wiki
FOO = bar
```


sets `FOO` to `"bar"`, not `" bar"`. However, sometimes whitespace is significant,
and calling macros is one example. For example, we used to have a call


```wiki
$(call all-target, $$($1_$2_INPLACE))
```


and this passed `" $$($1_$2_INPLACE)"` as the argument to `all-target`. This in turn generated


```wiki
.PHONY: all_ inplace/bin/ghc-asm
```


which caused an infinite loop, as make continually thought that `ghc-asm` was out-of-date, rebuilt it,
reinvoked make, and then thought it was out of date again.



The moral of the story is, avoid white space unless you're sure it'll be OK!


