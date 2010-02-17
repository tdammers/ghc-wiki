


# The `Makefile` architecture



`make` is great if everything
works--you type `make install` and lo! the
right things get compiled and installed in the right places.  Our
goal is to make this happen often, but somehow it often doesn't;
instead some weird error message eventually emerges from the
bowels of a directory you didn't know existed.



The purpose of this section is to give you a road-map to
help you figure out what is going right and what is going
wrong.


## Debugging



Debugging `Makefile`s is something of a
black art, but here's a couple of tricks that we find
particularly useful.  The following command allows you to see
the contents of any make variable in the context of the current
`Makefile`:



`$ make show VALUE=HS_SRCS`



where you can replace `HS_SRCS` with the
name of any variable you wish to see the value of.



GNU make has a `-d` option which generates
a dump of the decision procedure used to arrive at a conclusion
about which files should be recompiled.  Sometimes useful for
tracking down problems with superfluous or missing
recompilations.



The output of ``make -pq [TARGET]'` command can be helpful
as well. This command prints the database of rules and variables,
running nothing.


## A small example



To get started, let us look at the
`Makefile` for an imaginary small program,
`small`.  Each program or library in the GHC
source tree typically has its own directory, in this case we'll
use `$(TOP)/small`.
Inside the `small/` directory there will be a
`Makefile`, looking something like
this:


```wiki
TOP = ..

ENABLE_SHELL_WRAPPERS = YES
EXTRA_CLEAN = myFile
EXTRA_DISTCLEAN = myFile

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal.mk
```


this `Makefile` has three
sections:


- We start by defining `$(TOP)` to point to the top of the tree.

- Next there are some variables that we can set to control how `small` is built.
  The most common one is `ENABLE_SHELL_WRAPPERS`, which we can set to `YES`
  if we want to use Cabal's binary wrappers feature. This means that, on
  unix-like OSes, we install `small.wrapper` as a shell script wrapper
  around the real `small` binary.

  Less common are `EXTRA_CLEAN` and `EXTRA_DISTCLEAN`, which list extra files
  to be removed during `make clean` and `make distclean`.

- Finally we include (One of the most important
  features of GNU `make` that we use is the ability for a `Makefile` to
  include another named file, very like `cpp`'s `#include`
  directive.)
  the common build system components.
  First is `boilerplate.mk` which, as its name suggests,
  consists of a large quantity of standard
  `Makefile` code.  We discuss this
  boilerplate in more detail in [the mk/boilerplate.mk file](#themk/boilerplate.mkfile).

  Note that you **must** define the `make` variable
  `TOP`
  to be the top-level directory of the source tree, containing
  the `mk`
  directory in which the `boilerplate.mk`
  file is.  It is *not* OK to simply say

  ```wiki
  include ../mk/boilerplate.mk  # NO NO NO
  ```

  Why?  Because the `boilerplate.mk`
  file needs to know where it is, so that it can, in turn,
  `include` other files.  (Unfortunately,
  when an `include`d file does an
  `include`, the filename is treated relative
  to the directory in which `make` is being
  run, not the directory in which the
  `include`d sits.)  In general,
  *every file `foo.mk` assumes
  that
  `$(TOP)/mk/foo.mk`
  refers to itself.* It is up to the
  `Makefile` doing the
  `include` to ensure this is the case.

  Second is `cabal.mk`, which is the rules for how to build a
  Cabal package in the build system.
  It contains the rules that tell `make` how
  to make the [standard targets](building/using#).
  The reason that this standard code isn't part of
  `boilerplate.mk` is that some directories are built differently,
  either because they have not yet been migrated to build with Cabal,
  or because they have needs that don't fit well into a generic
  system.

## Boilerplate architecture



Every `Makefile` includes a
`boilerplate.mk`
file at the top.  In this section we discuss what is in this
file.  In general:



      


- `boilerplate.mk` consists of:

  - *Definitions of millions of
    `make` variables* that
    collectively specify the build configuration.  Examples:
    `HC_OPTS`,
    the options to feed to the Haskell compiler;
    `NoFibSubDirs`,
    the sub-directories to enable within the
    `nofib` project;
    `GhcWithHc`,
    the name of the Haskell compiler to use when compiling
    GHC in the `ghc` project.
  - *Standard pattern rules* that
    tell `make` how to construct one file
    from another. These rules are becoming more and more
    redundant as we move to building with Cabal instead.

>
> >
> >
> > `boilerplate.mk` needs to be
> > `include`d at the *top*
> > of each `Makefile`, so that the user can
> > replace the boilerplate variable definitions by
> > simply giving a new definition or pattern rule in the
> > `Makefile`.  `make`
> > simply takes the last definition as the definitive one.
> >
> > Instead of *replacing* boilerplate
> > definitions, it is also quite common to
> > *augment* them. For example, a
> > `Makefile` might say:
> >
> >
> > ```wiki
> > SRC_HC_OPTS += -O
> > ```
> >
> >
> > thereby adding "`-O`" to
> > the end of
> > `SRC_HC_OPTS`.
> >
> >
>

- `target.mk` is part of the old build system, but is still used
  to compile some parts of GHC. It contains
  `make` rules for the standard targets
  described in [standard targets](building/using#).  These
  rules are selectively included, depending on the setting of
  certain `make` variables.  These variables
  are usually set in the middle section of the
  `Makefile` between the two
  `include`s.
  `target.mk` must be included at the
  end (rather than being part of
  `boilerplate.mk`) for several tiresome
  reasons:

  - `make` commits target and
    dependency lists earlier than it should.  For example,
    `target.mk` has a rule that looks
    like this:

    ```wiki
    $(HS_PROG) : $(OBJS)
    	 $(HC) $(LD_OPTS) $< -o $@
    ```

    If this rule was in
    `boilerplate.mk` then
    `$(HS_PROG)`
    and
    `$(OBJS)`
    would not have their final values at the moment
    `make` encountered the rule.  Alas,
    `make` takes a snapshot of their
    current values, and wires that snapshot into the rule.
    (In contrast, the commands executed when the rule
    "fires" are only substituted at the moment
    of firing.)  So, the rule must follow the definitions
    given in the `Makefile` itself.
  - Unlike pattern rules, ordinary rules cannot be
    overriden or replaced by subsequent rules for the same
    target (at least, not without an error message).
    Including ordinary rules in
    `boilerplate.mk` would prevent the
    user from writing rules for specific targets in specific
    cases.
  - There are a couple of other reasons I've
    forgotten, but it doesn't matter too much.

## The `mk/boilerplate.mk` file



If you look at
`$(GHC_TOP)/mk/boilerplate.mk`
you will find that it consists of the following sections, each
held in a separate file:



      


### `config.mk`



is the build configuration file we discussed at
length in [getting the build you want](building/using#).


### `paths.mk`



is part of the old build system, i.e. it is not used by packages that are built with Cabal.
            
`paths.mk` defines `make` variables for
pathnames and file lists.  This file contains code for
automatically compiling lists of source files and deriving
lists of object files from those.  The results can be
overriden in the `Makefile`, but in
most cases the automatic setup should do the right
thing.



The following variables may be set in the
`Makefile` to affect how the automatic
source file search is done:



            


- `ALL_DIRS`
  Set to a list of directories to search in
  addition to the current directory for source
  files.

- `EXCLUDED_SRCS`
  Set to a list of source files (relative to the
  current directory) to omit from the automatic
  search.  The source searching machinery is clever
  enough to know that if you exclude a source file
  from which other sources are derived, then the
  derived sources should also be excluded.  For
  example, if you set `EXCLUDED_SRCS`
  to include `Foo.y`, then
  `Foo.hs` will also be
  excluded.

- `EXTRA_SRCS`
  Set to a list of extra source files (perhaps
  in directories not listed in
  `ALL_DIRS`) that should be
  considered.


            



The results of the automatic source file search are
placed in the following make variables:



            


- `SRCS`
  All source files found, sorted and without
  duplicates, including those which might not exist
  yet but will be derived from other existing sources.
  `SRCS` *can* be
  overriden if necessary, in which case the variables
  below will follow suit.

- `HS_SRCS`
  all Haskell source files in the current
  directory, including those derived from other source
  files (eg. Happy sources also give rise to Haskell
  sources).

- `HS_OBJS`
  Object files derived from
  `HS_SRCS`.

- `HS_IFACES`
  Interface files (`.hi` files)
  derived from `HS_SRCS`.

- `C_SRCS`
  All C source files found.

- `C_OBJS`
  Object files derived from
  `C_SRCS`.

- `SCRIPT_SRCS`
  All script source files found
  (`.lprl` files).

- `SCRIPT_OBJS`
  "object" files derived from
  `SCRIPT_SRCS`
  (`.prl` files).

- `HSC_SRCS`
  All `hsc2hs` source files
  (`.hsc` files).

- `HAPPY_SRCS`
  All `happy` source files
  (`.y` or `.hy` files).

- `OBJS`
  the concatenation of
  `$(HS_OBJS)`,
  `$(C_OBJS)`, and
  `$(SCRIPT_OBJS)`.


            



Any or all of these definitions can easily be
overriden by giving new definitions in your
`Makefile`.



What, exactly, does `paths.mk`
consider a "source file" to be?  It's based
on the file's suffix (e.g. `.hs`,
`.lhs`, `.c`,
`.hy`, etc), but this is the kind of
detail that changes, so rather than enumerate the source
suffices here the best thing to do is to look in
`paths.mk`.


### `opts.mk`



defines `make` variables for option
strings to pass to each program. For example, it defines
`HC_OPTS`,
the option strings to pass to the Haskell compiler.  See
[Pattern rules and options](#Patternrulesandoptions).


### `suffix.mk`



is part of the old build system, i.e. it is not used by packages that are built with Cabal.



`suffix.mk` defines standard pattern rules--see [Pattern rules and options](#Patternrulesandoptions).



      



Any of the variables and pattern rules defined by the
boilerplate file can easily be overridden in any particular
`Makefile`, because the boilerplate
`include` comes first.  Definitions after this
`include` directive simply override the default
ones in `boilerplate.mk`.


## Platform settings



There are three platforms of interest when building GHC:
      
      


- The *build* platform.
  The platform on which we are doing this build.

- The *host* platform
  The platform on which these binaries will run.

- The *target* platform
  The platform for which this compiler will generate code.


      
These platforms are set when running the
`configure` script, using the
`--build`, `--host`, and
`--target` options.  The `mk/config.mk`
file defines several symbols related to the platform settings (see
`mk/config.mk` for details).



We don't currently support build &amp; host being different, because
the build process creates binaries that are both run during the build,
and also installed.



If host and target are different, then we are building a
cross-compiler.  For GHC, this means a compiler
which will generate intermediate .hc files to port to the target
architecture for bootstrapping.  The libraries and stage 2 compiler
will be built as HC files for the target system (see [Porting GHC](building/porting) for details).



More details on when to use BUILD, HOST or TARGET can be found in
the comments in `config.mk`.


## Pattern rules and options



This section relates mainly to the old build system.



The file
`suffix.mk`
defines standard *pattern rules* that say how
to build one kind of file from another, for example, how to
build a `.o` file from a
`.c` file.  (GNU `make`'s
*pattern rules* are more powerful and easier
to use than Unix `make`'s *suffix
rules*.)



Almost all the rules look something like this:


```wiki
%.o : %.c
      $(RM) $@
      $(CC) $(CC_OPTS) -c $< -o $@
```


Here's how to understand the rule.  It says that
*something*`.o` (say
`Foo.o`) can be built from
*something*`.c`
(`Foo.c`), by invoking the C compiler (path
name held in `$(CC)`), passing to it
the options `$(CC_OPTS)` and
the rule's dependent file of the rule
`$<` (`Foo.c` in
this case), and putting the result in the rule's target
`$@` (`Foo.o` in this
case).



Every program is held in a `make`
variable defined in `mk/config.mk`--look
in `mk/config.mk` for the complete list.  One
important one is the Haskell compiler, which is called
`$(HC)`.



Every program's options are are held in a
`make` variables called
`<prog>_OPTS`.  the
`<prog>_OPTS` variables are
defined in `mk/opts.mk`.  Almost all of them
are defined like this:


```wiki
CC_OPTS = \
  $(SRC_CC_OPTS) $(WAY$(_way)_CC_OPTS) $($*_CC_OPTS) $(EXTRA_CC_OPTS)
```


The four variables from which
`CC_OPTS` is built have the following
meaning:



      


- `SRC_CC_OPTS`:
  options passed to all C compilations.

- `WAY_<way>_CC_OPTS`:
  options passed to C compilations for way
  `<way>`. For example,
  `WAY_mp_CC_OPTS`
  gives options to pass to the C compiler when compiling way
  `mp`.  The variable
  `WAY_CC_OPTS` holds
  options to pass to the C compiler when compiling the
  standard way.  ([Way management](#Waymanagement) dicusses
  multi-way compilation.)

- `<module>_CC_OPTS`:
  options to pass to the C compiler that are specific
  to module `<module>`.  For example,
  `SMap_CC_OPTS` gives the
  specific options to pass to the C compiler when compiling
  `SMap.c`.

- `EXTRA_CC_OPTS`:
  extra options to pass to all C compilations.  This
  is intended for command line use, thus:

>
> >
> >
> > `$ make libHS.a EXTRA_HC_OPTS="-v"`
> >
> >
>


      


## The main `mk/target.mk` file



This section relates mainly to the old build system.



`target.mk` contains canned rules for
all the standard targets described in [standard targets](building/using#).  It is complicated by the fact
that you don't want all of these rules to be active in every
`Makefile`.  Rather than have a plethora of
tiny files which you can include selectively, there is a single
file, `target.mk`, which selectively includes
rules based on whether you have defined certain variables in
your `Makefile`.  This section explains what
rules you get, what variables control them, and what the rules
do.  Hopefully, you will also get enough of an idea of what is
supposed to happen that you can read and understand any weird
special cases yourself.



      


- `HS_PROG`.
  If `HS_PROG` is defined,
  you get rules with the following targets:

  - `HS_PROG`
    itself.  This rule links
    `$(OBJS)` with the Haskell
    runtime system to get an executable called
    `$(HS_PROG)`.
  - `install`
    installs
    `$(HS_PROG)` in
    `$(bindir)`.

- `C_PROG`
  is similar to `HS_PROG`,
  except that the link step links
  `$(C_OBJS)` with the C
  runtime system.

- `LIBRARY`
  is similar to `HS_PROG`,
  except that it links
  `$(LIB_OBJS)` to make the
  library archive `$(LIBRARY)`,
  and `install` installs it in
  `$(libdir)`.


      



Some rules are "double-colon" rules,
thus


```wiki
install :: $(HS_PROG)
      ...how to install it...
```


GNU `make` treats double-colon rules as
separate entities.  If there are several double-colon rules for
the same target it takes each in turn and fires it if its
dependencies say to do so.  This means that you can, for
example, define both `HS_PROG` and
`LIBRARY`, which will generate two rules for
`install`.  When you type `make install` both rules
will be fired, and both the program
and the library will be installed, just as you wanted.


## Recursion



In leaf `Makefile`s the variable
`SUBDIRS`
is undefined.  In non-leaf `Makefile`s,
`SUBDIRS` is set to the list of
sub-directories that contain subordinate
`Makefile`s.  *It is up to you to
set `SUBDIRS` in the
`Makefile`.* There is no automation
here--`SUBDIRS` is too important to
automate.



When `SUBDIRS` is defined,
`target.mk` includes a rather neat rule for
the standard targets ([standard targets](building/using#) that
simply invokes `make` recursively in each of
the sub-directories.



*These recursive invocations are guaranteed to
occur in the order in which the list of directories is specified
in `SUBDIRS`. *This guarantee can
be important.  For example, when you say `make boot` it can be important that the recursive invocation
of `make boot` is done in one sub-directory
(the include files, say) before another (the source files).
Generally, put the most independent sub-directory first, and the
most dependent last.


## Way management in the old build system



We sometimes want to build essentially the same system in
several different "ways".  For example, we want to build GHC's
runtime system libraries with and without thread support,
so that there is an appropriately-built library archive to link
with when the user compiles his program.  It would be possible
to have a completely separate build tree for each such "way",
but it would be horribly bureaucratic, especially since often
only parts of the build tree need to be constructed in multiple
ways.



Instead, the
`target.mk`
contains some clever magic to allow you to build several
versions of a system; and to control locally how many versions
are built and how they differ.  This section explains the
magic.



The files for a particular way are distinguished by
munging the suffix.  The "normal way" is always
built, and its files have the standard suffices
`.o`, `.hi`, and so on.
In addition, you can build one or more extra ways, each
distinguished by a *way tag*.  The object
files and interface files for one of these extra ways are
distinguished by their suffix.  For example, way
`mp` has files
`.mp_o` and
`.mp_hi`.  Library archives have their
way tag the other side of the dot, for boring reasons; thus,
`libHS_mp.a`.



A `make` variable called
`way` holds the current way tag.
*`way` is only ever set on the
command line of `make`* (usually in
a recursive invocation of `make` by the
system).  It is never set inside a
`Makefile`.  So it is a global constant for
any one invocation of `make`.  Two other
`make` variables,
`way_` and
`_way` are immediately derived from
`$(way)` and never altered.  If
`way` is not set, then neither are
`way_` and
`_way`, and the invocation of
`make` will build the "normal
way".  If `way` is set, then the other
two variables are set in sympathy.  For example, if
`$(way)` is "`mp`",
then `way_` is set to
"`mp_`" and
`_way` is set to
"`_mp`".  These three variables are
then used when constructing file names.



So how does `make` ever get recursively
invoked with `way` set?  There are two ways
in which this happens:



      


- For some (but not all) of the standard targets, when
  in a leaf sub-directory, `make` is
  recursively invoked for each way tag in
  `$(WAYS)`.  You set
  `WAYS` in the
  `Makefile` to the list of way tags you
  want these targets built for.  The mechanism here is very
  much like the recursive invocation of
  `make` in sub-directories ([Recursion](#Recursion)).
  It is up to you to set
  `WAYS` in your
  `Makefile`; this is how you control what
  ways will get built.

- For a useful collection of targets (such as
  `libHS_mp.a`,
  `Foo.mp_o`) there is a rule which
  recursively invokes `make` to make the
  specified target, setting the `way`
  variable.  So if you say `make Foo.mp_o`
  you should see a recursive
  invocation `make Foo.mp_o way=mp`,
  and *in this recursive invocation the pattern rule
  for compiling a Haskell file into a `.o`
  file will match*.  The key pattern rules (in
  `suffix.mk`) look like this:

  ```wiki
  %.$(way_)o : %.lhs
        $(HC) $(HC_OPTS) $< -o $@
  ```

  Neat, eh?

- You can invoke `make` with a
  particular `way` setting yourself, in order
  to build files related to a particular
  `way` in the current directory.  eg.

  ```wiki
  $ make way=p
  ```

  will build files for the profiling way only in the current
  directory. 

## Way management in the new build system



When building with Cabal, way management is much simpler.



For libraries, we simply tell Cabal whether or not it should also
build profiling versions of the library.



There are a couple of cases where we want something similar:
For GHC itself, we want stage1, stage2 and stage3 builds; and for
some utils, as discussed earlier, we want to build the util with
both the bootstrapping compiler and the stage2 compiler.



When Cabal builds a package, by default it does its building in a
`dist` directory. In order to build packages in different ways, we
use a different directory for each way, using the `--distpref` flag
to tell Cabal which one to use.


## When the canned rule isn't right



Sometimes the canned rule just doesn't do the right thing.
For example, in the `nofib` suite we want the
link step to print out timing information.  The thing to do here
is *not* to define
`HS_PROG` or
`C_PROG`, and instead define a special
purpose rule in your own `Makefile`.  By
using different variable names you will avoid the canned rules
being included, and conflicting with yours.


