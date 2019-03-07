# The Newcomer's Guide To GHC Development

This page is intended to serve as the first stop for those people who say, "I
want to contribute to GHC, but I don't know quite where to begin." Begin here.

If you have any questions along the way, don't hesitate to reach out to the
community. There are people on the [mailing lists and
IRC](mailing-lists-and-irc) who will gladly help you (although you may need to
be patient). Don't forget that all GHC developers are still learning; your
question is never too silly to ask.

The path to a successful merge request looks like this:

- Installing Prerequisites
- Getting The Code
- Building GHC
- Finding a task to work on
- Working with the code
- Submitting Your Code

## Installing Prerequisites

GHC builds on many platforms; check [Setting up your system
for building GHC](building/preparation) for detailed instructions for your
platform of choice.

To build GHC, you'll need both a fairly regular set of build tools as well as a Haskell toolchain. The standard tools are:

- **git**
- **a standard build toolchain**: autoconf, automake, libtool, make, gcc, g++
- **python3**
- **libtinfo**
- **libgmp**
- **ncurses**
- **xz-utils**

Install these as usual. You will need the development packages for system
libraries (usually named like `git-dev`, `python3-dev`, etc. on Unix-like OS distributions).

The Haskell toolchain has:

- **GHC**; you need a reasonably new version, we recommend the latest stable
  release, which you can find via the [GHC homepage](https://www.haskell.org/ghc/).
- **Cabal**; either install through your distro, and then upgrade with `cabal
  install cabal-install`, or follow the steps to install manually outlined on
  [Cabal's Hackage Page](https://hackage.haskell.org/package/cabal-install)
- **Alex** and **Happy**; use `cabal install alex` and `cabal install happy` to install these.

For building documentation, you will also need the following, though you may
opt to skip this part for the time being.

- **sphinx** (probably called `python-sphinx` or similar)
- **xetex (texlive) and fonts** (e.g. `texlive-xetex`,
  `texlive-fonts-recommended`, `fonts-lmodern`, `texlive-latex-recommended`,
  `texlive-latex-extra` - refer to your distro for available packages)

### Required Accounts

In order to submit merge requests, file issues, and participate in code reviews, you will need to create a gitlab account, or sign in with GitHub account. GHC uses its own gitlab environment, found at [gitlab.haskell.org](https://gitlab.haskell.org/).

## Getting The Code

Since you will be committing patches, you will need a GitLab fork, so it's best
to get that out of the way right away. Sign into GitLab, find [the GHC
project](https://gitlab.haskell.org/ghc/ghc), and fork it.

Then, clone it to your development machine:

```sh
# clone GHC's main Git repository (creates './ghc' folder in CWD)
git clone --recursive git@gitlab.haskell.org:yourgitlabusername/ghc.git
```

Note the `--recursive`; this is needed to pull in all the required git
submodules. Should you forget this, you can say
`git submodule update --init` after cloning.

## Building GHC

```sh
cd ghc/

# Build GHC
./hadrian/build.sh -c -j --flavour=devel2
# On Windows, use instead:
# hadrian/build.bat -c -j --flavour=devel2
```

- The `-j` flag says to run a parallel build, guessing an appropriate number of
  parallel jobs. If you are low on system resources (particularly CPU or disk
  I/O), it may be faster to not use this feature.
- The `-c` flag says to also bootstrap GHC's Haskell dependencies and configure
  the build, if necessary.
- `--flavour=devel2` selects a build configuration more appropriate for working
  on the compiler itself; without it, a release build will be made, which takes
  significantly longer and is less convenient for development.

Now go make yourself some coffee while the build runs. Don't be too worried if it
takes around 20 minutes or more for your first build.

This may also be a good time to orient yourself on the GHC codebase (see the
"Further Reading" section below for some starting points) and configure your
editor.

### A Note To Windows Users

For the remainder of this document, we will use the Linux/Unix style
invocations; if you are on Windows, please substitute `hadrian/build.bat` for
`./hadrian/build.sh` as needed.

Further, there are several ways of getting a Unix-like command line on Windows,
and each comes with some caveats. Depending on which approach you use, you may want to use one of these alternative build methods:

- `hadrian/build.stack.bat`
- `hadrian/build.cabal.bat`
- `hadrian/build.nix.bat`

### A Note On Hadrian

Hadrian is GHC's new build system, based on [Shake](https://shakebuild.com/).
It is pretty stable already and should work just fine, but if you run into
issues, it might help to read the below.

Further information on building GHC with Hadrian can be found
[here](https://gitlab.staging.haskell.org/ghc/ghc/wikis/building/hadrian/quick-start),
and on [hadrian's own repository](https://github.com/snowleopard/hadrian).

### Running your freshly-built GHC

If all went according to plan, you should now have a working compiler in
`./_build/stage1/bin/ghc`. Under normal circumstances, you will not want to
actually install that compiler. Instead, you can invoke it directly, either as
a regular GHC:

```sh
./_build/stage1/bin/ghc
```

Or in interactive mode, a.k.a. `ghci`:

```sh
./_build/stage1/bin/ghc --interactive
```

Let's give it a spin:

```sh
./_build/stage1/bin/ghc -e '1+2'
```

This is a nice quick test with fairly good coverage.


If you want to take it a bit further, paste the following into a file named
`test.hs`:

```haskell
module Main where

main = do
  putStrLn "All is fine."
```

Compile it with:

```sh
./_build/stage1/bin/ghc test.hs
```

This should produce a binary `test`, which you can run as `./test`; it should
print:

```
All is fine.
```

### Rebuilding

While working on GHC you will need to rebuild often. Most of the time, it is
possible to avoid full, slower builds. Here are a few things you can do
to speed things up:

- Rebuild only what you're interested in. For example, to only build
  GHC itself, say `./hadrian/build.sh stage2:exe:ghc-bin`. See
  [the Hadrian README](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md) for details.
- Freeze the stage 1 compiler: `--freeze1`. Most of the time, rebuilding the
  stage 1 compiler is not necessary, but it is incredibly difficult to
  determine this in an automatic fashion, so we need to explicitly tell the
  build system to not rebuild Stage 1.
- Picking a faster *build flavor*. For working on the compiler, the `devel2`
  flavour is usually the most appropriate: `--flavour=devel2`. If you can
  afford to skip dependency rebuilds, and compile without any optimizations,
  then the `quickest` flavour is probably best.

At this point, it is probably worth mentioning the concept of "Stages". In a nutshell:

- **Stage 0** is your bootstrap compiler (installed from a binary release).
- **Stage 1** is the new GHC codebase compiled with Stage 0.
- **Stage 2** is the final, release-grade build, made with Stage 1.
- **Stage 3** is a compiler built with the Stage 2 compiler. It is not normally
  built, but can be enabled; its main use is in building cross-compilers.

Note that the subdirectories under `./_build` are named after the stage that
produced them, not the one that is stored in them. This is why
`_build/stage1/bin/ghc` is the Stage **2** compiler, built with Stage 1.

[Idiom/Stages](building/architecture/idiom/stages) explains the concept in more
detail, and provides a rationale.

### Running Tests

GHC comes with an extensive test suite, located in `./testsuite`; the actual
tests are organized into a directory tree under `./testsuite/tests`, sorted by
topic or general feature area. For example, tests relating to GHC's relatively
new dependently-typed features can be found in `./testsuite/tests/dependent`.

To run the entire testsuite, use:

```sh
./hadrian/build.sh test
```

This should take a while.

During development, you will not normally want to re-run the entire testsuite
all the time; instead, you probably want to focus on just a handful of tests
to watch, only to run the full testsuite when you're done, to check for
regressions. Here's how you do that:

```sh
./hadrian/build.sh test --only=T11432
```

This will run only tests named `T11432`, anywhere in the testsuite subtree. You
can specify multiple tests, separated by whitespace:

```sh
./hadrian/build.sh test --only="T11432 T12345 T11111"
```

## Finding a task to work on

You probably have an idea what you want to work on already - but if not,
do check the [issues tagged
"newcomer"](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=newcomer) -
these are issues we expect to be "low hanging fruit".

Of course, we can't be sure how hard a task is before doing it, so
apologies in advance if one of these is too hard.

Either way, **if you are going to work on something, make sure a ticket exists
for it**. This is essential for all sorts of things, but most importantly,
attaching things to tickets makes sure they don't get lost. So if there is no
ticket for it yet, please use the [issue tracker](https://gitlab.haskell.org/ghc/ghc/issues/new) to do so.

We'd like to encouraged you to ask for a starting point on IRC or the
`ghc-devs` [mailing list](mailing-lists-and-irc). You'll find someone familiar
with the process who can help you find a ticket that matches your expertise and you'll also find people who can help you when you get stuck.

## Working with the code

GHC is a decades-old codebase, with plenty of historic quirks to it, and the things it does are intrinsically complex, so it can be quite overwhelming.
Please don't let that discourage you, and don't hesitate to ask questions.

### Coding Style

#### General

GHC doesn't follow a uniform coding style across all modules, and we do not
enforce any in particular. The general rule is to stick to the same coding
style as is already used in the file you're editing. If you must make stylistic
changes, commit them separately from functional changes, so someone looking
back through the change logs can easily distinguish them.

In general, it's much better to write code that is transparent than to write code that is short.

#### Comments

Most of the comments in our codebase are fairly straightforward. One convention
that deserves some explanation however is a system we call "Notes", which we
believe strikes a good balance between minimal disruption of the code itself,
and sufficient amounts of explanations, background information, etc.

Here's what that looks like:

```haskell
prepareRhs :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs env (Cast rhs co)    -- see Note [Float coercions]
  | (ty1, _ty2) <- coercionKind co      -- Do *not* do this if rhs is unlifted 
  , not (isUnLiftedType ty1)            -- see Note [Float coercions (unlifted)]
  = do  { (env', rhs') <- makeTrivial env rhs
        ; return (env', Cast rhs' co) }

        ...more equations for prepareRhs....

{-

Note [Float coercions]
~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  
        ...more stuff about coercion floating...
-}
```

Please pay attention to:

- The Note itself is a long block of prose with a header in a standard format.
  It can (and often will) be quite long, and include examples.
- The wiggly underline is part of the convention and should always be present.
- The Note title acts as an "anchor", and the inline reference to the Note must
  replicate the word "Note" and the Note's title that follows exactly, to ease
  navigation.
- The Note reference is always put in the direct vicinity of the code it
  pertains to. It may be accompanied by a very short hint, as seen here.
- The Note itself however can be anywhere that makes sense. Often, you will see
  a whole slab of Notes at the end of a module, or in between sections of a
  module. Sometimes, though not always, there is a bit of a narrative to the
  group of Notes as a whole.
- Notes can be referenced across modules; in that case, the reference is
  written like this: `-- see Note [Float coercions] in SpecConstr.lhs`

### Adding Tests

Tests live in the `./testsuite/tests` subdirectory, organized by topic / area.
Usually, tests are further organized into `should_compile`, `should_run`, and
`should_fail` subdirectories, though this is merely a convention. Inside each
"leaf" directory, you will find a file named `all.T`, which defines the tests
that live in the directory, and next to it, all the files that are needed to
compile (and possibly run) those tests.

Here are the tests for properly adding a test case:

1. Determine and navigate to the appropriate subdirectory in the testsuite
   tree.
2. Pick a name for your test case. Unless you have a good reason to deviate,
   the convention is to use the corresponding ticket number, prefixed with a
   `T`. If you need more than one test case for that ticket, append something
   to make it unique - either a sequential lowercase letter, or some meaningful
   hint. Note however that some subdirectories use a different naming
   convention, which takes precedence over the `Tnnnnn` format.
   Let's assume `T12345` is your chosen test name.
3. Create a file `T12345.hs`, and implement the test in it (as Haskell code).
4. Open the `all.T` file, and add your test. It should look something like
   this: `test('T12345', normal, compile_and_run, [''])`.
5. Navigate back to the project root and run your test: `./hadrian/build.sh
   test --only="T12345"`.

Further reading:

- The full guide to [adding test cases](building/running-tests/adding) explores
  more features of the testsuite driver.

## Submitting Your Code

As a general note, don't be afraid to make mistakes here: if you do, people
will kindly tell you and help you fix them. We much prefer a contribution that
isn't perfect yet over one that we never get to see at all.

### Committing

The usual git hygiene applies:

- Create a new branch for each merge request, based off of `master`.
- Write a one-line summary, preferably no more than 50 columns, followed by 1
  blank line, followed by an (optional, but recommended) longer description of
  the commit.
- Tastefully squash commits before submitting. A merge request may contain
  multiple commits, but in the interest of keeping line noise low, functionally
  related commits should be squashed together. Also, as a rule of thumb, each
  individual commit should validate cleanly (see below) on its own, so as not
  to disturb `git bisect`.
- Rebase onto `master` before submitting. This makes merge conflicts later on
  less likely.

On top of that, a few GHC-specific conventions:

- Prefer imperative over third-person. Good: `Add frabber squabbing to
  avoid bleebles`. Less good: `Adds frabber squabbing to avoid bleebles`.
- Mention relevant ticket(s) in the commit, like so: `#14567`.

### Validating

GHC uses a Continuous Integration (CI) setup that automatically builds and
tests all new merge requests, and in general, we require CI to pass before a
patch gets merged into `master`.

The `validate` command allows you to perform exactly the same
build-and-test run locally, with the same configuration, so that you
can avoid submitting patches that break the build (and would thus needlessly
clog the CI queue). Running it is as simple as this:

```sh
./validate
```

`validate` is also a convenient way of doing a comprehensive and fairly
reliable full test run, providing more certainty at the expense of taking
(much) longer than running subsets of the testsuite on fast rebuilds.

Beware, however, that a full `validate` run can take upwards of an hour,
depending on your hardware, so it may or may not be worth it.

### Issuing Merge Requests

Once you are happy with the state of your feature branch, issue a merge
request:

- Push your branch if you haven't already
- In gitlab, navigate to your fork
- Select "Merge Requests" in the navigation menu on the left
- Click the big green "New Merge Request" button
- Follow the UI - it should be fairly self-explanatory.

### What Happens Next?

- The CI system will pick up your merge request, build it, run it, and report
  back to gitlab. If all goes according to plan, the merge request will be
  flagged "green". Keep in mind that a CI build takes quite some time, and
  there is usually a queue of merge requests to be built, so don't expect
  immediate feedback here.
- Someone privileged and knowledgeable will review your code.  Most likely,
  they will not accept your merge request right away, but instead tell you to
  change a few things, or ask you to clarify why you did certain things the way
  you did. A very common request is "could you add a Note explaining
  this-and-that in more detail please?" So don't be alarmed or discouraged;
  this is normal and part of the process, and absolutely does not mean your
  contributions aren't welcome. They very much are. In a similar spirit, the
  review queue tends to be quite long, so please be patient.
- Once the reviewers are sufficiently happy with your patch, it will be merged
  into `master`, and make its way into an upcoming GHC release. There is
  nothing you need to do for this to happen yourself, though please do holler
  if it takes awfully long.

## When things go pear-shaped

- A common source of errors is not updating your submodules after switching
  branches or checking out a different commit. So if you see a build failing
  mysteriously, `git submodule update` is one of the first things to try.
- You may also want to re-run the "boot" and "configure" steps; just add `-b`
  to your Hadrian command to do so - or use `./hadrian/build.sh boot` and
  `./hadrian/build.sh configure` to run them separately.
- Sometimes, though hopefully not often, you may have to start from scratch,
  throwing away all existing build artifacts and intermediates. Simply deleting
  the `_build` subdirectory will do that for you. Note that this will require a
  full rebuild of the stage 1 compiler.
- For a more radical approach, `git clean` can restore your working copy to a
  pristine state. Due to the submodule setup we use, you will have to issue a
  command like this one:
  `git clean -xdf && git submodule foreach 'git clean -xdf' && git submodule update --init`.

## Further Reading

- [Building](building)
- [Building GHC With Hadrian](building/hadrian/quick-start)
- [Hadrian](https://github.com/snowleopard/hadrian)
- [Working Conventions](working-conventions)
- [The GHC Commentary](commentary)
- [Debugging](debugging)
- [The Architecture of Open Source Applications: The Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html),
  by Simon Marlow and Simon Peyton-Jones, two of the chief architects of GHC
  and Haskell itself
