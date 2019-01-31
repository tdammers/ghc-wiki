# The Newcomer's Guide To GHC Development

This page is intended to serve as the first stop for those people who say, "I
want to contribute to GHC, but I don't know quite where to begin." Begin here. 

If you have any questions along the way don't hesitate to reach out to the
community. There are people on the [mailing lists and
IRC](mailing-lists-and-irc) who will gladly help you (although you may need to
be patient). Don't forget that all GHC developers are still learning; your
question is never too silly to ask.



## Installing Prerequisites

GHC builds on a plethora of platforms; check [Setting up your system
for building GHC](building/preparation) for detailed instructions for your
platform of choice.

A minimal list of prerequisites:

### Essential Tools:

- **git**
- **a typical build toolchain**: autoconf, automake, libtool, make, gcc, g++
- **python3**

Install these as usual.

### Essential Build Dependencies:

- **libtinfo**
- **libgmp**
- **ncurses**
- **xz-utils**

Install these as usual. You will need the development packages for these
(typically named `something-dev` on most Unix-like OS distributions).

### Haskell Toolchain:

- **GHC**; you need a reasonably new version, we recommend the latest stable
  release, which you can find via the [GHC homepage](https://www.haskell.org/ghc/).
- **Cabal**; either install through your distro, and then upgrade with `cabal
  install cabal-install`, or follow the steps to install manually outlined on
  [Cabal's Hackage Page](https://hackage.haskell.org/package/cabal-install)
- **Alex** and **Happy**; use Cabal to install these.

### Dependencies for building documentation

- **sphinx** (probably called `python-sphinx` or similar)
- **xetex (texlive) and fonts** (e.g. `texlive-xetex`,
  `texlive-fonts-recommended`, `fonts-lmodern`, `texlive-latex-recommended`,
  `texlive-latex-extra` - refer to your distro for available packages)

### Required Bureaucracy

GHC uses its own gitlab environment, found at
[gitlab.haskell.org](https://gitlab.haskell.org/). In order to submit merge
requests, file issues, and participate in code reviews, you will need an
account.

## Getting The Code

```sh
# needed only once, URL rewrite rule is persisted in ${HOME}/.gitconfig
git config --global url."git@gitlab.haskell.org:ghc/packages-".insteadOf git@gitlab.haskell.org:ghc/packages/

# clone GHC's main Git repository (creates './ghc' folder in CWD)
git clone --recursive git@gitlab.haskell.org:ghc/ghc.git
```

## Making your first build

```sh
cd ghc/

# Build GHC
./hadrian/build.sh -c -j
# On Windows, use instead:
# hadrian/build.bat -c -j

# The -j flag says to run a parallel build, guessing an appropriate number of
# parallel jobs. If you are low on system resources (particularly CPU or disk
# I/O), it may be faster to not use this feature.
#
# The -c flag says to also bootstrap GHC's Haskell dependencies and configure
# the build, if necessary.
```

Now go make yourself some coffee while the build runs.

## Running your freshly-built GHC

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

Let's give it a spin. Paste the following into a file named `test.hs`:

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

## Running Tests

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

## Rebuilding

While working on GHC, you will need to rebuild often; however, most of the
time, it is possible to avoid a full, slow build. Here's a few things you can
do to speed things up:

- Rebuild only the things that you're interested in. E.g.:
  `./hadrian/build.sh _build/stage1/bin/ghc` to build only the compiler.
- Freeze the stage 1 compiler: `--freeze1`. Most of the time, rebuilding the
  stage 1 compiler is not necessary, but the build system is not smart enough
  to figure this out on its own; freezing stage 1 tells it to *never* rebuild
  stage 1.
- Picking a faster *build flavor*. For working on the compiler, the `devel2`
  flavour is usually the most appropriate: `--flavour=devel2`. If you can
  afford to skip dependency rebuilds, and compile without any optimizations,
  then the `fastest` flavour is probably best.

At this point, it is probably worth mentioning the concept of "Stages".

In a nutshell:

- **Stage 0** is your bootstrap compiler (installed from a binary release).
- **Stage 1** is the new GHC codebase compiled with Stage 0.
- **Stage 2** is the final, release-grade build, made with Stage 1.

Note that the subdirectories under `./_build` are named after the stage that
produced them, not the one that is stored in them. This is why
`_build/stage1/bin/ghc` is the Stage **2** compiler, built with Stage 1.

[Idiom/Stages](building/architecture/idiom/stages) explains the concept in more
detail, and provides a rationale.

## Finding a task to work on

Since you came here, you probably have an idea what to work on already - but if
not, do check the issues tagged "newcomer" **(TODO: put the proper link to the
issue tracker here)** - these are issues we expect to be "low hanging fruit".
Of course, we can't ever be sure of how hard a task is before doing it, so
apologies if one of these is too hard.

Either way, **if you are going to work on something, make sure a ticket exists
for it**. This is essential for all sorts of things, but most importantly,
attaching things to tickets makes sure they don't get lost. So if there is no
ticket for it yet, do file one. **(TODO: put link to issue tracker here)**

Apart from that, you are encouraged to ask for a starting point on IRC or the
`ghc-devs` [mailing list](mailing-lists-and-irc). There someone familiar with
the process can help you find a ticket that matches your expertise and help you
when you get stuck.

## Working with the code

GHC is an old codebase, with plenty of historic quirks to it, and the things it
does are intrinsically complex, so it can be quite overwhelming. Don't let that
discourage you, and don't hesitate to ask questions.

### Coding Style

#### General

GHC doesn't follow a uniform coding style across all modules, and we do not
enforce any particular one. The general rule is to stick to the same coding
style as is already used in the file you're editing. If you must make stylistic
changes, commit them separately from functional changes, so that someone
looking back through the change logs can easily distinguish them.

It's much better to write code that is transparent than to write code that is
short.

#### Comments

We mainly use 3 types of comments in GHC.

First, all top-level entities should have Haddock comments that briefly
describe what they do and, if needed, why they are there. Haddock comments
are included in HTML documentation, so as usual, this is the place to document
an entity's public-ish interface.

Second, short explanations, caveats, warnings, etc., are written inline. As a
general rule of thumb, these should be one-liners, or even fit on the same line
together with the thing they document. This is great for nitty-gritty
implementation details.

Third, for longer explanations, background information, bigger-picture
rationales, history, etc., we have adopted a system we call "Notes". Here's
what that looks like:

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

Note that:

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

### Committing

The usual git hygiene applies:

- Create a new branch for each merge request, based off of `master`. If you
  have push permissions on the main `ghc/ghc` repo on
  [gitlab.haskell.org](https://gitlab.haskell.org), you may create and push
  your branch there; otherwise, fork the project and push to your fork.
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

Before submitting your code, you should "validate" it:

```sh
./hadrian/build.sh validate
```

GHC uses a Continuous Integration (CI) setup that automatically builds and
tests all new merge requests. The `validate` command allows you to perform an
identical build-and-test run locally, with the same configuration, so that you
can avoid submitting patches that break the build (and would thus needlessly
clog the CI queue).

`validate` is also a nice, convenient way of doing a comprehensive and fairly
reliable full test run, providing more certainty at the expense of taking
(much) longer than running subsets of the testsuite on fast rebuilds.

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
  review queue tends to be quite long, so please do be patient.
- Once the reviewers are sufficiently happy with your patch, it will be merged
  into `master`, and make its way into an upcoming GHC release. There is
  nothing you need to do for this to happen yourself, though please do holler
  if it takes awfully long.

## When things go pear-shaped

TODO

## Further Reading

- [Building](building)
- [Working Conventions](working-conventions)
- [The GHC Commentary](commentary)
- [Debugging](debugging)
- [The Architecture of Open Source Applications: The Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html),
  by Simon Marlow and Simon Peyton-Jones, two of the chief architects of GHC
  and Haskell itself
