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

## Getting The Code

```sh
# needed only once, URL rewrite rule is persisted in ${HOME}/.gitconfig
git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/

# clone GHC's main Git repository (creates './ghc' folder in CWD)
git clone --recursive git://github.com/ghc/ghc
```

## Making your first build

```sh
cd ghc/

# configure build
cp mk/build.mk.sample mk/build.mk

## edit mk/build.mk to remove the comment marker # on the line "BuildFlavour = devel2"

./boot
./configure

# NOTE: On Windows you need to download some binary distributables before being
# able to build. This only has to be done once and can be done by adding a flag
# to the call to configure:
./configure --enable-tarballs-autodownload

# Build GHC
# The -j flag says how many parallel jobs to use. Depending on your system, the
# best value tends to be between N/2 and N, where N is the number of cores on
# your machine. Pick lower values if you're low on RAM, or if have a slow
# disk, or if you have other important or demanding tasks running.
make -j8
```

Now go make yourself some coffee while the build runs.

## Running your freshly-built GHC

If all went according to plan, you should now have a working compiler in
`./inplace/bin/ghc-stage2`. Under normal circumstances, you will not want to
actually install that compiler. Instead, you can invoke it directly, either as
a regular GHC:

```sh
./inplace/bin/ghc-stage2
```

Or in interactive mode, a.k.a. `ghci`:

```sh
./inplace/bin/ghc-stage2 --interactive
```

Let's give it a spin. Paste the following into a file named `test.hs`:

```haskell
module Main where

main = do
  putStrLn "All is fine."
```

Compile it with:

```sh
./inplace/bin/ghc-stage2 test.hs
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
make test
```

This should take a while.

During development, you will not normally want to re-run the entire testsuite
all the time, so instead, you can navigate to a subdirectory and run `make`
there. Try it:

```sh
cd ./testsuite/tests/dependent
make
cd ../../..
```

Another option is to only run selected test cases:

```sh
TEST=T11432 make test
```

This will run only tests named `T11432`, anywhere in the testsuite subtree.

## Rebuilding

While working on GHC, you will need to rebuild often; however, most of the
time, it is possible to avoid a full, slow build. Here's a few things you can
do to speed things up:


1. Select `BuildFlavour = devel2` in your `mk/build.mk`.
   (Further reading: [Make GHC build more quickly](building/using#how-to-make-ghc-build-quickly))
2. In most cases, you will not make changes in more than one subdirectory at
   once. So instead of saying `make` at the top level, you can `cd` into the
   directory where you made your changes first, that is, `compiler`, `utils`,
   `ghc`, or `libraries`.
   (Further reading: [Building a single sub-component](building/using#building-a-single-sub-component))
3. Set `stage=2` in your `mk/build.mk` file. This will keep the Stage 1
   compiler "frozen", re-running only Stage 2 compilation. For most development
   work, this is perfectly fine.
   (Further reading: [Freezing the stage 1 compiler](building/using#freezing-stage-1),
   [Stages](building/architecture/idiom/stages))
4. Use `make fast`. This will skip rebuilding dependencies.
   (Further reading: [Skip dependency building](building/using#skip-dependency-building))

At this point, it is probably worth mentioning the concept of "Stages".

In a nutshell: Stage 0 is your bootstrap compiler (installed from a binary
release); Stage 1 is the new GHC codebase compiled with Stage 0; Stage 2 is the
final, release-grade build, made with Stage 2.

[Idiom/Stages](building/architecture/idiom/stages) explains the concept in more
detail, and provides a rationale.

## Finding a task to work on

Since you came here, you probably have an idea what to work on already - but if
not, do check the issues tagged "newcomer" **(TODO: put the proper link to the
issue tracker here)** - these are issues we expect to be "low hanging fruit".
Of course, we can't ever be sure of how hard a task is before doing it, so
apologies if one of these is too hard.

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

TODO

## Submitting Your Code

### Committing

TODO

### Validating

TODO

### Issuing Merge Requests

TODO

## When things go pear-shaped

TODO

## Further Reading

TODO
