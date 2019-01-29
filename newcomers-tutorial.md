# The Newcomer's Guide To GHC Development

This page is intended to serve as the first stop for those people who say, "I
want to contribute to GHC, but I don't know quite where to begin." Begin here. 

If you have any questions along the way don't hesitate to reach out to the
community. There are people on the [mailing lists and
IRC](mailing-lists-and-irc) who will gladly help you (although you may need to
be patient). Don't forget that all GHC developers are still learning; your
question is never too silly to ask.



## Installing Prerequisites

For the sake of keeping things simple, we will assume a Debian-based GNU/Linux
system. GHC builds on a plethora of platforms; check [Setting up your system
for building GHC](building/preparation) for detailed instructions for your
platform of choice.

### Essential Dependencies

The following list of packages are essential for GHC development:

```sh
sudo apt-get install git autoconf automake libtool make gcc g++ \
   libgmp-dev ncurses-dev libtinfo-dev python3 xz-utils
```

### GHC

To build GHC, you need GHC. Unfortunately, the version that ships with current
Debian systems is too old to build a current GHC; we recommend installing the
latest stable release from the [GHC page](https://www.haskell.org/ghc/). At the
time of writing, the latest release is GHC 8.6.3, which can be downloaded
from [here](https://www.haskell.org/ghc/download_ghc_8_6_3.html).

So:

```sh
wget https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-x86_64-deb9-linux.tar.xz
tar xf ghc-8.6.3-x86_64-deb9-linux.tar.xz
cd ghc-8.6.3
./configure
sudo make install
cd ..
```

Use `ghc --version` to check that you are now using the correct version:

```
The Glorious Glasgow Haskell Compilation System, version 8.6.3
```

For non-Debian platforms, refer to [the GHC download page](https://www.haskell.org/ghc/download_ghc_8_6_3.html)
for an appropriate install package.

### Cabal

You'll need an up-to-date version of Cabal. If you have one installed already,
you can **use cabal to install cabal**:

```sh
cabal install cabal-install
```

Otherwise, you will have to **install Cabal manually** from Hackage:

```sh
wget https://hackage.haskell.org/package/cabal-install-2.4.1.0/cabal-install-2.4.1.0.tar.gz 
tar xf cabal-install-2.4.1.0.tar.gz
cd cabal-install-2.4.1.0
./bootstrap.sh
echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.bashrc
```

Use `cabal --version` to check that you are using the correct version:

```
cabal-install version 2.4.1.0
compiled using version 2.4.1.0 of the Cabal library 
```

Refer to [the Cabal page on Hackage](https://hackage.haskell.org/package/cabal-install)
for the latest version of Cabal and adjust appropriately.

### Alex and Happy

These two fellows are needed for generating lexers and parsers. The versions
available from Debian are too old, but they can easily be installed with Cabal:

```sh
cabal install alex happy
```

### Dependencies for building documentation

```sh
sudo apt-get install python-sphinx texlive-xetex texlive-fonts-recommended fonts-lmodern texlive-latex-recommended texlive-latex-extra
```

### Dependencies for various development tasks

```sh
sudo apt-get install linux-tools-generic xutils-dev
```


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
