


# Setting up a Linux system for building GHC



If you're on a recent Linux system, then you should be able to get a working build environment by installing the following packages using your system's package manager.


## Docker



If you are familiar with docker and comfortable doing all your work in the docker container with a default bash shell. This is a 1 step install for a development image (ghc build requirements plus a few development related tools). The runghc binary can not be run outside of the docker container because GHC will be compiled with paths only available in the container.
First cd into your ghc directory that you should check out according to [Building/GettingTheSources](building/getting-the-sources), then


```wiki
   sudo docker run --rm -i -t -v `pwd`:/home/ghc gregweber/ghc-haskell-dev /bin/bash
```


That's it!
This mounts your ghc source code into the docker container.
This way you can still hack on ghc with Emacs, etc, but you are just building from the docker container.
Note that `arc` (the GHC patch submission tool) is installed in the image (although you can also use it from your docker host) along with vim-tiny for editing commit messages.



Send pull requests to [
https://github.com/gregwebs/ghc-docker-dev](https://github.com/gregwebs/ghc-docker-dev) if something is out-of-date.


## Fedora



Install the [
required tools](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools) using the following command for Fedora 22 and later (for earlier versions of Fedora, use `yum` instead of `dnf`):


```wiki
   sudo dnf install glibc-devel ncurses-devel gmp-devel autoconf automake libtool gcc gcc-c++ make perl python ghc happy alex git
```


For building the documentation: (User's Guide and Cabal guide):
(optional)


```wiki
   # GHC > 7.10
   sudo dnf install python-sphinx
   # GHC <= 7.10
   sudo dnf install docbook-utils docbook-utils-pdf docbook-style-xsl
```


other  packages that are useful for development:
(optional)


```wiki
   sudo dnf install strace patch
```


Now the system should be ready to build GHC.



For a quickstart, follow the commands listed under:



[
https://github.com/ghc/ghc\#building--installing](https://github.com/ghc/ghc#building--installing)


## Debian, Ubuntu, and other Debian-based systems



You can make sure you have all dependencies by


```wiki
   sudo apt-get build-dep ghc
```


But this might install some packages you do not use in your system (e.g. Sphinx).  Alternatively install the following:


```wiki
   sudo apt-get install git autoconf automake libtool make gcc g++ \
     libgmp-dev ncurses-dev libtinfo-dev python3 xz-utils
```


(`ncurses-dev` is needed by the `terminfo` package, and `g++` is needed by a couple of tests, `ghcilink003` and `ghcilink006`).



Optional: install LLVM from \<[
http://apt.llvm.org](http://apt.llvm.org)\> (only necessary to make the `-fllvm` flag work). [Commentary/Compiler/Backends/LLVM/Installing](commentary/compiler/backends/llvm/installing#) will tell you which version to install.



Due to the nature of Debian, you may have difficulty building GHC \>7.6 due to version incompatibilities with the Happy and Alex packages.  To alleviate this issue simply install both packages using the `haskell-platform` provided `cabal`.


```wiki
   cabal install alex happy
```


If you install alex and happy using cabal as shown above, you will need to add the cabal installation directory to PATH before running the `configure` script.


```wiki
   export PATH=$HOME/.cabal/bin:$PATH
```


For building the documentation (User's Guide):


```wiki
   # GHC > 7.10:
   sudo apt-get install python-sphinx texlive-xetex texlive-fonts-recommended fonts-lmodern texlive-latex-recommended texlive-latex-extra
   # GHC <= 7.10:
   sudo apt-get install dblatex docbook-xsl docbook-utils libxml2-utils texlive-font-utils
```


other packages that are useful for development:


```wiki
   sudo apt-get install linux-tools-generic xutils-dev
```


The package `linux-tools` includes `perf`, see [Debugging/LowLevelProfiling/Perf](debugging/low-level-profiling/perf). The package `xutils-dev` provides the `lndir` program, need for running `make sdist` and useful for maintaining a separate build tree, see [Building/Using](building/using).



For [validating patches](testing-patches) :


## Arch



Install the [
required tools](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools):


```wiki
   sudo pacman -S ghc ghc-static perl gcc make happy alex cabal-install autoconf automake python python-sphinx libedit numactl
```


Note that due to questionable decisions made by Arch's Haskell packaging (namely the decision to package only dynamic objects), it's quite important that you do not attempt to use the `haskell-*` packages to bootstrap your GHC tree. 


## Nix/NixOS



The recommended way to create an environment in which to build GHC is to use Alp's `ghc.nix` repository. 


1. Clone `https://github.com/alpmestan/ghc.nix` into your `ghc` folder.
1. `nix-shell ghc.nix/`


You can then perform a build by running


```wiki
configurePhase
buildPhase
```


Enable parallel builds in the nix build environment:


```wiki
   export NIX_BUILD_CORES=4
```