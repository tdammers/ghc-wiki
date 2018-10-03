# Quick setup for GHC 7.11 and later


1. Install Apple's command line tools

  - Install the latest command line tools from [
    http://developer.apple.com/downloads](http://developer.apple.com/downloads) **or**
  - install XCode, launch XCode, open Preferences, select "Downloads" and install the command line tools
1. Install Homebrew [ http://brew.sh](http://brew.sh)
1. `brew install autoconf automake python3`
1. Install a recent GHC version using your method of choice
1. `cabal install alex happy haddock`
1.  Install tools for building the docs:

  - `sudo easy_install pip`
  - `sudo pip install sphinx`
1. Done!


If you want to build a PDF of the docs you'll also need a TeX installation: [
https://tug.org/mactex/](https://tug.org/mactex/)


# Setting up a MacOS X system for building  GHC (HEAD)



**If** your host OS X environment indicates that GCC is a wrapper for clang, (easily detected by looking at output of `/usr/bin/gcc --version` in the terminal),
then please make sure you have an up to date release of GHC, Currently 7.8.3.



You will need to install several tools, using one of  [
Homebrew](http://mxcl.github.com/homebrew/)  MacPorts or Fink.


## Xcode (GCC)


### For Lion (10.7), Xcode 7.3 or higher



Firstly, you need to install the Xcode Command Line tools from Apple. You can do that in two ways (the second is faster):


1. Install all of Xcode:

  - Install Xcode from the Mac App Store.
  - Launch Xcode.
  - In the Preference dialog of Xcode, select the "Downloads" pane and install "Command line tools".
1. Install the command line tools only:

  - At the [
    downloads page of Apple Developer](http://developer.apple.com/downloads), download the latest "Command line tools".
  - Install them.

### Previous versions of OS X and Xcode



Get the most recent version of Apple's Xcode tools that you can. Your OS X CD has a version on it. You may be able to download a newer version from the [
Apple Developer Connection](http://developer.apple.com/tools/xcode) website. You may need to sign up for a free membership in the Apple Developer Connection, and downloading may still cost a little money.  In later versions of OS X (10.6 / 10.7), Apple added the "App Store". Xcode is available within the App Store for "Free".



Successful builds of older GHC sources have been reported using Xcode 3.0, 2.4 and 2.4.1 on Intel Macs. Xcode 2.2.1 is known *not* to work out of the box on Intel Macs, and Xcode 3.0 is known *not* to work out of the box on PowerPC Macs ([\#2887](http://gitlabghc.nibbler/ghc/ghc/issues/2887)). Versions prior to 3.1 may build GHC successfully, but choke on certain libraries.


## GHC



Secondly, you need an installation of GHC for use as your bootstrap compiler environment. 



There are 4 different choices. Choose the one you are most comfortable with! The options below should be a GHC version \>= 7.8.3.


1. Install a [binary distribution from GHC](http://www.haskell.org/ghc/download). 
1. Get the relocatable .app bundle using [
  ghcformacosx](http://github.com/ghcformacosx/ghc-dot-app)
1. Use one of Fink, MacPorts or Homebrew.
1. Install the [
  Haskell Platform](http://www.haskell.org/platform/).  If your OS X version predates 10.8, This build (currently of GHC 7.8.3) is known to support as far back as OS X 10.6 


**NB:** You need to use a binary distribution of GHC 7.4.1 (or later) as your bootstrap compiler.


## Additional Haskell tools



Make sure you have up to date versions of  `alex` and `happy` installed and visible in your shell path. `cabal update ; cabal install alex happy` will install the most recent release for you.



If you are trying out  using GHC head ( currently GHC 7.9 ) for software dev, you will need cabal and cabal install  Head  (current Cabal Head is 1.21, or a release version \>= 1.22, currently cabal versions \>= 1.21 are only on the github master for cabal and cabal-install).


## Important Considerations



By default GHC tries to link to a system installed GMP lib, but depending on how you wish to distribute the resulting applications,
its worth considering either using one of the non GMP integer libs.
If you're OK with static linking GMP into the GHC RTS,
you can add the line 



`libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-intree-gmp`



to you `mk/build.mk` file  (which hosts all your other build system config preferences also.)


## Supporting Older OS X versions with your build



Running a command like  `export MACOSX_DEPLOYMENT_TARGET=10.7` before building will imply setting `-mmacosx-version-min=version` for clang and friends, and will result in a GHC build that can run on any OS X `>= version` (where version can be older than the host machine's OS X version). 



Note that this the deployment target does not affect library availability. This means that builds performed on Sierra (macOS 10.12) systems (which have `clock_gettime`), will be incompatible with previous OS X releases (see [\#12858](http://gitlabghc.nibbler/ghc/ghc/issues/12858)). If you need to build a binary distribution on Sierra which is compatible with previous releases, it's best to simply disable `clock_gettime` support at configuration time, such as by setting the environment variables as follows


```wiki
export MACOSX_DEPLOYMENT_TARGET=10.7
            # Only Sierra onwards supports clock_gettime. See #12858.      
export ac_cv_func_clock_gettime=no
```

## Other tools



Thirdly, if you want to build the development version of GHC from the Git repositories, you also need to install the GNU auto tools. You can build them yourself or use any of brew, fink, or macports:


- (Homebrew): `brew install autoconf automake python3`


Fourthly, if you like to use GHC's LLVM backend:


- (Homebrew): `brew tap homebrew/versions ; brew install llvm34`


GHC 7.8 and older does not support LLVM \>=3.5, which is the default version of llvm installed by Homebrew. Thus you have to install LLVM 3.4 for those GHC versions


## Documentation



To build the documentation you need Sphinx. 


```wiki
sudo pip install sphinx
```


If you are using the Apple provided version of Python you will need to install `pip` before the above command works:


```wiki
sudo easy_install pip
```

### GHC 7.10 and earlier



Finally, if you want to build the documentation you need to install DocBook, PsUtils, and a LaTeX distribution such as [
MacTeX](https://tug.org/mactex/mactex-download.html). You can install it like so:


- (Homebrew): `brew install docbook`, `brew install docbook-xsl`, `brew install docbook2x`, `brew install psutils`


Without `docbook2x`, the build fails after attempting network access for a required DTD. Without `psutils`, the build fails when trying to build the PostScript file used to generate `haddock.pdf`.



DocBook is a fairly large system unto itself and configuring it to build the documentation in its various formats can be a maze. Verifying that your SGML\_CATALOG\_FILES and XML\_CATALOG\_FILES environment variables are pointed at the right places will fix most problems. (What should they point to? After `brew install docbook-xsl` they are both empty.)



the following environment variables setting (using FISH shell syntax) results in a working ghc build that has working html docs and supports any OSX versions \>= the specified deployment target!


```wiki
set -xl XML_CATALOG_FILES /usr/local/etc/xml/catalog
set -xl MACOSX_DEPLOYMENT_TARGET 10.7
```

## Docker



See the Linux page for instructions on running Docker
[
https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Linux\#Docker](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Linux#Docker)


## VMWare / Virtualbox



If you are familiar with VMWare or Virtualbox you can compile Haskell on Linux. You can go to the [Building/Preparation/Linux](building/preparation/linux) page to install GHC.


