# Building and Porting GHC



This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.


## Contents



**Getting started**


- [Setting up your system for building GHC](building/preparation) (including platform specific instructions)
- [Getting the sources](building/getting-the-sources) (here's the [list of Git repositories](repositories) that we use)
- [Quick Start: just build GHC](building/quick-start)
- Newcomers should also go to [Newcomers](newcomers).


**More detailed information about using the build system**


- [Using the build system](building/using)
- [Standard targets](building/standard-targets)
- [Making GHC's source code searchable with Hoogle](building/hoogle)
- [Building the documentation](building/docs)
- [Installing GHC from a build](building/installing)
- [Unregisterised builds](building/unregisterised)
- [Loading GHC into GHCi](building/in-ghci)


**How to test and benchmark GHC**


- [Validating Patches](testing-patches)
- [Running the regression test suite](building/running-tests)
- [Testing GHC against all of Hackage](hackage-testing)
- [The NoFib benchmark suite](building/running-no-fib)


**Information about libraries (= packages)**


- [Installing extra libraries for your in-place GHC](debugging/installing-packages-inplace)
- [The libraries on which GHC depends](commentary/libraries)


**More detailed information about how the build system works**


- [Overview of files and directories](commentary/source-tree)
- [Architecture of the build system](building/architecture)
- [Modifying the build system](building/modifying)


**Porting GHC and building cross compilers**


- [Platforms that GHC currently supports](platforms)
- [Cross-compilation](building/cross-compiling)
- [Compiling for 32 bits on 64 bits](building/compiling32on64)
- [Setting up a Debian system to build a GHC cross-compiler](building/cross-compiling-on-debian)
- [Some notes on the various moving parts involved in the Windows toolchain](surviving-windows)


**Troubleshooting**


- [FAQ: Solving common problems with building GHC](building/troubleshooting)

## Contributed documentation



Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.


- [SonyPS3](sony-p-s3) : Hints for building on the Sony PS3
- [Arm64](arm64) : An attempt to cross-compile GHC for Android on ARM64.
