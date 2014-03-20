# GHC Code Owners






GHC has an enormous "surface area", and has grown way beyond the
capacity of any individual, or even research group, to maintain and
develop.  Its continued success is built on the active contributions
of a large group of highly talented people, who take responsiblity 
for significant parts of the system.  This page summarises who does
what. 



See also 


- The [GHC Team](team-ghc) page, which includes a list of committers.
- The [working on GHC](working-conventions) page, which tell you how we work together.

## What it means to "own" part of GHC



What does it mean to be the "owner" of a chunk of GHC, say X?


- Being the owner of a chunk of GHC is a public service.  Everyone else gives the owner lots of kudos for looking after X on our behalf.
- The owner is the first port of call for guidance about how X works.
- If you want to work on X, it makes sense to talk to the owner first, to see what he or she is planning, and to debate the design.
- The owner feels a sense of ownership. That means taking responsibility for keeping it clean and beautiful, and refactoring when necessary.
- The owner can't be expected to fix every buglet, but feels a sense of responsibility to make sure that serious bugs are fixed.
- Owners are expected to step down if they are no longer able to make the time commitment required.


Ownership means being the "first among equals" rather than "having exclusive control":


- It does not mean that everyone else can relax and say "oh Fred is dealing with X".  Fred needs help!
- It does not mean that no changes can happen to X without the owner's say-so, most especially if the owner becomes hard to contact.
- It does not mean that the owner can make decisions that the community disagrees with. Being willing to roll up your sleeves lends weight to your voice, but owners should seek consensus for contentious issues.


 
When contacting an owner, always cc `ghc-devs@haskell.org`, so that others can contribute.  It's a community thing.



The GHC repository is held on a machine called "Rock" (TODO link to new infrastructure information.)


## Current Owners



This section lists the current owners of various parts of GHC.  If
you'd like to take responsibility for something, tell us (on the
`ghc-devs` list). Ideally all parts of GHC should have someone who
claims responsibility for it.  Although some of these parts are
cross-cutting concerns, we also list the directories in the GHC source
tree that are most directly implicated.


- Overall guidance and advice (the GHC Tsars)

  - Simon Peyton Jones, Simon Marlow

- Build system, release process.

  - Austin Seipp
  - Directories: `mk/`, `distrib/, `ghc-tarballs/, `rules/`, `utils/`, `compiler/main`


 


- Foreign function interface (FFI)

  - Austin Seipp
  - Directories: `libffi/`

- Documentation

  - Austin Seipp
  - Directories: `docs/`

- The main `HsSyn` data type

  - Simon Peyton Jones
  - Directories: `compiler/hsSyn`

- Renamer

  - Simon Peyton Jones
  - Directories: `compiler/rename`

- Type inference and interface files

  - Simon Peyton Jones
  - Directories: `compiler/types`, `compiler/typecheck`, `compiler/iface`


   


- Core, System FC, Simplifier and other optimisations on Core

  - Simon Peyton Jones
  - Directories: `compiler/coreSyn`, `compiler/deSugar`, `compiler/simplCore`, `compiler/specialise`, `compiler/stranal`

- Native code generator, register allocation

  - Ben Lippmeier, Simon Marlow
  - Directories: `compiler/nativeGen`

- LLVM code generation

  - David Terei
  - Directories: `compiler/llvmGen`

- Runtime system, SMP support

  - Simon Marlow
  - Directories: `rts/`, `includes/`

- GHCi, and debugger

  - **Needs love**
  - Directories: `compiler/ghci`, `ghc/`, `driver/`.

- Template Haskell

  - Simon Peyton Jones
  - Directories: `libraries/template-haskell`, plus bits scattered over `compiler/`

- Data Parallel Haskell

  - Manuel Chakravarty, Ben Lippmeier
  - Directories: `compiler/vectorise`

- Safe Haskell

  - David Terei
  - Directories: `compiler/basicTypes`, `compiler/deSugar`, `compiler/ghci`, `compiler/hsSyn`, `compiler/iface`, `compiler/main`, `compiler/parser`, `compiler/rename`, `compiler/typecheck`, `compiler/types`

- Cloud Haskell

  - Tim Watson, Jeff Epstein
  - Code at: `http://haskell-distributed.github.com`

- Performance Tsars

  - Johan Tibell, David Terei, Bryan O'Sullivan
  - Directories: `nofib/`


Cross-cutting compiler directories: `parser/`, `prelude/`, `profiling/`, `utils/`.


## Platform maintainers



The following table lists the people who have generously agreed to be responsible for the GHC port to a particular platform, or to produce binary packages for a certain platform.



For a full description of GHC support for various platforms, see [Platforms](platforms).


- **x86**

  - Windows (targetting [ mingw](http://www.mingw.org)): Sigbjorn Finne
  - Windows (targetting [
    Cygwin](http://www.cygwin.com)): Position open; port bitrotted
  - Linux (generic): Simon Marlow,
    Simon Peyton Jones
  - FreeBSD/i386: FreeBSD Haskell Team
  - OpenBSD: Matthias Kilian
  - openSUSE Linux/x86 packages: Peter Trommler
  - Gentoo Linux/x86 packages: Gentoo Haskell team
  - Debian GNU/Linux/x86 packages: Kari Pahula
  - MacOS X: Manuel Chakravarty
  - Fedora packages: Fedora Haskell SIG

- **PowerPC**

  - AIX: Audrey Tang
  - Linux: Ryan Lortie, Erik de Castro Lopo
  - Gentoo Linux packages (32 and 64bit): Gentoo Haskell team
  - Debian GNU/Linux/powerpc packages: Kari Pahula
  - MacOS X: Wolfgang Thaller,Thorkil Naur
  - Fedora packages: Fedora Haskell SIG (until F12)
  - openSUSE Linux packages: Peter Trommler

- **Sparc**

  - Solaris: Position open
  - Linux:   Position open
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/sparc packages: Kari Pahula

- **x86\_64**

  - Linux: Simon Marlow
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/amd64 packages: Kari Pahula
  - FreeBSD/amd64: FreeBSD Haskell Team
  - OpenBSD: Matthias Kilian
  - Fedora packages: Fedora Haskell SIG
  - openSUSE Linux packages: Peter Trommler

- **Mips64**

  - Irix: Don Stewart

- **Mips**

  - Debian GNU/Linux/mips packages: Kari Pahula

- **Mipsel**

  - Debian GNU/Linux/mipsel packages: Kari Pahula

- **IA-64**

  - Linux: Matt Chapman
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/ia64 packages: Kari Pahula

- **Alpha**

  - Dec OSF: Ken Shan
  - Linux: Position open.
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/alpha packages: Kari Pahula

- **HPPA**

  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/hppa packages: Kari Pahula

- **S/390**

  - Debian GNU/Linux/s390 packages: Kari Pahula

- **Arm**

  - Debian GNU/Linux/arm packages: Kari Pahula
  - iOS packages: Luke Iannini

## Libraries



Some [libraries](commentary/libraries) are very closely coupled to GHC, come with any GHC installation, and are properly considered part of GHC.  (This list is very incomplete.)


- IO Manager

  - Johan Tibell, Andreas Voellmy

## The Full Glasgow Haskell Team



Aside from general code owners, there is a fairly large group of people who commit to GHC, many for different reasons, and all of various backgrounds. When writing patches or looking for help, it's good to see if anyone is interested in the same area.



For more details including the full list of people involved, see [TeamGHC](team-ghc).


