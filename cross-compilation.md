# Cross Compiling GHC



This page contains the design notes for cross compilation.  For instructions on how to actually do it, see [Building/CrossCompiling](building/cross-compiling).



Support for cross-compilation works reasonably well in 7.8.1.  Previous versions had various issues which are usually work-aroundable.


## General Problem



The most general case is a developer on build platform (B), wishing to build a GHC that runs on host platform (H) which produces code that runs on target platform (T). But, we need not handle such a general case: There are two common cases:


- Building a **cross-compiler**: Create a compiler that runs on one platform, but targets another. Examples are building a GHC that:

  - runs on Mac OS X, but targets iOS
  - runs on x86\_64 linux, but targets i386
  - runs on some existing GHC supported platform, but targets a smaller embedded platform
- **Cross-building** a normal compiler: Build on one platform a compiler that runs on, and targets another.  Examples:

  - [
    TakeoffGW](http://takeoffgw.sourceforge.net/) is a distribution of Unix tools for Windows, built by cross-compiling on a Linux machine.  They would like to be able to build and distribute GHC this way.  It might be useful for us to be able to cross-compile a Windows GHC from Linux too.
  - build a 64-bit GHC on OS X, by cross-compiling using the 32-bit version.
  - We could port to Win64 ([\#1884](http://gitlabghc.nibbler/ghc/ghc/issues/1884)) by cross-compiling using a 32-bit Windows GHC.
  - Other porting tasks might be easier, given a suitable cross-compilation toolchain.


It seems reasonable to limit ourselves to these two cases.


## Meshing with GHC's 2-Stage Build



The GHC build, in general, is a two stage process, involving three GHC compilers and two sets of build libraries:


- **Stage 0**: the GHC that is already on the build system (the one you specify using `--with-ghc` when configuring the build), comes with a set of built libs, could be older than the version of GHC being built
- **libs boot**: libs that the current version of GHC being built relies on that are either absent or too old in older versions of GHC that might be being used as Stage 0. These libs are built with Stage 0 GHC, and linked into the Stage 1 GHC being built.
- **Stage 1**: the first GHC built, compiled by the Stage 0 GHC, and linked with both libs from that GHC installation, and the boot libs.
- **libs install**: libs that will accompany the final compiler, built by the Stage 1 GHC
- **Stage 2**: the final GHC built, compiled by the Stage 1 GHC, and linked with only the install libs


One way we could have done this is:


<table><tr><th>               </th>
<th>**Stage 0** </th>
<th> **libs boot** </th>
<th> **Stage 1** </th>
<th> **libs install** </th>
<th> **Stage 2** 
</th></tr>
<tr><th>**built on** </th>
<th> ---          </th>
<th> *build*       </th>
<th> *build*     </th>
<th> *build*          </th>
<th> *build*     
</th></tr>
<tr><th>**runs on**  </th>
<th>*build*     </th>
<th> *build*       </th>
<th> *build*     </th>
<th> *host*           </th>
<th> *host*      
</th></tr>
<tr><th>**targets**  </th>
<th>*build*     </th>
<th> ---             </th>
<th> *host*      </th>
<th> ---                </th>
<th> *target*    
</th></tr></table>



But that isn't terribly useful, because if *host* /= *target*, then libs-install can't be used with the stage 2 compiler.  Furthermore, the build only allows for probing for the properties (word size, library function availability, etc...) of one platform.  So this setup would not support *host* /= *target*.  To cross-compile you would set *host* to something different from *build*.



An alternative way to set it up is:


<table><tr><th>               </th>
<th>**Stage 0** </th>
<th> **libs boot** </th>
<th> **Stage 1** </th>
<th> **libs install** </th>
<th> **Stage 2** 
</th></tr>
<tr><th>**built on** </th>
<th> ---          </th>
<th> *build*       </th>
<th> *build*     </th>
<th> *host*           </th>
<th> *host*      
</th></tr>
<tr><th>**runs on**  </th>
<th>*build*     </th>
<th> *host*        </th>
<th> *host*      </th>
<th> *target*         </th>
<th> *target*    
</th></tr>
<tr><th>**targets**  </th>
<th>*host*      </th>
<th> ---             </th>
<th> *target*    </th>
<th> ---                </th>
<th> *target*    
</th></tr></table>



Which is actually equivalent, but it makes *target* the platform you set (which is more natural, we expect to set *target* when configuring a cross-compiler).  In this setup, *build* must equal *host*, because we must be able to run the stage 1 compiler to compile libs-install.



This works out just fine for the two use cases we've identified:


- **Cross-compiler**: a Stage 1 compiler & libs install
- **Cross-building**: a Stage 2 compiler & libs install


And both setups start by configuring with *target* set to the target platform.


## Tool-chains



These kinds of builds need two tool-chains: One that runs on B/H, and compiles for B/H, the "host-tool-chain" or HT, and one that runs on B/H, but compiles for T, the "cross-tool-chain" or XT. The tool-chains include many programs needed: gcc, ld, nm, ar, as, ranlib, strip, and even ghc! The stage0 GHC is effectively part of the HT, and the stage1 we are building is going to become part of the XT. The tool-chain also includes a raft of information about the tools: does ar need ranlib, which extra ld flags need to be passed, etc.



Even in a non-cross build, the current build system takes some care to achieve a limited form of tool-chain separation. In particular, when using the stage0 GHC, the build should be using the tool chain that that compiler is designed to work with -- which may not be the tool chain specified on the ./configure command line. This is only partially fulfilled. For example, while the build uses the stage0 GHC to compile C sources, so that the stage0 compatible gcc will be used, the build also uses other various tools ferreted out by ./configure (ar and ranlib for example).


## Autoconf



(this is slightly out of date now --SDM 18 Jan 2013)



Autoconf offers only limited support for cross compiling. While it professes to know about three platforms, base, host, and target; it knows only about one tool-chain. It uses that tool-chain to determine two classes of information: Information about how to use the tool-chain itself, and information about the target of that tool-chain. Hence, in the cross-compilation case, it makes sense for ./configure to be told about XT.



Autoconf's concept and variable $cross\_compiling only gets set if B ≠ H. This is correct from the standpoint of compiling a simple program (for which T is irrelevant). In our normalized version of B/H/T, B = H, so the logic of autoconf needs to be amended.



This leaves us with the issue of how to tell it about parts of HT it can't infer from the stage0 compiler. We need a new set of variables that know how to compile, link and run things on the host, which if cross compiling need to be different. There needs to be some way to pass those on the configure line. Perhaps something like:


```wiki
--with-host-cc=...
--with-host-as=...
--with-host-ld=...
--with-host-ar=...
--with-host-strip=...
```


A tricky aspect is that some properties of the tool chain are probed by Autoconf ("is cc gcc?", "does ar need ranlib?"). These probes technically should be performed for each tool-chain.



Both ./configure, cabal configure, and hsc2hs desire to run things built for T. If the XT contains an emulator, then this is possible. Two approaches need to be supported here:


1. Autoconf can now discern many values without running code and configure.ac / aclocal.m4 scripts can be changed to avoid running in many cases. (For example in libraries/base I rewrote things to use AC\_COMPUTE\_INT rather than AC\_RUN\_IFELSE to find the sizes of htypes.)
1. Plumb the need to call the emulator to run in the right places. An alternative is to use an alternate linker command that inserts the emulator into those build executables (but this is tricky as you don't want to use that link when building for the real target...)

## Make Files



The overall build sequencing needs to recognize the cross compilation configuration, and adjust build targets and final packaging to match.



There are few other places where the make system needs to get fixed up to use the correct tool-chain at the right time.



There are a set of CPP symbols that are defined when compiling both Haskell and C code:


- *xxx*`_BUILD_ARCH`, *xxx*`_BUILD_OS`: the build platform
- *xxx*`_HOST_ARCH`, *xxx*`_HOST_OS`: the host platform
- *xxx*`_TARGET_ARCH`, *xxx*`_TARGET_OS`: the target platform


There are also similar Make variables. These need to be normalized into something more rational:  At present there the usage is somewhat sloppy, since in most builds all three are the same.


## Status


### March 2011: Mark Lentczner



I actually have much of the above working. At this point I can build and link and run a stage1 cross-compiler. I have plumbed two tool-chains through the top level ./configure and make, and have gotten through configuring several libraries with the in-place cabal. I have patches almost ready to go for items above marked *pending*, and will submit them soon, if this whole approach agrees with everyone.



In general, the problems have all been in plumbing the concepts of XT vs. HT around the build system. While I've been able to fudge it for most of the components though there are places where my work around is forced.


### Jan 2012: Gabor Greif



Looks like Mark has submitted patches and
[
they have been integrated by Ian](http://www.haskell.org/pipermail/cvs-ghc/2011-April/061685.html)
into the main repository. This means GHC v7.4.1 \*should\* be ready for the procedures outlined above.



I am about to test whether the theory matches practice and report back here.


### Jan 2012: Stephen Blackheath



I've submitted patches that make cross compiling work. I will also write some build instructions in the Building Guide.


### Jan 2013: Stephen Paul Weber



Cross-compiling is almost working in HEAD for 7.7.  Check the mailing list for patches that make it work (at least for some platforms).


### Jan 2013: Simon Marlow



I've done some cleanup of the build system, fixed some problems, and created the user instructions [Building/CrossCompiling](building/cross-compiling).



Cross-compiling is working smoothly in at least one setup (Linux/x86\_64 to Linux/armv6 unregisterised (Raspberry Pi))


### Apr 2017: Sergei Trofimovich



I've cleaned up a few more places to make cross-compiling and cross-building work smoother.



More targets work in various directions. Some notable ones:


- x86\_64-linux -\> aarch64-linux (64-bit LE -\> 64-bit LE)
- x86\_64-linux -\> armv7a-linux (64-bit LE -\> 32-bit LE)
- x86\_64-linux -\> m68k-linux (64-bit LE -\> 32-bit BE)
- i386-linux -\> powerpc64-linux (32-bit LE -\> 64-bit BE)


**make install** for a cross-build and cross-compiler more robust.


---


## Questions


### Jan 2012: Gabor Greif


- The [build stages](building/architecture/idiom/stages) page says that Template Haskell is disabled in stage1 compilers. This would mean that a cross-compiler is devoid of an important feature. Any tweak possible to fix this?
- It is unclear from the above how we specify a target C compiler. `--with-gcc` does not seem to cut it.


Stephen Blackheath: I intend to get TH working on cross compilers, so I'm working on this.
 


