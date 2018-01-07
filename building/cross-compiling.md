


# Cross-compiling GHC



This page describes how to do cross-compilation with GHC.  That is:


- **Building GHC as a cross-compiler**: Create a compiler that runs on one platform, but targets another. Examples are building a GHC that:

  - runs on Mac OS X, but targets iOS
  - runs on x86\_64 linux, but targets i386 (though sometimes you can just [install and use i386 GHC on x86\_64](building/compiling32on64))
  - runs on some existing GHC supported platform, but targets a smaller embedded platform
- **Cross-compiling GHC itself**: Build on one platform a compiler that runs on, and targets another.  Examples:

  - [
    TakeoffGW](http://takeoffgw.sourceforge.net/) is a distribution of Unix tools for Windows, built by cross-compiling on a Linux machine.  They would like to be able to build and distribute GHC this way.  It might be useful for us to be able to cross-compile a Windows GHC from Linux too.
  - build a 64-bit GHC on OS X, by cross-compiling using the 32-bit version.
  - We could port to Win64 ([\#1884](http://gitlabghc.nibbler/ghc/ghc/issues/1884)) by cross-compiling using a 32-bit Windows GHC.
  - Other porting tasks might be easier, given a suitable cross-compilation toolchain.

## Terminology and background



(For more background and design docs see [CrossCompilation](cross-compilation).)



Traditional cross-compilation terminology defines three platforms:


- *build* - the platform we're building on
- *host* - the platform the compiler will run on
- *target* - the platform the compiler we're building will generate code for


These are the platforms given to the `configure` script when configuring the build.



GHC does not support all three platforms being different.  The rule is:


- *build* must equal *host*


We'll see why that is if we consider which platforms the various parts of the build use.


- **Stage 0**: the GHC that is already on the build system (the one you specify using `--with-ghc` when configuring the build), comes with a set of built libs, could be older than the version of GHC being built
- **libs boot**: libs that the current version of GHC being built relies on that are either absent or too old in older versions of GHC that might be being used as Stage 0. These libs are built with Stage 0 GHC, and linked into the Stage 1 GHC being built.
- **Stage 1**: the first GHC built, compiled by the Stage 0 GHC, and linked with both libs from that GHC installation, and the boot libs.
- **libs install**: libs that are built by the Stage 1 GHC, and installed by `make install`.
- **Stage 2**: the final GHC built, compiled by the Stage 1 GHC, and linked with libs-install

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
<th> *build*       </th>
<th> *host*      </th>
<th> *target*         </th>
<th> *target*    
</th></tr>
<tr><th>**targets**  </th>
<th>*build*     </th>
<th> ---             </th>
<th> *target*    </th>
<th> ---                </th>
<th> *target*    
</th></tr></table>



(this is not the only way we could have done it, for more rationale see [CrossCompilation](cross-compilation))



So in order to use the stage 1 compiler to build libs-install, we must be able to run it, and hence *host* must be the same as *build*.  You never need to specify *host*, just specify *target* when making a cross-compiler.



So considering the two cases we identified at the top of the page:


- **Building GHC as a cross-compiler** - this is the stage 1 compiler
- **Cross-compiling GHC itself** - this is the stage 2 compiler


both of these cases are handled in the same way.


## Tools to install



First you want to install a C compiler and related tools that generate code for your target platform.  You'll need:


- `gcc`
- `ld`
- `nm`
- `objdump`
- C libraries


(basically gcc + binutils + libc).  These need to be installed somewhere different from your native gcc & binutils so they don't conflict.  We assume that your `gcc` knows where its libraries live, otherwise you will probably need to add more flags to your `build.mk` settings to tell it.



If you are using LLVM as your compiler back end, you will need to make sure the llc and opt executables are in your search path. No other configuration is necessary. Also see the ARM-specific note below.



Also install the other tools needed to build GHC on your platform: see [Building/Preparation](building/preparation).


## Getting your source tree



Follow the instructions in [Building/GettingTheSources](building/getting-the-sources).


## Configuring the build



The C cross-compiler and tools are usually installed with the platform name as a prefix, e.g. if the target platform is `arm-linux-gnueabihf` then the gcc cross-compiler is named `arm-linux-gnueabihf-gcc`.  If your cross-compiling toolset is set up like this, then add the directory containing the tools to your `PATH`, and just say:


```wiki
./configure --target=<target>
```


and `configure` will find all the tools, using `<target>` as the prefix. Note that it's best to double-check that `configure` finds the correct compiler for your target platform. For instance, if your target's `gcc` has a version suffix (e.g. as would happen if you install `gcc-6-aarch64-linux-gnu` but not `gcc-aarch64-linux-gnu` on Debian) `configure` will likely fail to find it and instead use your host's `gcc`. Hilarity will ensue.



In GHC 7.8 a bug in the configure macros prevents it from finding the cross-compiling gcc, so you will always need to use `--with-gcc`.



If you need to specify the tools explicitly, then you can say


```wiki
./configure --target=<target> --with-gcc=<gcc> --with-ld=<ld> --with-nm=<nm> --with-objdump=<objdump>
```


Note: if you are cross-compiling for a platform that doesn't have a native code generator or registerised LLVM support, then you should also add


```wiki
  --enable-unregisterised
```


(the build system will probably do this automatically for you anyway, but it doesn't hurt to be explicit)



Your target triplet/quad must have the general form `<arch>-<os>-<abi>` or `<arch>-<vendor>-<os>-<abi>`. If configure complains that your arch, vendor or OS is unknown, then you will need to modify the checkArch(), checkVendor() or checkOS() function in **aclocal.m4**, then get autotools to re-create the configure script using the **autoreconf** command.


## `build.mk` settings



Note: when you name your `build.mk` as \<target\>`-build.mk` then its settings will be effective for \<target\> and the regular `build.mk` will be ignored. This is handy if you build a regular GHC and/or cross-compilers from the same working directory, as it avoids the hassle of juggling with settings files.



If you are only interested in building a cross-compiler, then you can add


```wiki
Stage1Only = YES
```


to your `mk/build.mk`, and the build system will stop before building stage 2.  The resulting cross-compiler and tools can be installed as usual with 'make install'.



If your cross-toolset does not include the GMP library, then you should also add:


```wiki
INTEGER_LIBRARY = integer-simple
```


since even though we have a copy of GMP in the GHC source tree, it cannot be cross-compiled (ToDo: why not?).  You must put this in `mk/build.mk` before building anything, because `INTEGER_LIBRARY` cannot be changed without doing a full `make distclean`.



If you don't have an `ncurses` for your target available in your build environment, you can add `WITH_TERMINFO=NO` to `mk/build.mk` to build GHC and its utilities without `terminfo` support (since [
Phab:D3177](https://phabricator.haskell.org/D3177)).


### Producing binary distributions



It is possible to produce a mostly functional binary distribution of the built stage2 compiler using `make binary-dist`. Note, however, that the distribution will not include utilities typically built by the stage2 compiler (as this must run on the target).


## Using `cabal`



Extra packages can be installed using `cabal` with your cross-compiler.  The recipe is:


```wiki
$ cabal --with-ghc=<cross-ghc> --with-ld=<ld> ...
```


You can do this even without installing your cross-compiler, just use `$TOP/inplace/bin/ghc-stage1` as `<cross-ghc>`.



NB. you should ensure that the cross-compiled packages won't conflict with your native packages.  If the version of your `cross-ghc` is dated, such as 7.7.20130116, then that may be enough to avoid a conflict.



Another way to do this is to modify your `$HOME/.cabal/config` to include the platform in the installation directory for packages:


```wiki
install-dirs user
  libsubdir: $arch-$os/$pkgid/$compiler
```

## CPU/platform specific notes


### ARM



Only LLVM versions == 3.0 and \>= 3.2 support GHC for ARM targets. There was a regression in LLVM version 3.1, the result of which is bad generated code that crashes.


### iOS



See the [Building a GHC cross-compiler for Apple iOS targets](building/cross-compiling/i-os) page, but also take note of the ARM-specific notes above.


## Troubleshooting


### `#error WORD_SIZE_IN_BITS != GMP_LIMB_BITS not supported`



If you see,


```wiki
libraries/integer-gmp/cbits/wrappers.c:41:3: error: #error WORD_SIZE_IN_BITS != GMP_LIMB_BITS not supported
```


you are likely trying to compile GHC against a `libgmp` compiled for the wrong platform. This may be caused by a number of things, but the following may be helpful in tracking down the issue:


- `WORD_SIZE_IN_BITS` is defined as `SIZEOF_VOID_P` in `includes/ghcautoconf.h` (which is produced by `./configure`)
- `GMP_LIMB_BITS` is defined by `libraries/integer-gmp/gmp/include/gmp/gmp-impl.h` (if you are compiling with an in-tree GMP).
