# Notes on Porting GHC to Barrelfish



This page contains a braindump of issues that came up when porting GHC to the [
Barrelfish OS](http://barrelfish.org).


## Approach



Since Barrelfish is an research operating system and does not support the tools required by GHC to compile Haskell programs, the approach taken was to build a stage1 cross-compiler, that runs on Linux but builds binaries for Barrelfish.  



The main changes required were:
 


- To create a new OS specific subdirectory, called barrelfish, in the RTS sources alongside those for Win32 and posix.  This contains the OS specific functions for dealing with aspects like signals, time, memory, threads, etc.  At present most of these functions are stubs in the barrelfish port, but OSMem and OSThread have been fleshed out.
- To make sure all files are compiled and linked with the correct flags (i.e., the includes point to the barrelfish headers, not the linux ones, and the application is linked with Barrelfish libraries, not the linux ones).
- To \#ifdef out code which not approriate on Barrelfish - the majority of this is in the base and unix libraries - the RTS actually seems more portable than these libraries to me.

## Issues



One of the main issues was that package dependencies made it very difficult to remove the **unix** package from the build process.  This package was require for the bootstrap stage 0 (i.e., for **Cabal**), however, I did not want to build it as part of the stage1 libraries because Barrelfish does not support the full Posix API require by this package.  If I removed it from the root level ghc.mk, then the bootstrap stage would not build due to dependency issues.  In the end it became easier just to leave the **unix** package in, but \#ifdef out the parts which would cause problems in Barrelfish.  What is really required is some way to specify which packages should be built at different stages.



There was no easy way to globally specify different default compiler or linker flags for different stages.  I needed this since I wanted the normal flags for the bootstrap stage 0 build (so ghc-stage1 can execute on Linux), however I needed the stage 1 compiler to use a different set of flags in order to build binaries for Barrelfish.  I added STAGE\_X\_CC\_FLAGS and STAGE\_X\_LD\_FLAGS variables to the build system to support this.



The build system does not propagate the flags and arguments given to the root level configure script to the libraries configure scripts.  I changed the build system to do this.



The configure scripts required some tweeking to support cross compiling.


## Status



Currently the modified ghc-stage1 compiler can build a helloworld Haskell program that will run in Barrelfish.  I am currently looking working on getting the threaded build of the runtime system to run properly so that we can run proper parallel benchmarks.


## Notes



I currently configure ghc with the following command (Yikes, I know...):


```wiki
./configure --build=x86_64-unknown-linux --host=x86_64-unknown-barrelfish --target=x86_64-unknown-barrelfish 
--x-includes=/home/rmcilroy/barrelfish.ghc/include/ --x-libraries=/home/rmcilroy/barrelfish.ghc/build/x86_64/lib/
CFLAGS="-fno-builtin -nostdinc -m64 -mno-red-zone -fPIE -fno-stack-protector -U__linux__ 
-imacros ${BF_ROOT}/include/deputy/nodeputy.h -DBARRELFISH -DMAX_CPUS=64 -DCONFIG_INTERCONNECT_DRIVER_LMP 
-DCONFIG_INTERCONNECT_DRIVER_UMP -DCONFIG_FLOUNDER_BACKEND_LMP -DCONFIG_FLOUNDER_BACKEND_UMP -g  
-I${BF_ROOT}/include -I${BF_ROOT}/include/arch/x86_64 -I${BF_BUILD_DIR}/include " CPPFLAGS="-fno-builtin -nostdinc 
-m64 -mno-red-zone -fPIE -fno-stack-protector -U__linux__ -imacros ${BF_ROOT}/include/deputy/nodeputy.h 
-DBARRELFISH -DMAX_CPUS=64 -DCONFIG_INTERCONNECT_DRIVER_LMP -DCONFIG_INTERCONNECT_DRIVER_UMP 
-DCONFIG_FLOUNDER_BACKEND_LMP -DCONFIG_FLOUNDER_BACKEND_UMP -g  -I${BF_ROOT}/include 
-I${BF_ROOT}/include/arch/x86_64 -I${BF_BUILD_DIR}/include " LDFLAGS="${BF_BUILD_DIR}/lib/crt0.o  
${BF_BUILD_DIR}/errors/errno.o -Wl,-section-start,.data.rel.ro=0x800000 -Wl,-z,max-page-size=0x1000
-fno-builtin -nostdlib -m64 -L${BF_BUILD_DIR}/lib/ -Wl,--start-group -lmsun -lposixcompat -lvfs 
-lnfs -llwip -ltimer -lbarrelfish -lc"
```


I uses the following settings in the build.mk file:


```wiki
# Fast build with barrelfish support
BuildFlavour = bf

# -------- A Fast barrelfish build with optimised libs ----------------------------------

ifeq "$(BuildFlavour)" "bf"

# TODO - Not hard code these...
BF_ROOT            = ${HOME}/barrelfish.ghc/
BF_BUILD_DIR       = ${BF_ROOT}/build/x86_64/

STAGE_1_CC_OPTS    = -fno-builtin -nostdinc -m64 -mno-red-zone -fPIE -fno-stack-protector -U__linux__ -imacros
${BF_ROOT}/include/deputy/nodeputy.h -DBARRELFISH -DMAX_CPUS=64 -DCONFIG_INTERCONNECT_DRIVER_LMP 
-DCONFIG_INTERCONNECT_DRIVER_UMP -DCONFIG_FLOUNDER_BACKEND_LMP -DCONFIG_FLOUNDER_BACKEND_UMP -g  -I${BF_ROOT}/include 
-I${BF_ROOT}/include/arch/x86_64 -I${BF_BUILD_DIR}/include
STAGE_2_CC_OPTS    = ${STAGE_1_CC_OPTS}

STAGE_1_LD_OPTS    = -static ${BF_BUILD_DIR}/lib/crt0.o  ${BF_BUILD_DIR}/errors/errno.o 
-Wl,-section-start,.data.rel.ro=0x800000 -Wl,-z,max-page-size=0x1000 -fno-builtin -nostdlib -m64 
-L${BF_BUILD_DIR}/lib/ -Wl,--start-group -lmsun -lposixcompat -lvfs -lnfs -llwip -ltimer -lbarrelfish -lc
#endif 
STAGE_2_LD_OPTS    = ${STAGE_1_LD_OPTS}

LIBFFI_LD_OPTS     = -static -Wl,-section-start,.data.rel.ro=0x800000 -Wl,-z,max-page-size=0x1000 -fno-builtin
-nostdlib -m64 -L${BF_BUILD_DIR}/lib/ -lbarrelfish -lc

BUILTIN_HC_OPTS    = $(addprefix -optc, $(STAGE_1_CC_OPTS)) $(addprefix -optl, $(STAGE_1_LD_OPTS)) -opta-m64

SRC_HC_OPTS        = -H64m -O0 -fasm
GhcStage1HcOpts    = -O -fasm
GhcLibHcOpts       = -O -fasm
GhcLibWays         = v
GhcRTSWays         = v thr
SplitObjs          = NO
HADDOCK_DOCS       = NO
BUILD_DOCBOOK_HTML = NO
BUILD_DOCBOOK_PS   = NO
BUILD_DOCBOOK_PDF  = NO
INTEGER_LIBRARY    = integer-simple

endif
```