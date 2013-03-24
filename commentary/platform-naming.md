# Platforms



Please read [this wiki page](cross-compilation) on cross compilation for a better understanding of the situation here. There are three platforms of interest to GHC when compiling and running:
 


- The **Build** platform. This is the platform on which we are building GHC.
- The **Host** platform. This is the platform on which we are going to run this GHC binary, and associated tools.
- The **Target** platform. This is the platform for which this GHC binary will generate code.

## Limitations



At the moment, there is limited support for having different values for build, host, and target. Please refer to the [cross compilation](cross-compilation) page for more details. In particular:



The build platform is currently always the same as the host platform. The build process needs to use some of the tools in the source tree, for example ghc-pkg and hsc2hs. 



If the target platform differs from the host platform, then this is generally for the purpose of building .hc files from Haskell source for porting GHC to the target platform. Full cross-compilation isn't supported (yet). 


## Macros



In the compiler's source code, you may make use of the following CPP symbols:


- *xxx*`_TARGET_ARCH`
- *xxx*`_TARGET_VENDOR`
- *xxx*`_TARGET_OS`
- *xxx*`_HOST_ARCH`
- *xxx*`_HOST_VENDOR`
- *xxx*`_HOST_OS`


where *xxx* is the appropriate value: eg. `i386_TARGET_ARCH`. However **GHC is moving away from using CPP for this purpose** in many cases due to the problems it creates with supporting cross compilation.



So instead of it the new plan is to always build GHC as a cross compiler and select the appropriate values and backend code generator to run and runtime. For this purpose there is the Platform module ([compiler/utils/Platform.hs](/trac/ghc/browser/ghc/compiler/utils/Platform.hs)). That contains various methods for querying the DynFlags ([compiler/main/DynFlags.hs](/trac/ghc/browser/ghc/compiler/main/DynFlags.hs)) value for what platform GHC is currently compiling for. You should use these when appropriate over the CPP methods.


