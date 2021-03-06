# Raspberry Pi



To build a cross-compiler from Linux to Raspberry Pi (running Raspbian), first grab the cross-compilation toolset:


```wiki
git clone https://github.com/raspberrypi/tools.git
```


Let's say this created a directory `$tools`.  Now add the tools to your path:


```wiki
PATH=$PATH:$tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin
```


Follow the instructions in [Building/CrossCompiling](building/cross-compiling).  A few more RPi specific tips:



To configure, use


```wiki
./configure --target=arm-linux-gnueabihf --enable-unregisterised
```


You can also build registerised: leave out the `--enable-unregisterised` option, but then you **must** install a suitable LLVM (see below).



You'll need to use `integer-simple`, because the cross-compilation environment doesn't include GMP (see [Building/CrossCompiling](building/cross-compiling)).



The build should go successfully all the way to stage 2.  You can then use the stage 1 compiler on the host as a cross-compiler.


## Using LLVM



Note that LLVM 2.9 does not work for registerised code generation on ARM as it does not support GHC calling convention for ARM platform. This was added into LLVM 3.0 release but due to management issue was missing in LLVM 3.1
and later merged for inclusion in LLVM 3.2. So please use either LLVM 3.0 or LLVM 3.2 when cross-compiling to ARM. This also applies if you are doing native compilation on ARM/Linux system.


- When **unregisterised**, the C backend will be used by default, but you can optionally use LLVM (see below).  Code generated using LLVM is compatible with code generated using the C backend.
- When **registerised**, LLVM is the only backend that supports registerised compilation on ARM, so it will be used automatically.  You don't have to do anything except ensure that a suitable version of LLVM is installed.


To use LLVM when unregisterised, add these to your `mk/build.mk`:


```wiki
GhcLibHcOpts       = -O -fllvm
GhcRtsHcOpts      += -fllvm
```