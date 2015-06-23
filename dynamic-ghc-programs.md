
**This is implemented, and partially working. See [DynamicLinkingDebate](dynamic-linking-debate) for further contemplation.** See also [DynamicByDefault](dynamic-by-default) for a more far-reaching approach that we didn't go for in the end.


# Dynamic GHC programs



Currently, GHCi doesn't use the system linker to load libraries, but instead uses our own "GHCi linker". Unfortunately, this is a large blob of unpleasant code that we need to maintain, it already contains a number of known bugs, and new problems have a tendency to arise as new versions of OSes are released. We are therefore keen to get rid of it!



There is some benefit (in terms of both bugs fixed and code removed) to removing it for a particular platform (e.g. Linux(elf)/x86), more benefit to removing it for a particular OS (e.g. Linux(elf)), but the most benefit is gained by removing it entirely.



Our solution is to switch GHCi from using the "static way", to using the "dynamic way". GHCi will then use the system linker to load the `.dll` for the library, rather than using the GHCi linker to load the `.a`.



This is controlled by the `DYNAMIC_GHC_PROGRAMS` build system variable. If it is `YES`, then we build ghci the dynamic way. If it is `NO` then we build it the static way.


## Build times, and `-dynamic-too`



The default way, as used by `ghc Foo.hs` for example, will remain the vanilla (static) way. But GHCi uses dynamic libraries, so we need to build all the libraries both ways (both in the GHC build, and when the user `cabal-install`s 3rd party libraries). This would double the compilation time needed.



We have therefore added a `-dynamic-too` flag. This allows a single invocation of GHC to build both the vanilla and dynamic ways. The GHC build system uses this if `DYNAMIC_TOO` is `YES`, and Cabal HEAD uses it if the compiler supports it.


## Current status


### Unix-like platforms



Both `DYNAMIC_GHC_PROGRAMS` and `DYNAMIC_TOO` work on unix-like platforms. They are enabled by default provided the platform supports dynamic libraries.


### Windows



On Windows, `DYNAMIC_GHC_PROGRAMS` works, but `DYNAMIC_TOO` doesn't. Currently, `DYNAMIC_GHC_PROGRAMS` is disabled, although it could be enabled at the expense of longer compilation times. Linking time is also significantly higher for the dynamic way, but we aren't aware of any way to improve that.


### Other platforms



We do not know the situation with other platforms, such as iOS and Android. We do not know whether they have a system loader that they can use, or whether it would be useful to keep the GHCi linker around for them.


## Bugs



As well as the [ticket for implementing dynamic GHCi (\#3658)](http://gitlabghc.nibbler/ghc/ghc/issues/3658), the table below lists the related tickets and the platforms that they affect. Most, if not all, of these would be immediately fixed by switching to dynamic GHCi.


<table><tr><th>Ticket</th>
<th>Affects OS X x86\_64?</th>
<th>Affects OS X x86?</th>
<th>Affects Linux x86\_64?</th>
<th>Affects Linux x86?</th>
<th>Affects Windows x86\_64?</th>
<th>Affects Windows x86?</th>
<th>Affects other platforms?
</th></tr>
<tr><th>[\#781 GHCi on x86\_64, cannot link to static data in shared libs](http://gitlabghc.nibbler/ghc/ghc/issues/781)</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#1883 GHC can't find library using "short" name](http://gitlabghc.nibbler/ghc/ghc/issues/1883)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#2283 WIndows: loading objects that refer to DLL symbols](http://gitlabghc.nibbler/ghc/ghc/issues/2283)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#3242 ghci: can't load .so/.DLL for: m (addDLL: could not load DLL)](http://gitlabghc.nibbler/ghc/ghc/issues/3242)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#3654 Mach-O GHCi linker lacks support for a range of relocation entries](http://gitlabghc.nibbler/ghc/ghc/issues/3654)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#4244 Use system linker in GHCi to support alpha, ia64, ppc64](http://gitlabghc.nibbler/ghc/ghc/issues/4244)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**
</th></tr>
<tr><th>[\#5062 Patch: Debug output for OS X linker and coding standard upgrades](http://gitlabghc.nibbler/ghc/ghc/issues/5062)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#5197 Support static linker semantics for archives and weak symbols](http://gitlabghc.nibbler/ghc/ghc/issues/5197)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**
</th></tr>
<tr><th>[\#5435 GHCi linker should run constructors for linked libraries](http://gitlabghc.nibbler/ghc/ghc/issues/5435)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**
</th></tr>
<tr><th>[\#6107 GHCi runtime linker cannot link with duplicate common symbols](http://gitlabghc.nibbler/ghc/ghc/issues/6107)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**
</th></tr>
<tr><th>[\#7043 32-bit GHC ceiling of negative float SEGFAULT: 11](http://gitlabghc.nibbler/ghc/ghc/issues/7043)</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7056 GHCi loadArchive "libiconv.a":failed Unknown PEi386 section name \`.drectve'](http://gitlabghc.nibbler/ghc/ghc/issues/7056)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7072 GHC interpreter does not find stat64 symbol on Linux](http://gitlabghc.nibbler/ghc/ghc/issues/7072)</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7097 linker fails to load package with binding to foreign library](http://gitlabghc.nibbler/ghc/ghc/issues/7097)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7103 Compiler panic, when loading wxc in GHCi](http://gitlabghc.nibbler/ghc/ghc/issues/7103)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7134 ghc-7.6.0.20120810-x86\_64-windows.exe -\> internal error R\_X86\_64\_PC32](http://gitlabghc.nibbler/ghc/ghc/issues/7134)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7207 linker fails to load package with binding to foreign library (win64)](http://gitlabghc.nibbler/ghc/ghc/issues/7207)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7299 threadDelay broken in ghci, Mac OS X](http://gitlabghc.nibbler/ghc/ghc/issues/7299)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7357 GHC.exe gives an internal error while linking vector's Monadic.hs](http://gitlabghc.nibbler/ghc/ghc/issues/7357)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr></table>


## Other issues



[\#4824](http://gitlabghc.nibbler/ghc/ghc/issues/4824), [\#5289](http://gitlabghc.nibbler/ghc/ghc/issues/5289), [\#5291](http://gitlabghc.nibbler/ghc/ghc/issues/5291), [\#5620](http://gitlabghc.nibbler/ghc/ghc/issues/5620)


### Cabal support



Currently released versions of Cabal/cabal-install don't handle dynamic GHCi well. In particular, if a library uses Template Haskell, then Cabal will build the ways in the wrong order, so compilation will fail. Cabal in HEAD handles it properly.


### Other approaches



In [\#3658](http://gitlabghc.nibbler/ghc/ghc/issues/3658) there is some discussion of related design decisions etc.


