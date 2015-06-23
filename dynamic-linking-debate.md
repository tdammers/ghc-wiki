# The Dynamic Linking Debate



In GHC 7.8 we [made](dynamic-ghc-programs) GHCi dynamically-linked.  That is, it links against compiled Haskell packages (it already used dynamically-linked C libraries, that is not the issue here).



Why did we do this?  The main reason is that we need a complete linker implementation in the RTS, and that's hard (\~7000 LoC currently, supporting three object file formats).  This is a lot of code to get right, and a lot to maintain.  Supporting all the dark corners of the linking semantics is difficult: e.g. weak symbols were not supported (but have since been implemented).  The linker semantics on OS X in particular change from time to time, and we have to keep up.



The alternative is to use the system dynamic linker.  The following section outlines the implications of the switch.


## Implications of switching to dynamic linking



Dynamic linking implies


- Generating position-independent code (the `-fPIC` flag), so that the code can be loaded at any address in memory
- Generating dynamic references (the `-dynamic` flag) to entities outside of the current shared library


GHC's output is not deterministic, which means that if we compile both a static and a dynamic library, they need distinct sets of interface files.  The mechanism for doing this in GHC is "ways"; so dynamic linking is a "way" just like profiling.  When you compile with `-dynamic`, GHC looks for the dynamic version of all the packages.



We decided to keep static linking as the default for ordinary standalone executables (i.e. not use [DynamicByDefault](dynamic-by-default)), because dynamic linking implies a performance penalty, and there are concerns about backwards compatibility for build systems and suchlike.



To ensure that all the libraries are available both to use in GHCi and to use when static linking, cabal has to build everything twice.  To mitigate the overhead of building everything twice, we implemented the `-dynamic-too` flag, which generates both static and dynamic object files from a single compilation, sharing most of the compilation pipeline and only performing the code-generation steps twice.



GHCi can load object files if some source files are precompiled.  To make this work with dynamic linking, we have to link the object files together into a temporary shared library in `/tmp`, and then load that.  To make it possible for new object files to override old object files, we had to load shared libraries into separate namespaces, which lead to breakage in the old static linker that we still haven't found a good fix for: [\#8935](http://gitlabghc.nibbler/ghc/ghc/issues/8935).



When using TemplateHaskell, we have to be able to load object code into GHCi, which means we need to have dynamic versions of all the object files.  Therefore, we automatically enable `-dynamic-too` when `TemplateHaskell` is on.  This slows down compilation, and is an annoying special case.


## Pros of dynamic linking


- GHCi starts up faster (when the dynamic libs are cached)
- We can get GHCi support on some platforms where we couldn't before, using the LLVM backend + dynamic linking
- Some weird workarounds could go away: the GlobalStore in the RTS and related nonsense in the base package, which is necessary to handle the fact that with static linking we had two copies of the base package loaded.  There is similar stuff in the GHC package for the same reason (see [\#8276](http://gitlabghc.nibbler/ghc/ghc/issues/8276))
- If we could fully replace the static linker, we could remove that code from the RTS (but see "Do we need the static linker anyway?" below).

## Cons of dynamic linking


- `-dynamic-too` is buggy, slow, and has an ugly implementation
- linking shared libs on the fly in GHCi is annoying
- It still doesn't work on Windows, because we can't get the GHC package to fit into a DLL.
- In order to support GHCi loading dynamic object files, we had to disable a nice optimisation in `-dynamic` that made all intra-package calls fast static calls instead of indirect dynamic calls.

## Do we need the static linker anyway?



What happens if we build a static executable and link in the GHC package?  Can we use the interpreter?  Currently the answer is yes, and we fall back to using the RTS linker in this case.  (The converse also applies if static linking is the default).



The dynamic linker doesn't support unloading objects currently, so if you want that functionality then the static linker is the only option.  The Haxl project at Facebook is using the static linker for this reason, and also due to the performance overhead of dynamic linking.  It's not known whether the dynamic linker can support unloading.


## Do we need the dynamic linker anyway?



Dynamic linking is useful: it reduces executable sizes for one thing.  We've supported dynamic linking of standalone executables for a while, just not in GHCi.  Supporting it in GHCi has brought with it a lot of trouble.


## Linking C++ code



One of the stated reasons for switching to dynamic linking was that we could support C++ code much better in GHCi.  I suspect this is a non-issue.


- We support linking shared libraries containing C++ code regardless of whether GHCi is linking statically or dynamically.  This has always worked.
- If you have C++ code in your Haskell package, then it should also work both ways, now that the RTS linker supports running constructor functions ([\#5435](http://gitlabghc.nibbler/ghc/ghc/issues/5435)).

## Bugs in the static linker



Copied from [DynamicGhcPrograms](dynamic-ghc-programs):


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
<tr><th>[\#6107 GHCi runtime linker cannot link with duplicate common symbols](http://gitlabghc.nibbler/ghc/ghc/issues/6107)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**
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
<tr><th>[\#7103 Compiler panic, when loading wxc in GHCi](http://gitlabghc.nibbler/ghc/ghc/issues/7103)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#8935 Obscure linker bug leads to crash in GHCi](http://gitlabghc.nibbler/ghc/ghc/issues/8935)</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>**YES**
</th></tr></table>


## Bugs in dynamic linking



Disclaimer: I just searched for bugs mentioning "dyn", there are probably duplicates and tickets that are easily fixed.  Some of these tickets are specific to dynamic support in GHCi, and some affect dynamic linking in general.


<table><tr><th>Ticket</th>
<th>Affects OS X x86\_64?</th>
<th>Affects OS X x86?</th>
<th>Affects Linux x86\_64?</th>
<th>Affects Linux x86?</th>
<th>Affects Windows x86\_64?</th>
<th>Affects Windows x86?</th>
<th>Affects other platforms?
</th></tr>
<tr><th>[\#4824 Windows: Dynamic linking doesn't work out-of-the-box](http://gitlabghc.nibbler/ghc/ghc/issues/4824)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#5620 Dynamic linking and threading does not work on Windows](http://gitlabghc.nibbler/ghc/ghc/issues/5620)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#5786 Dynanmic way fails when GHC built with LLVM backend](http://gitlabghc.nibbler/ghc/ghc/issues/5786)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#5982 Incorrect dynamic library name in OSX](http://gitlabghc.nibbler/ghc/ghc/issues/5982)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8228 GHC built under Windows does not generate dyn\_hi files](http://gitlabghc.nibbler/ghc/ghc/issues/8228)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#7298 Test 2228 fails with dynamic-by-default](http://gitlabghc.nibbler/ghc/ghc/issues/7298)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#7478 setSessionDynFlags does not always work](http://gitlabghc.nibbler/ghc/ghc/issues/7478)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[dynamicToo001 fails on Windows](http://gitlabghc.nibbler/ghc/ghc/issues/7665)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8024 Dynamic linking not working on PowerPC Linux.](http://gitlabghc.nibbler/ghc/ghc/issues/8024)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8420 Spurious dynamic library dependencies](http://gitlabghc.nibbler/ghc/ghc/issues/8420)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8721 Testsuite not reporting errors for DYN way on OS X](http://gitlabghc.nibbler/ghc/ghc/issues/8721)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8736 GHCi doesn't load .dyn\_o files appropriately](http://gitlabghc.nibbler/ghc/ghc/issues/8736)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#8909 ppc dyn executable compiled with ghc-7.8.1 RC2 segfaults](http://gitlabghc.nibbler/ghc/ghc/issues/8909)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#9121 Presence of dyn\_o files not checked upon recompilation](http://gitlabghc.nibbler/ghc/ghc/issues/9121)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#9176 GHC not generating dyn\_hi files](http://gitlabghc.nibbler/ghc/ghc/issues/9176)
</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>


