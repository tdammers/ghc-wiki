
**We no longer plan to take this approach. See [DynamicGhcPrograms](dynamic-ghc-programs) instead.**



In particular, [
DYNAMIC\_BY\_DEFAULT has likely bitrotted](https://mail.haskell.org/pipermail/glasgow-haskell-users/2014-October/025339.html): it does not seem possible to build with that flag on anymore (tested on Linux on [a48464a7d2858bad28cfd1f393e82589825e62db](/trac/ghc/changeset/a48464a7d2858bad28cfd1f393e82589825e62db/ghc)).  It still seems possible to manually toggle `DYNAMIC_BY_DEFAULT` in `lib/ghc-*/platformConstants`, but this is probably considered an unsupported feature.


# Dynamic by default



Currently, GHCi doesn't use the system linker to load libraries, but instead uses our own "GHCi linker". Unfortunately, this is a large blob of unpleasant code that we need to maintain, it already contains a number of known bugs, and new problem have a tendency to arise as new versions of OSes are released. We are therefore keen to get rid of it!



There is some benefit (in terms of both bugs fixed and code removed) to removing it for a particular platform (e.g. Linux(elf)/x86), more benefit to removing it for a particular OS (e.g. Linux(elf)), but the most benefit is gained by removing it entirely.



Our solution is to switch GHCi from using the "static way", to using the "dynamic way". GHCi will then use the system linker to load the `.dll` for the library, rather than using the GHCi linker to load the `.a`.



(See [\#3658](http://gitlabghc.nibbler/ghc/ghc/issues/3658) for related design decisions etc.)



For this to work, there is technically no need to change anything else: ghc could continue to compile for the static way by default. However, there are 2 problems that arise:


1. cabal-install would need to install libraries not only for the static way (for use by ghc), but also for the dynamic way (for use by ghci). This would double library installation times and disk usage.
1. GHCi would no longer be able to load modules compiled with `ghc -c`. This would violate the principle of least surprise, and would make it harder to work around GHCi's limitations (such as performance, and lack of support for unboxed tuples).


Given these 2 issues, we think that if making GHCi use dynamic libraries, we should also make ghc compile the "dynamic way" by default.


## Current status


### Unix-like platforms



We have everything working, although currently disabled, for all platforms UNIX-like platforms in GHC HEAD. We have tested it on Linux/x86\_64, Linux/x86, OSX/x86\_64, OSX/x86 and Linux/s390(unregisterised). To enable it, set `DYNAMIC_BY_DEFAULT = YES` in `mk/build.mk` or `mk/validate.mk` as appropriate.


### Windows



Currently, we don't know how to do dynamic-by-default on Windows in a satisfactory way. We can build dynamic libraries, but we don't have a way of telling them where to find their DLLs.



We are not currently working on this, but if anyone is interested in rolling up their sleeves then we would be very grateful! We have some [more details](dynamic-by-default/windows) on the problem, and how it might be solvable.


### Other platforms



We do not know the situation with other platforms, such as iOS and Android. We do not know whether they have a system loader that they can use, or whether it would be useful to keep the GHCi linker around for them.


## Bugs



As well as the [ticket for implementing dynamic-by-default (\#3658)](http://gitlabghc.nibbler/ghc/ghc/issues/3658), the table below lists the related tickets and the platforms that they affect. Most, if not all, of these would be immediately fixed by switching to dynamic-by-default.


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


## Performance



There are some performance questions to consider before making a decision.


### Performance of the dynamic way



Full nofib results showing the effect of switching to dynamic-by-default are available for 
[
OS X x86\_64](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86_64.html),
[
OS X x86](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86.html),
[
Linux x86\_64](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-linux-x86_64.html) and
[
Linux x86](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-linux-x86.html). There is also a table of the highlights below. In summary:



(We don't have Windows performance numbers as we don't have dynamic-by-default working on Windows yet).



Binary sizes are way down across the board, as we are now dynamically linking to the libraries.



Things are rosiest on OS X x86\_64. On this platform, `-fPIC` is always on, so using dynamic libraries doesn't mean giving up a register for PIC. Overall, performance is a few percent *better* with dynamic by default.



On OS X x86, the situation is not so nice. On x86 we are very short on registers, and giving up another for PIC means we end up around 15% down on performance.



On Linux x86\_64 we have more registers, so the effect of giving one up for PIC isn't so pronounced, but we still lose a few percent performance overall.



For unknown reasons, x86 Linux suffers even worse than x86 OS X, with around a 30% performance penalty.


<table><tr><th></th>
<td>
    </td>
<th>static -\> dynamic
on OS X x86\_64</th>
<td>
    </td>
<th>static -\> dynamic
on OS X x86</th>
<td>
    </td>
<th>static -\> dynamic
on Linux x86\_64</th>
<td>
    </td>
<th>static -\> dynamic
on Linux x86</th>
<td>
</td></tr>
<tr><th>Binary Sizes</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-95.8%</th>
<th>-95.8%</th>
<th>-95.8%</th>
<th>-95.9%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>-93.1%</th>
<th>-92.8%</th>
<th>-92.6%</th>
<th>-92.4%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>-94.6%</th>
<th>-94.5%</th>
<th>-94.5%</th>
<th>-94.4%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Run Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.2%</th>
<th>+11.7%</th>
<th>-2.5%</th>
<th>+16.6%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>+1.6%</th>
<th>+20.0%</th>
<th>+9.6%</th>
<th>+40.3%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>+0.2%</th>
<th>+15.8%</th>
<th>+3.3%</th>
<th>+27.9%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-6.9%</th>
<th>+10.3%</th>
<th>-2.5%</th>
<th>+16.6%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>-0.3%</th>
<th>+20.4%</th>
<th>+9.6%</th>
<th>+40.3%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>-3.7%</th>
<th>+15.2%</th>
<th>+3.3%</th>
<th>+27.9%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Mutator Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.3%</th>
<th>+8.9%</th>
<th>-5.0%</th>
<th>+18.3%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>+1.9%</th>
<th>+18.3%</th>
<th>+7.5%</th>
<th>+46.8%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>+0.3%</th>
<th>+13.5%</th>
<th>+1.1%</th>
<th>+31.8%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Mutator Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-4.5%</th>
<th>+7.7%</th>
<th>-5.0%</th>
<th>+18.3%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>+0.3%</th>
<th>+18.8%</th>
<th>+7.5%</th>
<th>+46.8%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>-2.1%</th>
<th>+13.1%</th>
<th>+1.1%</th>
<th>+31.8%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>GC Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.4%</th>
<th>+16.3%</th>
<th>+5.6%</th>
<th>+13.4%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>+1.8%</th>
<th>+27.1%</th>
<th>+11.2%</th>
<th>+24.0%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>+0.2%</th>
<th>+21.6%</th>
<th>+8.4%</th>
<th>+18.6%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>GC Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.5%</th>
<th>+15.8%</th>
<th>+5.6%</th>
<th>+13.4%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>+1.3%</th>
<th>+25.6%</th>
<th>+11.2%</th>
<th>+24.0%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>-0.1%</th>
<th>+20.6%</th>
<th>+8.4%</th>
<th>+18.6%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Compile Times</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-11.7%</th>
<th>+6.2%</th>
<th>-1.8%</th>
<th>+27.0%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>+1 s.d.</th>
<th>-0.5%</th>
<th>+18.2%</th>
<th>+7.8%</th>
<th>+37.8%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Average</th>
<th>-6.3%</th>
<th>+12.1%</th>
<th>+2.9%</th>
<th>+32.3%</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>


### OS X x86 vs x86\_64



Currently, some people use the x86 version of GHC on OS X for performance reasons. It's not clear for how much longer this will be viable, as other OS X libraries start dropping x86 support.



Full nofib results comparing the two are
[
here for static by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-base.html),
[
here for dynamic by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-dyn.html), and
[
here for comparing static x86 to dynamic x86\_64](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-dyn.html).
The highlights are in the table below.



The left-hand column shows the status quo: x86\_64 only beats x86 in mutator time, and that is a shallow victory as the higher GC time means that total runtime is worse for x86\_64.



The middle column shows what the situation would be if we switch to dynamic instead. Allocations, memory use etc remain higher due to all word-sized things being twice as big. However, the combination of x86\_64's performance improving, and x86's performance getting worse, means that x86\_64 is now faster overall.



The right-hand column shows the difference between static x86 and dynamic x86\_64.


<table><tr><th></th>
<th>x86 -\> x86\_64
when static by default</th>
<th>x86 -\> x86\_64
when dynamic by default</th>
<th>x86 static -\> x86\_64 dynamic</th></tr>
<tr><th>Binary Sizes</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+38.0%</th>
<th>+7.4%</th>
<th>-95.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+38.6%</th>
<th>+30.6%</th>
<th>-92.0%</th></tr>
<tr><th>Average</th>
<th>+38.3%</th>
<th>+18.5%</th>
<th>-94.3%</th></tr>
<tr><th>Allocations</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+63.2%</th>
<th>+63.2%</th>
<th>+63.2%</th></tr>
<tr><th>+1 s.d.</th>
<th>+114.4%</th>
<th>+114.4%</th>
<th>+114.4%</th></tr>
<tr><th>Average</th>
<th>+87.0%</th>
<th>+87.0%</th>
<th>+87.0%</th></tr>
<tr><th>Run Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-23.5%</th>
<th>-31.6%</th>
<th>-23.6%</th></tr>
<tr><th>+1 s.d.</th>
<th>+36.1%</th>
<th>+14.7%</th>
<th>+37.0%</th></tr>
<tr><th>Average</th>
<th>+2.1%</th>
<th>-11.4%</th>
<th>+2.3%</th></tr>
<tr><th>Elapsed Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-18.2%</th>
<th>-30.0%</th>
<th>-22.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+40.1%</th>
<th>+17.0%</th>
<th>+38.3%</th></tr>
<tr><th>Average</th>
<th>+7.0%</th>
<th>-9.5%</th>
<th>+3.3%</th></tr>
<tr><th>Mutator Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-32.4%</th>
<th>-38.8%</th>
<th>-32.4%</th></tr>
<tr><th>+1 s.d.</th>
<th>+20.1%</th>
<th>+3.0%</th>
<th>+20.7%</th></tr>
<tr><th>Average</th>
<th>-9.9%</th>
<th>-20.6%</th>
<th>-9.7%</th></tr>
<tr><th>Mutator Elapsed Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-28.7%</th>
<th>-37.9%</th>
<th>-32.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>+22.5%</th>
<th>+4.4%</th>
<th>+21.3%</th></tr>
<tr><th>Average</th>
<th>-6.6%</th>
<th>-19.5%</th>
<th>-9.2%</th></tr>
<tr><th>GC Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+4.5%</th>
<th>-11.9%</th>
<th>+4.1%</th></tr>
<tr><th>+1 s.d.</th>
<th>+74.8%</th>
<th>+54.1%</th>
<th>+76.3%</th></tr>
<tr><th>Average</th>
<th>+35.2%</th>
<th>+16.5%</th>
<th>+35.5%</th></tr>
<tr><th>GC Elapsed Time</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+7.9%</th>
<th>-8.0%</th>
<th>+7.1%</th></tr>
<tr><th>+1 s.d.</th>
<th>+75.1%</th>
<th>+56.7%</th>
<th>+76.0%</th></tr>
<tr><th>Average</th>
<th>+37.4%</th>
<th>+20.0%</th>
<th>+37.3%</th></tr>
<tr><th>Total Memory in use</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.7%</th>
<th>-1.9%</th>
<th>-1.8%</th></tr>
<tr><th>+1 s.d.</th>
<th>+88.9%</th>
<th>+88.9%</th>
<th>+88.9%</th></tr>
<tr><th>Average</th>
<th>+36.3%</th>
<th>+36.1%</th>
<th>+36.2%</th></tr>
<tr><th>Compile Times</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+11.9%</th>
<th>-8.9%</th>
<th>+2.5%</th></tr>
<tr><th>+1 s.d.</th>
<th>+21.1%</th>
<th>+2.9%</th>
<th>+17.2%</th></tr>
<tr><th>Average</th>
<th>+16.4%</th>
<th>-3.1%</th>
<th>+9.6%</th></tr></table>


### Implications of the performance difference



If GHCi uses dynamic libraries by default, then `ghci` will need to be dynamically linked. It would make sense to therefore also have `ghc` be dynamically linked. This means that any performance difference will also affect the performance of the compiler (this is already accounted for in the "Compile Times" in the nofib results).



It would still be possible to compile programs using the "static way" by giving ghc the `-static` flag, and users would be able to configure `cabal-install` to do so by default if they wish. Then programs would be exactly the same as they are today. However, this would have the drawback that `cabal-install` would need to be configured to install libraries for the static way as well as the dynamic way, so library installation would take twice as long.


## Other issues


### Cabal support



Currently released versions of Cabal/cabal-install don't handle dynamic-by-default GHCs well, as they don't pass the `-static` flag when building for static ways (as they assume that it is enabled by default). We should get fixed versions out as soon as possible ([\#7439](http://gitlabghc.nibbler/ghc/ghc/issues/7439)).


### Profiling



Should we support both static and dynamic profiling ways? If not, which?



If we support both, it would be a little odd if `ghc -prof` used the static profiling libraries. Presumably `ghc -prof -dynamic` would use dynamic profiling libraries, but what about `ghc -dynamic -prof`? So if we support both then you would presumably need to say `ghc -static -prof` to use the static ones.



Currently Cabal has separate `--enable-library-profiling` and `--enable-shared` flags, but we don't have a way to distinguish static-profiling from dynamic-profiling. If we want to support both then we'll need to add a Cabal flag for it.


## Questions



In summary, we need to answer the following questions:


1. Should we enable dynamic by default on OS X x86\_64?
1. Should we enable dynamic by default on OS X x86?
1. Should we enable dynamic by default on Linux x86\_64?
1. Should we enable dynamic by default on Linux x86?
1. Should we enable dynamic by default on Windows x86\_64?
1. Should we enable dynamic by default on Windows x86?
1. Should we enable dynamic by default on other platforms?
1. For platforms using dynamic by default, should Cabal also install static libraries by default?
1. Should `ghc -prof` use dynamic or static libraries when dynamic by default?


For 1 and 3, the performance impact appears negligible (or perhaps even negative) and some bugs will be fixed, so we would suggest that the answer should be yes.



For 2 and 4, there would be a considerable performance impact, but there is again a negligible impact if you instead switch to using x86\_64. We believe that this would be feasible for the vast majority of users for whom performance is a concern, and it would greatly simplify the code base, so again we would suggest that the answer should be yes.



For 5 and 6, we will first have to get it working. Windows already uses different code paths quite a lot, so even if we end up deciding not to go dynamic-by-default on Windows, a lot of the ugly, buggy code will be removed. However, there are some known bugs on Windows, and we would be able to remove more code if we switched all platforms, so we are hopeful that we will be able to do dynamic-by-default here too.



For 7, this makes the difference between being able to use ghci and not being able to use ghci, and performance is already compromised for unregisterised platforms. Therefore this looks like a definite yes.



For 8, this is a trade-off between the convenience of always having static libraries available (which may be important for people for whom performance is critical), and doubling the time needed to install extra libraries. On balance, we'd suggest that the answer should be no. If we go for yes, then we'd probably want a little extra intelligence in cabal, which checks that e.g. there is a static version of base installed before trying to install the static way, as development compilers in particular may only be built with the dynamic libraries available.



For 9, we'd suggest that `-prof` should use dynamic libraries, but it should also still be possible to use static libraries if also using the `-static` flag.


