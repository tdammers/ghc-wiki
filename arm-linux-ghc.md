# Revival



Everything from here to the "Original Page" heading is for the revival in July 2008.


## GHC 6.8 for N810



I have an unregisterised GHC 6.8 build for the N810.



You can get the N810 GHC packages at: [
ftp://ripplelabs.com/pub/ghc-debs/](ftp://ripplelabs.com/pub/ghc-debs/)



Some other packages are required that are available elsewhere, such as gcc.



I have not tried them in about a year, so I'm not sure if they run on the latest SDK or OS. They may not work at all - I'm not sure I uploaded all the right packages. Let me know if you have problems.



It's not really practical to run GHC on the N810 because of memory limitations, but you can compile small programs. It could be used with the SDK to compile Haskell programs for the N810. Also, GHC + libs won't fit in the program area - I moved all the libs to external flash and symlinked them to get GHC running on the device.



If I get time, I might use GHC 6.8 to bootstrap 6.10.



-- Dustin DeWeese



(You can contact me at *firstname* dot *lastname* at gmail dot com)


## jhc Success



I heard that John Meacham's jhc compiler produces portable C code, so I grabbed the jhc-0.5.20080307 tarball, built it, wrote a quick Hello, World! test, and built that with jhc. It produced an x86 binary, but also the C code which included the correct gcc command line at the top. Logging into the Nokia scratchbox development environment, I ran the command. It built successfully, and ran on the scratchbox environment. It runs on the device! So we have a working H98 cross-compiler, effectively. This is a long way better than nothing.



jhc isn't self-hosting, and requires gcc anyway, so it can't run on the device itself.



It's good to finally post some positive progress.


## Attempt with Hugs



Since we have jhc to provide cross-compiled binaries, we search for a way to run Haskell code natively on the device. Hugs is written in C, portable, and has been successfully ported to the Nintendo DS. That platform is both ARM-based and far more limited and special-purpose than most.



Building the Hugs source inside the scratchbox environment seems to go fine, but then on the device it fails to start, saying:


```wiki
Prelude not found on current path: ".:{Home}/lib/hugs/packages/*:/usr/local/lib/hugs/packages/*:{Hugs}/packages/*"

FATAL ERROR: Unable to load prelude
```


The path /usr/local/lib/hugs/packages is in fact where the Prelude lives, however, exactly where it should be (as far as I know), in /usr/local/lib/hugs/packages/hugsbase/Hugs/Prelude.hs



Anyone know what could be wrong here?


## New Update on [\#1346](http://gitlabghc.nibbler/ghc/ghc/issues/1346)



[\#1346](http://gitlabghc.nibbler/ghc/ghc/issues/1346), the bug tracking having bootstrapping working again, has had its milestone changed from 6.10.1 to 6.12, as the build system overhaul has grown into too large of a project and 6.10 needs to be released before it will be completed.



And so that basically takes renewed bootstrapping off the table. This project is back to starting from an unregisterised 6.6, working up to an unregisterised 6.10, then a registerised 6.10, and then (assuming the code generator back-end overhaul that is planned for 6.10 still goes through. I don't know the status of it. Anyone have the Trac number for it?) in the long term to a full native codegen 6.10.


## Hackathon?



A Hackathon in the San Diego area for sometime in 2008 was suggested. Thoughts?


## Owned Devices



All developers, what ARM-based devices do you have available for testing?


- shepheb has a Nokia N810.
- kazuya has a Nokia N810.

## Unregisterised 6.6.1 Notes


### Another False Start



I had to install 6.6.1 on my system to compile 6.6.1, first off. Then I apparent need an older gcc (have 4.3.0). Rather than dive deeper into that (it would require an old gcc on the scratchbox dev environment too, too much pain when I have an alternative), I'm just going to try to build an unregisterised 6.8.2 with the patch posted in [\#1346](http://gitlabghc.nibbler/ghc/ghc/issues/1346). The results for that build attempt follow, after the info for the failed 6.6.1 one.


### Original 6.6.1 Build Intro



Building using the 6.6.1 released source. Note that I already have libgmp and libffi installed on both machines (though libffi isn't being used).


### Target-side Prep



Flawless! (Again, I had libgmp installed this time).


### Host-side


#### It complains about Perl 5.10



./configure failed saying my Perl version wouldn't work. perl --version reveals it's 5.10, and I had to edit aclocal.m4, thus:


```wiki
AC_DEFUN([FPTOOLS_CHECK_PERL_VERSION],
[$PerlCmd -v >conftest.out 2>&1
   if grep "v5.6" conftest.out >/dev/null 2>&1; then
      :
   else
      if grep "v5.8" conftest.out >/dev/null 2>&1; then
         :
      else
         if grep "v5.10" conftest.out >/dev/null 2>&1; then
            :
         else
            if grep "version 6" conftest.out >/dev/null 2>&1; then
               :
            else
               AC_MSG_ERROR([your version of perl probably won't work, try up\
grading it.])
            fi
         fi
      fi
   fi
rm -fr conftest*
])
```


making it accept 5.10 too. Then I ran autoreconf, and ./configure ran to completion.


## Third Unregistered Build - GHC 6.8.2



Using the patch posted at [\#1346](http://gitlabghc.nibbler/ghc/ghc/issues/1346), I'm trying to build 6.8.2.


### Target-side


#### utils/pwd/pwd again



Same problem with utils/pwd/pwd as in the first attempt below, with the same solution. After that fix both machines configure successfully.


### Host-side


#### Tripped up at the last again



Got all the way to make hc-file-bundle Project=Ghc, the last host-side step for now, before running into trouble. It couldn't find the file rts/AutoApply\_debug.hc, so I touched it and ran the command again. I have no idea if that will break anything later on, but we shall see.


### Back to the Target


#### Stuck on libraries/ again



I'm manually stepping through distrib/hc-build, and get stuck on


```wiki
make -C libraries boot all GhcBootLibs=YES
```


with the error message


```wiki
> make -C libraries boot all GhcBootLibs=YES
make: Entering directory `/home/braden/ghc/ghc-6.8.2/libraries'
make: *** No rule to make target `base/Makefile', needed by `boot'.  Stop.
make: Leaving directory `/home/braden/ghc/ghc-6.8.2/libraries'
> echo $?
2
```

## First Unregisterised Hack Notes


### Why it doesn't work



On discussion with Igloo (Ian Lynagh) on \#haskell, it turns out that hc-file bootstrapping has been broken in 6.8 since the beginning, meaning that only platforms with a working GHC prior to 6.8 can build a 6.8 or later. This undercuts the porting effort until [\#1346](http://gitlabghc.nibbler/ghc/ghc/issues/1346) is fixed. I may try the "quick hack" diff attached to see if it works out. The comment from the user who posted it seems to imply that it's incomplete, though.


### The original attempt



This documents shepheb's first attempt at a crude, unregisterised build for the Nokia N810 using Maemo 4 Diablo and the Maemo SDK (scratchbox-based)



I'm following the [Building/Porting](building/porting) guide's section on porting to a new platform.


### Target-side Changes


#### utils/pwd/pwd is an x86 binary, and there's no GHC to recompile it



The guide's suggestion to run


```wiki
./configure --enable-hc-boot --enable-hc-boot-unregisterised
```


fails because utils/pwd/pwd is a Haskell binary that I can't run, since this is the ARM-based emulated target system and not the x86 it was compiled for. I can't reach the host's GHC to recompile it, so I just replaced it with the hacky


```wiki
#!/bin/sh
echo /home/braden/ghc/ghc-6.9.20080614
```


and the above ./configure worked fine.


#### libgmp



The 


```wiki
cd includes
make
```


step in the porting guide fails with an error message implying that libgmp headers were missing. Well clearly, they aren't included in the Maemo SDK. I grabbed the GMP sources inside the SDK scratchbox environment, and did


```wiki
./configure
make
make check
make install
```


and all was well. Since make check passed, I have to hope it (cross-)compiled properly and I have a working libgmp now.



After that the includes/ make worked fine. That completes the target-side part for now.


### Host-side Changes


#### utils/pwd/pwd again



utils/pwd/pwd failed again, not sure why this time (I run an i686 Arch Linux and custom kernel, and I'm using 6.8.2. was it built for i386? using 6.9.\*?). Anyway, running


```wiki
cd utils/pwd
ghc -o pwd pwd.hs
```


fixed it, so that's that.


#### Changing build.mk



As the porting guide says, I had to change the TARGETPLATFORM to arm-unknown-linux. The LeadingUnderscore setting can be left at NO, which is the target-side setting.


#### Need libffi



GHC 6.9 now uses libffi,  and so I needed to install it too. I Googled libffi, installed the newest version (it was released in April 2008 so I assume it's old enough to be the one used in 6.9), built and installed it on the host machine.



Its header files are not installed into a system-wide include directory, so I symlinked them into /usr/include. Then the cd compiler && make boot && make step ran fine (though it took long enough running natively on my Core 2 Duo 2.8 GHz that I fear how long the emulated stage 2 will take).


#### make in /rts



This fails saying 


```wiki
ghc-6.9.20080614: could not execute: /home/braden/src/ghc-6.9.20080614/driver/mangler/ghc-asm
```


OLD:
There is no such file, though there is the script driver/mangler/ghc-asm.lprl. I tried making it executable with no results, but I'm not really sure what to do next. Any pointers are greatly appreciated!



Of possible note is that I re-ran ./configure a while back while trying to get /compiler to make boot && make in an effort to make it find libffi. I doubt that would matter, but I'll record it anyway.



UPDATE:
I fixed this one, just run


```wiki
cd driver/mangler
make
```


to build ghc-asm, and then the


```wiki
cd rts && make boot && make
```


run flawlessly.


#### make hc-file-bundle Project=Ghc fails



Cut down at the last! Here's the last part of the output:


```wiki
echo ghc-6.9.20080614/libraries/base/GHC/PrimopWrappers.hs >> hc-files-to-go
echo ghc-6.9.20080614/compiler/parser/Parser.hs >> hc-files-to-go
echo ghc-6.9.20080614/compiler/parser/ParserCore.hs >> hc-files-to-go
echo ghc-6.9.20080614/compiler/main/ParsePkgConf.hs >> hc-files-to-go
echo ghc-6.9.20080614/libraries/haskell-src/Language/Haskell/Parser.hs >> hc-files-to-go
tar czf ghc-6.9.20080614-i386-unknown-linux-hc.tar.gz `cat hc-files-to-go`
tar: ghc-6.9.20080614/rts/AutoApply_thr.hc: Cannot stat: No such file or directory
tar: ghc-6.9.20080614/rts/AutoApply_debug.hc: Cannot stat: No such file or directory
tar: ghc-6.9.20080614/rts/AutoApply_thr_debug.hc: Cannot stat: No such file or directory
tar: ghc-6.9.20080614/rts/AutoApply_thr_p.hc: Cannot stat: No such file or directory
tar: ghc-6.9.20080614/libraries/base/GHC/PrimopWrappers.hs: Cannot stat: No such file or directory
tar: Error exit delayed from previous errors
make: *** [hc-file-bundle] Error 2
```


Those files don't exist, here's an ls -l of rts/Auto\* and libraries/base/GHC/


```wiki
$ ls -l rts/Auto*
-rw-r--r-- 1 braden users  77562 2008-07-03 20:26 rts/AutoApply.cmm
-rw-r--r-- 1 braden users   2634 2008-06-14 13:20 rts/AutoApply.h
-rw-r--r-- 1 braden users 175505 2008-07-03 20:26 rts/AutoApply.hc
-rw-r--r-- 1 braden users  28896 2008-07-03 20:26 rts/AutoApply.o
-rw-r--r-- 1 braden users  77562 2008-07-03 20:26 rts/AutoApply_debug.cmm
-rw-r--r-- 1 braden users  34784 2008-07-03 20:26 rts/AutoApply_debug.debug_o
-rw-r--r-- 1 braden users  77562 2008-07-03 20:26 rts/AutoApply_thr.cmm
-rw-r--r-- 1 braden users 175505 2008-07-03 20:26 rts/AutoApply_thr.thr_hc
-rw-r--r-- 1 braden users  28904 2008-07-03 20:26 rts/AutoApply_thr.thr_o
-rw-r--r-- 1 braden users  77562 2008-07-03 20:27 rts/AutoApply_thr_debug.cmm
-rw-r--r-- 1 braden users  34788 2008-07-03 20:27 rts/AutoApply_thr_debug.thr_debug_o
-rw-r--r-- 1 braden users  77562 2008-07-03 20:27 rts/AutoApply_thr_p.cmm
-rw-r--r-- 1 braden users 180881 2008-07-03 20:27 rts/AutoApply_thr_p.thr_p_hc
-rw-r--r-- 1 braden users  31248 2008-07-03 20:27 rts/AutoApply_thr_p.thr_p_o

$ ls -l libraries/base/GHC/
total 6940
-rw-r--r-- 1 braden users 456468 2008-07-03 20:30 Arr.hc
-rw-r--r-- 1 braden users  27585 2008-06-14 13:31 Arr.lhs
-rw-r--r-- 1 braden users 158095 2008-07-03 20:30 Base.hc
-rw-r--r-- 1 braden users  37851 2008-06-14 13:31 Base.lhs
-rw-r--r-- 1 braden users 174470 2008-07-03 20:32 Conc.hc
-rw-r--r-- 1 braden users  42057 2008-06-14 13:31 Conc.lhs
-rw-r--r-- 1 braden users    536 2008-07-03 20:32 ConsoleHandler.hc
-rw-r--r-- 1 braden users   4545 2008-06-14 13:31 ConsoleHandler.hs
-rw-r--r-- 1 braden users   1460 2008-07-03 20:32 Desugar.hc
-rw-r--r-- 1 braden users   1018 2008-06-14 13:31 Desugar.hs
-rw-r--r-- 1 braden users   9670 2008-07-03 20:32 Dotnet.hc
-rw-r--r-- 1 braden users   1882 2008-06-14 13:31 Dotnet.hs
-rw-r--r-- 1 braden users 284441 2008-07-03 20:30 Enum.hc
-rw-r--r-- 1 braden users  21720 2008-06-14 13:31 Enum.lhs
-rw-r--r-- 1 braden users   5401 2008-07-03 20:32 Environment.hc
-rw-r--r-- 1 braden users    476 2008-06-14 13:31 Environment.hs
-rw-r--r-- 1 braden users  29973 2008-07-03 20:31 Err.hc
-rw-r--r-- 1 braden users   4704 2008-06-14 13:31 Err.lhs
-rw-r--r-- 1 braden users    950 2008-06-14 13:31 Err.lhs-boot
-rw-r--r-- 1 braden users   5086 2008-07-03 20:31 Exception.hc
-rw-r--r-- 1 braden users   5041 2008-06-14 13:31 Exception.lhs
-rw-r--r-- 1 braden users  17611 2008-07-03 20:32 Exts.hc
-rw-r--r-- 1 braden users   3055 2008-06-14 13:31 Exts.hs
-rw-r--r-- 1 braden users 679803 2008-07-03 20:31 Float.hc
-rw-r--r-- 1 braden users  35844 2008-06-14 13:31 Float.lhs
-rw-r--r-- 1 braden users  60228 2008-07-03 20:31 ForeignPtr.hc
-rw-r--r-- 1 braden users  13324 2008-06-14 13:31 ForeignPtr.hs
-rw-r--r-- 1 braden users 577252 2008-07-03 20:32 Handle.hc
-rw-r--r-- 1 braden users  66352 2008-06-14 13:31 Handle.hs
-rw-r--r-- 1 braden users 215127 2008-07-03 20:32 IO.hc
-rw-r--r-- 1 braden users  35712 2008-06-14 13:31 IO.hs
-rw-r--r-- 1 braden users 459483 2008-07-03 20:31 IOBase.hc
-rw-r--r-- 1 braden users  38839 2008-06-14 13:31 IOBase.lhs
-rw-r--r-- 1 braden users 501468 2008-07-03 20:31 Int.hc
-rw-r--r-- 1 braden users  31664 2008-06-14 13:31 Int.hs
-rw-r--r-- 1 braden users 112191 2008-07-03 20:30 List.hc
-rw-r--r-- 1 braden users  23569 2008-06-14 13:31 List.lhs
-rw-r--r-- 1 braden users  73014 2008-07-03 20:30 Num.hc
-rw-r--r-- 1 braden users  10799 2008-06-14 13:31 Num.lhs
-rw-r--r-- 1 braden users 480700 2008-07-03 20:32 PArr.hc
-rw-r--r-- 1 braden users  26686 2008-06-14 13:31 PArr.hs
-rw-r--r-- 1 braden users   7527 2008-07-03 20:31 Pack.hc
-rw-r--r-- 1 braden users   3312 2008-06-14 13:31 Pack.lhs
-rw-r--r-- 1 braden users  40733 2008-07-03 20:31 Ptr.hc
-rw-r--r-- 1 braden users   5937 2008-06-14 13:31 Ptr.lhs
-rw-r--r-- 1 braden users 371859 2008-07-03 20:31 Read.hc
-rw-r--r-- 1 braden users  23992 2008-06-14 13:31 Read.lhs
-rw-r--r-- 1 braden users 509294 2008-07-03 20:31 Real.hc
-rw-r--r-- 1 braden users  16977 2008-06-14 13:31 Real.lhs
-rw-r--r-- 1 braden users  14969 2008-07-03 20:30 ST.hc
-rw-r--r-- 1 braden users   5113 2008-06-14 13:31 ST.lhs
-rw-r--r-- 1 braden users   6354 2008-07-03 20:31 STRef.hc
-rw-r--r-- 1 braden users   1412 2008-06-14 13:31 STRef.lhs
-rw-r--r-- 1 braden users 330295 2008-07-03 20:30 Show.hc
-rw-r--r-- 1 braden users  16288 2008-06-14 13:31 Show.lhs
-rw-r--r-- 1 braden users   7852 2008-07-03 20:31 Stable.hc
-rw-r--r-- 1 braden users   3921 2008-06-14 13:31 Stable.lhs
-rw-r--r-- 1 braden users  46768 2008-07-03 20:31 Storable.hc
-rw-r--r-- 1 braden users   7545 2008-06-14 13:31 Storable.lhs
-rw-r--r-- 1 braden users  29052 2008-07-03 20:32 TopHandler.hc
-rw-r--r-- 1 braden users   4815 2008-06-14 13:31 TopHandler.lhs
-rw-r--r-- 1 braden users    253 2008-06-14 13:31 TopHandler.lhs-boot
-rw-r--r-- 1 braden users  23608 2008-07-03 20:31 Unicode.hc
-rw-r--r-- 1 braden users   7979 2008-06-14 13:31 Unicode.hs
-rw-r--r-- 1 braden users    479 2008-06-14 13:31 Unicode.hs-boot
-rw-r--r-- 1 braden users  13658 2008-07-03 20:32 Weak.hc
-rw-r--r-- 1 braden users   4895 2008-06-14 13:31 Weak.lhs
-rw-r--r-- 1 braden users 615579 2008-07-03 20:31 Word.hc
-rw-r--r-- 1 braden users  32589 2008-06-14 13:31 Word.hs
```


The finish line is in sight, but I'm stuck.



UPDATE: I touched all of those files and it built. No idea if it will work in the end though, that one GHC/PrimopWrappers file in particular is worrying.


### Back on the Target Machine


#### libffi and libgmp again



Need to compile these on the target too, and again put the two ffi\*.h files in one of the include directories searched by the GHC build process (I used /usr/include).


#### Apparently I need a GHC already running?



I unpack the hc-file distribution, and then manually step through the actions of distrib/hc-build. The line to make the libraries fails, ultimately because $(GHC) is empty. Unsurprising, since we don't have a GHC for the target yet. So I built compiler/ first, in defiance of the script. It failed too, late in the process, because it couldn't find -lHSregex-compat. Since this seems to be part of libraries/, I'm at a catch-22.


# Original Page


# GHC port for arm-unknown-linux-gnu



My goal is to create a registerised port of GHC to the nokia 770.


## Status



There is currently an unregisterised build available for Maemo 1.x. This project is temporarily on hold while two big transitions take place:


1. Maemo 2.0 supports the new EABI standard which affects a bunch of things (done) (for more info see [
  http://wiki.debian.org/ArmEabiPort](http://wiki.debian.org/ArmEabiPort) )
1. GHC 6.6 release (soon)

## Setting up the build environment



I have been using the standard maemo cross-development environment. Instructions for setting
up this environment can be found here:



[
http://www.maemo.org/platform/docs/tutorials/Maemo\_tutorial.html\#settingup](http://www.maemo.org/platform/docs/tutorials/Maemo_tutorial.html#settingup)


## Changes to standard procedure



The updated instructions on this page should now work:



[http://www.haskell.org/ghc/docs/latest/html/building/sec-porting-ghc.html\#unregisterised-porting](http://www.haskell.org/ghc/docs/latest/html/building/sec-porting-ghc.html#unregisterised-porting)



With two small changes:



(1) I had to add --srcdir=. 



Anyplace configure is called I get this error:


```wiki
This configuration does not support the `--srcdir' option..
```


Adding --srcdir=. makes the error go away.



(2) ghc/Makefile SUBDIRS ordering



This has been fixed in head, but if you download the 6.4.2 release you will need to
edit ghc/Makefile and change the ordering of the SUBDIRS so that lib comes before compiler.



This is the default ordering:


```wiki
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs compiler lib utils driver
else
```


and you want


```wiki
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs lib compiler utils driver
else
```


That should get to the point of having a ghc-inplace built.



NOTE: if you try to move the directory to a new location or name,
the inplace compiler will stop working because it has absolute paths
hard coded to the current location.


## Build ghc using ghc-inplace



(1) unroll the ghc source tarball into a new directory.



(2) ./configure --srcdir=. --with-ghc=/abs/path/to/ghc-inplace



(3) create a mk/build.mk with these two lines:


```wiki
GhcUnregisterised = YES
GhcWithNativeCodeGen = NO
```


(4) make



(5) make install



I think there may have been one other step in there somewhere...



This should build and install ghc. Unfortunately, the floating point
code will be broken.


## Run the test suite



(1) get the testsuite that corresponds to your release, for example:



[
http://haskell.org/ghc/dist/ghc-testsuite-6.4.2.tar.gz](http://haskell.org/ghc/dist/ghc-testsuite-6.4.2.tar.gz)



(2) untar it in the ghc-6.4.2 directory.



(3) edit mk/test.mk and change the -e config.time\_prog line to:


```wiki
        -e config.timeout_prog=\"\" \
```


I had to do this because the timeout program interacted badly with
the scratchbox shell causing all the tests to timeout and fail.



(4) cd to test/ghc-regress



(5) make TEST\_HC=ghc fast 



or



(5) make TEST\_HC=ghc \# for a longer test


# Step By Step Porting to Maemo 2.0


```wiki
T & H

wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src.tar.bz2
tar -xvjf ghc-6.6-src.tar.bz2
cd ghc-6.6
```

```wiki
T

$ ./configure --enable-hc-boot --enable-hc-boot-unregisterised --srcdir=.
$ cd includes
$ make
```

```wiki
H

$ ./configure --srcdir=.
```


Create H/mk/build.mk, with the following contents:


```wiki
H

GhcUnregisterised = YES
GhcLibHcOpts = -O -fvia-C -keep-hc-files
GhcRtsHcOpts = -keep-hc-files
GhcLibWays =
SplitObjs = NO
GhcWithNativeCodeGen = NO
GhcWithInterpreter = NO
GhcStage1HcOpts = -O
GhcStage2HcOpts = -O -fvia-C -keep-hc-files
SRC_HC_OPTS += -H32m
GhcBootLibs = YES
```


Change Target\* and TARGET\* variables in H/mk/config.mk


```wiki
H

TARGETPLATFORM			= arm-unknown-linux

TargetPlatform_CPP		= arm_unknown_linux
TargetArch_CPP			= arm

arm_unknown_linux_TARGET       = 1
arm_TARGET_ARCH      = 1
```


Copy T/ghc/includes/ghcautoconf.h, T/ghc/includes/DerivedConstants.h, and T/ghc/includes/GHCConstants.h to H/ghc/includes. Note that we are building on the host machine, using the target machine's configuration files. This is so that the intermediate C files generated here will be suitable for compiling on the target system.



Touch the generated configuration files, just to make sure they don't get replaced during the build:


```wiki
H

$ cd H/ghc/includes
$ touch ghcautoconf.h DerivedConstants.h GHCConstants.h mkDerivedConstants.c
$ touch mkDerivedConstantsHdr mkDerivedConstants.o mkGHCConstants mkGHCConstants.o
```


I just followed the guide upto making the hc bundle. I had to comment out this line in H/Makefile:


```wiki
#	echo ghc-$(ProjectVersion)/libraries/haskell-src/Language/Haskell/Parser.hs >> hc-files-to-go
```


because that file does not seem to exist anymore.


## Wrong Stuff



Oops, I mis-read the directions, so this next section is junk.



Build the compiler on the host. There seems to be a circular depends between utils and compat so I had to hack it a bit. First edit H/utils/Makefile and remove ghc-pkg from the SUBDIRS list in the else clause.


```wiki
H

else
SUBDIRS = mkdependC mkdirhier runstdtest hasktags hp2ps hsc2hs \
	  parallel prof unlit genprimopcode genapply runghc
endif
```


Then run 'make boot' in the utils directory


```wiki
H

$ cd H/ghc-6.6/utils
$ make boot
```


Now restore ghc-pkg to the SUBDIRS line:


```wiki
H

else
SUBDIRS = mkdependC mkdirhier runstdtest ghc-pkg hasktags hp2ps hsc2hs \
	  parallel prof unlit genprimopcode genapply runghc
endif
```


And build H/ghc-6.6/compat and then utils:


```wiki
H

$ cd H/ghc-6.6/compat
$ make boot && make
$ cd H/ghc-6.6/utils
$ make boot && make
```