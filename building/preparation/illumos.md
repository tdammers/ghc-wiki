


# Setting up a Illumos system for building GHC



If you're on a recent Illumos system, then you should be able to get a working build environment by installing the following packages using your system's package manager.


## SmartOS



Use `pkgin` to install the following:


- `build-essential`
- `ghc`


Configure `PATH` to contain `~/.cabal/bin`.



Use `cabal` to install the following:


- `happy`
- `alex`


From here return to [Building](building) and follow [Getting the sources](building/getting-the-sources) then [Just building and installing GHC](building/quick-start).


## [OpenIndiana](building/preparation/open-indiana)


>
>
> These instructions have only been checked for GHC 7.6.3 on [OpenIndiana](building/preparation/open-indiana) 151a8. They should also apply to earlier and later versions of GHC. 
>
>

### **Installing pre-compiled GHC 7.2.2 binary package **



In order to build GHC on [OpenIndiana](building/preparation/open-indiana) we'll be using already precompiled version of ghc 7.2.2 from sfe repository


```wiki
pfexec pkg install runtime/ghc 
```

### *Configuring system for building*



To configure system for building we need to specify correct gmp include directory


```wiki
./configure --with-gmp-includes=/usr/include/gmp
```

### **Bulding GHC**



In order to build GHC you need to use GNU's gmake command instead of [OpenIndiana](building/preparation/open-indiana)'s make which is


```wiki
gmake
```