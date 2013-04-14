# FreeBSD Support for GHC


## Versions Supported



FreeBSD 8.x and 9.x on both `i386` (x86) and `amd64` (x86\_64) are fully supported, support for 10.x is in progress, one may experience problems.


## Building on FreeBSD



Build instructions for FreeBSD are incorporated in the [Building Guide](building).  In particular, here is how to [set up your FreeBSD system for building GHC](building/preparation/free-bsd).


## Installing on FreeBSD



The recommend way to install GHC on FreeBSD is just to install it as part of the Haskell Platform port, i.e. [
devel/hs-haskell-platform](http://www.freshports.org/devel/hs-haskell-platform).  This can be done via old pkg\_tools:


```wiki
# pkg_add -r hs-haskell-platform
```


Or via pkg:


```wiki
# pkg install hs-haskell-platform
```


Note that you can find ports for many popular Haskell software on FreeBSD.  For the complete listing, please consult [
the haskell category](http://www.freshports.org/haskell) in the Ports Collection.



An experimental developer repository can be also found on [
GitHub](https://github.com/freebsd-haskell/freebsd-haskell) and you read more about the FreeBSD status on its [
FreeBSD wiki page](https://wiki.freebsd.org/Haskell).


