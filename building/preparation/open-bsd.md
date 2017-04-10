


# Setting Up a OpenBSD System for Building GHC



note: this is for building on OpenBSD Current as of 2017-04-05

note: The wxallow setting will reduce your security (From OpenBSD's point of view)


## Prerequisite


- The wxallowed mount option must be specified for the filesystem where the build will take place, and optionally /tmp 


You can either set it by editing the `/etc/fstab` like so: 


```wiki
01020304050607.h /usr/local ffs rw,nodev,wxallowed 1 2 
```


and rebooting, or you can temporary set a mount as wxallowed by running this command: `mount -uo wxallowed /usr/local`. (where /usr/local is the partition you'd like to affected, run `mount` to see them all)
See [
https://www.openbsd.org/faq/upgrade60.html](https://www.openbsd.org/faq/upgrade60.html) for a more detailed explanation.


- The login class for the user you will be performed the build as needs to have a sufficiently large 'datasize' (datasize-cur: 4096M has been known to work)

## Required Tools



Install the [
required tools](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools):


```wiki
doas pkg_add ghc gmake autoconf%2.69 automake%1.15 gmp libiconv alex happy git
```


to also (optionally) build the docs


```wiki
doas pkg_add py-sphinx
```

## Some required environment variables


```wiki
export AUTOCONF_VERSION=2.69
export AUTOMAKE_VERSION=1.15
```


Optionally (if your /tmp does not have the wxallowed mount option (don't forget to mkdir $HOME/tmp))


```wiki
export TMP=$HOME/tmp
export TEMP=$HOME/tmp
```

## Get the Sources



see [Getting the sources](building/getting-the-sources)


## Building


```wiki
./boot
./configure --with-iconv-libraries=/usr/local/lib \
--with-iconv-includes=/usr/local/include \
--with-gmp-libraries=/usr/local/lib \
--with-gmp-includes=/usr/local/include \
--with-ffi-libraries=/usr/local/lib \
--with-ffi-includes=/usr/local/include \
--with-system-libffi
gmake
```


see also [Quick Start: just build GHC](building/quick-start)


