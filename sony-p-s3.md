
Assuming a Yellow Dog Linux install on the Sony PS3, you can get working ghc binaries (in rpm form) for ppc from 
[
Fedora Extras Repository](http://download.fedora.redhat.com/pub/fedora/linux/extras/5/ppc/repoview/G.group.html)



To build from source, (assuming you have installed a ghc binary package already) you may find the default `./configure` detects the system as `powerpc64-unknown-linux`.  This blew up for me when compiling `StgCRun.c` with assembler `junk at end of line, first unrecognized character is `@'` errors.  The fix is to pass an argument to configure to force a normal powerpc build `./configure --build=powerpc-unknown-linux`.



Build time was about 8 hours (with extralibs), including a couple of hours on the penultimate source file to be compiled.


