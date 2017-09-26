# Quick Start to just building GHC



Below are quick instructions for **just building and installing GHC**. If you are an aspiring GHC developer, read the [Newcomers](newcomers) page instead.



The following instructions assume that you have [got the sources](building/getting-the-sources) and [installed the necessary tools](building/preparation).  In particular for Windows users, all the commands below must be executed in the MinGW shell, not Command Prompt or PowerShell.



Optionally, you customize your build by creating the file `mk/build.mk` using `mk/build.mk.sample` as a template.



If you obtained the repository via `git clone --recursive`, you will need to first run:


```
$ ./boot
```


<sub>This step isn't necessary if you obtained the source from a tar archive.</sub>



Next, run the `./configure` script followed by `make` to start the build:


```
$ ./configure # Windows users must append "--enable-tarballs-autodownload"
$ make
```


<sub>On Windows you need to download some binary distributables before being able to build.  This only has to be done once and can be done by adding the `--enable-tarballs-autodownload` flag to the call to `./configure`.</sub>



By default (without any `mk/build.mk` customization), this will do a 2-stage bootstrap build of the compiler, with profiling libraries.


## Run without installing



You can find the binaries built by make in the `inplace/bin` directory under the root of the ghc source tree. The binaries can be run from here without installing.


## Installing



After building, to install GHC (by default in `/usr/local`):


```
$ make install
```


You may need to use `sudo`.


## More information



Has your question not been answered? See [Building](building) for more resources. Also, don't hestitate to [ask for help](mailing-lists-and-irc).


