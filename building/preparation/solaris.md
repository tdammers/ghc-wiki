# Setting up a Solaris system for building GHC



These instructions have only been checked for GHC 6.12.1 on Solaris 10 on SPARC. They should also apply to later versions of GHC, Solaris 8 and later, and perhaps Solaris on x86. 



GHC versions 6.10.1 and earlier don't have a working SPARC native code generator, and have many small build issues with Solaris. Use GHC 6.12.1 or later.


## Installing GNU packages



GHC relies on many GNU-isms that are not supported by the native Solaris build tools. The following environment is known to work. Later versions may work but have not been tested. Taking the time to install these tools is likely to be less painful than debugging build problems due to unsupported versions (and this is your official warning).


>
> <table><tr><th> GNU binutils 2.20  </th>
> <th> for GNU ld, maybe others 
> </th></tr>
> <tr><th> GNU coreutils 8.4  </th>
> <th> for GNU tr, maybe others 
> </th></tr>
> <tr><th> GNU make 3.81     </th>
> <th> make files use GNU extensions 
> </th></tr>
> <tr><th> GNU m4 1.4.13     </th>
> <th> 
> </th></tr>
> <tr><th> GNU sed 4.2           </th>
> <th> build scripts use GNU extensions 
> </th></tr>
> <tr><th> GNU tar 1.20         </th>
> <th> Solaris tar doesn't handle large file names 
> </th></tr>
> <tr><th> GNU grep 2.5      </th>
> <th> build scripts use GNU extensions 
> </th></tr>
> <tr><th> GNU readline 5 </th>
> <th> 
> </th></tr>
> <tr><th> GNU ncurses 5.5 </th>
> <th> 
> </th></tr>
> <tr><th> Python 2.6.4 </th>
> <th> needed to run the testsuite with multiple threads 
> </th></tr>
> <tr><th> GCC 4.1.2       </th>
> <th> this exact version is strongly recommended (3.4.3 is known to not work, see [\#8829](http://gitlabghc.nibbler/ghc/ghc/issues/8829)) 
> </th></tr></table>
>
>


Some of these can be obtained as binary versions from the  [
blastwave.org](http://www.blastwave.org/) collection, others need to be downloaded as source from [
gnu.org](http://www.gnu.org).



The blastwave libraries are usually installed under `/opt/csw`, so you may need to manually set `LD_LIBRARY_PATH` to point to them:


```wiki
export LD_LIBRARY_PATH=/opt/csw/lib
```

## Using a bootstrapping GHC



You can either get a binary distribution from the GHC download page or use some other pre-existing GHC binary. These binaries usually assume that required libraries are reachable via LD\_LIBRARY\_PATH, or are in `/opt/csw`. If you get errors about missing libraries or header files, then the easiest solution is to create soft links to them in, `lib/ghc-6.12.1` and `lib/ghc-6.12.1/include` of the installed binary distribution. These paths are always searched for libraries / headers.


