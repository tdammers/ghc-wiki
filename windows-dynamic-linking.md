### Dynamic Linking GHC On Windows


#### Summary current situation



GHC on Windows currently does not support dynamic linking.
The primary reason for this is a limitation in the `PE` specification 
which uses for the `ordinal` value (a unique number/index associated with a exported symbol)
a 16bit int. Which limits the amount of symbols we can export to `2^16-1 (~65k)` symbols.



GHC however seemed to need many more symbols than this. When this limit is reached LD silently 
add garbage for the `2^16th` symbol and above. This would then cause various runtime crashed.



To counter this a utility called `dll-split` was added to GHC in order to split the `GHC` dll
into multiple dlls so they fit under this limit. The idea is to assign modules to one of two split dlls
and to keep all modules that depend on eachother in the same dll. This to prevent recursive dependencies between
the dlls.



This as is turns out is very finicky and one has to manually maintain a list of "root" modules in the make file
and dll-split then checks the reachability on build. If you introduce a cyclic reference you would get a compile 
error.


#### Proposed solution



The proposal and the fix implemented for the situation is to use a common way of dealing with cyclic dependencies at link
time on Windows: import libraries.



Import libraries are a very simple format, actually there are two formats:


- The PE spec defines one called "short import library format" which defines a new layout to essentially tell you where you can find a symbol. e.g. `free => my_c_lib.dll`
  These are produced by the Microsoft tool `lib.exe` (ending with the `.lib` extension) and are very small and compact and supported by virtually all Windows compilers, including GCC.


   


- GCC defines it's own import format (files commonly ending with `.dll.a` or `.a` extension). The format of the GCC variant is exactly the same as a normal archive. Except it contains no
  code. the `.idata` sections will contain the symbol and the dll in which to find the symbol. This import format can be produced by `libtool`.


   
Import libraries essentially allow us to "promise" the linker that the symbols are defined somewhere else. The linker doesn't follow the symbols, or even check for the presence of the DLL.
This is how mutual recursive linking is done on Windows.



Import libraries are commonly created by means of a `Module Definition file` [
https://msdn.microsoft.com/en-us/library/28d6s79h.aspx](https://msdn.microsoft.com/en-us/library/28d6s79h.aspx)



This file allows quite a few things (like symbol hiding, forwarding, proxying, renaming etc).



But for simplicity a simple example is:


```wiki
LIBRARY foo.dll
EXPORTS
  foo
  bar
```


Using `libtool` we can create an import library from the `.def` file.



It's also important to note that there is two types of import libraries:


- Normal import libraries:

>
> >
> >
> > In this case, the dll is added to the IAT of the PE file it was linked into and the symbols used from the dll
> > are added into the import list of the IAT entry. This allows the OS loader to load the DLL and all it's dependency.
> > The OS loader is of course smart enough to not recurse endlessly.
> >
> >
>


        


- Second variant is a delay load import library:

>
> >
> >
> > In this variant the dll and symbols are not added to the IAT, but instead the linker produces stubs around the use of the
> > symbols in question. When the symbol is first used this forces the load of the dll. So this means that the dll is not loaded 
> > at startup but only as needed.
> >
> >
>


        


>
> >
> >
> > This is important, because it allows us to, if needed, use OS hooks to change where it looks for DLLs. [
> > https://msdn.microsoft.com/en-us/library/z9h1h6ty.aspx](https://msdn.microsoft.com/en-us/library/z9h1h6ty.aspx)
> > For instance, if we want to allow distribution of a Haskell runtime separately from GHC (like .NET, JAVA etc allow) then we can 
> > use this to place the dlls anywhere and have the main of a haskell program locate the runtime.
> >
> >
>


        


#### Implementation



The implementation does the following:


>
>
> 1) creates a .def files from all the symbols defined in the input object files
> 2) counts and splits the object files in X number of partitions. Making there's enough room
>
>
> >
> >
> > for the entire object file in the dll (so that all the symbols of one object file end up in the same dll).
> >
> >
>
>
> 3) creates X .def files and import libraries for these X dlls to be created.
> 4) creates X dlls using the import libraries on the link to break the cyclic dependencies. 
>
>
> >
> >
> > The dlls are given the suffix `-pt<num>.dll` to indicate that they were split.
> >
> >
>
>
> 5) Finally merges the X import libraries into one import libraries which does multiple redirections.
>
>
> >
> >
> > This removes the need for any other parts of the system to know the dlls were split.
> > Because of LD's search order:
> >
> >
> > ```wiki
> > libxxx.dll.a
> > xxx.dll.a
> > libxxx.a
> > cygxxx.dll (*)
> > libxxx.dll
> > xxx.dll
> > ```
> >
> >
> > specifying `-lfoo` will always pick the import library. So by merging it they don't have to know about the merge happening.
> >
> >
>


This splitting is done automatically in the build system. As a consequence `dll-split` and related functionality can be completely removed.


#### Function exporting



On Windows, to indicate that a function is to be exported one would use the attribute
`__declspec(dllexport)` and to import a function from a dll `__declspec(dllimport)`.



These attributes are normally placed on the C code. In `GHC` however we just have the object files to work with usually.
Which means GHC emulates the behaviour of using the attributes without actually using them. Usually an exported function
is called via trampoline. These trampolines can be recognized using the prefix `__imp_`. 



GHC thus when dynamically linked on Windows, looks for an exported function with the prefix `__imp_` when looking for functions
in packages other than the one being compiled. When we export functions, the trampolines are created by the import library.



So no extra work has to be done there.



The one thing that was previously overlooked is the secondary effect of the export attribute, which is to turn of LD's default
behaviour of exporting \*all\* symbols. Since LD does not know which symbols we actually want to export, it ends up exporting all
symbols, \*including\* those from any static archives we include in the link!. This is what's causing our explosive increase
in exported functions. Most of it are functions that we should not be exporting because they do not form part of the API we wanted
to expose.



To fix this, I use `--retain-symbols-file` to indicate to `LD` the list of symbols to retain during the link and this include in the export.



This brings the largest dll (GHC dll) down to about \~44k symbols. Way below the limit. So the automatic partitioning will not kick in yet as
it is not needed.


### Misc



The following are some important pieces of the puzzle.


#### *build-dll-win32.sh*



The DLL compiling logic Is implemented in this shell script. This script should be used to compile libraries on Windows (The Makefiles have
been updated to point to this shell script on Windows.). This script will automatically split a DLL is needed without any extra user action needed.



The resulting libraries can also be used as if they were not split without any special treatment.


#### *compiler/main/Manifest.hs*



Contains the new logic for manifest files. Coincidentally, SxS manifest do allow for an RPATH like functionality.
By using config files (which cannot be embedded in the exe) you can specify probing paths for SxS searches.


```wiki
<!-- <appname>.exe.config -->
<configuration>   
  <windows>
    <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
      <probing privatePath="bin;..\bin" />
    </assemblyBinding>
  </windows>
</configuration>
```


Unfortunately according to MSDN you are limited to a laughable 9 entries..


#### *driver\\utils\\dynwrapper.c*



As with Linux, a dynamic GHC on Windows is a wrapper. However unlike Linux it's not a shell script
but a driver exe. The main application is compiled into a dll named \*application.exe.dll\* and the
wrapper loads this dll and sets the search path for libraries.



This process is driven by *rules\\build-prog.mk*.


#### Runtime selection



The last remaining hurdle is that of runtime selection. Currently at link time a runtime is picked by the compiler to compile the libraries with.
This is because on Windows, a PE file cannot be created with undefined symbols. So we must satisfy the link with an import library which points
to a dll. (Note that libtool allows you to omit the `LIBRARY` entry, in which case it just makes an invalid IAT entry)



As such, the patch only works for one RTS at the moment: normal rts. Dynamic, threaded, prof etc will all segfault because of multiple versions of
the rts being loaded at the same time and both being used.



So we need to delay the choice of RTS as much as possible so that the final executable can make the choice. To do this we can use a slight alteration of
[\#10352](http://gitlabghc.nibbler/ghc/ghc/issues/10352) .



For this to work on Windows we need a few things:



name all the rts variants the same. e.g. `ghc-rts.dll` but place them in different folders:


```wiki
rts
   \ normal   \ ghc-rts.dll
   \ threaded \ ghc-rts.dll
   \ profiled \ ghc-rts.dll
```


etc. We can then delay load the rts dll only. (This can be done by making the import library for the RTSs a lazy import library as described above).
All the RTS versions should have the same ABI so that shouldn't be an issue.



Delay loading will require some changes to GHC, specifically static const data can no longer be just referenced from the DLL. To get the data you have to
go through a function. The overhead can be minimized somewhat by always inlining these accessor functions. This is a limitation of the delay load mechanism.



When delay loading there are no more entries in the '.idata' section and everything goes through a trampoline which loads the DLL on demand. Obviously referencing
data would end up referencing the address of the trampoline instead of the actual data behind it.



During compilation of a `.exe` we can then set the search path using `AddDllDirectory` to allow the loader to pick the correct `RTS` variants for the `.exe`
and all the DLLs since they are all in the same process space as the `exe` and so will inherit the search path.



For dynamic libraries we can override the `hs_init` function and do the same. I think we can use ld's `--wrap symbol` for this so existing code don't need any changing:
[
http://ftp.gnu.org/pub/old-gnu/Manuals/ld-2.9.1/html\_node/ld\_3.html](http://ftp.gnu.org/pub/old-gnu/Manuals/ld-2.9.1/html_node/ld_3.html)


