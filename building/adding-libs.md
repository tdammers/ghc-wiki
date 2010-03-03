# Manually adding libraries to a GHC build



A GHC source tree for building usually comes with the core libraries, see [Getting the sources](building/getting-the-sources).
The [darcs-all script](building/darcs-all) also gets the "extra" libraries if you give the flag --extra.
However, if you only need one particular library, you can use darcs to manually add it to the build. You need to know the directory name (i.e. the package name) of the library which you want to add.



Suppose you have downloaded a GHC source tree as advised:


```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x darcs-all
  $ ./darcs-all get
```


You are now in directory `ghc`. 
Descend into `libraries` and issue a `darcs get <repo>` (where `<repo>` is the repository of the package you want to get, ending in the package `<name>`.
A later `./darcs-all pull ` should now pull not only ghc and the core libraries, but also any library you have added like this.


```wiki
  $ cd libraries
  $ darcs get http://darcs.haskell.org/packages/<name>
  $ cd ..
  $ ./darcs-all pull
```


A list of available packages and their repositories can be found at [DarcsRepositories](darcs-repositories). This page also tells you which libraries are core libraries and which are optional for a GHC build.


