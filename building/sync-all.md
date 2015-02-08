
**Note**: This page is here mostly for historical reasons. A GHC tree used to consist of multiple independent  repositories. The `sync-all` script let you operate over them all at once. It is still present in the root directory of the tree, but since all [repositories](repositories) have been turned into git submodules, you shouldn't need it anymore. See [getting the sources](building/getting-the-sources) for up-to date information about fetching the sources, and [git working conventions](working-conventions/git) and [git submodules](working-conventions/git/submodules) for the new submodule workflow.


### sync-all: operations over all GHC repositories at once



The full documentation for `sync-all` is found by using the `--help` option:


```wiki
  $ ./sync-all --help
```


(you can also find it by looking in sync-all)


### Pulling new patches



If you have an existing tree, here is how to pull new patches into all repositories.


```wiki
  $ ./sync-all pull
  $ ./sync-all get
```


The second step is required in the event that new packages or repositories have been added to GHC.



See [Building/Hacking](building/hacking) if you've never built anything before. See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.



You can also pull patches from another tree, by registering the other tree as a remote, and giving it a name (here `anotherghc`):


```wiki
  $ ./sync-all -r /another/ghc remote add anotherghc
  $ ./sync-all pull anotherghc
```


where `/another/ghc` is a path to another local GHC repository.  You can specify a remote repository here too, e.g. `-r git://github.com/ghc` (remember to omit the final "ghc.git" when using a remote repo).



The `sync-all` command is useful for finding out what patches you have relative to another repository:


```wiki
  $ ./sync-all fetch anotherghc
  $ ./sync-all new anotherghc
```


this tells you which patches there are in your local repository tree relative to the tree over in `/another/ghc`.


