


# Getting the GHC sources



There are two ways to get sources to GHC: download a source distribution, or clone the [
git](http://git-scm.com/) repositories.


## Source distributions



A source distribution is a file like `ghc-7.8.3-src.tar.xz`, which contains a complete snapshot of the source tree for a particular version of GHC. Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/).



Source distributions are easier to build, because we also include the output from running certain external tools like [
Happy](http://haskell.org/happy), so you don't need to install these tools.


## Git



GHC uses Git for revision control (version 1.7.8 or newer recommended).



[Repositories](repositories) gives a list of all the git repositories used by GHC.   A source tree consists of more than one repository: at the top level there is the main GHC repository, and certain subdirectories contain separate git repositories.


### Cloning HEAD



A complete GHC source tree can be obtained (located in `ghc`) by running the following command:


```
git clone --recursive git://git.haskell.org/ghc.git
```


**PLEASE READ THIS:** There are some specific Git workflows which will make GHC development a lot more pleasant; read [how to use git with GHC](working-conventions/git) and [how to contribute a patch to GHC](working-conventions/fixing-bugs) for suggestions about this.



Notes:


- `git.haskell.org` is reachable via IPv6 as well as IPv4.

- If you're behind a **firewall blocking port 9418** (or `git clone git://...` fails for some other reason), try replacing `git://` by `http://` or `https://` in the instructions above.

- The above directions are valid for cloning GHC 7.9 or newer. For cloning GHC 7.8 or earlier, see the [legacy](building/getting-the-sources/legacy) instructions.

### Getting a branch



The above instructions will get the HEAD, the main trunk of GHC development. There is also a branch for each stable release line, as well as branches for development of major new features. The active branches are listed on [ActiveBranches](active-branches).



To get a branch, you need to get from a repo that contains the branch; in particular, local clones of the central repo will normally not include the branches that are in the central repo.



You can clone a specific branch via:


```
git clone -b <branchname> --recursive git://git.haskell.org/ghc.git ghc-<branchname>
```


and switch between branches on an existing clone by


```
git checkout <other-branchname>
git submodule update --init
```


**Note:** The instructions above apply to branches that contain the commit [\[db19c665ec5055c2193b2174519866045aeff09a/ghc\]](/trac/ghc/changeset/db19c665ec5055c2193b2174519866045aeff09a/ghc) which converted all sub-repos into submodules. To clone a branch prior to that commit, follow the [legacy](building/getting-the-sources/legacy) instructions instead. It is best not to attempt to cross that commit with `git checkout`; instead make a fresh clone of the desired branch directly.


### Getting a tag



Starting with GHC 7.10.1, you can simply clone a specific tag via:


```
git clone -b ghc-7.10.1-release --recursive git://git.haskell.org/ghc.git ghc-7.10.1
```


For 7.8 or earlier, follow the [legacy](building/getting-the-sources/legacy) instructions.


### Cloning from GitHub



The official mirror for GHC on GitHub is located at [
https://github.com/ghc/ghc](https://github.com/ghc/ghc).



First configure the following Git url rewrites to account for the different naming scheme on GitHub (due to GitHub not supporting `/` in repository names) before cloning (those rules are persisted in `${HOME}/.gitconfig` so you need to perform it only once):


```
git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/ 
git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/ 
git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/ 
git config --global url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/ 
git config --global url."git@github.com:ghc/packages-".insteadOf       git@github.com:ghc/packages/ 
```


and then simply proceed by


```
git clone --recursive git://github.com/ghc/ghc
```