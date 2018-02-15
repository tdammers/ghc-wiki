


# Using git with GHC



This page will help you to use git effectively when [contributing patches to GHC](working-conventions/fixing-bugs), and extends upon the instructions provided in [getting the sources](building/getting-the-sources).


## A single GHC git tree


### Submodules



See [git submodules](working-conventions/git/submodules) when making changes to one of the libraries or utils that have their own repository in a GHC tree.


### Branches


#### The stable branch



The [releases page](working-conventions/releases) has a list of stable branches.


#### Development branches



See [ActiveBranches](active-branches) for a description of **known** development branches.



The live list of currently **existing** branches in the `ghc` repository can be browsed via [
http://git.haskell.org/ghc.git/heads](http://git.haskell.org/ghc.git/heads).



New development branches names should be prefixed with `wip/` (e.g. "`wip/dependent-types`"), as otherwise the current Git server-side configuration disallows branch deletion and [
non-fast-forward updates](http://stackoverflow.com/questions/4684352/whats-a-fast-forward-in-git).


## Working with a separate build tree



Sometimes we want to separate the build tree from the source tree.
There are a few advantages to doing this:


- You can make multiple different builds from the same sources,
  perhaps for testing different build settings, or for building
  on different platforms.

- You might want to put the source tree on a remote, backed-up,
  filesystem, but keep your build tree on a local fast unbacked-up
  drive (this is a configuration we use regularly at GHC HQ).  It
  doesn't matter if you lose the build tree: it can easily be
  regenerated.

- It's easy to blow away a build tree and start again, without
  modifying your source tree.  `make maintainer-clean` is usually
  good for this too, but it can miss files that it doesn't know
  about, or files that are remnants from older versions of GHC.

- It helps to avoid mistakes whereby you edit a file that happens
  to be automatically generated, instead of the original source
  file (e.g. editing `config.mk` instead of `config.mk.in`).  If
  you only edit files in the source tree, then this can't happen.


However, if you just want to build the software once on a single
platform, then your source tree can also be your build tree, and you
can skip the rest of this section.


### Creating a build tree with lndir



**Windows users**: so far as we know, symbolic links do not work right on MSYS at least, so we never use separate source and build trees on Windows.



**Mac OS X 10.8 users**: Apple no longer includes X11 with Xcode (which provided `lndir`). Install [
XQuartz](http://xquartz.macosforge.org/landing/) \>= 2.7.2 or build it directly in `utils/lndir` (as below).



A *build tree* is just an exact copy of the source tree, except that
every file in it is a symbolic link to the appropriate file in the
source tree.  There are "standard" Unix utilities that make such
copies, so standard that they go by different names: `lndir` and
`mkshadowdir` are two (If you don't have either, the GHC source
tree contains sources for the X11 `lndir` check out
`utils/lndir`).  To create a separate build tree, the typical sequence is something like this:


```wiki
  $ mkdir ghc-build
  $ cd ghc-build
  $ lndir <source>
  $ ln -s <source>/.git .
```


Where `<source>` is the directory containing your source tree.  Note the last step: GHC's `configure` script likes to see the `.git` directory, and by default `lndir` will not link `.git` directories.  Things will still work if you omit this step, but the GHC version number for your build won't contain the date (i.e. it will be "7.7" instead of something like "7.7.20121218").



You need to be a bit careful when using a build tree, that any new files you create
(if you do any development work) are in the source tree, not the build
tree!  This is especially easy to mess up when creating new tests, so watch out.


### Creating a buildtree with git-new-workdir



[
Since Git 2.9, \`git worktree\` works well enough with submodules](https://stackoverflow.com/questions/31871888/what-goes-wrong-when-using-git-worktree-with-git-submodules), so that the [
\`wtas\` alias](https://stackoverflow.com/a/31872051/388010) does what we want: `git clone --recursive git://git.haskell.org/ghc.git pristine && cd pristine/ && git wtas ../<feature-worktree>`. If you can't get it to work, read on.



I (ezyang) use Git workdirs to manage all of my GHC checkouts. It is quite nice: there is logically only one local repository, and just many checkouts of it, so it's very easy to share patches between various checkouts and your branches are all in one centralized place. However, sharing all of your submodules too takes a bit of work to setup, since Git doesn't natively support this workflow.



Here's what I do:


1. Start by making some pristine GHC checkout (call it `ghc-pristine`) which is an ordinary Git clone of the main GHC repository.

1. Remove all of the submodules that Git recursively created. This is because they are in the wrong format. You can do it with this command: `for i in `git submodule status | cut -d' ' -f3`; do rm -rf $i; done`

1. Re-checkout the submodules using a normal git clone, rather than the submodule tool. This can be done with this command: `for i in `git submodule status | cut -d' ' -f2`; do git clone git://git.haskell.org/`echo "$i" | sed s/libraries/packages/ | sed s/utils\\///`.git $i; done` (On OS X you might have to escape the backslash two more times)

1. Finish off the configuration by running `git submodule init` and `git submodule update`


Now, to create a new workdir, run `git-new-workdir ghc-pristine ghc-newdir`, and then inside `ghc-newdir`, run `for i in `git submodule status | cut -d' ' -f2`; do rmdir $i; git-new-workdir ../ghc-pristine/$i $i; done` to recursively make workdirs of all the submodules.


### Creating a buildtree with git-new-workdir-recursive



This is a script to automate the steps from the previous section: [
https://github.com/thomie/git-new-workdir-recursive](https://github.com/thomie/git-new-workdir-recursive).


## Push access



If (as a developer) you have been granted push privileges to `git.haskell.org`, you need to take into account that only the `ssh://` URLs support authentication (and hence `git push`ing to).



The following Git URL rewrite rules (which need to be configured only once as they're persisted in the `${HOME}/.gitconfig` file due to `--global`) take care of transparently redirecting `git push`es to the `ssh://` Git URL counterparts:


```
git config --global url."ssh://git@git.haskell.org/".pushInsteadOf git://git.haskell.org/ 
```


This uses the `ssh://` protocol (which has much higher latency due to the SSH handshake occurring for each connect) only for `git push` operations, and the very fast unauthenticated `git://` protocol for everything else (if you originally cloned `git://git.haskell.org/ghc.git`)


- If possible, commit often.  This helps to avoid conflicts.

## More git tips



The [Git Tricks](working-conventions/git/tricks) page describes some more suggestions and tips for using Git.


