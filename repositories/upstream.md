
The information on this page is slightly out of date; see [GitRepoReorganization](git-repo-reorganization) and [\#8545](http://gitlabghc.nibbler/ghc/ghc/issues/8545) for latest news


# Upstream repositories



Many of the libraries and tools in a GHC tree are actually maintained by someone else. They therefore have a separate upstream repository, from which we need to pull. That repository may be either a darcs or a git repository; in the darcs case, we also need to convert to a git repository for use in a GHC tree. However, if the darcs repository is on another server, then we first need to mirror it for the conversion program to use. This diagram shows how changes migrate from one repo to another:



[](/trac/ghc/attachment/wiki/Repositories/Upstream/repos.png)



This means that when making changes needed in GHC to one of these libraries, we first need to put the changes in the upstream repository. Note that a git hook prevents you from pushing patches to the ghc repos until they are already in the git mirror repos, so that we cannot forget to send changes upstream.



The mirrors are updated automatically each night, but you can force an immediate update by running `/srv/darcs/do_mirrors` on `darcs.haskell.org`.


## Repository locations



Note that the following table might be out of date, please refer to GHC's [
packages](http://git.haskell.org/?p=ghc.git;a=blob;f=packages;hb=HEAD) file which is always up to date as otherwise scripts will break.



Moreover, the list of upstream Darcs repos mirrored as Git repositories can be found [
here](http://git.haskell.org/?a=project_list;pf=darcs-mirrors).


Here is where the upstream repositories are, and their mirrors.  The master repository is identified in green.

<table><tr><th>darcs</th>
<th>git</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>darcs upstream</th>
<th>darcs mirror</th>
<th>git upstream</th>
<th>git mirror</th>
<th>ghc (validated) repo</th>
<th>in-tree</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/kolmodin/binary.git</th>
<th>http://darcs.haskell.org/git-mirrors/binary/binary.git/</th>
<th>http://darcs.haskell.org/packages/.git/</th>
<th>libraries/binary</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/bytestring.git</th>
<th>http://darcs.haskell.org/git-mirrors/bytestring/.git/</th>
<th>http://darcs.haskell.org/packages/bytestring.git/</th>
<th>libraries/bytestring</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/cabal.git</th>
<th>http://darcs.haskell.org/git-mirrors/Cabal/.git/</th>
<th>http://darcs.haskell.org/packages/Cabal.git/</th>
<th>libraries/Cabal</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/containers.git</th>
<th>http://darcs.haskell.org/git-mirrors/containers/.git/</th>
<th>http://darcs.haskell.org/packages/containers.git/</th>
<th>libraries/containers</th></tr>
<tr><th>http://code.haskell.org/haskeline/</th>
<th>http://darcs.haskell.org/darcs-mirrors/haskeline/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/haskeline/.git/</th>
<th>http://darcs.haskell.org/packages/haskeline.git/</th>
<th>libraries/haskeline</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/ekmett/mtl.git</th>
<th>http://darcs.haskell.org/git-mirrors/mtl/.git</th>
<th>http://darcs.haskell.org/packages/mtl.git/</th>
<th>libraries/mtl</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/pretty.git</th>
<th>http://darcs.haskell.org/git-mirrors/pretty/</th>
<th>http://darcs.haskell.org/packages/pretty.git/</th>
<th>libraries/pretty</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/random.git</th>
<th>http://darcs.haskell.org/git-mirrors/random/</th>
<th>http://darcs.haskell.org/packages/random.git/</th>
<th>libraries/random</th></tr>
<tr><th>http://code.haskell.org/terminfo/</th>
<th>http://darcs.haskell.org/darcs-mirrors/terminfo/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/terminfo/.git/</th>
<th>http://darcs.haskell.org/packages/terminfo.git/</th>
<th>libraries/terminfo</th></tr>
<tr><th>http://code.haskell.org/time/</th>
<th>http://darcs.haskell.org/darcs-mirrors/time/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/time/.git/</th>
<th>http://darcs.haskell.org/packages/time.git/</th>
<th>libraries/time</th></tr>
<tr><th>http://code.haskell.org/\~ross/transformers</th>
<th>http://darcs.haskell.org/darcs-mirrors/transformers/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/transformers/.git/</th>
<th>http://darcs.haskell.org/packages/transformers.git/</th>
<th>libraries/transformers</th></tr>
<tr><th></th>
<th></th>
<th>https://github.com/glguy/utf8-string.git</th>
<th>http://darcs.haskell.org/git-mirrors/utf8-string/</th>
<th>http://darcs.haskell.org/packages/utf8-string.git/</th>
<th>libraries/utf8-string</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/win32.git</th>
<th>http://darcs.haskell.org/git-mirrors/Win32/</th>
<th>http://darcs.haskell.org/packages/Win32.git/</th>
<th>libraries/Win32</th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/xhtml.git</th>
<th>http://darcs.haskell.org/git-mirrors/xhtml/</th>
<th>http://darcs.haskell.org/packages/xhtml.git/</th>
<th>libraries/xhtml</th></tr>
<tr><th>http://code.haskell.org/primitive/</th>
<th>http://darcs.haskell.org/darcs-mirrors/primitive/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/primitive/.git/</th>
<th>http://darcs.haskell.org/packages/primitive.git/</th>
<th>libraries/primitive</th></tr>
<tr><th>http://code.haskell.org/vector/</th>
<th>http://darcs.haskell.org/darcs-mirrors/vector/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/vector/.git/</th>
<th>http://darcs.haskell.org/packages/vector.git/</th>
<th>libraries/vector</th></tr></table>


## Modifiying libraries for which there is an upstream repository



The process for updating libraries for which there is an upstream repo
is a a little more complicated than for libraries where the repo is part
of the GHC setup:


1. Any changes needed by GHC should be made not only in our repository,
  but also in the upstream repository.

1. Being used in a GHC tree should not make life harder for the
  upstream maintainer.


Note that these two objectives are to some extent in conflict: If a
change in GHC or one of its libraries requires a change in a library
with an upstream repo then, in order to satisfy objective 1, the
maintainer would need to apply the patch, but if doing so is currently
inconvenient for them then this would fail objective 2. This policy
therefore tries to find the best compromise, without being too onerous
for any party.



For these repositories, we use a "git submodule" rather than a normal
repository. Using submodules means that the repository doesn't need to
follow a linear path through the git history, but can instead jump
around, for example from a release commit on one branch to the next
release commit on a different branch.


### From the GHC developer's point of view



If you are not modifying these packages then you don't need to do
anything special: A regular `git submodule update` will update the submodules
as normal. However, you may find it useful to run


```wiki
git config --global diff.ignoreSubmodules dirty
```


or each time you run `git status` or `git diff`, git will check for
changes not only in the GHC repository, but also in all the submodules.
(you must have `git >= 1.7.3` for this to work).



If you need to modify one of these libraries, then ordinarily you should
first send the modifications upstream. Ideally upstream will apply the
patches and make a release (the easiest way to accomplish objective 1 is
for changes to be applied upstream *first*, so that they can't be
forgotten about after being applied to GHC's repo). You can then update
GHC's submodule by running


```wiki
cd libraries/foo
git reset --hard some_commit_id
cd ../..
git commit -a
```


There are some scenarios where you may need to modify GHC's repository
without the upstream repository already having the change that you need:


- The maintainer may tell you that they are too busy to deal with the
  package at the moment, or not be responding at all. In this case, it
  may be necessary to make changes only to GHC's repositories in the
  short term, and for the changes to be merged upstream later.

- In a GHC stable branch, we may be using an old version of a library
  that we need to make a change to, but upstream may only be interested
  in working on the latest version rather than also maintaining old
  release branches. In that case, we would only make the change in the
  GHC respository.


In order to make the change in this case, you


```wiki
cd libraries/foo
git commit -a
git push -f origin HEAD:refs/heads/ghc-head
cd ../..
git commit -a
```


(use e.g. `ghc-7.6` rather than `ghc-head` if this patch is for a branch only).


### From the upstream maintainer's point of view



Upstream maintainers don't need to do anything special. You can continue
to use any version control system and whatever branching policy works best
for you. However, there are two issues to be aware of:


- For libraries that are shipped with GHC, we need to have releases of
  libraries that can build with that GHC. There may be no suitable
  existing release (most commonly due to trivial things such as library
  dependencies needing to be changed, but sometimes due to real changes
  in other libraries or the compiler), in which case we will request
  that you make a suitable release or, if it is not convenient for you
  to do so, we can make one on your behalf (in which case it will
  normally have only the minimal changes necessary since the previous
  release).

- Sometimes we may need to make changes to old versions of libraries,
  as we try to avoid making interface changes within GHC stable
  branches and upstream development may have moved on since a GHC
  stable branch was created. When this happens it is up to you whether
  the changes are sent upstream as normal (and maintained in an upstream
  branch), or whether they are left only in the GHC repository. Note that
  if they are made only the GHC repository then we will probably need to
  make a release from the GHC repository, as per the previous point.
