


## Git tricks



When working with GHC, there are a lot of ways you can use Git to make your life easier. Below are some of them:


### Ignoring unrecorded changes in submodules



Use


```wiki
  $ git config --global diff.ignoreSubmodules dirty
```


to stop Git in the ghc repo from checking for unrecorded changes in the submodules.


### Locate commit by SHA1



You can use `http://git.haskell.org/.findhash/<sha1-prefix>` or the convenient form below (also available at the front page of [
http://git.haskell.org/](http://git.haskell.org/)):






### Selectively record changes to commit



Do you miss Darcs? Do you hate it when a file contains a bugfix \*and\* a new feature, and you want to commit both separately? That's OK! Just run:


```wiki
$ git add -p
```


This opens the **interactive diff selector**, which behaves a lot like `darcs record`. It will go through every change you have made in the working tree, asking if you want to `git add` it to the index, so you can commit it afterwords.



**Nota bene**: this only adds files *to the index*, it does not commit them. Afterwords, you may commit the result using `git commit`. **Do not use `git commit -a`**, or you will just add all the changes to the commit!


### Selectively cherry-pick a commit from a branch



You still miss Darcs. One thing that would be great is if you could just 'pluck' one commit from a branch into your tree, but not the others. Sounds good - `git cherry-pick` to the rescue!


```wiki
$ git checkout master
$ git cherry-pick <sha1 id>
```


this will checkout to master, and pull in *only* the commit you refer to. **It does not create a merge**, it's as if the commit had existed on this branch all along. This is wonderfully useful for selectively plucking changes from someone's Git tree, or branch.


### Merge a branch into a Super Big Commit



Let's say you have a branch `foo` you would like to merge into master, but you have 10 small commits on `foo`, and you only want to make 1 Big Commit on master. Many times, we land features in a single 'big commit' to keep the history clean. This is easily doable with:


```wiki
$ git checkout master
$ git merge --squash foo
```


and then you can commit your new, unstaged changes into a big commit after fixing any conflicts. `--squash` basically tells git *to merge the changes, but not merge the commits*. This is exactly what you want.


### Basic rebases



What if you have a branch that's slightly out of date called `foo`, and you want to bring it up to date with master?


```wiki
$ git checkout master
$ git pull origin master
$ git rebase master foo
```


This will:


- Checkout to master.
- Update master to the latest upstream version.
- Rebase `foo` onto `master`.


Where *rebasing* includes:


- Checkout to the branch `foo`.
- Discard all the commits you have made on `foo`, temporarily
- Bring `foo` up to date with `master` (by *fast-forwarding* the tree)
- Replay all your previous commits from `foo` onto the New-And-Improved `foo` branch


This, in effect, will bring `foo` up to date with master, while preserving your commits.



Q: **But there was a conflict**! A: That's OK. If `git rebase` encounters a conflict while replaying your work, **it will stop and tell you so**. It will ask you to **fix the conflict, and `git add` the conflicting files**. Then you can continue using `git rebase --continue`.



Q: **I started to rebase, but I confused myself and don't know how to get out! Help**! A: You can always run `git rebase --abort`, which will abort the current rebase operation, and return you to your working tree.


### Using the reflog



Eventually when working in the repository, you'll invariably do something on accident that will delete work. Or maybe not delete your work - perhaps you simply want to "undo that thing you just did a minute ago". If you have never committed the changes, then you're out of luck (commit often, commit early - even locally!) But have you ever done something like:


- Accidentally lost a commit, by deleting a branch?
- Accidentally lost a commit through rebasing?
- Amended a commit (`git commit --amend`), only to find out you broke it, and you want to *undo* the amendment?
- Accidentally overwrote a branch with dangerous operation, like `git push --force`?


While you may think all hope is lost, **the reflog can save you from all of these, and more**. In short, the reflog *is a log that records every modification which Git tracks*. To understand that, first understand this: despite its appearance, the Git data model has a core tenet: *it is immutable - data is never deleted, only new copies can be made* (the only exception is when garbage collection deletes nodes which have no outstanding references - much like our own GC!) Not even a rebase - which can rewrite the history - can actually delete old data.



Second, we need to understand an **important part of `git checkout`**: the purpose of `checkout` is *not* to switch branches. Checkout, roughly speaking, **allows you to check out your tree to any state, revision, or copy in the history**. You don't have to checkout to a branch: you can checkout to a commit from 3 weeks ago, a commit that *does not exist on a branch*, or a completely empty branch with nothing in common. You can checkout the entire tree, or you could checkout an individual file, or a single directory. The point being: **checkout takes you to a state in the history.**



So with that in mind, think of `reflog` like the audit log you can use to see what operations were performed on the immutable git history. *Every* operation is tracked. Let's look at an example, from Austin's *validation tree* he uses to push commits:


```wiki
$ git reflog --date=relative # this will open an interactive pager
ad15c2b HEAD@{5 hours ago}: pull -tu origin master: Fast-forward
75a9664 HEAD@{27 hours ago}: merge amp: Fast-forward
1ef941a HEAD@{27 hours ago}: checkout: moving from amp to master
75a9664 HEAD@{27 hours ago}: commit (amend): Implement the AMP warning (#8004)
daa9a30 HEAD@{28 hours ago}: rebase -i (finish): returning to refs/heads/amp
daa9a30 HEAD@{28 hours ago}: rebase -i (pick): Implement the AMP warning (#8004)
b20cf4e HEAD@{28 hours ago}: rebase -i (pick): Fix AMP warnings.
1ef941a HEAD@{28 hours ago}: checkout: moving from amp to 1ef941a82eafb8f22c19e2643685679d2454c24a
3e8c33e HEAD@{28 hours ago}: commit: Fix AMP warnings.
70406bc HEAD@{28 hours ago}: reset: moving to HEAD~
d2afc83 HEAD@{28 hours ago}: cherry-pick: Fix most AMP warnings.
70406bc HEAD@{28 hours ago}: commit (amend): Implement the AMP warning (#8004)
697f9da HEAD@{28 hours ago}: cherry-pick: Implement the AMP warning (#8004)
1ef941a HEAD@{28 hours ago}: checkout: moving from master to amp
```


**The most recent operations are first, and older operations appear chronologically**. Let's note a few things:


- The *work you previously had still exists, and has a commit ID*. It is on the far left.
- The reflog tells you what operation resulted in the commit: in my history, we can see I did:

  - At one point, I reset my tree and undid my latest commit (in `70406bc`, using `git reset`.) Then I kept working.
  - Several `git cherry-pick` operations.
  - Several commits, and some `git commit --amend` operations.
  - I checked out to master.
  - Then I did a merge of the `amp` branch, which was a fast-forward: my previous changes had rebased the `amp` branch.
  - Later on, I pulled my tree and I got some updates from upstream.
- The reflog tells you what was modified; in this case it shows you the commits I changed.


With this information, **I can now restore my tree to any of those partial states**. For example, let's say I `git commit --amend` the AMP patch in `75a9664`, and did some more stuff. But then it turns out I didn't want any of that, **and I didn't want the amendment either**. I can easily do:


```wiki
$ git checkout -b temp daa9a30
```


Now, I am on the `temp` branch, **and my HEAD commit points to the patch, without any amendments**. I've essentially checked out to a point in the tree without any of those changes - because `git` never modifies the original data, this old copy still exists. Now that I am on the `temp` branch, I can do any number of things. Perhaps I can just delete the old `amp` branch, and merge the `temp` branch instead now.



As you can see, the `reflog` saved me here: I undid some nasty work in my personal tree, which otherwise might have been much more error prone or difficult to perform.



**The reflog is not needed often, but it is often indispensable when you need it.**


## Intermediate Git tricks



See [
25 Tips for Intermediate Git Users](https://www.andyjeffries.co.uk/25-tips-for-intermediate-git-users/) blog post.


## Advanced Git tricks



Finally, there are some **advanced tips**, not for the faint of heart:


### Interactive rebases



At a certain point of git usage, you'll want to rewrite history by *rebasing interactively*. This can be done by running:


```wiki
$ git rebase -i <commit range>
```


For example:


```wiki
$ git rebase -i HEAD~10
```


will allow you to interactively rebase the last 10 commits on your branch. This power allows you to:


- **Reorder patches**, by reordering the entries in the rebase list. If two patches can be applied independently (or, as we would say in the Darcs world, *the patches commute*), you can always switch their order and everything will be OK.
- **Drop patches**, and completely remove them from the history, by removing them from the list.
- **Squash commits**, which will let you compress a series of commits into one.
- **Reword commits**, which will let you rewrite the commit message for any commit in the list, without touching anything else. (This is one of the most common ones I - Austin Seipp - use.)
