# How to contribute a new feature to GHC



We welcome your involvement in making GHC able to do more. Here's how to do it. Note that proposals for a change to the libraries (including base) should be send to the [
libraries mailinglist](http://haskell.org/haskellwiki/Library_submissions).


1. Open a [
  feature request](https://ghc.haskell.org/trac/ghc/newticket?type=feature+request) **ticket** on Trac.

  - *Alternatively*: Create a Pull Request on [
    GitHub (ghc-proposals)](https://github.com/ghc-proposals/ghc-proposals/). 

1. Write down the **specification** of the feature and create a Wiki [page](proposal) for it. Specifying before implementing is obviously a good idea; and it makes it possible for others to comment on your design before you invest heavily in building the feature.

1. Get **feedback** by emailing a suitable list (`ghc-devs` for nitty-gritty GHC internals, `glasgow-haskell-users` for user-visible features). Often you'll get useful ideas. Update the wiki page as needed. 

1. Put a link and a **summary** to the discussion in the Trac ticket.

1. For language extensions, **wait for approval** by GHC HQ.

1. Follow the instructions for **[contributing a patch](working-conventions/fixing-bugs)**.

1. Include updates to the **user manual** that documents your feature. For features that introduce a new flag, at least two sections need to be updated:

  - The [
    flag reference](https://downloads.haskell.org/~ghc/master/users-guide/flags.html) section generated from the modules in `utils/mkUserGuidePart/Options/`.
  - The flag descriptions in `docs/users_guide/using.rst` provide a detailed explanation of each flags' usage. 

1. Add a section to the latest **release notes** (`docs/users_guide/<version>-notes.rst` or `libraries/base/changelog.md`).

## Things to bear in mind



While we love new features, we think twice before incorporating them into GHC's main code base. Here are some things to bear in mind:
 


- It may seem that running 'git push' is practically free for us; you have done all the hard work.  But it isn't:

  - It effectively commits us to maintaining it indefinitely, and worrying about its interactions with existing features
  - We try hard to keep GHC's language design somewhat coherent.  GHC deliberately tries to host a variety of ideas, not all of which may be good, but we try to keep it under control.
  - We have to do quality-control on your proposal; we don't want to push un-maintainable code... or even well-written code that we can't understand.
  - If your proposal breaks something else, we'll get blamed regardless.  
  - Subsequent new features have to take account of interactions with your feature.

- New features should be switchable with their own flag, by default off.  We used to have just one flag `-fglasgow-exts` but nowadays we try to be much more selective.

- We are much happier about features whose implementation is:   

  - **Localised**: only a handful of places in the code are changed.
  - **Orthogonal**: no new invariants or constraints are added that subsequent changes must bear in mind. This is really important; any change that imposes costs on later, apparently unrelated, changes is much more costly. 


 


- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your proposal the attention it deserves. However, if you hear nothing for a couple of weeks then please feel free to ping us, in case your proposal has slipped through the cracks.


If you are working on a feature that you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.


