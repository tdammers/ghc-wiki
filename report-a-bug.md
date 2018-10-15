# Reporting bugs in GHC



Glasgow Haskell is a changing system so there are sure to be bugs in it.



To report a bug, either:


- Preferred:

  - [register](/trac/ghc/register) an account on this Trac
  - Create a [new bug](/trac/ghc/newticket?type=bug), and enter your bug report. You can also search the bug database here to make sure your bug hasn't already been reported (if it has, it might still help to add information from your experience to the existing report).
- Less preferred:

  - To submit an anonymous bug: use login "guest", password "guest"

## How do I tell if I should report my bug?


- **Is it a bug at all?**.  Take a look at the [
  FAQ](http://haskell.org/haskellwiki/GHC/FAQ) and [What to do when something goes wrong](http://www.haskell.org/ghc/docs/latest/html/users_guide/gone_wrong.html) from the GHC User's Guide, which will give you some guidance as to whether the behaviour you're seeing is really a bug or not.

- **Duplicate bug reports**.  Please search for existing tickets on the [
  bug tracker](http://hackage.haskell.org/trac/ghc) (search box in top right hand corner) or [
  Google](http://www.google.com/?q=site:ghc.haskell.org/trac/ghc/ticket%20).  If your problem has already been reported, it saves time to have all the manifestations of the same bug gathered together.  If you get an error message from GHC, a good search key is usually the non-program-specific part of the error message.

  If you do find an existing ticket that seems to describe the same problem, then

  - Add a comment that explains how it manifests for you, and add your description of how to reproduce it (see below)
  - Add yourself to the CC list for the bug. We will try to prioritise bugs that affect a lot of people, and the length of the CC list is how we are currently determining this.  Use a comma or space (but not semicolon) to separate your email address from the next one.

- **RTS bugs**.  However, if you encounter a crash from the runtime system, then don't bother searching for existing tickets - just create a new ticket.  These indicate a general RTS failure of some kind, and can arise due to a wide range of causes, so it is easier for us to track each failure in a separate ticket.  

>
>
> Runtime system errors usually manifest as one of the following error messages:
>
>
> ```wiki
> internal error: evacuate: strange closure type ...
> internal error: scavenge: unimplemented/strange closure type ...
> internal error: update_fwd: unknown/strange object ...
> internal error: stg_ap_v_ret
> ```


- **If in doubt, just report your bug**.

## What to put in a bug report



The Trac bug report system has various fields. Here's how to fill them in:


- **Short summary**.  This is what appears in one-bug-per line lists, so try to make it as informative as you can. A snippet of the error message, if there is one, usually makes a good summary.

- **Type** says what kind of Trac ticket this is:

  - **bug**: incorrect behaviour by GHC
  - **feature request**: something you would like GHC to do
  - **task** (for use by GHC developers only): something we intend to do sometime

- **Full description**.  This is where you describe your bug in details.  See "What information to provide" below.  Use the [wiki markup](trac-wiki-misc) in your description, especially `{{{` ... `}}}` brackets to mark up literal code.

- **CC**.  If you are only logged in as *guest* then please consider including your email address (using a comma or space as separator, not a semicolon). That way we can ask you questions, and you'll get email from Trac when something happens to your bug.

- **Operating system** and **Architecture**. Only fill these in if you think the bug is platform dependent. Otherwise, use `Unknown/Multiple`.

- **Component** and **Version**. Please pay particular attention to **version**, which is the version of GHC that you are running. Ideally, the version field is used to denote the first version a bug/issue is known to exist in (regardless whether that version is "supported" or not). 

- **Priority**, **Milestone**, **Assign to**, **Difficulty**, **Test case**, **Blocking**, **Blocked by**:  these are all for use by GHC developers; please don't fill these in.


See also [GHC working conventions](working-conventions).
 


## Full description: what information to provide in the body of your bug report



The name of the bug-reporting game is: facts, facts, facts. Don't omit them because "Oh, they won't be interested…".



**The absolutely key thing is that we must be able to reproduce the bug**.  Without this, we are virtually helpless; we know there's a problem but we usually can make no progress with fixing it.  The easiest way to help us reproduce the bug is to provide us with a program that elicits it:


- **As small as possible**.  It costs you real work to "boil down" the bug from a big program to a small one, but the plain truth is that the easier the bug is to reproduce, and the smaller the test program (= smaller debug output), the less time it costs us to fix it. Also, as you are familiar with the code, it is generally easier for you to boil it down than for us to. Please note that instances of certain classes, whether hand-written or derived, can produce a substantial amount of code. In particular, if you can demonstrate the bug without writing or deriving instances of `Show`, `Read`, `Generic`, or `Data`, that is generally preferable. To demonstrate evaluation, prefer `deepseq` to `show`, and write any necessary `NFData` instances by hand.
- **Zero dependencies**.  If we have to run `cabal install`, we can't put it in the testsuite *as is*. Dependencies also make it substantially harder to determine exactly where a bug was introduced, as it may be difficult or impossible to compile those dependencies at an arbitrary GHC commit. If you don't have time to eliminate all dependencies, or if you're unable to do so, you **must include version numbers or git commit hashes for each remaining dependency**. Failing to do this makes it *extremely* difficult to reproduce the problem; we will have to try to guess likely version numbers or commits based on ticket entry dates, which is both painful and unreliable, especially since library maintainers will often add workarounds for the very issues reported here.


One way to cut down programs is to replace library functions with definitions like


```wiki
  displayWidget :: This -> IO That
  displayWidget = undefined
```


and thereby avoid the necessity for the supporting library.



Here is a check-list of things to cover in your description:


1. The source code of the program that shows the bug.  You can give the code inline, or attach a file, or attach a tarball.
1. \[Only if you think the bug is platform dependent\]: what kind of machine are you running on, and exactly what version of the operating system are you using? (on a Unix system, `uname -a` or `cat /etc/motd` will show the desired information.) In the bug tracker, this information can be given in the "Architecture" and "Operating system" fields.
1. What version of GCC are you using? `gcc -v` will tell you.
1. Run the sequence of compiles/runs that caused the offending behaviour, cut-and-paste the whole session into the bug report. We'd prefer to see the whole thing.
1. Add the `-v` flag when running GHC, so we can see exactly what was run, what versions of things you have, etc.
1. Add the `-dcore-lint` flag when running GHC.  This adds some significant internal consistency-checking, which often nails bugs early.
1. What is the program behaviour that is wrong, in your opinion?


If you are a Hero and track down the problem in the compilation-system sources, please [send us patches](working-conventions/fixing-bugs).


