# Immix Garbage Collector



In a [
Google Summer of Code project](http://socghop.appspot.com/gsoc/student_project/show/google/gsoc2010/haskell/t127230760695), [
marcot](http://wiki.debian.org/MarcoSilva) started an implementation of the Immix Garbage Collector in GHC.  It's not in a state where it can be included in GHC yet, but it's functional, don't have known bugs and gets better results than the default GC in the [
nofib](http://www.dcs.gla.ac.uk/fp/software/ghc/nofib.html) suite.  On the other hand, it gets worse results than the default GC for the nofib/gc suite.  The implementation was reported on these blog posts: [
1](http://marcotmarcot.wordpress.com/2010/05/17/google-summer-of-code-weekly-report-1/) [
3](http://marcotmarcot.wordpress.com/2010/05/31/summer-of-code-weekly-report-3/) [
4](http://marcotmarcot.wordpress.com/2010/06/04/summer-of-code-weekly-report-4/) [
5](http://marcotmarcot.wordpress.com/2010/06/15/summer-of-code-weekly-report-5/) [
6](http://marcotmarcot.wordpress.com/2010/06/18/immix-on-ghc-summer-of-code-weekly-report-6/) [
7](http://marcotmarcot.wordpress.com/2010/06/29/immix-on-ghc-summer-of-code-weekly-report-7/) [
8](http://marcotmarcot.wordpress.com/2010/07/05/immix-on-ghc-summer-of-code-weekly-report-8/) [
9](http://marcotmarcot.wordpress.com/2010/07/07/immix-on-ghc-summer-of-code-weekly-report-9/) [
10](http://marcotmarcot.wordpress.com/2010/07/21/immix-on-ghc-summer-of-code-weekly-report-10/) [
11](http://marcotmarcot.wordpress.com/2010/08/10/immix-on-ghc-summer-of-code-report-11/) [
12](http://marcotmarcot.wordpress.com/2010/08/13/immix-on-ghc-summer-of-code-report-12-debconf-debian-day-bh/)


# The patches



There are [ some patches available](http://people.debian.org/~marcot/immix/).


## The main patch


- [
  Generated with darcs diff -u](http://people.debian.org/~marcot/immix/immix.patch)
- [ Darcs bundle](http://people.debian.org/~marcot/immix/immix.dpatch)


This patch includes the basic implementation of Immix.  It's tested, and has no known bugs.  In [
the measurements](http://people.debian.org/~marcot/immix/log.tar.gz), it has shown these results:


<table><tr><th> </th>
<th> **Runtime** </th>
<th> **Memory used** 
</th></tr>
<tr><th> **nofib** </th>
<th> -2.9% </th>
<th> -1.7% 
</th></tr>
<tr><th> **nofib/gc** </th>
<th> +4.3% </th>
<th> +1.2% 
</th></tr></table>



Currently, it overwrites the mark/sweep algorithm?.  It uses the same mark bits as mark/compact and mark/sweep?, but consider these bits in groups of 32 or 64, depending on the architecture used, which are called lines.  It creates a list of free lines for each [
generation](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC/Aging), and allocates on them when possible.



As only the first part of each object in memory is marked in the bitmap?, it skips the first free line for each group of subsequent lines, because it's possible that an object that starts in the previous line is using part of it.  Also, it doesn't deal with [blocks](commentary/rts/storage/block-alloc) that objects bigger than the size of a line, called medium sized objects, marked with `BF_MEDIUM`.



The mark stack is used to ensure that the objects allocated on lines get scavenged.


## Line before inscreasing block size


- [
  Generated with darcs diff -u](http://people.debian.org/~marcot/immix/order.patch)
- [ Darcs bundle](http://people.debian.org/~marcot/immix/order.dpatch)


Before the implementation of Immix, the code in todo\_block\_full did the following:


1. Try to increase the block size.
1. If it could not be increased, get a new block.


With Immix, it turned to:


1. If we were allocating in a block, try to increase the block size.
1. If it could not be increased, search for a line.
1. If there're no free lines, get a new block.


Another possibility for it is:


1. Search for a line.
1. If there are no free lines **and** we were allocating in a block, try to increase the block.
1. If it could not be increased, get a new block.


Basically, this swaps 1 and 2, making it prefer allocating on lines than
increasing the block size.  In the measurements done so far, it has not shown
significative improvements over the way the code is now, so I'll keep it here
to benchmark again when another thing changes, like:


## Allocate in lines in minor GCs


- [
  Generated with darcs diff -u](http://people.debian.org/~marcot/immix/minor.patch)
- [ Darcs bundle](http://people.debian.org/~marcot/immix/minor.dpatch)


This small patch makes it possible to allocate on lines during minor GCs,
removing the check about being in a major GC for the search for lines and for
the creating of the mark stack.  Maybe it shouldn't be so small, because it's
not working.  The code is being debugged, and possibly there will be a fix
soon.


## Remove partial list



With the allocation on lines, it's possible not to allocate on partially full
blocks.  By making all blocks full (with possibly free lines), there'll be no
need to use the list of partial blocks.  This is not done yet.


# To do


- Make it faster and use less memory than the default GC for all benchmarks
- Correct "Allocate in lines in minor GCs"
- Implement and bechmark "Remove partial lists"
