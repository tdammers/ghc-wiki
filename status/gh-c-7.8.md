# Release plans for GHC 7.8


## Tickets



*Note that anything not listed here is off Austin's radar.*



[
https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&group=status&milestone=7.8.3](https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&group=status&milestone=7.8.3)


## The Dynamic Story



The dynamic story is complex. Here's the breakdown:


<table><tr><th>              </th>
<th>Linux (i386)</th>
<th>Linux (x86\_64)</th>
<th>FreeBSD</th>
<th>OS X 10.7 (x86\_64)</th>
<th>OS X 10.8 (x86\_64)</th>
<th>OS X 10.9 (x86\_64)</th>
<th>Windows i386</th>
<th>Windows x86\_64
</th></tr>
<tr><th>Dynamic GHCi  </th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**
</th></tr>
<tr><th>`-dynamic-too`</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**
</th></tr>
<tr><th>`-dynamic`    </th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**
</th></tr></table>


- **Dynamic GHCi**: 

  - YES: `ghci` uses the system linker to link `.so` or `.dll` dynamic libraries, to avoid linker bugs. This is controlled by `DYNAMIC_GHC_PROGRAMS=YES`.
  - NO: `ghci` uses GHC's hand-written linker to link `.o` files.
- **`-dynamic`**: makes GHC produce `.so`/`.dll` files rather than `.o` files.
- **`-dynamic-too`**: strictly an optimization, `-dynamic-too` allows the compiler to build static and dynamic object files at once. This is convenient for Dynamic GHCi support.

## The Windows Conundrum


- Windows is a bit difficult right now.

  - **Good news**: 64bit builds work using the fancy new MSYS2 environment with a few (\~6) failures!
  - **Bad news**: 32bit builds work well using the **old** environment

    - Austin confirmed the latest HEAD worked in the old 32bit environment, but not the msys2 one: the `ghc-stage2.exe` segfaults, and Austin hasn't tracked down why.
    - Obvious theory: msys2 environment is incorrectly configured somewhere
    - On the upside, 32bit in the old environment seems quite stable (\~3 test failures,) even if `make` is a bit nutty.
  - It seems `-dynamic` is busted, as well as `-dynamic-too`
  - Consequently, GHCi can't be dynamically linked.
  - We're punting all three of them for the RC.

    - This leaves GHC in the same place it was before essentially (but 64bit is in a difficult spot, see [\#7134](http://gitlabghc.nibbler/ghc/ghc/issues/7134))


(Related but not immediately critical: we have too many DLL symbols, and are very close to the limit ([\#5987](http://gitlabghc.nibbler/ghc/ghc/issues/5987)). Linking also takes a long time ([\#8229](http://gitlabghc.nibbler/ghc/ghc/issues/8229)))


## Other things


- Austin Seipp needs to upload the primops compatibility package for 7.8. This is is easy: mostly a copy of `compiler/utils/ExtsCompat64.hs` into a Cabal package. See also [
  the compatibility module page](http://www.haskell.org/haskellwiki/Compatibility_Modules).
