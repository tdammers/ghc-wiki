# Backpack pre-release



**This page is out of date; Backpack is released in GHC 8.2! See [
https://ghc.haskell.org/trac/ghc/wiki/Backpack](https://ghc.haskell.org/trac/ghc/wiki/Backpack) **



If you would like to try out our new Backpack support in GHC, we invite you to download and install a pre-release version of GHC with various Backpack: [
http://web.stanford.edu/\~ezyang/backpack/](http://web.stanford.edu/~ezyang/backpack/) You can build the sources using  the instructions in [Building/QuickStart](building/quick-start), and there are also some binary distributions available.



Source distributions:


- [
  ghc-7.11.20150312-src.tar.bz2](http://web.stanford.edu/~ezyang/backpack/ghc-7.11.20150312-src.tar.bz2). This provides the compiler and a minimal set of libraries. For more information on building, see the [building guide](building).
- [
  ghc-7.11.20150312-testsuite.tar.bz2](http://web.stanford.edu/~ezyang/backpack/ghc-7.11.20150312-testsuite.tar.bz2). If you unpack this tarball on top of the above then you will be able to run the testsuite.
- [
  ghc-7.11.20150312-windows-extra-src.tar.bz2](http://web.stanford.edu/~ezyang/backpack/ghc-7.11.20150312-windows-extra-src.tar.bz2). This provides extra build tools which are a requirement for building on Windows. Simply unpack this tarball on top of the regular source distribution.


Binary distributions:


- [
  ghc-7.11.20150312-x86\_64-unknown-linux.tar.bz2](http://web.stanford.edu/~ezyang/backpack/ghc-7.11.20150312-x86_64-unknown-linux.tar.bz2). Generic amd64 Linux. This is a complete build, including interactive system, profiling libraries and documentation. It was made on a Arch Linux system and requires libgmp.so.10 (typically GMP 5.x). 
- [
  ghc-7.11.20150312-x86\_64-unknown-mingw32.tar.bz2](http://web.stanford.edu/~ezyang/backpack/ghc-7.11.20150312-x86_64-unknown-mingw32.tar.bz2). Windows 64bit. This is a complete build. It also includes support for compiling C++ files. 

## Examples



In this section, we give a few examples of using "Backpack in the small." They are a little contorted, since Backpack's primary design case was to facilitate at the level of libraries/packages; however, it's a bit difficult to experiment with a system if you're told that it is only suitable to build a giant software system! Backpack can be used to do small-scale separate modular development, since it is a proper module system; it just can be a bit messy at times.



As a convention, all of the code examples are runnable.  Simply copy paste any code samples prefixed with a comment `-- filename.hs` into an appropriately named file.


### Hello World



To test that everything is working properly, create the following two files:


```wiki
-- hello.bkp
executable hello where
  includes: base
  main-is: hello
```

```wiki
-- hello.hs
main = putStrLn "Hello world!"
```


and then compile with:


```wiki
ghc --backpack hello.bkp
./hello
-- Hello world!
```

### Reusable import lists



Suppose you have a Haskell file `P.hs` that looks like this:


```wiki
module P(ptest) where
import System.Directory (doesFileExist)
ptest = doesFileExist "no_it_does_not" >>= print
```


In particular, this file has an explicit import list on its import (perhaps imagine that `P.hs` is a bit longer and has a lot more imports), because you wanted to be more specific about what functions you depend on.



Suppose you are writing a new module `Q.hs`, in which you want to reuse the import list; however, you're less than keen about copying the list over. Is there any way to reuse the import list in some way?



We can do this by replacing what is currently a direct dependency on `directory`, ala:


```wiki
package p where
    includes: base, directory
    exposed-modules: P Q
```


with an indirect dependency on a **signature file**:


```wiki
package p where
    includes: base
    exposed-modules: P Q
    required-signatures: System.Directory
```


which lists all of the names and the type signatures of the functions we're planning on using:


```wiki
-- p/System/Directory.hsig
module System.Directory where
doesFileExist :: FilePath -> IO Bool
```


Now, `P.hs` and `Q.hs` can omit their import lists:


```wiki
-- p/P.hs
module P(ptest) where
import System.Directory
ptest = doesFileExist "no_it_does_not" >>= print
```

```wiki
-- p/Q.hs
module Q(qtest) where
import System.Directory
qtest = doesFileExist "no_really" >>= print
```


The last ingredient is that, eventually, we have to tell GHC which actual implementation of `System.Directory` is desired. We can do this by simply including both package p and package directory in the same package:


```wiki
executable Main where
    includes: base, directory, p
    main-is: Main
```

>
>
> **In progress:** At the moment, order matters! Make sure the `directory` include (providing the implementation) comes before the `p` include (requiring the implementation).
>
>


This juxtaposition triggers a linking step, where GHC recognizes that `p` has a hole (signature without an implementation) named `System.Directory`, while `directory`.



Here's the complete Backpack file:


```wiki
-- importlist.bkp
package p where
    includes: base
    exposed-modules: P Q
    required-signatures: System.Directory

executable Main where
    includes: base, directory, p
    main-is: Main
```


as well as the source code for a little test script:


```wiki
-- Main.hs
import P
import Q
main = ptest >> qtest
```


These files will compile with `ghc --backpack importlist.bkp`.



You can check that other functions from `System.Directory` are not available by editing `P.hs` or `Q.hs` to attempt to use another function from the module, e.g. `doesDirectoryExist`.



In general, to import a subset of the interface of a module, you create an hsig file which contains the signatures you want. Additionally, an actual implementation of the module must not be in scope (if it is in scope, it takes precedence over the signatures).


>
>
> **Under construction:** The error message you get when you attempt to use a function which is available from the underlying implementation but not from your signature could be improved.
>
>

>
>
> **Open question:** Should there be an easier way of loading a specific implementation narrowed to some interface? The most general way to use Backpack suggests that you should commit to an implementation as late as possible, which means this style of development should be discouraged.
>
>

## Type classes



In this example, we'll develop a sorted list of integers (akin to `Data.Map`) which requires the integers in question to have the moral equivalent of `Ord` instance. However, instead of using the `Ord` type-class (for which we can only have one), we'll use a module signature instead (in the style of [
modular type classes](http://www.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf)). This will let us support multiple orderings for the integers, without having to write our list data structure twice and without having to cast integers into an alternate data type. (Note: this example can be generalized to handle arbitrary data types, but for simplicity this example is hard-coded for integers.)



The first step is to define an interface for implementations of comparison functions on integers. In our case, it's very simple: a specialized version of `compare` under name `cmp`:


```wiki
-- IntOrd-sig/IntOrd.hsig
module IntOrd where  
cmp :: Int -> Int -> Ordering
```


We can place this signature in a package all of itself, and mark the signature as reusable by specifying it in the `exposed-signatures` list of the package (which means that if this package is included in another package, the signature is visible for import by modules.)


```wiki
package IntOrd-sig where  
    includes: base  
    exposed-signatures: IntOrd  
```

>
>
> **Remark:** `Int` in this example refers to the `Int` implicitly imported via `Prelude`. To become abstract in the data type as well, you could add a `data Int` declaration to the signature file.
>
>


We can write a few implementations for this function, which we do in the package `IntOrd-impls`:


```wiki
-- IntOrd-impls/IntOrd/Asc.hs
module IntOrd.Asc where
cmp :: Int -> Int -> Ordering  
cmp x y = compare x y

-- IntOrd-impls/IntOrd/Desc.hs
module IntOrd.Desc where
cmp :: Int -> Int -> Ordering
cmp x y = compare y x
```


It's worth noting that these are just normal module definitions:


```wiki
package IntOrd-impls where  
    includes: base  
    exposed-modules: IntOrd.Asc IntOrd.Desc  
```


That is to say, the signature is completely independent from the implementation.


>
>
> **Open question:** While they are independent, it is desirable to immediately check that `IntOrd.Asc` and `IntOrd.Desc` properly implement `IntOrd`.  However, it's probably not correct for `IntOrd-impls` to have a dependency on `IntOrd-sigs`; in general, no such dependency exists. 
>
>


Now, to actually write the sorted list module, we simply **import** the signature:


```wiki
-- SortedIntList/SortedIntList.hs
{-# LANGUAGE DeriveDataTypeable #-}
module SortedIntList(SortedIntList, empty, insert, toList) where

import Data.Typeable
import IntOrd

newtype SortedIntList = SL [Int]
    deriving (Show, Typeable)
 
empty :: SortedIntList
empty = SL []

insert :: Int -> SortedIntList -> SortedIntList
insert x (SL []) = SL [x]
insert x (SL ys@(y:ys')) =
    case cmp x y of
        GT -> SL (y : toList (insert x (SL ys')))
        _ -> SL (x : ys)

toList :: SortedIntList -> [Int]
toList (SL xs) = xs
```


The signature is pulled into scope by including the signature package:


```wiki
package SortedIntList where  
    includes: base, IntOrd-sig  
    exposed-modules: SortedIntList  
```


To use this "parametrized" implementation, we simply include `SortedIntList`, while stating what module `IntOrd` should be implemented with. (We also rename the module so we get distinct names for the different "implementations")


```wiki
executable Main where  
    includes: base  
              IntOrd-impls  
              SortedIntList (IntOrd as IntOrd.Asc,  
                             SortedIntList as SortedIntList.Asc)  
              SortedIntList (IntOrd as IntOrd.Desc,  
                             SortedIntList as SortedIntList.Desc)  
    main-is: Main
```


We can finally write a little test program which exercises our implementation:


```wiki
-- Main.hs
import qualified SortedIntList.Asc as A
import qualified SortedIntList.Desc as D
import Data.Typeable
        
main = do
    let -- Unlike the newtype trick for normal type classes, you can use
        -- regular Ints directly
        x = 3 :: Int
        y = 2 :: Int
        as = A.insert x (A.insert y A.empty)
        ds = D.insert x (D.insert y D.empty)
    -- Yes it works...
    print as
    print ds
    -- ...No, they're not type equal!
    print (cast as :: Maybe D.SortedIntList)
```


Here is the full Backpack file:


```wiki
-- sorted.bkp
package IntOrd-sig where  
    includes: base  
    exposed-signatures: IntOrd  
  
package SortedIntList where  
    includes: base, IntOrd-sig  
    exposed-modules: SortedIntList  
  
package IntOrd-impls where  
    includes: base  
    exposed-modules: IntOrd.Asc IntOrd.Desc  
  
executable Main where  
    includes: base  
              IntOrd-impls  
              SortedIntList (IntOrd as IntOrd.Asc,  
                             SortedIntList as SortedIntList.Asc)  
              SortedIntList (IntOrd as IntOrd.Desc,  
                             SortedIntList as SortedIntList.Desc)  
    main-is: Main
```


You can compile with:


```wiki
ghc --backpack sorted.bkp
```

>
>
> **Remark:** It is interesting to observe how Haskell went down an evolutionarily different path in order to support the case of multiple instances on a single data type.  In Haskell'98, the way to reuse the `insert` function with different `Ord` instances is to either use the non-overloaded version `insertBy`; in modern GHC Haskell you can newtype `Int`, and use the new `coerce` function described in [Roles](roles) in order to get from a `[Int]` to a `[DescInt]` in constant time.
>
>

