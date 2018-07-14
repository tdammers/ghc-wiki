# Backpack



This is the launch page for Backpack, actively maintained by Edward (as of Nov 2017). 



Backpack is a system for retrofitting Haskell with an applicative, mix-in module system. It has been implemented in GHC 8.2 and cabal-install 2.0, but it is [
not supported by Stack](https://github.com/commercialhaskell/stack/issues/2540).



The documentation for how to use Backpack is a bit scattered about at this point, but here are useful, up-to-date (as of 2017-04-02, prior to GHC 8.2's release) references:


- This pair of blog posts: Try Backpack, [
  ghc --backpack](http://blog.ezyang.com/2016/10/try-backpack-ghc-backpack/) and [
  Cabal packages](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/) have up-to-date tutorials for using the main features of Backpack, with and without Cabal.

- The [
  GHC manual section on module signatures](https://downloads.haskell.org/~ghc/master/users-guide/separate_compilation.html#module-signatures) gives the gory details about how Backpack's signature files (hsig) work. A more user-friendly version of this can be found on [
  Haskell wiki "Module signature"](https://wiki.haskell.org/Module_signature)

- There is not yet a manual entry in Cabal for how Cabal works. This section is under development.

- Edward Z. Yang's [
  thesis](https://github.com/ezyang/thesis/releases) contains detailed information about the specification and implementation of Backpack. We also have an older [
  paper draft](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf) which was submitted to ICFP'16. History nuts can also read the original [
  POPL paper](http://plv.mpi-sws.org/backpack/) but note that Backpack has changed dramatically since then.

- Hackage does not yet support uploads of Backpack-using packages. [
  next.hackage](http://next.hackage.haskell.org:8080/) is a Hackage instances running a development branch of Hackage that can handle Backpack; for now, Backpack-enabled packages should be uploaded here.


You might find it useful to find some code using Backpack.  Here are the biggest examples worth looking at:


- [
  backpack-str](https://github.com/haskell-backpack/backpack-str) defines a signature and implementations for strings. It is quite comprehensive, and the packages are available on next.hackage.

- [
  coda](https://github.com/ekmett/coda) parametrizes over a few core data types including notions of "delta". It takes advantage of zero-cost abstraction, which lets it split into multiple data types, while still ensuring they are UNPACKed in the end.

- [
  streamy](https://github.com/danidiaz/streamy) defines a signature and implementations for "streaming" libraries (e.g., conduit, pipes and streaming).

- [
  haskell-opentracing](https://github.com/ocharles/haskell-opentracing) defines a signature for the OpenTracing standard, a middleware built on top of this signature, and (at the moment) a single backend to Jaeger.

- [
  reflex-backpack](https://github.com/ezyang/reflex-backpack) is a kind of crazy experiment at Backpack'ing Reflex.  Reflex uses a lot of advanced GHC features and it took some coaxing to get Backpack to handle it all, but handle it all it did!


Some more out-of-date documents:


- [
  Backpack specification](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst). This was subsumed by my thesis but once Backpack stabilizes it will be worth distilling the thesis PDF back into a more web-friendly format.

## Known gotchas



**Can I use this with Stack?** No, Backpack requires support from the package manager, and Stack integration has not been implemented yet.



**Can I use this with Template Haskell?** Yes, but you need GHC 8.2.2; GHC 8.2.1 has a critical bug which means that most real-world uses of TH will not work. See [
this issue](https://github.com/haskell/cabal/issues/4755) and [
this issue](https://github.com/haskell/cabal/issues/4865) for two examples of this occurring in the wild.



**Can I use this with the C preprocessor?** No, this is buggy (you'll get an error `<command line>: unknown package: hole`). See [\#14525](http://gitlabghc.nibbler/ghc/ghc/issues/14525) for the issue and a patch.



**Make sure cabal-version is recent enough.** ([
\#4448](https://github.com/haskell/cabal/issues/4448)) If you set the `cabal-version` of your package too low, you may get this error:


```wiki
Error:
    Mix-in refers to non-existent package 'pkg'
    (did you forget to add the package to build-depends?)
    In the stanza 'executable myexe'
    In the inplace package 'pkg'
```


This is because internal libraries are feature-gated by the `cabal-version` of your package. Setting it to `cabal-version: >= 2.0` is enough to resolve the problem.



**You can't instantiate a dependency with a locally defined module.** Consider the following package:


```wiki
library
  other-modules: StrImpl
  build-depends: foo-indef
  mixins: foo-indef requires (Str as StrImpl)
```


This looks like it should work, but actually it will fail:


```wiki
Error:
    Non-library component has unfilled requirements: StrImpl
    In the stanza 'library'
    In the inplace package 'mypkg-1.2'
```


The reason for this is Backpack does not (currently) support instantiating a package with a locally defined module: since the module can turn around and \*import\* the mixed in `foo-indef`, which would result in mutual recursion (not presently supported.)



To solve this problem, just create a new library to define the implementing module. This library can be in the same package using the convenience library mechanism:


```wiki
library str-impl
  exposed-modules: StrImpl

library
  build-depends: str-impl, foo-indef
  mixins: foo-indef requires (Str as StrImpl)
```


**How can I declare that a module implements a signature?**  In traditional Haskell style, you can write `x :: Type` to specify that a value `x` should have some type.  In Backpack, specifying that a module implements a signature is done out-of-line; you must create a third component to link them together (e.g., a test suite):


```wiki
test-suite implements
  type:                exitcode-stdio-1.0
  main-is:             Main.hs
  build-depends:
    base,
    foo-implementation,
    foo-sig
  default-language:    Haskell2010
```


A few notes about this encoding:


- If you need to specify that a module implements multiple signatures, you include all of those signatures in the same implements test or create separate implements tests.

- Being a test suite, this requires you to create a dummy `Main.hs` file (`main = return ()` is sufficient) and add a `base` dependency.  So why do we pick a test suite?  A test suite will ensure that you have in fact filled all of the holes of a `foo-sig`, whereas a regular library will happily pass through any unfilled holes, making it easy for you to think that a check has occurred when it has not.

- You might wonder if you can skip defining an extra test-suite by mixing in the signature package from the implementation package. Unfortunately, this runs afoul the "you can't instantiate a dependency with a local module" restriction. Additionally, this adds an extra spurious dependency to your package which is not actually needed.

## Backpack-related tickets



Backpack-related tickets are marked with keyword 'backpack'. If the ticket is assigned to ezyang, it means he's planning on working on it.




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: keywords: backpack, status: new, max: 0, col: id,
col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: keywords: backpack, status: new, max: 0, col: id,
col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: keywords: backpack, status: new, max: 0, col: id,
col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: keywords: backpack, status: new, max: 0,
col: id, col: type, col: summary, col: priority, col: owner, desc: 1,
order: priority)
      </th>
<th>
        
        Owner (Ticket query: keywords: backpack, status: new, max: 0, col: id,
col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#1409](http://gitlabghc.nibbler/ghc/ghc/issues/1409)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Allow recursively dependent modules transparently (without .hs-boot or anything)](http://gitlabghc.nibbler/ghc/ghc/issues/1409)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#9351](http://gitlabghc.nibbler/ghc/ghc/issues/9351)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [add ability to version symbols .c for packages with C code](http://gitlabghc.nibbler/ghc/ghc/issues/9351)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10749](http://gitlabghc.nibbler/ghc/ghc/issues/10749)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Boot file instances should imply superclasses](http://gitlabghc.nibbler/ghc/ghc/issues/10749)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10827](http://gitlabghc.nibbler/ghc/ghc/issues/10827)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [GHCi should support interpeting multiple packages/units with separate DynFlags](http://gitlabghc.nibbler/ghc/ghc/issues/10827)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#12703](http://gitlabghc.nibbler/ghc/ghc/issues/12703)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Expand Backpack's signature matching relation beyond definitional equality](http://gitlabghc.nibbler/ghc/ghc/issues/12703)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13151](http://gitlabghc.nibbler/ghc/ghc/issues/13151)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Make all never-exported IfaceDecls implicit](http://gitlabghc.nibbler/ghc/ghc/issues/13151)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13266](http://gitlabghc.nibbler/ghc/ghc/issues/13266)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Source locations from signature merging/matching are bad](http://gitlabghc.nibbler/ghc/ghc/issues/13266)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13469](http://gitlabghc.nibbler/ghc/ghc/issues/13469)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [-fdefer-type-errors for Backpack](http://gitlabghc.nibbler/ghc/ghc/issues/13469)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#14212](http://gitlabghc.nibbler/ghc/ghc/issues/14212)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Give better error message with non-supported Backpack/TH use](http://gitlabghc.nibbler/ghc/ghc/issues/14212)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#14478](http://gitlabghc.nibbler/ghc/ghc/issues/14478)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Abstract pattern synonyms (for hsig and hs-boot)](http://gitlabghc.nibbler/ghc/ghc/issues/14478)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#15391](http://gitlabghc.nibbler/ghc/ghc/issues/15391)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Maybe ghc-pkg register should unregister packages with "incompatible" signatures](http://gitlabghc.nibbler/ghc/ghc/issues/15391)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#15984](http://gitlabghc.nibbler/ghc/ghc/issues/15984)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Backpack accepts ill-kinded instantiations. Can cause GHC panic](http://gitlabghc.nibbler/ghc/ghc/issues/15984)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#16219](http://gitlabghc.nibbler/ghc/ghc/issues/16219)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Backpack - TH+indefinite module interface file error](http://gitlabghc.nibbler/ghc/ghc/issues/16219)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10266](http://gitlabghc.nibbler/ghc/ghc/issues/10266)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Split base for Backpack](http://gitlabghc.nibbler/ghc/ghc/issues/10266)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10681](http://gitlabghc.nibbler/ghc/ghc/issues/10681)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Teach GHC to interpret all hs files as two levels of hs-boot files (abstract types only/full types + values)](http://gitlabghc.nibbler/ghc/ghc/issues/10681)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#12680](http://gitlabghc.nibbler/ghc/ghc/issues/12680)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Permit type equality instances in signatures](http://gitlabghc.nibbler/ghc/ghc/issues/12680)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13149](http://gitlabghc.nibbler/ghc/ghc/issues/13149)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Giving Backpack a Promotion](http://gitlabghc.nibbler/ghc/ghc/issues/13149)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13262](http://gitlabghc.nibbler/ghc/ghc/issues/13262)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Allow type synonym family application in instance head if it has no free variables](http://gitlabghc.nibbler/ghc/ghc/issues/13262)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13361](http://gitlabghc.nibbler/ghc/ghc/issues/13361)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Better type synonym merging/subtyping for Backpack](http://gitlabghc.nibbler/ghc/ghc/issues/13361)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#13765](http://gitlabghc.nibbler/ghc/ghc/issues/13765)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [GHC cannot parse valid Haskell98 whose first identifier is named signature](http://gitlabghc.nibbler/ghc/ghc/issues/13765)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#14210](http://gitlabghc.nibbler/ghc/ghc/issues/14210)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [bkp files cannot find TemplateHaskell symbols (even without Backpack features)](http://gitlabghc.nibbler/ghc/ghc/issues/14210)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#10871](http://gitlabghc.nibbler/ghc/ghc/issues/10871)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Implement "fat" interface files which can be directly compiled without source](http://gitlabghc.nibbler/ghc/ghc/issues/10871)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th>[\#12717](http://gitlabghc.nibbler/ghc/ghc/issues/12717)</th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      [Permit data types in signatures to be implemented with equivalent pattern synonyms (and vice versa)](http://gitlabghc.nibbler/ghc/ghc/issues/12717)
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



