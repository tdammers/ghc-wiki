# Haskell Objective-C FFI


## Goals



We aim for an extension of the Standard Haskell Foreign Function Interface to include bindings to Objective-C.  We like to make it easy to embed Haskell code in Objective-C programs as well as the other way around.  Moreover, we like to be able to use object hierarchies stored in nib files directly in Haskell code.


### Consequences of the goals



We need to be able to subclass Objective-C classes (and in that process overwrite methods, add new class and instance methods, and add properties).  We only directly support properties with setter and getter methods and not direct access to ivars; although, the latter can be gained by using the standard C FFI.


### Pragmatics



As far as the use of Haskell code from Objective-C goes, we can imagine three kinds of implementations: (1) We can make everything fully dynamic through the ObjC runtime at application runtime or (2) we can generate `.m` stubs statically during application compile time (from within GHC), and (3) we can directly generate object files that adhere to the conventions for ObjC object files.



Option (1) has the disadvantage that all class initialisation code needs to be executed after the program has loaded and before any other code runs.  We need to arrange that for dynamically loaded objects, too.  Option (2) and (3) lead to a similar outcomes (in terms of object files), but Option (2) seems easier to implement and has some precedent in the C stubs generated for Haskell functions dynamically exported to C.



We require the programmer to supply headers for all Objective-C classes implemented in Haskell.  This enables the Objective-C compiler to do type checking and enables Interface Builder to recognise properties used as outlets.


## Architecture



Following the structure of the C FFI, the basic Objective-C FFI has two major components: (1) an extension of the Haskell language by new forms of `foreign` declarations and (2) a standard library.  The C FFI uses the `ccall` (and `stdcall`) calling convention for `foreign` declarations, whereas Objective-C uses the new `objc` calling convention.  The C-specific FFI library is `Foreign.C` (aka `CForeign`) and, for Objective-C, we use `Foreign.ObjectiveC`.



In addition to the low-level FFI, higher-level libraries and tools may provide more convenient APIs.  In particular, the basic FFI makes no attempt to model Objective-C's class hierarchy and overloading of selectors in Haskell's type system.  This is the responsibility of higher-level libraries.


## Subtopics



We discuss the following subtopics on separate pages:


- [Foreign declarations for Objective-C](objective-c/foreign-declarations)
- [Sending messages](objective-c/messaging)
- [Objective-C classes](objective-c/classes)
- Categories & protocols
- [Naming conventions](objective-c/naming) (they are not enforced, but recommended)
- [Memory management](objective-c/memory-management) 
- `Foreign.ObjectiveC`


We also have some basic ideas for type-safe usage of Objective-C above the FFI layer:


- [Type-safe messaging](objective-c/type-safe-messaging)
- How to deal with subclassing and existentials in Haskell code
- On-the-fly class creation

## Background and related work


- [
  The Haskell 98 Foreign Function Interface 1.0](http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffi.html)
- [
  Objective-C 2.0 Runtime Programming Guide](http://developer.apple.com/mac/library/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Introduction/Introduction.html)
- [
  Objective-C 2.0 Runtime Reference](https://developer.apple.com/documentation/Cocoa/Reference/ObjCRuntimeRef/Reference/reference.html)
- [ Chicken scheme objc egg](http://chicken.wiki.br/objc)
- [
  Tim Scheffler's three part article series on writing a Cocoa app in Haskell](http://tscheff.blogspot.com/2010/03/cocoa-application-almost-completely.html)

## Development team


- [ Manuel M. T. Chakravarty](http://www.cse.unsw.edu.au/~chak/)
- [ André Pang](http://algorithm.com.au/)


Please send us any suggestions or comments that you might have.


