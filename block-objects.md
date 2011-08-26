# FFI Support for C Block Objects



Apple recently [
proposed](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1370.pdf) the inclusion of lambda abstractions (closures) into C/C++/Objective-C and facilitated an implementation in the `clang` compiler framework.  They called this language extension *blocks* (or *block objects*).  It is widely used in the APIs of OS X 10.6 (Snow Leopard) and 10.7 (Lion).  This page is about extending the Haskell 2010 FFI to directly support blocks — i.e., to enable Haskell functions to be marshalled as blocks to C and to enable C blocks to be marshalled as Haskell functions to Haskell land.  This extension will be enabled by the language option `BlockObjects`.


## Example: passing a Haskell functions as an argument



As an example, consider the library function [
qsort\_b](http://developer.apple.com/library/mac/#documentation/darwin/reference/manpages/man3/qsort_b.3.html):


```wiki
void
qsort_b(void *base, size_t nel, size_t width, int (^compar)(const void *, const void *));
```


In C, we might use this function as described in Apple's introduction to blocks: [
Using a Block Directly](http://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/Blocks/Articles/bxGettingStarted.html#//apple_ref/doc/uid/TP40007502-CH7-SW2).  We would like to be able to do the same in Haskell by declaring:


```wiki
foreign import ccall "stdlib.h" qsort_b 
  :: Ptr (StablePtr a)                                    -- pointer to array of stable Haskell values
  -> CSize                                                -- size of the array
  -> CSize                                                -- size of a stable pointer to Haskell
  -> (Ptr (StablePtr a) -> Ptr (StablePtr a) -> IO Int)   -- comparison function
  -> IO ()

myCharacters = ["TomJohn", "George", "Charles Condomine"]
```


Note that the arguments to the comparison function are pointers to the array elements that need to be compared.  Each of these array elements is, in turn, a stable pointer to a Haskell thunk.



When then use the C function from Haskell as follows:


```wiki
do {   -- convert a list of strings into a C array of stable pointers to those strings in the Haskell heap
   ; ptrs       <- mapM newStablePtr myCharacters
   ; sortedPtrs <- withArray ptrs $ \myCharactersArray -> do
       {
           -- get the size in bytes of a stable pointer to a Haskell string
       ; let elemSize = fromIntegral $ sizeOf (undefined :: StablePtr String)

           -- invoke C land 'qsort_b' with a Haskell comparison function passed as a block object; mutates 'myCharactersArray'
       ; qsort_b myCharactersArray (genericLength myCharacters) elemSize
           (\l r -> do { l <- deRefStablePtr =<< peek lPtr
                       ; r <- deRefStablePtr =<< peek rPtr
                       ; return $ fromOrdering (l `compare` r)
                       })

           -- obtain the sorted list of stable pointers from the sorted array
       ; peekArray (length ptrs) myCharactersArray
       }

      -- turn the array of Haskell strings back into a list of strings
   ; mySortedCharacters <- mapM deRefStablePtr sortedPtrs
   }
```


Here we compare entire strings and not just the first characters as in the C implementation.  The marshalling function `fromOrdering` is defined as follows:


```wiki
fromOrdering :: Ordering -> Int
fromOrdering LT = -1
fromOrdering EQ = 0
fromOrdering GT = 1
```

## Example: returning a C block



Conversely, a C block object can be used as a function in Haskell.  Given the following C prototype


```wiki
typedef void (^callback_t)(int);

callback_t 
get_callback (void);
```


assume the FFI declaration


```wiki
foreign import ccall get_callback :: IO (CInt -> IO ())
```


We might use the imported C function as follows:


```wiki
do
  callback <- get_callback
  callback 42
```


**TODO** Is there a better example? Something from an official API?


## Storage management



**TODO** How do we recover a Haskell function's storage once the function has been turned into a block object and passed to a C function?  (NB: the environment of the function may hold on to large data structures, which will only be freed once the function is freed.)



When we marshal a C block object into a Haskell function, we need to ensure that the Haskell storage manager releases the block object (with `Block_release()`) once the Haskell land function becomes unreachable in the Haskell heap.


## The gory details



The following subpages provide details on implementing this functionality.  **(The following four subpages are still stubs.)**


- [Detailed specification of the language extension](block-objects/specification) 
- [Explicitly marshalling block objects between C and Haskell land](block-objects/faking-it)
- [Extending GHC](block-objects/extending-ghc)
- [An extended example: Using XPC](block-objects/using-xpc)

## Background


- [
  Blocks Programming Topics (Apple)](http://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/Blocks/Articles/00_Introduction.html)
- [
  Block Implementation Specification (Apple via clang)](http://clang.llvm.org/docs/Block-ABI-Apple.txt)
- [
  N1370: Apple’s Extensions to C](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1370.pdf)
- [
  Some background on the implementation of blocks](http://cocoawithlove.com/2009/10/how-blocks-are-implemented-and.html)
- [
  Dynamic generation of method implementations with blocks](http://www.friday.com/bbum/2011/03/17/ios-4-3-imp_implementationwithblock/)
