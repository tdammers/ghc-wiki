
GHC's support for Unicode is not as good as it could be. This page collects relevant tickets:


## FFI spec behaviour for CString


- [\#5061](http://gitlabghc.nibbler/ghc/ghc/issues/5061): Implement FFI spec behaviour for \*CString family

- [\#1414](http://gitlabghc.nibbler/ghc/ghc/issues/1414): CString marshalling functions do not perform the specified conversion

## Not decoding/encoding file names when we should be


- [\#3309](http://gitlabghc.nibbler/ghc/ghc/issues/3309): getArgs should return Unicode on Unix

- [\#3308](http://gitlabghc.nibbler/ghc/ghc/issues/3308): getArgs should return Unicode on Windows

- [\#3307](http://gitlabghc.nibbler/ghc/ghc/issues/3307): System.IO and System.Directory functions not Unicode-aware under Unix

- [\#4006](http://gitlabghc.nibbler/ghc/ghc/issues/4006): System.Process doesn't encode its arguments

- [\#4855](http://gitlabghc.nibbler/ghc/ghc/issues/4855): Debug.Trace.trace mangles Unicode strings

## More encoding support


- [\#3977](http://gitlabghc.nibbler/ghc/ghc/issues/3977): Support double-byte encodings (Chinese/Japanese/Korean) on Windows

## Unicode API issues


- [\#4471](http://gitlabghc.nibbler/ghc/ghc/issues/4471): Incorrect Unicode output on Windows Console

- [\#3569](http://gitlabghc.nibbler/ghc/ghc/issues/3569): ghci can't handle utf-8 chinese char correctly when modify

## Misc


- [\#2507](http://gitlabghc.nibbler/ghc/ghc/issues/2507): quotation characters in error messages

- [\#5088](http://gitlabghc.nibbler/ghc/ghc/issues/5088): TextEncoding iconv instances are shared between threads

## Interesting links


- Markus Kuhn's message about [
  UTF-8b](http://hyperreal.org/~est/utf-8b/releases/utf-8b-20060413043934/kuhn-utf-8b.html)


 


- Python's [ PEP-383](http://www.python.org/dev/peps/pep-0383/)
