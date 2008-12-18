# Performance Measurements of other Multi-Precision Libraries



The benchmarks below were made with unmodified multi-precision libraries for Integral Arithmetic compiled using Apple gcc 4.0.1 with optimisation settings: -O3 -ftree-vectorize -falign-loops=16.  The tests performed Multiplication, Squaring, Powers (up to 7) and Division each 1,000,000 times at the base level of bit-precision (the number of bits in the operands).  Higher levels of precision performed incrementally fewer rounds: the base level (1,000,000 / (i \* 3)) where i is the number for the round, incremented from 0.  For example, at a bit-precision of 512 (second bit-precision test), the number of rounds was (1,000,000 / (1 \* 3)) = (1,000,000 / 3) = 333,333 rounds.  Multi-precision libraries may use unsigned chars, unsigned ints, unsigned long ints, unsigned long long ints or doubles, so the actual number of "words" in each multi-precision array may differ; for multi-precision real numbers using doubles, integer precision was calculated at 48.3 bits of real precision per double, rounded up to 49.  (49 bits conservatively equates to about 9 decimal digits of precision, see, e.g., [
What Every Computer Scientist Should Know about Floating-Point Arithmetic](http://docs.sun.com/source/806-3568/ncg_goldberg.html).)  Libraries tested were:


- [ ARPREC](http://crd.lbl.gov/~dhbailey/mpdist/) 
- [ OpenSSL's BN](http://www.openssl.org/) (part of libcrypto)
- [ GMP](http://swox.com/gmp/)
- [ LibTomMath](http://math.libtomcrypt.com/)
- [
  Crypto++](http://www.eskimo.com/~weidai/cryptlib.html) a cryptographic library in C++, the Integer class
- [ Botan](http://botan.randombit.net/) a cryptographic library in C++, 
- [ MPI](http://www.cs.dartmouth.edu/~sting/mpi/)
- [ MAPM](http://www.tc.umn.edu/~ringx004/mapm-main.html)


Crypto++, Botan, MPI and MAPM showed performance far below ARPREC, OpenSSL's BN, GMP and LibTomMath, so results are only shown for the last four.  Note that there are other libraries available for arbitrary precision arithmetic other than those mentioned or tested here.  Most of those other libraries are licensed under the GPL, while the remainder, such as the [
decNumber](http://www2.hursley.ibm.com/decimal/decnumber.html) library (free, under the [
ICU license](http://source.icu-project.org/repos/icu/icu/trunk/license.html)) are designed and tuned for operations that would be difficult to translate into Haskell's Integer primitive.



[](/trac/ghc/attachment/wiki/ReplacingGMPNotes/PerformanceMeasurements/Multiplication.gif)



[](/trac/ghc/attachment/wiki/ReplacingGMPNotes/PerformanceMeasurements/Squaring.gif)



[](/trac/ghc/attachment/wiki/ReplacingGMPNotes/PerformanceMeasurements/Powers_log10.gif)



[](/trac/ghc/attachment/wiki/ReplacingGMPNotes/PerformanceMeasurements/Division.gif)


