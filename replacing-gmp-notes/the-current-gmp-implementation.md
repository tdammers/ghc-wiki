# The Current GMP Implementation



Esa Ilari Vuokko, who at one time attempted to replace GMP with [
LibTomMath](http://math.libtomcrypt.com/), posted several messages with good notes on the current implementation.  Much of what is on this page is derived from those notes.  See, [
Replacement for GMP(3)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010669.html) and [
Replacement for GMP(4)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010674.html).



[](/trac/ghc/attachment/wiki/ReplacingGMPNotes/TheCurrentGMPImplementation/GMP_interface.jpg)


### GMP and the Storage Manager



The most insidious and unique feature of the GHC implementation with GMP is memory management.  GHC uses the special memory functions in GMP ( `mp_set_memory_functions( (*alloc)(), (*realloc)(), (*dealloc)()` ) to let GMP use GHC's own garbage collector.  The memory functions are implemented in [rts/sm/Storage.c](/trac/ghc/browser/ghc/rts/sm/Storage.c) as:


```wiki
	  static void* stgAllocForGMP (size_t size_in_bytes);
	  static void  stgDeallocForGMP (void *ptr STG_UNUSED, 
		  			 size_t size STG_UNUSED);
	  static void* stgReallocForGMP (void *ptr, 
					 size_t old_size, 
					 size_t new_size);
```


These special allocation functions bring most GMP memory usage into the GHC heap but do not seem that efficient otherwise.  (I could be wrong --PDT.)  Allocation uses the internal `allocate()` interface, so no garbage collection is performed during a GMP operation.  Note that GMP operations may use these functions to allocate more memory themselves, after being called from Haskell code.  The memory allocated is a simple array of words (`W_`), rounded up to a whole number.


### Special Functions



GHC adds its own functions for string conversion, least common multiple (lcm) and conversion to and from floating point (floats and doubles).  In particular, the GHC designers decided to perform their own operations for encoding a GMP number (really, the array of mp\_limb\_t) to floating point numbers (floats or doubles).  GMP provides functions for some of these operations:


<table><tr><th> **Operation** </th>
<th> **GMP function** </th>
<th> **Notes** 
</th></tr>
<tr><th> convert to double </th>
<th> mpz\_get\_d() </th>
<th> rounds toward zero, avoids overflow 
</th></tr>
<tr><th> convert to double
return exponent separately </th>
<th> mpz\_get\_d\_2exp() </th>
<th> rounds toward zero, avoids overflow 
</th></tr>
<tr><th> convert to float </th>
<th> mpz\_get\_f()
</th>
<th></th></tr>
<tr><th> convert to string
supports bases from 2 to 36 </th>
<th> mpz\_get\_str() </th>
<th> more efficient than Haskell version,
with PackedString, no conversion to Haskell string necessary 
</th></tr>
<tr><th> conversion from string </th>
<th> mpz\_inp\_str() </th>
<th> more efficient than Haskell version,
with PackedString, input string efficiently convertable to a c\_str
(it is already an array of chars) 
</th></tr></table>



The real problem (no pun intended) is conversion to doubles and floats.  The `__encodeDouble` function, in [rts/StgPrimFloat.c](/trac/ghc/browser/ghc/rts/StgPrimFloat.c), may overflow:


```wiki
StgDouble
__encodeDouble (I_ size, StgByteArray ba, I_ e) /* result = s * 2^e */
{
    StgDouble r;
    const mp_limb_t *const arr = (const mp_limb_t *)ba;
    I_ i;

    /* Convert MP_INT to a double; knows a lot about internal rep! */
    for(r = 0.0, i = __abs(size)-1; i >= 0; i--)
	r = (r * GMP_BASE) + arr[i];

    /* Now raise to the exponent */
    if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
	r = ldexp(r, e);

    /* sign is encoded in the size */
    if (size < 0)
	r = -r;

    return r;
}
```


If the problem section hurt your eyes, here is some more pain to drive the point home:


```wiki
    /* __abs(size) is the size of the array --PDT */
    for(r = 0.0, i = __abs(size)-1; i >= 0; i--)
	r = (r * GMP_BASE) + arr[i];
```


Note the problems:


- GMP numbers may be very large, much greater than the maximum value for a float (addition and multiplication operations may overflow); 
- there is no specification of rounding mode--this operation relies on whatever hardware rounding may take effect; and,
- certain floating point hardware exceptions (traps) for overflow or underflow may be triggered (though this is not generally the case; note: on x86 machines floating point exceptions may not be triggered but the resulting float may denormalized).


Nowhere in this function is there a check for whether the GMP number (the `const mp_limb_t *arr`) may be greater than the size of a double and either warn the user, possibly with an `ArithException`, or round the resulting double toward zero.  



Compare the problem section in `__encodeDouble` to the exponent check in the internal GMP function, `mpn_get_d`, that avoids overflow by comparing the relative magnitude of the GMP number with the maximum magnitude of the double:


```wiki
   /* note: exp is a long int, given as an argument to min_get_d as 0L --PDT */
  /* Adjust exp to a radix point just above {ptr,size}, guarding against
     overflow.	After this exp can of course be reduced to anywhere within
     the {ptr,size} region without underflow.  */
  if (UNLIKELY ((unsigned long) (GMP_NUMB_BITS * size)
		> (unsigned long) (LONG_MAX - exp)))
    {
      if (_GMP_IEEE_FLOATS)
	goto ieee_infinity;

      /* generic */
      exp = LONG_MAX;
    }
  else
    {
      exp += GMP_NUMB_BITS * size;
    }
```


(If you want to see the full `mpn_get_d` function, it is in the file `[toplevel gmp source]/mpn/generic/get_d.c` .)  



There is no check in the Haskell code *using* `__encodeFloat` or `__encodeDouble`, in [libraries/base/GHC/Float.lhs](/trac/ghc/browser/ghc/libraries/base/GHC/Float.lhs).  For example, the `encodeFloat` Haskell function under class `RealFloat` uses `__encodeDouble` directly:


```
encodeFloat (J# s# d#) e = encodeDouble# s# d# e
```



This is not the case with the safer `fromRat` function which ensures the Integer falls in the range of the mantissa (see the source code).



The problem with `__encodeFloat` and `__encodeDouble` is normative--a matter of design.  The current implementation places the responsibility for ensuring that large integers converted to floating point numbers do not overflow or underflow and it is practically impossible (with rounding) to join encoding and decoding and come out with a reasonably equivalent result, as in:


```
(uncurry encodeFloat) . decodeFloat
```



(Note: this is a standard problem with floating point.  A replacement library could probably do little better.)



A replacement library for GMP might use the GMP strategies of including a special bitwise conversion (with appropriate masks) and a hardware-based version.  An unconventional solution might perform the rounding manually (but with relatively portable predictability) using interval arithmetic.  



As for printing GMP numbers, [libraries/base/GHC/Num.lhs](/trac/ghc/browser/ghc/libraries/base/GHC/Num.lhs) defines a special internal function `jtos` to handle `showsPrec` and `showList` for `Integer` as an instance of the class `Show`.  The `jtos` function uses the primitive GMP-based function `quotRemInteger` and performs many conversions from Integer to Int.  This is not as efficient as the internal GMP functions, especially for large numbers, because each conversion allocates extra storage for GMP as noted in [Replacing GMP -- Optimisation Opportunities](replacing-gmp-notes#).


### GMP Library Implementation



The GMP library, like most multi-precision libraries has a fundamental limitation that might seem odd if you are only familiar with Haskell--not likely, but it bears mention anyway!  GMP functions require that their operands be separate entities.  (Remember: an operation such as `a + b` has three entities: the two operands and the result.)  That is, if you want to add `mpz_t a` to `mpz_t b`, and place the result in `mpz_t c`, you are fine but if you try to add `a` to `a` you will run into trouble.  (The problem is, the multi-precision integer library functions are designed so the operands cannot alias; this is more of a problem for complex operations such as multiplication than simple operations like addition.)  This limitation might be overcome by designing the API differently, for example:


1. compare the operands in Haskell, Cmm or whatever before you pass them to the \_add function; 
1. if the operands are the same, create a separate copy of the operand
1. -- you are o.k. for the case where two operands cannot be the same.  


For the case where one of the operands and the result is the same you would have to check outside the general Haskell, Cmm or whatever function wrapping the \_add function--you would seem to need a higher level construct.  This is already handled by the Haskell language, since `Integer`s are subject to the same functional properties as other non-mutable variables.  There does seem to be a problem with `Int`s and `Integer`s in GHCi--I (PDT) am not sure whether this is a naming problem (the name after `let` is a binding-name) or an evaluation problem.  Here is an example:


```

  (in GHCi)
  > let m_integer = 123456789 :: Integer
  > let n_integer = m_integer + m_integer
  -- this will show o.k., but the implementation involves *always* making extra temporaries
  > n_integer
  246913578

  -- a = a + a
  > let m_integer = m_integer + m_integer
  --"m_integer" after "let" here is a new binding
  > let n_integer = m_integer + m_integer

  -- this will be fine until you try to evaluate it:
  > n_integer
  *** Exception: stack overflow

  -- now, try the same thing with plain Ints:
  -- this will default to an Int
  > let a = 5
  > let a = a + a
  > let b = a + a
  > b
  *** Exception: stack overflow
```


