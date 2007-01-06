# Functions that operate on Integer values



*Note*: this is a very rough overview of the Integer functions in GHC.  It is not yet even a sketch of functions that a replacement library should implement, although the [Primitive Functions (in Cmm or C)](replacing-gmp-notes/required-integer-functions#) provide an overview of what GMP currently provides.


## GHC Functions


<table><tr><th> **Module/Class** </th>
<th> **Haskell Function** </th>
<th> **Notes** 
</th></tr>
<tr><th> GHC.Num </th>
<th> (+) </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> (-) </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> (\*) </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> div </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> negate </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> abs </th>
<th>  
</th></tr>
<tr><th> GHC.Num </th>
<th> signum </th>
<th>  
</th></tr>
<tr><th> GHC.Num </th>
<th> subtract </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> even </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> odd </th>
<th>  
</th></tr>
<tr><th> GHC.Num </th>
<th> gcd </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> lcm </th>
<th> rts/PrimOps.cmm --\> Haskell 
</th></tr>
<tr><th> GHC.Num </th>
<th> `(^)` </th>
<th> need exceptions for overflow 
</th></tr>
<tr><th> GHC.Num </th>
<th> `(^^)` </th>
<th> need exceptions for overflow 
</th></tr>
<tr><th> GHC.Num </th>
<th> zeroInteger </th>
<th> (see other primitives below)  
</th></tr>
<tr><th> GHC.Num(Eq) </th>
<th> \< </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Eq) </th>
<th> \<= </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Eq) </th>
<th> \> </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Eq) </th>
<th> \>= </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Eq) </th>
<th> == </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> succ </th>
<th> simple addition (+) 
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> pred </th>
<th> simple subtraction (-) 
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> toEnum </th>
<th> int2Integer (primitive) 
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> fromEnum </th>
<th> integer2Int (primitive) 
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> enumFrom </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> enumFromThen </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> enumFromTo </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Enum) </th>
<th> enumFromThenTo </th>
<th>  
</th></tr>
<tr><th> GHC.Num(Show) </th>
<th> showsPrec </th>
<th> jtos (internal Haskell function) 
</th></tr>
<tr><th> GHC.Num(Show) </th>
<th> showList </th>
<th> jtos (internal Haskell function) 
</th></tr>
<tr><th> GHC.Float </th>
<th> floatRadix </th>
<th> rts/StgPrimFloat.c 
</th></tr>
<tr><th> GHC.Float </th>
<th> encodeFloat </th>
<th> rts/StgPrimFloat.c --\> foreign import GHC.Float 
</th></tr>
<tr><th> GHC.Float </th>
<th> decodeFloat </th>
<th> rts/StgPrimFloat.c --\> rts/Rts.h --\> PrimOps.cmm 
</th></tr>
<tr><th> GHC.Float </th>
<th> encodeDouble </th>
<th> rts/StgPrimFloat.c --\> foreign import GHC.Float 
</th></tr>
<tr><th> GHC.Float </th>
<th> decodeDouble </th>
<th> rts/StgPrimFloat.c --\> rts/Rts.h --\> PrimOps.cmm 
</th></tr>
<tr><th> Data.Bits </th>
<th> (.&.) </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> (.\|.) </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> xor </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> complement </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> shift </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> rotate </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> bit </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> setBit </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> clearBit </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> complementBit </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> testBit </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> bitSize </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> isSigned </th>
<th>  
</th></tr>
<tr><th> Data.Bits </th>
<th> shiftL </th>
<th> Haskell function 
</th></tr>
<tr><th> Data.Bits </th>
<th> shiftR </th>
<th> Haskell function 
</th></tr>
<tr><th> Data.Bits </th>
<th> rotateL </th>
<th> rotates operate as shifts for Integer 
</th></tr>
<tr><th> Data.Bits </th>
<th> rotateR </th>
<th> rotates operate as shifts for Integer 
</th></tr>
<tr><th> StringBuffer </th>
<th> parseInteger </th>
<th> Haskell function 
</th></tr></table>


## Primitive Functions (in Cmm or C)


<table><tr><th> **Module/Class** </th>
<th> **Haskell Function** </th>
<th> **Notes** 
</th></tr>
<tr><th> GHC.Prim </th>
<th> integer2Int\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> integer2Word\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> int2Integer\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> word2Integer\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> int64ToInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> word64ToInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> decodeDouble\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> decodeFloat\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> plusInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> minusInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> timesInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> gcdInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> gcdIntegerInt\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> divExactInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> quotInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> remInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> cmpInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> cmpIntegerInt\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> quotRemInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> divModInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> andInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> orInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> xorInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Prim </th>
<th> complementInteger\# </th>
<th> from rts/PrimOps.cmm 
</th></tr>
<tr><th> GHC.Float </th>
<th> floatRadix </th>
<th> rts/StgPrimFloat.c 
</th></tr>
<tr><th> GHC.Float </th>
<th> encodeFloat </th>
<th> rts/StgPrimFloat.c --\> foreign import GHC.Float 
</th></tr>
<tr><th> GHC.Float </th>
<th> decodeFloat </th>
<th> rts/StgPrimFloat.c --\> rts/Rts.h --\> PrimOps.cmm 
</th></tr>
<tr><th> GHC.Float </th>
<th> encodeDouble </th>
<th> rts/StgPrimFloat.c --\> foreign import GHC.Float 
</th></tr>
<tr><th> GHC.Float </th>
<th> decodeDouble </th>
<th> rts/StgPrimFloat.c --\> rts/Rts.h --\> PrimOps.cmm 
</th></tr></table>


