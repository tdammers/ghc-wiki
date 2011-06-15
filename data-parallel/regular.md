# DArrays - Haskell Support for Array Computations \[OUT OF DATE\]


>
>
> **This page has been superseded by the Repa paper in ICFP'10: [
> http://www.cse.unsw.edu.au/\~chak/papers/KCLPL10.html](http://www.cse.unsw.edu.au/~chak/papers/KCLPL10.html)**
>
>


The library provides a layer on top of DPH unlifted arrays to support multi-dimensional arrays, and shape polymorphic 
operations for fast sequential and parallel execution. The interface for delayed arrays is similar, but in contrast 
to operations on the former, any operation on a delayed array is only actually evaluated when elements are accessed outside the DArray framework.



The current implementation of the library exposes some implementation details the user shouldn't 
have to worry about. Once the design of the library is finalised, most of these will be hidden by distinguishing 
between internal types and representation types.


## Strict Arrays, Delayed Array and Shape Data Type



Both strict and delayed arrays are parametrised with their shape - that is, their dimensionality and size 
of each dimension. The shape type class has the following interface:


### DArrays



DArrays are collections of values of \`primitive' type, which are
member of the class Data.Parallel.Array.Unlifted.Elt, which includes
all basic types like Float, Double, Bool, Char, Integer, and pairs
constructed with Data.Parallel.Array.Unlifted.(:\*:), including ().


### The Shape class



Values of class Shape serve two purposes: they can describe the dimensionality and 
size of an array (in which case we refer to them as 'shape'), and they can also refer 
to the position of a particular element in the array (in which case we refer to them
as an 'index'). It provides the following methods:


```wiki
class (Show sh, U.Elt sh, Eq sh, Rebox sh) => Shape sh where
  dim   :: sh -> Int           
  -- ^number of dimensions (>= 0)
  size  :: sh -> Int           
  -- ^for a *shape* yield the total number of  elements in that array
  toIndex :: sh -> sh -> Int     
  -- ^corresponding index into a linear representation of 
  -- the array (first argument is the shape)

  fromIndex:: sh -> Int -> sh   
  -- ^given index into linear row major representation, calculates 
  -- index into array                               

  range      :: sh -> U.Array sh
  -- ^all the valid indices in a shape. The following equality should hold: 
  -- map (toIndex sh) (range sh) = [:0..(size sh)-1:]

  
  inRange      :: sh -> sh -> Bool
  -- ^Checks if a given index is in the range of an array shape. I.e.,
  -- inRange sh ind == elem ind (range sh)

  zeroDim      :: sh
  -- ^shape of an array of size zero of the particular dimensionality

  intersectDim :: sh -> sh -> sh
  -- ^shape of an array of size zero of the particular dimensionality  

  next:: sh -> sh -> Maybe sh
  -- ^shape of an array of size zero of the particular dimensionality    
```


Note that a `Shape` has to be in the type class `Elt` imported from `Data.Parallel.Array.Unboxed` so 
that it can be an element of  `Data.Parallel.Array.Unboxed.Array`.



The following instances are defined


```wiki
instance Shape () 
instance Shape sh => Shape (sh :*: Int) 
```


so we have inductively defined n-tuples of `Int` values to represent shapes. This somewhat unusual representation
is necessary to be able to inductively define operations on `Shape`. It should, however, be hidden from the library
user in favour of the common tuple representation.



The multiparameter type class `Subshape sh sh'` contains all pairs of shapes `sh` and `sh'`, for which the dimensionality of `sh'` is
less than that of `sh`.  


```wiki
class (Shape sh, Shape sh') => Subshape sh sh' where
  addDim     :: sh -> sh' -> sh    
  modDim     :: sh -> sh' -> sh    
  inject     :: sh -> sh' -> sh
```


The method `addDim` adds the sizes of two shapes (or positions of two indices). If `sh'` is a strict subshape of
`sh`, the fields of `sh` are copied when no corresponding fields of `sh'` exist, accordingly for `modDim`


### Representation, Order of Elements, and Lifted Values



As mentioned when introducing the functions `toIndex` and `range`, the following relationship should hold:


```wiki
map (toIndex sh) (range sh) = [:0..(size sh)-1:]
```


this means that, for example


```wiki
range (() :*: 2 :*: 3) =
   [() :*: 0 :*: 0, () :*: 0 :*: 1,  ....., () :*: 0 :*: 2, () :*: 1 :*: 0,....
```

## Operations on Arrays and Delayed Arrays


### Array Creation and Conversion



Strict arrays are simply defined as record containing a flat data array and shape information:


```wiki
data Array dim e where
  Array:: { arrayShape    :: dim                -- ^extend of dimensions
          , arrayData     :: U.Array e          -- flat parallel array
           }  -> Array dim e
  deriving Show

toArray  :: (U.Elt e, Shape dim) => dim -> U.Array e -> Array dim e
fromArray:: (U.Elt e, Shape dim) => Array dim e -> U.Array e 
```


Delayed arrays, in contrast, in addition to the shape, only contain a function which, given an index,
yields the corresponding element.


```wiki
data DArray dim e where 
  DArray :: {dArrayShape::dim -> dArrayFn:: (dim -> e)} -> DArray dim e
```


Delayed arrays can be converted to and from strict arrays:


```wiki
toDArray:: (U.Elt e, Array.Shape dim)   => Array.Array dim e -> DArray dim e
fromDArray:: (U.Elt e, Array.Shape dim) => DArray dim e      -> Array dim e
```


the result of `toDArray` is a DArray which contains an indexing function into 
an array. In general, the function `dArrayFn` can be much more complex.  The function 
`forceDArray`  (should this be called `normalizeDArray`?) forces the evaluation `dArrayFn` on
every index of the range, and replaces `dArrayFn` by a simple indexing function into an array
of the result values. 


```wiki
forceDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> DArray dim e
```


Singular (zero-dimensional) arrays are isomorphic to scalar values and can be converted to 
one using the following function: 


```wiki
toScalar:: U.Elt e  => DArray () e -> e
```


Note that in contrast to all the previous operations, `toScalar` requires the array to be of a particular 
dimensionality.


## Collection Oriented Operations


### Elementwise Application of Functions



The `map` operation takes a function over element types and applies it to every
data element of the DArray, which can have arbitrary dimensionality.


```wiki
map:: (U.Elt a, U.Elt b, A.Shape dim) =>  (a -> b) -> DArray dim a -> DArray dim b
```


Similarily, `zip` and `zipWith` apply to every data element in the array as well. Both arguments
have to have the same dimensionality (which is enforced by the type system). If they have a different
shape, the result will have the intersection of both shapes. For example, zipping an array of shape
`(() :*: 4 :*: 6)` and `(() :*: 2 :*: 8)` results in an array of shape `(() :*: 2 :*: 6)`.


```wiki
zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
zip:: (U.Elt a, U.Elt b, A.Shape dim) => 
  DArray dim a -> DArray dim b-> DArray dim (a :*: b)
```


The function `fold` collapses the values of the innermost rows of  an array of at least dimensionality 1.


```wiki
fold :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray (dim :*: Int)  e  -> DArray dim e
```


Again, it's not possible to use `fold` directly to collapse an array along any other axis, but, as 
we will see shortly, this can be easily done using other functions in combination with `fold`.



Related to  `fold`, we have `scan`:


```wiki
scan :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray dim   e  -> Array dim e
```


Note that `scan` returns a value of type `Array`, not `DArray`: this means that, if we apply scan to an array and access one of its elements, the whole array will be created.


### Support for Parallel Execution



Since the implementation of DArrays builds on the DPH library, all the array operations can be executed in parallel. That is, we compiling a DArray program, the compiler generates a sequential as well as a parallel executable. All collective operations, like `map`, `fold`, and so on are executed in parallel. 


### Shape Polymorphic Computations on Arrays



The library provides a range of operation where the dimensionality of
the result depends on the dimensionality of the argument in a
non-trivial manner, which we want to be reflected in the type system. 
Examples of such functions are generalised selection, which allows for 
extraction of subarrays of arbitrary dimension, and generalised replicate,
which allows replication of an array in any dimension (or dimensions). For example,
given a three dimensional matrix, we can use select to extract scalar element values,
rows, columns, as well as two dimensional matrices along any of the three axes.



For selection, we can informally state the relationship between dimensionality of
the argument, the selector, and the result as follows:


```wiki
select:: Array dim e -> <select dim' of dim array> -> Array dim' e
```


Another example for such a generalised function would be a generalised
`map`, which can apply a function to all elements, all rows, all
columns, or submatrices of different orientation of a multidimensional
array.



For the former example, we need a way to express the relationship between the
shape of the argument and the shape and orientation of the result, as well as
the numerical position of the structure (i.e., first, second, third element). 
In case of the generalised `map`, we don't need the numerical information, since
the operation will be applied to all elements, rows, columns etc. 



To express this dependency between input and output shape and orientation,
as well as possibly a concrete position,  the library provides the `Index` GADT, 
which expresses a relationship between the source and the projected dimension. 
It is defined as follows:


```wiki
data Index a initialDim projectedDim where
  IndexNil   :: Index a initialDim initialDim
  IndexAll   :: (Shape init, Shape proj) =>      
                   Index a init proj -> Index a (init :*: Int) (proj :*: Int)
  IndexFixed :: (Shape init, Shape proj) => a -> 
                   Index a init proj -> Index a (init :*: Int)  proj
```


To refer to a specific element, the type parameter `a` is instantiated with the type `Int`, otherwise
with the unit type:


```wiki
type SelectIndex = Index Int
type MapIndex    = Index ()
```


Given this definition, the type of `select` now becomes


```wiki
select:: (U.Elt e, Shape dim, Shape dim') => Array dim e -> SelectIndex dim dim'  -> Array dim' e
```


Even though the index type is well suited to express the relationship
between the selector/multiplicator and the dimensionality of the
argument and the result array, it is inconvenient to use, as the
examples demonstrate. We therefore need some syntactic sugar to improve
the usability of the library.  In the following, the use a SAC-like notation for 
values of Index-type in comments to improve the readability of the examples.



Example:


```wiki
arr:: Array (() :*: Int :*: Int :*: Int) Double

arr' :: () :*: Int :*: Int
arr' = select arr (IndexFixed 3 (IndexAll (IndexAll IndexNil))) -- (3,.,.)
```


We could generalise this further, to extract from any array `arr` which is at least one dimensional 
the third element:


```wiki
arr:: Shape dim => Array (dim :*: Int) Double

arr' :: Array dim Double
arr' = select arr (IndexFixed 3 IndexNil)   -- (3,*)
```


The index type is also used to express the type of generalised replicate


```wiki
replicate:: Array dim' e -> SelectIndex dim dim'  -> Array dim e
```


which, given an array, can be used to expand it along any dimension. For example,


```wiki
simpleReplicate:: (U.Elt e, Shape dim) => Array dim e -> Int -> Array (dim :*: Int) e
simpleReplicate arr n =
  replicate arr (IndexFixed n IndexNil)  -- (*,n)
```


replicates the argument array (which can of any dimensionality) `n` times and behaves
thus similarly to list replicate, whereas 


```wiki
elementwiseReplicate:: (U.Elt e, Shape dim) => 
  Array (dim :*: Int) e -> Int -> Array (dim :*: Int :*: Int) e
elementwiseReplicate arr n =
  replicate arr (IndexAll (IndexFixed n IndexNil))    -- (*,n,.)
```


replicates each element of an array `n` times (similarly to `map (replicate n)` on lists).



Note that the library provides no way to statically check the pre- and
postconditions on the actual size of arguments. This has
to be done during run time using assertions.


## \`Nesting' Array Functions



We already introduced the `map` function, which applies a given function to all data elements
of an array:


```wiki
map:: (U.Elt a, U.Elt b, A.Shape dim) =>  (a -> b) -> DArray dim a -> DArray dim b
```


We can't use this function, however, to apply a function to all columns, rows, or other sub-arrays of 
a multidimensional array, and generalising `map` to be able to handle this wouldn't make sense
in this framework. Consider, for example, a function `filter`, which takes a one-dimensional 
array and creates a new array containing only the even elements of the argument array. If we mapped
this function over all the rows of a two-dimensional array, the resulting structure would, in general,
not be a two dimensional array anymore, since each row might potentially have a different length. 
Therefore, we restrict the class of functions that can be mapped over sub-arrays to functions where 
the shape of the argument determines the shape of the result. All `mappable` (for the lack of a better term) 
functions can be implemented such that they abstract over the exact dimensionality of their argument, and have the type


```wiki
f::(A.Shape dim, U.Elt e, U.Elt e') => 
  DArray (dim :*: Int ..... :*: Int) e ->  DArray (dim :*: Int :*: .... :*: Int) e'
```


and those functions can be trivially mapped since


```wiki
 map f = f
```


The function `fold`, which we introduced earlier, is an example of a mappable library function. Applied to a matrix, `fold (+) 0` will calculate the sum of all rows. If run in parallel, `fold` itself is run in parallel, and all the rows are processed in parallel. 


```wiki
fold :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray (dim :*: Int)  e  -> DArray dim e
```


So, for example, we can write a mappable function which takes an array and selects every data element with 
an even index:


```wiki
  selectEvenInd:: (A.Shape dim, U.Elt e) => DArray (dim :*: Int) e -> DArray (dim :*: Int) e 
  selectEvenInd (DArray (sh :*: n ) f =
     DArray (sh :*: n `div` 2) (\(sh :*: n) -> f (sh :*: 2*n)
```


In this case `dim` could simply be unit, if and `selectEven` extracts all elements with an even index, or it could
be any other shape, and thus


```wiki
  map selectEvenInd = selectEvenInd
```


where `selectEvenInd` on the left and right-hand side of the equation are two different instances of the function. Now, lets
try and write function `selectEvenElems`, which selects all even elements from an array.  To determine the shape of the
result, it is not sufficient to look  at the shape of the argument. Instead, we have to calculate the new size by counting the
even elements in the array  using `fold`. The function `fold` is mappable, and returns an array. If the argument is a one-dimensional
array, the result is a singular array, which then can be converted to a scalar using `toScalar`.  The necessary application of `toScalar`,
however, also restricts `sh`, which can now only be unit, and therefore the whole operation `selectEvenElems` is restricted to one-dimensional
arrays, and not mappable.


```wiki
  selectEvenElems (DArray  (sh :*: n) f) = DArray (sh :*: newSize) <......>
     where
          newSize = toScalar $ fold (+) 0 $ map (\x -> if (even x) then 1 else 0) arr  
```

## Example 1: Matrix multiplication



As a simple example, consider matrix-matrix multiplication. We can either implement it by directly manipulating the array
function, or use the operations provided by the DArray library. Let as start with the former, which is more fairly similar to
what we would write using loops over array indices:


```wiki
mmMult1:: 
  DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult1 arr1@(DArray (() :*: m1 :*: n1) _) arr2@(DArray (() :*: m2 :*: n2) _) = 
  fold (+) 0 arrDP
  where 
    arrDP = DArray (():*: m1 :*: n2 :*:n1) 
       (\(() :*: i :*: j :*: k) -> (index arr1 (() :*: i :*: k)) * (index arr2 (() :*: k :*: j)))
```


In the first step, we create the intermediate three dimensional array which contains the products of all 
sums and rows, and in the second step, we collapse each of the rows to it's sum, to obtain the two dimensional
result matrix. It is important to note that the elements of `arrDP` are never all in memory (otherwise, the memory
consumption would be cubic), but each value is consumed immediately by `mapfold`. 



This implementation suffers from the same problem a corresponding C implementation would - since we access one
array row-major, the other column major, the locality is poor. Therefore, first transposing `arr2` and adjusting the
access will actually improve the performance significantly: 


```wiki
mmMult1:: 
  DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult1 arr1@(DArray (() :*: m1 :*: n1) _) arr2@(DArray (() :*: m2 :*: n2) _) = 
  fold (+) 0 arrDP
  where 
    arr2T = forceDArray $ transpose arr2
    arrDP = DArray (():*: m1 :*: n2 :*:n1) 
       (\(() :*: i :*: j :*: k) -> (index arr1 (() :*: i :*: k)) * (index arr2T (() :*: j:*: k)))

transpose:: DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
transpose (DArray (() :*: m :*: n) f) =
  DArray (() :*: n :*: m) (\(() :*: i :*: j) -> f (() :*: j :*: i))
```


However, we do need to force the actual creation of the transposed array, otherwise, the change would have no effect at all. We therefore 
use `forceDArray`, which converts it into an array whose array function is a simple indexing operation (see description of `forceDArray` above). This means that the second version requires more memory, but this is offset by improving the locality for each of the multiplications. 



As it is, `mmMult` can only take two-dimensional arrays as arguments, and is not mappable. If we look at the implementation closely, we can see that the restriction to two-dimensional arrays is unnecessary. All we have to do to generalise it is to adjust the type signatures and replace `()` with an arbitrary shape variable:


```wiki
mmMult1:: Shape dim => 
  DArray (dim :*: Int :*: Int)  Double-> DArray (dim :*: Int :*: Int)  Double-> DArray (dim :*: Int :*: Int)  Double  
mmMult1 arr1@(DArray (sh :*: m1 :*: n1) _) arr2@(DArray (sh' :*: m2 :*: n2) _) = 
  fold (+) 0 arrDP
  where 
    arr2T = forceDArray $ transpose arr2
    arrDP = DArray (sh:*: m1 :*: n2 :*:n1) 
       (\(sh :*: i :*: j :*: k) -> (index arr1 (sh :*: i :*: k)) * (index arr2T (sh :*: j:*: k)))

transpose:: Shape dim =>
   DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double  
transpose (DArray (sh:*: m :*: n) f) =
  DArray (sh :*: n :*: m) (\(sh :*: i :*: j) -> f (sh :*: j :*: i))
```


An alternative way to define matrix-matrix multiplication is in terms of the collective library functions provided. First, we
expand both arrays and, in case of `arr2` transpose it such that the elements which have to be multiplied match up. Then,
we calculate the products using `zipWith`, and then use `fold` to compute the sums:


```wiki
mmMult2:: (Array.RepFun dim, Array.InitShape dim, Array.Shape dim) => 
    DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double -> 
        DArray (dim :*: Int :*: Int)  Double  
mmMult2 arr1@(DArray (sh :*: m1 :*: n1) fn1) arr2@(DArray (sh' :*: m2 :*: n2) fn2) = 
   fold (+) 0 (arr1Ext * arr2Ext)
 where
    arr2T   = forceDArray $ transpose arr2  
    arr1Ext = replicate arr1 (Array.IndexAll (Array.IndexFixed m2 (Array.IndexAll Array.IndexNil))) --  (*,.,m2,.)
    arr2Ext = replicate arr2T               
                 (Array.IndexAll (Array.IndexAll (Array.IndexFixed n1 Array.IndexNil)))                                 -- (*,n1,.,.)

```


In this implementation, `transpose` is necessary to place the elements at the right position for `zipWith`, and we call `forceDArray` for
the same reason as in the previous implementation, to improve locality. Also, `mmMult2' outperforms `mmMult1`, as the use of `replicate\`
exposes the structure of the communication, whereas the general index calculations in `mmMult1` hide this structure, and thus are less efficient.


### Performance of Matrix-Matrix Multiplication



The following table contains the running times in milliseconds  of `mmMult1` and `mmMult2', applied to two matrices of with `size \* size` elements. As mentioned before, `mmMult2` is faster than `mmMult1`, as `replicate` can be implemented more efficiently than the general permutation which is the result of the element-wise index computation in `mmMult1`. This is the case for most problems: if it is possible to use collection oriented operations, than it will lead to more efficient code. We can also see that using `forceDArray` for improved locality has  a big impact on performance (we have O (size*size*size) memory accesses, and creating the transposed matrix has only a memory overhead of O(size*size)). `mmMult1\` without the
transposed matrix is about as fast as `mmMult2` without `forceDArray` (times omitted). We can also see that the speedup on two processors is close to the optimal speedup of 2.



To get an idea about the absolute performance of DArrays, we compared it to two C implementations. The first (handwritten) is a straight forward C implementation with three nested loops, iterations re-arranged to get better performance, which has a similar effect on the performance than the `forceDArray`/`transpose` step. The second implementation uses the matrix-matrix multiplication operation provided by MacOS accelerate library. We can see that, for reasonably large arrays, DArrays is about a factor of 3 slower than the C implementation if run sequentially. 


```wiki
  ----------------------------------------------------------------------
  size                                |       256 |       512 |    1024 | 
  ----------------------------------------------------------------------    
  mmMult1                             |       675 |      5323 |   42674 |
  ----------------------------------------------------------------------
  mmMult2                             |       345 |      2683 |   21442 |
  ----------------------------------------------------------------------
  mmMult2  (2PE)                      |       190 |      1463 |   11992 |
  ----------------------------------------------------------------------
  mmMult2 (without forceDArray)       |       974 |      8376 |   73368 |
  ----------------------------------------------------------------------
  mmMult2 (without forceDArray, 2PE)  |       508 |      4368 |   37677 |
  ----------------------------------------------------------------------
  C (hand written)                    |        34 |       514 |    7143 |
  ----------------------------------------------------------------------
  C (MacOS Accelerated Vector ops)    |        33 |       510 |    6949 |
  ----------------------------------------------------------------------
```

## Example 2: Red-Black Relaxation



(example taken from SAC web page)


```wiki
redBlack:: Array.Shape dim => Double -> Double -> DArray (dim :*: Int :*: Int :*: Int) Double ->
             DArray  (dim :*: Int :*: Int :*: Int) Double -> DArray (dim :*: Int :*: Int :*: Int) Double
redBlack factor hsq f arr@(DArray (d :*: l :*: n :*:m) fn)  = 
  applyFactor $ insert odd arr' $ sumD $ getBlack $ stencil arr'

  where
    arr' =  applyFactor $ insert even arr $ sumD $ getRed $ stencil arr

    applyFactor = zipWith (\fi -> \si -> factor *  (hsq * fi + si)) f
    
    sumD arr = fold (+) 0 arr 
 
    getRed (DArray (sh :*: l :*: m :*: n :*: c) f ) =
      DArray (sh :*: l-2 :*: m-2 :*: (n-1)`div` 2 :*: c) 
             (\(sh :*: h :*: i :*: j :*: c) -> f(sh :*: h+1 :*: i+1 :*: 2*j+1 :*: c)) 
    getBlack (DArray (sh :*: l :*: m :*: n :*: c) f) =
      DArray (sh :*: l-2 :*: m-2 :*:  (n-2) `div` 2:*: c) 
             (\(sh :*: h :*: i :*: j:*:c) -> f (sh :*: h+1 :*: i+1 :*: 2*j+2:*:c)) 

    isBorder (d :*: h :*: i :*: j) = ((h * i * j) == 0)  || 
      (h >= (l-1)) || (i >= (m-1)) || (j >= (n-1))

    insert p (DArray sh f) (DArray sh' f')  =
      DArray sh (\d@(sh :*: h :*: i :*: j) -> if ((isBorder d) || p j) 
                                                 then f d 
                                                 else (f' (sh :*: h-1 :*: i-1 :*: (j-1)`div` 2)))

    stencil (DArray sh  f) = 
      DArray (sh :*: 6) f' 
      where
        f' (sh :*: n :*: m :*: 0) = f (sh :*: n :*: m+1)
        f' (sh :*: n :*: m :*: 1) = f (sh :*: n :*: m-1)
        f' (sh :*: n :*: m :*: 2) = f (sh :*: n+1 :*: m)
        f' (sh :*: n :*: m :*: 3) = f (sh :*: n-1 :*: m)
        f' (sh :*: k :*: n :*: m :*: 4) = f (sh :*: k+1 :*: n :*: m)
        f' (sh :*: k :*: n :*: m :*: 5) = f (sh :*: k-1 :*: n :*: m)

```

## Example 3: 3D Fast Fourier Transformation



Applied FFT to each vector in a three dimensional matrix, once along each of the three axes, iterate a given number of times (example taken from SAC web page)


```wiki
fft3d:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3d it rofu  m | it < 1    = m
                | otherwise = fft3d (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = forceDArray . (fft rofu) . transpose'
    transpose' darr@(DArray (() :*: k :*: l :*: m) _) = 
      backpermute darr (() :*: m :*: k :*: l)
            (\(() :*: m' :*: k' :*: l') -> (() :*: k' :*: l' :*: m')) 

fft:: Array.Subshape dim  dim => 
  DArray (dim :*: Int) Complex -> DArray (dim :*: Int) Complex -> DArray (dim :*: Int) Complex 
fft rofu@(DArray ( _ :*: s) _ )  v@(DArray sh@(_ :*: n) f) 
  | n > 2     = assert (2 * s == n) $ 
    append (fft_left + fft_right) (fft_left - fft_right) sh
  | n == 2    = assert (2 * s == n) $ 
    DArray sh f'
  where 
    f' (sh :*: 0) = f (sh :*: 0) + f (sh :*: 1)
    f' (sh :*: 1) = f (sh :*: 0) - f (sh :*: 1)
    f' (sh :*: x) = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))

    rofu'     = split rofu (\(sh :*: i) -> (sh :*: 2*i))
    fft_left  = forceDArray $ rofu * (fft rofu' (split v (\(sh:*: i) -> (sh :*: 2*i))))
    fft_right = forceDArray $ fft rofu' (split v (\(sh:*: i) -> (sh :*: 2*i+1))) 
    
split:: Array.Shape dim => 
  DArray (dim :*: Int) Complex -> ((dim :*: Int) -> (dim :*: Int)) -> DArray (dim :*: Int) Complex
split arr@(DArray (sh :*: n) fn) sel =
  (DArray (sh :*: (n `div` 2)) (fn . sel)) 

```