## Data Parallel Haskell: NDP by example



Data Parallel Haskell provides the programmer with a type of parallel arrays (`[:.:]`) and a large number of operations on them: standard combinators like map, filter, various folds and scans as well as the usual array operations. Moreover, it extends Haskell with parallel array comprehensions such as `[: x+1 | x <- xs :]` and other syntactic sugar similar to what standard Haskell provides for lists. More details on the syntax can be found in [
http://www.cse.unsw.edu.au/\~chak/papers/CKLP01.html](http://www.cse.unsw.edu.au/~chak/papers/CKLP01.html).



Operations on parallel arrays have a parallel semantics: for instance, `sumP [:1..n:]` computes the sum of all numbers from `1` to `n` in parallel, utilising as many threads (and, thus, CPUs or cores) as requested by the programmer. A more involved example is the dot product of two vectors:


```wiki
dotp :: (Num a, PA a) => [:a:] -> [:a:] -> a
dotp xs ys = sumP (zipWithP (*) xs ys)
```


Here, the multiplications and then the additions are performed in parallel. In fact, the loop fusion framework which is part of the NDP library ensures that no intermediate array will be created, thus producing highly efficient code. Note that the function is polymorphic in the type of the array elements. Crucially, in addition to being instances of `Num`, they have to be instances of `PA` (the class of parallel array elements) which encapsulates the type-dependent representation of parallel arrays and basic parallel operations. Ultimately, it will be possible to automatically define a `PA` instance for any Haskell type by specifying a `deriving(PA)` clause in the type declaration.



Parallel operations can be arbitrarily nested without affecting the degree of parallelism. For instance, we can represent a sparse matrix as array containing, for each row, an array of index/value pairs for every non-zero element:


```wiki
type SM = [:[:(Int, Float):]:]
```


Then, we can define the multiplication of such a matrix with a dense vector as follows:


```wiki
smvm :: SM -> [:Float:] -> [:Float:]
smvm m v = [:sumP [:x * (v !: i) | (i,x) <- col:] | col <- m:]
```


Although we nest three levels of parallel computations in this code, the parallel depth complexity is logarithmic in the length of the longest row. The compiler automatically eliminates nested parallelism, producing code which can be executed efficiently on stock hardware.


