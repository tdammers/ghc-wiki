## Vectorisation



This page describes our approach to implementing vectorisation by extending our earlier implementation of closure conversion.  A central aspect is the ability to mix modules compiled with vectorisation with modules compiled without vectorisation.


>
>
> **The following description is incomplete.  For a detailed account of vectorisation, see [
> http://www.cse.unsw.edu.au/\~chak/papers/PLKC08.html](http://www.cse.unsw.edu.au/~chak/papers/PLKC08.html).**
>
>

### General strategy



Vectorisation is only going to **add** additional code and data structures to a module.  All the sequential code remains unmodified.  In particular, while vectorised code will call unvectorised code, there are no explicit static calls of unvectorised code into vectorised code.  Nevertheless, dynamic control flow can move from unvectorised to vectorised code by way of vectorised functional values being passed to higher-order unvectorised code.



NB: This is a significant departure from our earlier plan, where the original definition of a function `f` would be modified to call its vectorised variant `f_v` to do the actual work.  We changed our mind on this, as the new configuration appears to be simpler to implement.



From the policy of unvectorised code never directly calling vectorised code, it follows that the `Main` module of a program needs to be compiled with vectorisation enabled if the program is to make any use of vectorised code at all.  Moreover, as `Main.main` is an `IO` function and we certainly don't want to vectorise the `IO` monad, vectorisation will need to *partially vectorise* expressions by vectorising any subexpressions it can vectorise in an expression that cannot be vectorised in its entirety.  Consider the following example:


```wiki
main :: IO ()
main
  = do
      [nStr] <- getArgs
      let n = read nStr :: Int
      print $ sumP [:i*i | i <- [:1..n:]:]
```


Let us assume that the functions `getArgs`, `print`, and `read` are defined in modules that have not been vectorised.  When vectorising `main`, we want to use the vectorised versions of the functions `sumP`, `mapP` (implied by the comprehension), and `enumFromToP` (implied by the array constructor).  However, all the rest of the code will remain largely unchanged.  (What *largely unchanged* means precisely, we still have to define.)  In fact, there will be two versions of `main`.  The original `main` function that, according to our policy, does not use any vectorised code and `main_v` that has all the array code properly vectorised and all the enclosing `IO` code largely unchanged.  In order to make use of vectorisation, the runtime system will have to invoke `main_v`, not `main`.  Moreover, the code calling `main_v` will have to first set up the thread gang and whatever other extra initialisation is needed.  Whether to execute `main` or `main_v` on program start up is determined by whether the `-fvect` option is present during linking or not.


### Two array libraries



We have to different array libraries:


1. The library in `GHC.PArr`, which defines the wired in array constructor `[:.:]`.  It implements `[:.:]` as a parametric data type represented by vanilla boxed arrays.  It does not involve any type class and also no indexed types.  This code is used whenever arrays are mentioned in unvectorised code (i.e., in both all code of unvectorised modules and in the original versions of functions in vectorised modules).
1. The library in package ndp, which defines a type class `PA` and its associated data type `PArr`.   The type family `PArr` implements a flattened array representation for all types `t` for which there is a `PA t` instance.


Vectorisation transforms all uses of functions from `GHC.PArr` into uses of package ndp.  It can obviously only do that for computations manipulating values for whose type we have `PA` instances.


### Basic data types



The following data types are the basis for our representation of arrays and closures in vectorised code.


#### Indexed array family



The type class `PA` has an instance for every type of values that may occur as elements in flattened arrays.  The array type family `PArr` is associated with `PA`:


```wiki
class PA a where
  data PArr a
  replicateP :: Int -> a -> PArr a
  mapP       :: PA b => (a -> b) -> PArr a -> PArr b
  ..and so on..
```

#### Vectorised functions



For the moment, I am assuming that we only need to closure-convert lifted code, but not scalar code.  The rationale is that the closure representation is only needed to pack, merge, etc. array closures, which represent arrays of functions.



A data type to combine the scalar and lifted version of  a function (i.e., a glorified strict pair):


```wiki
data fS :|| fP = !fS :|| !fP
funS (fS :|| _ ) = fS
funP (_  :|| fP) = fP
```


On top of this we define a vectorised function space constructor:


```wiki
newtype a :-> b = VFun ((a -> b) :|| (PArr a -> PArr b))
vfunS (VFun (fS :|| _ )) = fS
vfunP (VFun (_  :|| fP)) = fP
```


So, we have `(->_v) = (:->)`.  Note that the type arguments to `(:->)` will be vectorised types (not vanilla types).  Consequently, instances of `PA` and `PArr` need to be defined for vectorised types.



Three questions may arise at this point:


- Why don't we combine `(:||)` and `(:->)` into one data type?  Answer: This won't work for functions involving unboxed types; in particular, we cannot write `Int# :-> Int#` (this would lead to a kind error).
- Do we really define the `PA` instances over the unvectorised types?  Answer: As `(->)` may be partially applied in the original program, we can have partial applications of `(->_v)`, and hence, of `(:->)`.  These may then be used to instantiate type constructor variables in converted types, where the type arguments will be vectorised.
- Why don't we closure convert scalar code (to keeps things more orthogonal)?  Answers: (1) I think the transformations are simpler like this, as only lifting has to perform on-the-fly closure conversion; (2) the generated Core code is smaller; (3) flattening and the ndp library place considerable stress on the simplifier, so reducing this a bit seems a good strategy; and (4) I am far from sure that the current approach to handling functions manipulating unboxed types would work if we closure-converted scalar code.

#### Lifted functions



In lifted code, we also use `(:||)` to represent pairs of scalar and lifted functions, but here the second component is an array closure; thusly:


```wiki
newtype a :=> b = LFun ((a -> b) :|| ACls (PArr a) (PArr b))

data ACls arr brr
 = forall e. PA e =>
     ACls { aclsFun :: !(PArr e -> arr -> brr)
          , aclsEnv :: PArr e
          }

lfunS (LFun (fS :|| _        )) = fS
lfunP (LFun (_  :|| ACls fP e)) = fP e
```


The following questions may arise:


- Why do we use array types as arguments of `ACls`, rather than array element types?  Answer: We need this to be able to handle functions manipulating unboxed values.  For example, consider a `inc# :: Int# -> Int#`.  What's the type of an array closure for `inc#`?  Given the current definition, we can denote the type as `ACls (UArr Int) (UArr Int)` (as `Int#* = UArr Int`).  In contrast, we would get a kind error if `ACls` would take element types and we'd use `ACls Int# Int#`.

### Transformations



We have a number of transformations that together realise vectorisation:


- [Type vectorisation](data-parallel/vectorisation/type-vectorisation) (`t*`): Similar as in closure-conversion, we have to convert types.  And similar as in closure conversion, the main effect here is on the representation of functions.  In addition to the use of closures instead of plain functions, we need two versions of each function: (1) a scalar version and (2) a version lifted into vector space.
- [Code vectorisation](data-parallel/vectorisation/code-vectorisation) (`e*`): This includes closure conversion and the pairing of scalar and lifted code.
- Code lifting? (`e^`): This converts operations on types `t` into operations on types `[:t:]`.

---


### TODO



Items that need to be addressed on the page:


- Introduction of `PA` dictionary arguments right after each big lambda in vectorised code.
- Transformation schemata
- Examples
