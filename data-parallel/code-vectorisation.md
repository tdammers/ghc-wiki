## Vectorisation for nested data parallelism



**TODO** This material needs to be revised and we need to come up with a plan for getting some programs to quickly get some programs to compile.  Also integrate the [lifted case example](data-parallel/code-vectorisation/lifted-case-example).



We will try to implement full blown vectorisation using an explicit closure representation on Core code after lambda lifting.  The transformation performs closure conversion and vectorisation in one sweep.  We represent scalar and array closures as follows:


```wiki
data a :-> b = forall env. PA env =>  Clo env     (env -> a -> b) ([:env:] -> [:a:] -> [:b:])
data a :=> b = forall env. PA env => AClo [:env:] (env -> a -> b) ([:env:] -> [:a:] -> [:b:])

(#) :: (a :-> b) -> a -> b
(#) (Clo env f _) = f env

(##) :: (a :=> b) -> [:a:] -> [:b:]
(##) (AClo envs _ fs) = fs envs
```


It is important that both kinds of closures include scalar and lifted code, as we need to move between `a :-> b` and `a :=> b` in both directions due to the functions:


```wiki
replicateP :: Int -> (a :-> b) -> (a :=> b)
replicateP n (Clo env f fs) = AClo (replicateP n env) f fs

(!:) :: (a :=> b) -> Int -> (a :-> b)
i !: AClo envs f fs = Clo (i !: envs) f fs
```


In other words, we move between the two types of closures simply by replicating and indexing into the environment.



We do not have any explicit type transformations.  These are all encoded using associated types of the parallel array type class `PA`:


```wiki
class PA e where
  data [:e:]
  type Vec e
  (!:) :: [:e:] -> Int -> e
  -- and so on

instance PA () where
  data [:():]  = PAUnit Int
  type Vect () = ()
  PAUnit len !: i | i < len   = ()
                  | otherwise = error "..."

instance PA Int where
  data [:Int:] = PAInt Int [!Int!]
  type Vect Int = Int
  PAInt l as !: i = as `indexU` i

instance (PA a, PA b) => PA (a, b) where
  data [:(a, b):]  = PAProd [:a:] [:b:]
  type Vect (a, b) = (Vect a, Vect b)
  PAProd as bs !: i = (as !: i, bs :! i)

instance (PA a, PA b) => PA (Either a b) where
  data [:Either a b:]    = PASum [:Bool:] [:Int:] [:a:] [:b:]
  type Vect (Either a b) = Either (Vect a) (Vect b)
  PASum sels idx as bs !: i = if sels!:i then a!:(idx!:i) else b!:(idx!:i)

instance PA a => PA [:a:] where
  data [:[:a:]:] = PAArr [:Int:] [:a:]
  type Vect [:a:] = [:[:a:]:]
  PAArr segd as !: i = sliceP from size as
    where
      segd' = scanlP (+) 0 segd
      from  = segd'!:i
      size  = segd !:i

instance (PA a, PA b) => PA (a -> b) where
  data [: a -> b :] = PAFun [:Vect a :-> Vect b:]
  type Vect (a -> b) = Vect a :-> Vect b
  PAFun as !: i = as !: i

instance (PA a, PA b) => PA (a :-> b) where
  data [: a :-> b :] = PAClo (a :=> b)
  type Vect (a :-> b) = a:-> b  -- shouldn't happen, right?
  PAClo (AClo envs f fs) !: i = Clo (envs!:i) f fs
```

### Mixing Vectorised and Scalar Code



We have two types of modules: (a) modules compiled as ever, which we call 'scalar modules', and (b) 'vectorised modules'.  Scalar modules export the same code as before.  Vectorised modules export additional identifiers.


- For every variable `f :: t`, we have **in addition**

  ```wiki
  f^p :: t^v
  f^p = V[[e]]
  ```

  the code for `f` is not the original scalar code.  Instead, it is defined as

  ```wiki
  f :: t
  f = unvect f^p
  ```
- For every data type `T`, we have in addition `T^v`.
- For every function `M.f :: a -> b` imported from a scalar module `M`, we generate and use the following definition instead:

  ```wiki
  f :: (a -> b)^v
  f = vect M.f
  ```


The functions `vect` and `unvect` are defined in the same type classes where `t^v` is defined as an associated type.  **!!TODO:** Try to define these two functions, to be sure we can actually do it.



Moreover, we like to have a toplevel declarations of the form `derive PA (T)` that create a suitable `PA` instance of a previously defined (and possibly imported) data type `T`.


### Various Ideas to Avoid Full Blown Vectorisation



We discussed some approaches that would lead to a certain degree of vectorisation, but avoid dealing with issues, such as arrays of functions.


- We could have rewrite rules as follows (for a vectorised function):

  ```wiki
  mapP f           -> f^
  mapP f^          -> inject f^
  mapP (inject f^) -> inject (mapP f^)
  ```

  where `inject` is the flatten/partition combination.

>
>
> **!!TODO:** What else was there???
>
>

