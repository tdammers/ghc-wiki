# Unpacking Arrays



(notes written by Simon Marlow and Johan Tibell at ICFP'11)


## Motivation



We want to have a data type containing `Array#` like this:


```wiki
data T a b = C a {-# UNPACK #-} (Array# b)
           | ...
```


and have the `Array#` unpacked into the constructor `C`, so that the representation would be something like this:


```wiki
+------------------------+
| C | a | n | x1......xn |
+---+---+----------------+
```


i.e. the `C` info table, the field of type `a`, a length field, and the payload of the `Array#`.  



For example we might use this to remove a layer of indirection in our immutable `Array` types.


## Source API



For now instead of having an implicit unpacked `Array#` type, lets define a new type of unpacked arrays:


```wiki
UnpackedArray#
```


(we can change the name later)



So the user would write:


```wiki
data T a b = C a (UnpackedArray# b)
           | ...
```


and we expose some new primitives:


```wiki
indexUnpackedArray# :: UnpackedArray# a -> Int# -> (# a #)
```


The question is, how are we going to represent `UnpackedArray#`?



Suppose that the compiler desugars `UnpackedArray#` to a pair of values:
 


- a pointer value (maybe type `InteriorArrayContainer# a`, but we could revisit this): this is a pointer to the containing constructor, `C`
- an `Int#`, which is the offset of the beginning of the unpacked array relative to the info table of the `C` constructor, that is, the offset of the array's length field.


Now, any variable of type `UnpackedArray#` also gets desugared into a pair of variables with the above types.  The `indexUnpackedArray#` primitive really takes \*two\* arguments, so let's give the desugared one a different name: 


```wiki
indexInteriorArray# :: InteriorArrayContainer# a -> Int# -> Int# -> (# a #)
```


(another option is to represent `UnpackedArray# a` by `(# InteriorArrayContainer# a, Int# #)` and have a general unboxed tuple desugaring that flattens unboxed tuples in argument and field positions.)


## Pattern matching and Indexing



Example: given our data type `T` above, suppose the user wrote


```wiki
f :: T Int Float -> Float
f t = case t of 
        C x arr# -> case indexUnpackedArray# arr# x of 
                      (# f #) -> F# f
```


We would desugar this into


```wiki
f :: T Int Float -> Float
f t = case t of 
        C x icarr# off# -> 
           case indexInteriorArray# icarr# off# x of 
                      (# f #) -> F# f
```


Note that the representation of the constructor `C` does not actually contain the two fields `icarr#` and `off#`, these are brought into existence (bound) by the code generator when generating the code for the `C` case alternative. 


## Construction



(ToDo: we have no idea what to do here... yet)


```wiki
prim :: Int# -> a -> UnpackedArray# a

C a ...
```

```wiki
newInteriorArrayContainer# :: Addr# -- info pointer
                           -> a     -- initial element to fill the array with
                           -> InteriorArray# a
```


This would make a constructor with the given info table.  The primop can tell how big the constructor is by looking at the info table.


## GC things


## Further extension


- unpacking more than one `Array#` into a constructor
- unpacking other types, like `MutVar#`, `MVar#` etc. (mutable types are tricky because we need write barriers etc.)

## List of changes that we think are necessary



This list is likely to be non-exhaustive...


- Add primops, new primitive types (`prelude/primops.txt`, `prelude/TysPrim.lhs`)
- Implement primops, (`codeGen/CgPrimOp.lhs`, `codeGen/StgCmmPrim.lhs`)
- Add desugaring of unpacked array types in argument and field positions, and `indexUnpackedArray#` to `indexInteriorArray#` (somewhere under `desugar/` probably)
- Add code generation for case alternatives on constructors containing interior arrays, they have to bind variables to the `InteriorArrayContainer#` and offset (`codeGen/CgCase.lhs`, `codeGen/StgCmmCase.lhs`).
