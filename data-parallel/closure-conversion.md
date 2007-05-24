## Closure conversion as part of vectorisation



**TODO** Describe the treatment of higher-order functions and closure conversion here. The relevant paper is [
http://www.cse.unsw.edu.au/\~chak/papers/LCK06.html](http://www.cse.unsw.edu.au/~chak/papers/LCK06.html). The approach is described in more detail in [
http://opus.kobv.de/tuberlin/volltexte/2006/1286/](http://opus.kobv.de/tuberlin/volltexte/2006/1286/).


### Workplan



Implement in following order:


1. Call a single closure-converted module from non-converted code (we know how to do this) ** DONE **
1. Use non-cc'd modules in cc'd modules ** DONE **
1. Import cc'd modules in other cc'd modules (we need to decide on how to put the additional CC info into ifaces) ** DONE **
1. CC data types ** DONE **


Unboxed values aren't really implemented yet, but we'll now go over to implementing vectorisation, which will work a bit different to what is described here.  See the [vectorisation page](data-parallel/vectorisation).



We have got some  [notes on the implementation](data-parallel/closure-conversion/impl-notes).


### Closure conversion without classes



We currently implement a [simple scheme](data-parallel/closure-conversion/class-less) that operates entirely on the Core representation and doesn't use conversion classes. 


### Alternative: Closure-converted types as indexed types



We don't use indexed types in the current implementation of closure conversion.  However, we may have to get back to some of the ideas from this approach when we implement vectorisation.



One option for implementing closure-conversion is to represent closure-converted types as an indexed type whose type index is the original type and to combine that indexed type in a type class with methods for converting between closure-converted and vanilla terms.  The details are under [indexed closure conversion](data-parallel/closure-conversion/indexed).  There are two potential benefits for this approach: (1) we will probably have to do something similar for vectorisation anyway - see the requirements of vectorisation? - and (2) it seems that we need less bookkeeping (e.g., the name of a closure converted data type is just the indexed type with the original data type as its index).  However, there are problems, too; in particular, as we currently don't have class contexts and polytypes as type indexes.


### Requirements of closure conversion


#### The straight forward part



The actual closure-conversion transformation on lambda terms, which we intend to perform on Core.


#### Type declarations, classes, and instances



*Type declarations.*
If the program contains a type declaration including an arrow type, we need a closure-converted version of that declaration (for use in the closure converted code); e.g., for


```wiki
data T = MkT (Int -> Int)
```


we need


```wiki
data T_CC = MkT_CC (Clo Int Int)
```


Note how this also introduces new constructor names.



*Classes.*
Moreover, if the closure-converted code uses a type class, we need a closure-converted type class.  It might appear that this is a non-issue on Core as classes have already been desugared to plain System F.  The trouble is that GHC does not use plain System F.  It extends it by a whole array of extra type features.  In particular, classes induce datatype declarations., and yhey almost always contain arrow types.  Hence, as we just discussed, they need to be transformed.  Matters are complicated by GHC not actually generating standalone datatypes declarations that are emitted into interface files, a class declaration siliently implies a data declaration - GHC calls this an *iface declaration sub-binder*.  So, it appears best to create for every class a closure-converted class that - as all other class declarations - implies the closure-converted class representation datatype.



*Instances.*
When we call overloaded closure-converted functions, we need to supply closure-converted dictionaries; i.e., closure-converted dfuns.  


#### Interface files



How much information do we want to put into interface files?  For example, for type declarations, we can add the closure-converted declaration to the interface file or we can only add the vanilla one and have the importing modules derive the converted form on the fly during iface type checking - similar to how iface declaration sub-binders are generated on the fly, instead of being put into the interface files.


#### Mixing converted and vanilla code



Mixing converted and vanilla code for closure-conversion is simple as long as we only have lambda terms (we just generate and apply closure as appropriate at places where we move from one representation to another).  However, data types make this more involved again.  Imagine the following scenario:


```wiki
-- converted code
data T = MkT (Int -> Int)

-- unconverted code
foo = MkT ((+) (1::Int))

-- converted code
appT :: T -> Int -> Int
appT (MkT f) x = f x

bar = appT foo
```


Here the result of `foo` needs to be converted before we can call `appT` in `bar`.  In other words, we need conversion functions between converted and unconverted versions of data types.



A problem becomes apparent when we change the example slightly:


```wiki
-- unconverted code
data T = MkT (Int -> Int)
foo = MkT ((+) (1::Int))

-- converted code
appT :: T -> Int -> Int
appT (MkT f) x = f x

bar = appT foo
```


Now, we don't have a converted version of T available.  Converting it on the fly might be problematic.  If `appT` and `bar` reside in different modules, we convert `T` twice and need to make sure the two versions are compatible.  Even worse, the module holding `bar` may only import `T` abstractly and hence can neither derive the converted version of `T` nor can it infer  suitable conversion functions.  In this case, we have to give up on converting `appT foo` (and maybe all of `bar`).



Alternatively, we might decide that we do not convert the case expression in `appT`'s Core code that scrutinises its first argument.  Then, the type of the converted function would be


```wiki
appT_CC :: Clo T (Clo Int Int)
```


i.e., it still uses `T`, not `T_CC`.  As callers of `appT_CC` also will **not** have a converted version of `T` available, everything still fits together nicely.  This appears to be a simpler scheme than on-the-fly type conversions.



The same should work for classes and their dictionaries.  If we can use unconverted functions in converted code, we can also select from unconverted dictionaries and use the unconverted methods.


---



**** OLD STUFF ****


### From the Skype discussion, 16 Mar 07



For each function `f::ty`, create a closure-converted function `fc::ty'`, where `ty'` is the closure-converted verion of `ty`.
Optimisation: make `fc=f`, if the code for `fc` will be identical to that of `f`.  



For each data type `T`, create a closure-converted data type `Tc`, whose constructors use `Clo` instead of `->`.  Optimisation: if `Tc` is identical to `T`, don't create a new data type.


