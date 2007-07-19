## Class Families



Our translation of data families in combination with the desugaring of classes into data types suggest the idea of **indexed class families**, which turns out to be rather useful for generalising class APIs for commonly used data structures.


### An example



As a motivating example take the following problem from John Hughes' *Restricted Data Types*.  Suppose we want to implement a set API as a type class.  Then, we find that the signature


```wiki
insert :: Set s => a -> s a -> s a
```


is too general.  We need additional type constraints whose exact form *depends on the type constructor* we use to construct the sets; i.e., it varies on an instance by instance basis.  For lists, we just need `Eq`, but for sets as finite maps, we need `Ord`.



With indexed class families, we can define a set class as follows:


```wiki
class Set s where
  class C s a
  empty  :: s a
  insert :: C s a => a -> s a -> s a
```


Here, the **associated class** `C` of `Set` is indexed by the class parameter `s`.



In instances for sets as lists


```wiki
instance Set [] where
  class Eq a => C [] a
  empty = []
  insert x s | x `elem` s = s
             | otherwise  = x:s

instance Eq a => C [] a                   -- Tiresome instance
```


and sets as finite maps


```wiki
newtype MapSet a = MapSet (Data.Map.Map a ())
instance Set MapSet where
  class Ord a => C MapSet a
  empty = Data.Map.empty
  insert x s = Data.Map.insert x () s

instance Ord a => C MapSet a               -- Tiresome instance
```


we instantiate `C` differently for different type indexes.  



The class-family instances have no members in this case, but use existing classes as a superclass to supply `insert` with the equality and ordering methods, respectively.  As we want to use these superclasses for sets of any element type of which we have an instance of the superclasses, we need a catch-all instance  for each class instance (the "tiresome instances" avove).  That is somewhat ugly especially, as it requires the use of `-fallow-undecidable-instances`.  Furthermore, if the class has no signatures, there is no other useful instance we could possibly give.



**SLPJ note**: I wonder whether it is ever useful to have a class instance with signatures?  Suppose we only allowed the signature-free form?  That would simplify the explanation in many ways (e.g. no need to say whether class instances can themselves have assoicated types!), and loses no expressive power.  I don't think it loses much convenience either.


### The language extension



We define class families as


```wiki
class family C a1 .. an
```


and class-family instances as


```wiki
class instance ctxt => C t1 .. tn where { sigs }
```


where I'd propose to not allow functional dependencies to keep matters simpler.



Class instances of class-family instances take the normal form.  The only additional constraint is that the class parameters are type instances of the class-family instance types.  That is, if we have


```wiki
instance ctxt' => C s1 .. sn where { .. }
```


then we need to have that each `si` is a type instance of `ti` for this to be a class instance of the class-family instance `C t1 .. tn`.



As with data families, the class families can be associated with a class by declaring them in the class.  In this case, we omit the keywords `family` and `instance` in the family and instance declarations, respectively.  Moreover, all type indexes of an associated class need to be class parameters of the parent class.



**OPEN QUESTIONs:** 


- Should an associated class be a (kind of) superclass of its parent.  At least, we may want to add it implicitly to the signature of each method.  Not sure about this, but Roman suggested it, too.
- Do we allow associated types and classes(?!?) in class-family instances?

### Further Examples



Indexed classes appear to be highly useful to achieve
modularity for type-class based generic programming approaches.
Here's a sketch of a "modular" extension of Hinze's "Generics for the masses" (GM)
approach 
and Laemmel's and Peyton Jones' "Scrap your boilerplate" (SYB3) approach using indexed classes.  We first explain why in the original
GM approach we cannot override generic with specific (ad-hoc) behavior
in a modular fashion. Then, we show how indexed classes
come to the rescue. Finally, we consider the SYB3 approach.


#### Generics for the masses



The main idea behind the GM approach is to provide
a uniform representation of data types in terms of
unit, sum and product types.
Generic functions are defined in terms of this uniform 
rather than the concrete structural representation of a data type.
The programmer only needs to maintain a type isomorphism between
the uniform and concrete representation.
Thus, there is no need to extend
the (now generic) definition of functions in case we include new data types.



Here is a (over-simplified) presentation of the GM approach
We only consider the uniform representations "literals" and "plus".


```wiki
data Lit = Lit Int
data Plus a b = Plus a b
class Generic g where
  lit :: g Lit
  plus :: g a -> g b -> g (Plus a b)
```


Below is a generic definition of some evaluation function.


```wiki
newtype Ev a = Ev{eval' :: a -> Int}
instance Generic Ev where
  lit = Ev (\x -> case x of Lit i -> i)
  plus a b =  
    Ev (\p -> case p of 
               (Plus x y) -> eval' a x + eval' b y)
```


In order to use the evaluator on its familiar type,
we need a ``dispatcher* function to select the appropriate case of
a generic function.
The most straightforward approach is to use an ad-hoc polymorphic
(therefore extensible) function.
*


```wiki
class Rep a where
  rep :: Generic g => g a
instance Rep Lit where
  rep = lit
instance (Rep a,Rep b) => Rep (Plus a b) where
  rep = plus rep rep
eval :: Rep t => t -> Int
eval = eval' rep
```


The dispatcher function rep will select the
appropriate generic case depending on the concrete type context.
We can straightforwardly introduce new generic functions (omitted here).



Suppose we introduce a new ad-hoc case "minus" which has the
same structural representation as "plus".


```wiki
data Minus a b = Minus a b
class Generic g => GMinus g where
  minus :: g a -> g b -> g (Minus a b)
instance GMinus Ev where
  minus a b =  
    Ev (\p -> case p of 
               (Minus x y) -> eval' a x - eval' b y)
instance (Rep a,Rep b) => Rep (Minus a b) where
  rep = minus rep rep
```


The problem is that we cannot access this new case, unless
we update the type of the dispatcher function rep. We must
change rep's declaration as follows.


```wiki
class Rep a where
  rep :: GMinus g => g a 
-- original code: rep :: Generic g => g a
```


But changing rep's class declaration requires to recompile the entire
program. Hence, extending generic definitions with ad-hoc cases
cannot be modularly.



Such problems go away if we use indexed classes.
More precisely, we use a type indexed class in rep's class declaration.



The generic cases.


```wiki
class Rep a where
  class Generic' g a
  -- or class Generic' g :: * -> Class using Tom's suggestion
  rep :: Generic' g a => g a
instance Rep Lit where
  class Generic g => Generic' g Lit
  -- better written as? Generic' g Lit = Generic g
  rep = lit
instance (Rep a,Rep b) => Rep (Plus a b) where
  class Generic g => Generic' g (Plus a b)
  rep = plus rep rep
eval :: Rep t => t -> Int
eval = eval' rep
```


The ad-hoc case. 


```wiki
class GMinus g where
  minus :: g a -> g b -> g (Minus a b)
instance GMinus Ev where
  minus a b =  
    Ev (\p -> case p of 
               (Minus x y) -> eval' a x - eval' b y)

instance (Rep a,Rep b) => Rep (Minus a b) where
  class GMinus g => Generic' g (Minus a b)
  rep = minus rep rep
```


Notice the use of indexed classes to select appropriate classes
for each instance.



General insight: It seems that via indexed classes we can encode
a type-passing type-class translation scheme. 


#### SYB3



The SYB3 approach faces similar challenges when it comes
to modularity. 



The generic cases.


```wiki
class Typable a => Data a where
 gmapQ :: (forall b. Data b => b -> r) -> a -> [r]

instance Data Char where
  gmapQ f c = []
instance Data a => Data [a] where  
  gmapQ f (x:xs) = [f x, f xs]
```


A new generic "size" function.


```wiki
class Size a where
  gsize :: a -> Int
-- specific instance
instance Size Name where ...
-- generic instance,
-- we use overlapping instances to cover all remaining cases
instance Data t => Size t where         -- (S)
  gsize t = 1 + sum (gmapQ gsize t)
```


The problem is that the instance (S) will not type check.
The program text gmapQ size is the trouble maker.
In this specific context, the combinator gmapQ expects as
its first argument a function of type


```wiki
forall a. Data a => a -> Int
```


and the actual argument gsize has type


```wiki
forall a. Size a => a -> Int
```


But the second type is not a subtype of the second
because we cannot derive 'Size a' from 'Data a' for any 'a'.
For this subtype relation to hold, if we can satisfy
This condition is satisfied 
if we make 'Sizes' a superclass of 'Data'.
But this will break modularity (we have to change
'Data's class declaration for each new generic function).



We fix the problem by by introducing an indexed class
in gmapQ's context. Thus, we can 'enable' the 'Size' class
when needed.


```wiki
-- type family 
class family Context ctxt :: * -> class

class Typeable t => Data ctxt t where
   gmapQ :: (forall e . Data e ctxt, Context ctxt e => e -> r) -> t -> [r]

instance Data Char ctxt where
   gmapQ f c = []

instance Data e ctxt => Data [e] ctxt where
   gmapQ f (x:xs) = [f x,f xs]


class Size t where
   gsize :: t -> Int

instance Size t where
   gsize = 42

...

instance Data t SizeCtxt => Size t where
   gsize x = 1 + sum (gmapQ gsize x)

data SizeCtxt -- uninhabited

class instance (Size e) => Context SizeCtxt e
```

#### Restricted Monads



**Tom**: This way we can also make Set an instance of (C)Monad. I believe lots of people
have been asking for this.


```wiki

class CMonad m where
  class Condition m :: * -> Class
  (>>=) :: (Condition a, Condition b) => m a -> (a -> m b) -> m b
  (>>)  :: (Condition a, Condition b) => m a -> m b -> m b
  return :: Condition a => a -> m a
  fail :: Condition a => String -> m a

instance CMonad [] where
  class Condition [] a
  ...

instance CMonad Set where
  class Eq a => Condition Set a
  ...

```

### Type checking



Like with data families, there is little impact on type checking.  Methods of class-family instances have signatures whose class constraints are not just variables.  For example,


```wiki
class instance C Int a where 
  foo :: a -> a
```


gives us


```wiki
foo :: C Int a => a -> a
```


Otherwise, superclasses and class instance introduce the usual given constraints.  



However, to implement superclass constraints, we need to have a `ClassInstEnv` (similar to the `InstEnv` and `FamInstEnv` right now).  For a vanilla class, if we have `C t1 .. tn` in the constraint pool, we just can add all superclasses of `C` at the appropriate instance types.  However, if `C` is a class family, we need to check whether there is a class-family instance `C r1 .. rn` and a substitution `theta`, such that `theta (C r1 .. rn) == C t1 .. tn`; if so, we can add the superclasses of `C r1 .. rn` at the instance types suggested by `theta`.  This check for a class-family instance requires a function `lookupClassInstEnv` (similar to the current `lookupInstEnv` and `loookupFamInstEnv`).



Finally, we need to exclude overlap of class-family instances in the same way as for data-family instances.


### Desugaring



A class family declaration corresponds to a data family:


```wiki
class family C a1 .. an
    ||
    vv
data family C a1 .. an
```


A class-family instance corresponds to a data-family instance, which is the classes dictionary type.


```wiki
class instance forall b1..bn. ctxt => C t1 .. tn where { sigs }
    ||
    vv
data :R42C b1 .. bn = R42C { super :: ctxt, sigs }
coe $Co:R42C b1 .. bn :: C t1 .. tn ~ :R42C b1 .. bn
$WR42C :: ctxt -> sigs -> C t1 .. tn
  -- the datacon wrapper and the field selectors
  -- use the coercion $Co:R42C to move between the
  -- indexed dictionary type and the representation
  -- dictionary type (the current code in MkId for
  -- data families should do all this already)
```


Moreover, the class-family instance will have a representation as a `Class.Class` in GHC, where the `classTyCon` is `:R42C` (i.e., the instance tycon of the dictionary).  We might also want `Class.Class` to have a `classParent` field as we have this at them moment for instance `TyCon`s.



Finally, a class instance of a class-family instance is translated as usual:


```wiki
instance forall c1..cm. ctxt' => C s1 .. sn where { methods }
    ||
    vv
$dCs1sm :: ctxt' -> C s1 .. sn
$dCs1sm dicts = $WR42C <superdict> methods
```


Moreover, we will have a `InstEnv.Instance` representation of the instance where `is_class` is the name of the class family and `is_tys` is `s1` to `sn`.  This is as lookup in an `InstEnv.InstEnv` does not need to make a distinction between vanilla classes and class-family instances.


### Related work



Compare to **composite class signatures** and **submodules** of the *Modular Type Classes* paper.


### Comments



Roman objects that he really would like collection interfaces to use synonym families (rather than class families) - for example, 


```wiki
class Collection c where
  type Element c
instance Eq a => Collection (Set a) where
  type Element (Set a) = a
instance Ord a => Collection (OrdSet a) where
  type Element (OrdSet a) = a
instance Collection [a] where
  type Element [a] = a
```


**SLPJ note**: I don't understand the question here.


