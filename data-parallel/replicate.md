# Preventing space blow-up due to replicate


## The problem



The vectorisation transformation lifts scalar computations into vector space.  In the course of this lifting, scalar constants are duplicated to fill an array, using the function `replicateP`.  Array computations are lifted in a similar manner, which leads to array constants being replicated to form arrays of arrays, which are represented as a segmented arrays.  A simple example is our  'smvm' example code:


```wiki
smvm :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm m v = [: sumP [: x * (v !: i) | (i,x) <- row :] | row <- m :]
```


Here the variable 'v' is constant in the array comprehensions and will be replicated while lifting the expression `v !: i`.   In other words, for every single element in a `row`, lifting implies the allocation of a separate copy of of the entire array `v` — and this only to perform a single indexing operation on that copy of `v`.  More precisely, in the lifted code, lifted indexing (which we usually denote by `(!:^)` is applied to a nested array consisting of multiple copies of `v`; i.e., it is applied to the result of `replicatePA (lengthPA row) v`.  



This is clearly wasted effort and space.  However, the situation is even worse in Ben's pathological example:


```wiki
treeLookup :: [:Int:] -> [:Int:] -> [:Int:]
treeLookup table xx
  | lengthP xx == 1
  = [: table !: (xx !: 0) :]
  | otherwise
  = let len     = lengthP xx
        half    = len `div` 2
        s1      = sliceP 0    half xx
        s2      = sliceP half half  xx           
      in concatP (mapP (treeLookup table) [:s1, s2:])
      -- or better: concatP (mapP (treeLookup table) (segmentP [:half, len - half:] xx))
```


Here `table` is constant in `mapP (treeLookup table) [:s1, s2:]`; hence, the entire `table` gets duplicated on each level of the recursion, leading to space consumption that is exponential in the depth of the recursion.


## What's happening here?



Replication of scalars and arrays is always a waste of time and space.  However, it is particularly problematic if the replicated structure is consumed by an indexing operation as it can change the asymptotic work complexity of the vectorised program.  This holds not only for indexing, but for any operation that consumes only a small part of its input array(s).  In other words, if a replicated structure is consumed in its entirety (for example by a fold), the asymptotic work complexity of replication matches that of consuming the structure.  For operations that only consume a small part of their input, that is not the case.  Hence, lifting, which introduces the replication, does increase asymptotic work.


## Our goal



Generally, we don't want to copy replicated data — it's a waste of space!  We definitely don't want to do it for large data structures, and in particular, we don't want to do it for arrays.  After all, that can change the asymptotic work complexity of a program.  To keep it simple, we are for the moment focusing on avoiding replicating arrays, as this is were our current practical problems are coming from.  (Independently of our current plans, some cases of replicated scalars are eliminated by fusion.)



To clarify the scope of the present work:



*Goals*


- Avoid physically creating multiple copies of the same array due to `replicateP` and `replicateP^` **introduced by vectorisation.**
- Ensure that consumers of replicated arrays only perform work in proportion to the size of the arrays before replication.


*Non-goals* (they are worthwhile goals, but we don't attempt them at the moment)


- Avoid physically creating multiple copies of scalars due to `replicateP` (this includes large scalars, such as list or trees).
- Avoid deep traversals of arrays of trees for `packP` and similar.


The main difference to Roman's original approach was that he included the above non-goals as goals.  We agreed to leave them as non-goals for the time being and return to them, once we are confident that we eliminated the main space blow-up. Although, Roman pointed out that it might be easier to prove that the new approach preserves work complexity through vectorisation — something, which we eventually will have to show.



NB: We will have to revisit replication of scalar structures as such scalar structures may be large trees.


## Where does the problematic replication originate?



The applications of `replicatePA` and `expandPA` that introduce the problematic replication of arrays are in the definition of `mapP`, specifically `replicatePA` is used in `mapP_S` and `expandPA` in `mapP_L` (see Page 403 of HtM):


```wiki
mapP_S :: (a :-> b) -> PA a :-> PA b
mapP_S (Clo env _ fl) xss
  = fl (replicatePA (lengthPA xss) env) xss

mapP_L :: PA (a :-> b) -> PA (PA a) -> PA (PA b)
mapP_L (AClo env _ fl) xss
  = unconcatPA xss (fl (expandPA xss env) (concatPA xss))
```


In both cases, we replicate the environment of a closure before we apply the lifted version of the function represented by the closure.  This is important as it guarantees that the consumer of these occurrences of `replicatePA` and `expandPA` process (topmost) segment structure in a homomorphic manner (after all, we are implementing a `map` function here)!



Our basic plan to avoid array duplication is to change `replicatePA` and `expandPA` such that they produce a segmented array that encodes the replication without unnecessarily copying data and that the consumer —the lifted function `fl`— processes segmented arrays with encoded replication in a special manner.  As we will see, that also leads to the requirement that index transformations on replicated arrays, such as `packP`, need to preserve the compact encoding.


## Fixing the problem: avoid to repeat segments


### Repeated segments



A replicated array results is always represented by a segmented array; more precisely, by a segmented array where a contiguous sequence of segments contains the same data.  For example, we have


```wiki
replicatePA 3 [:1, 2, 3:] = [:[:1, 2, 3:], [:1, 2, 3:], [:1, 2, 3:]:]
  where
    [:[:1, 2, 3:], [:1, 2, 3:], [:1, 2, 3:]:] = ([:3, 3, 3:], [:1, 2, 3, 1, 2, 3, 1, 2, 3:])
```


and


```wiki
expandPA [:2, 3:] [:[:1, 2:], [:3:]:] = [:[:1, 2:], [:1, 2:], [:3:], [:3:], [:3:]:]
  where
    [:[:1, 2:], [:1, 2:], [:3:], [:3:], [:3:]:] = ([:2, 2, 1, 1, 1:], [:1, 2, 1, 2, 3, 3, 3:])
```


NB: `expandPA` is lifted replication; `expandPA` is the name we used in HtM.


### Collapse repeated segments



In practice, segments descriptors store more information than just the segment length.  They at least additionally store the start position of each segment in the data array.  In the conventional representation, an invariant is that the start positions are such that the segments don't overlap.  To represent arrays with repeated segments more efficiently, we might relax that invariant.  Specifically, all start positions of a contiguous sequence of repeated segments may be the same; i.e., the segment data is stored only once per sequence of repeated segments.



Then, we have for `[:[:1, 2, 3:], [:1, 2, 3:], [:1, 2, 3:]:]`,


```wiki
start: [:0, 0, 0:]
len:   [:3, 3, 3:]
data:  [:1, 2, 3:])
```


and for `[:[:1, 2:], [:1, 2:], [:3:], [:3:], [:3:]:]`,


```wiki
start: [:0, 0, 2, 2, 2:]
len:   [:2, 2, 1, 1, 1:]
data:  [:1, 2, 3:])
```


This is merely a change in the array representation that does not affect vectorisation.



**TODO** I am not sure whether we want to talk about this representation at all.  Maybe just go to the one with virtual segments straight away.  I suspect the latter will be less confusing in the paper.


### Segment descriptor representation with virtual segments



Instead, of repeating the start indices in a segment descriptor, we alternatively might want to represent a segmented array with repeated segments by distinguishing its *physical* from its *logical* (or *virtual*) representation.  Specifically, instead of representing `[:[:1, 2, 3:], [:1, 2, 3:], [:1, 2, 3:]:]` as


```wiki
start: [:0, 0, 0:]
len:   [:3, 3, 3:]
data:  [:1, 2, 3:])
```


we might instead represent it as


```wiki
vsegs: [:0, 0, 0:]
pstart: [:0:]
plen:   [:3:]
data:  [:1, 2, 3:])
```


where `pstart`, `plen`, and `data` represent the underlying segmented array (with non-overlapping segments) and `vsegs` specifies the logical segments of the array, where physical segments may occur not at all, once, or multiple times.  In this example, the only physical segment is repeated three times.



Our second example, `[:[:1, 2:], [:1, 2:], [:3:], [:3:], [:3:]:]`, which we previously represented as


```wiki
start: [:0, 0, 2, 2, 2:]
len:   [:2, 2, 1, 1, 1:]
data:  [:1, 2, 3:])
```


will now be


```wiki
vsegs: [:0, 0, 1, 1, 1:]
pstart: [0, 2:]
plen:  [:2, 1:]
data:  [:1, 2, 3:])
```


We choose the representation with virtual segments over the one with repeated start indicies for the following reasons:


- `packP` on a nested array that arose from replication only needs to operate on the virtual segments.
- Clean separation between the physical representation and the logical.
- Probably easier to subsequentyly move to Roman's proposal that moves replication information to the top of the representation.

## Operations on arrays with virtual segments



Array consumers need to be adapted to work correctly on virtual segments and we need to be careful to make sure that although index space transformations introduced by vectorisation, such as `packP`, operate with a work complexity in proportion to the number of virtual segments (and not the size of the virtual number of elements).  Moreover, other consumers, such as folds, must operate in work complexity in proportion to the physical size of the segmented array (and not in proportion to the virtual size.)


### Lifted indexing



In the `smvm` example, a replicated array is consumed by lifted indexing to extract matching elements of the vector for all non-zero elements of the matrix.  Using just an length array as a segment descriptor without virtual segments, lifted indexing might be implemented as follows:


```wiki
(as_len, as_data) !:^ is = bpermutePA as_data ((prescanPA (+) 0 as_len) +^ is)
```


With overlapping segments, we have


```wiki
(as_vsegs, as_pstart, as_plen, as_data) !:^ is = bpermutePA as_data (bpermutePA as_start as_vsegs +^ is)
```


In the case of `smvm`, where the first argument is produced by `replicatePA (lengthPA row) v`, we have `as_vsegs = replicatePA (lengthPA row) 0` and `as-data = v`.  In other words, lifted indexing draws from a single copy of `v`, which is what we wanted.


### Splitting and combining (for lifted conditions)



Due to lifted conditionals (or, generally, case constructs), arrays with virtual segments may be split (packed) and combined.  Arrays with virtual segments can be split (or packed) by merely packing the virtual segment descriptor.  Hence, some physical segments may not appear as virtual segments at all.  This ensures good asymptotical complexity for packing, but also means that subsequent operations need to be careful to avoid operating on physical segments that do not appear as virtual segments, as this would be wasted work.



Arrays with virtual segments can not always be combined without duplicating the data corresponding to repeated segments (after all, a disjoint segment may be inserted into a sequence of repeated segments).  For simplicity, we may want to expand all repeated segments and remove all not used physical segments in `combinePA`.  (It seems that this should not lead to unexpected blow-ups as the repeated data now is part of a functions result and should be accounted for in its complexity estimate.)



Some examples:


```wiki
desire:dph-common-vseg benl$ ghc --interactive examples/Test.hs -package dph-par -package dph-prim-par -Iinclude


-- pprv : pretty print virtual array
*Test> pprv arrN3
[[0],[1,2,3],[5,6,7,8,9]]


-- pprp : pretty print physical array
*Test> pprp arrN3
PArray  3
   PNested
       vsegids:    [0,1,2]
       pseglens:   [1,3,5]
       psegstarts: [0,1,4]
       psegsrcs:   [0,0,0]
       PInt [0,1,2,3,5,6,7,8,9]


-- With segmented replicate we only replicate the vsegs fields
--  The functions ending PA' take lists for some arguments, and are just for experimentation.
--  Functions ending in plain PA take real arrays.

*Test> pprv $ replicatesPA' [2, 4, 3] arrN3
[[0],[0],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[5,6,7,8,9],[5,6,7,8,9],[5,6,7,8,9]]

*Test> pprp $ replicatesPA' [2, 4, 3] arrN3
PArray  9
   PNested
       vsegids:    [0,0,1,1,1,1,2,2,2]
       pseglens:   [1,3,5]
       psegstarts: [0,1,4]
       psegsrcs:   [0,0,0]
       PInt [0,1,2,3,5,6,7,8,9]


-- To pack an array, we pack the vsegs then drop out the psegs that 
-- aren't referenced by any vseg.

*Test> pprv $ packByTagPA' (replicatesPA' [2, 4, 3] arrN3) [1, 0, 0, 0, 0, 0, 1, 0, 1] 1
[[0],[5,6,7,8,9],[5,6,7,8,9]]

*Test> pprp $ packByTagPA' (replicatesPA' [2, 4, 3] arrN3) [1, 0, 0, 0, 0, 0, 1, 0, 1] 1
PArray  0
   PNested
       vsegids:    [0,1,1]
       pseglens:   [1,5]
       psegstarts: [0,4]
       psegsrcs:   [0,0]
       PInt [0,1,2,3,5,6,7,8,9]


-- Applying concatPA merges the two outermost layers.

*Test> pprv $ concatPA $ packByTagPA' (replicatesPA' [2, 4, 3] arrN3) [1, 0, 0, 0, 0, 0, 1, 0, 1] 1
[0,5,6,7,8,9,5,6,7,8,9]

*Test> pprp $ concatPA $ packByTagPA' (replicatesPA' [2, 4, 3] arrN3) [1, 0, 0, 0, 0, 0, 1, 0, 1] 1
PArray  11
   PInt [0,5,6,7,8,9,5,6,7,8,9]
```

### Reduction



We need to be careful when reducing an array with virtual segments, as we want the work complexity to be in proportion of the physical and not in proportion of the virtual segments.  Consider


```wiki
f xs is = mapP f is
 where
   f i = sumP xs + i
```


As `xs` is free in `f` it will be in the environment of the array closure constructed by the vectorised code.  Hence, given the implementation of `mapP` discussed earlier, `xs` will be replicated `length is` times.  However, we want to work complexity of `sumP xs` be in proportion of `length xs` and not of `length xs * length is`.



To this end, `sumP^` performs a reduction on the physical segments only.  Afterwards, the per-segment results are distributed over a vector of the length of the `vsegs` descriptor using `bpermutePA`.



As we saw in the previous subsection, some physical segments may not appear as virtual segments at all (if a replicated array shrunk by applying a pack operation).  Hence, we need to be careful that `sumP^` only reduces those physical segments of the array that are used by one or more virtual segments.


### Append


```wiki
*Test> :type arrN3
arrN3 :: PArray (PArray Int)

*Test> pprv arrN3
[[0],[1,2,3],[5,6,7,8,9]]

*Test> pprp arrN3
PArray  3
   PNested
       vsegids:    [0,1,2]
       pseglens:   [1,3,5]
       psegstarts: [0,1,4]
       psegsrcs:   [0,0,0]
       PInt [0,1,2,3,5,6,7,8,9]


*Test> pprv arrN4
[[7,8,9,10,11,12,13],[0],[1,2,3],[0]]

*Test> pprp arrN4
PArray  4
   PNested
       vsegids:    [0,1,2,3]
       pseglens:   [7,1,3,1]
       psegstarts: [0,7,8,11]
       psegsrcs:   [0,0,0,0]
       PInt [7,8,9,10,11,12,13,0,1,2,3,0]


-- Append is also an index space transform. When we append two arrays
-- we append the segmentation information, but just put both flat arrays
-- into the result, without copying their data. Note how the result
-- contains both PInt arrays from the source.

*Test> pprv $ arrN3 `appPA` arrN4
[[0],[1,2,3],[5,6,7,8,9],[7,8,9,10,11,12,13],[0],[1,2,3],[0]]


*Test> pprp $ arrN3 `appPA` arrN4
PArray  7
   PNested
       vsegids:    [0,1,2,3,4,5,6]
       pseglens:   [1,3,5,7,1,3,1]
       psegstarts: [0,1,4,0,7,8,11]
       psegsrcs:   [0,0,0,1,1,1,1]
       PInt [0,1,2,3,5,6,7,8,9]
       PInt [7,8,9,10,11,12,13,0,1,2,3,0]
```

### Splitting and joining (for distribution across threads)



Our idea is to continue to split the **data** array evenly across threads.  That may spread out the segments descriptor (length and starts arrays) very unevenly as repeated segments have an entry for each repetition in the segmentation information, but not in the data.



TODO Roman believes splitting arrays with repeated segments is a problem.  To me it doesn't seem to be that much of a problem.  (Keep in mind that we only need a restricted number of operations on arrays with repeated segments — all their consumers are homomorphisms as discussed in Plan B.


### Multiple levels of nesting (unconcat and concat)


```wiki
*Test> :type arrM6
arrM6 :: PArray (PArray (PArray Int))

-- This array has a complex representation that includes multiple flat vectors (the PInt vectors).
-- This is because it has been created by appending several other arrays together.

*Test> pprv arrM6
[[[7,8,9,10,11,12,13],[0],[1,2,3],[0]],[[0],[1,2,3]],[[0],[1,2,3],[5,6,7,8,9]],[[5,6,7,8,9]],[[1,2,3,4,5],[1,2,3],[7,8,9,10,11,12,13],[1,2,3]],[[5,6,7,8,9]]]

*Test> pprp arrM6
PArray  6
   PNested
       vsegids:    [0,1,2,3,4,5]
       pseglens:   [4,2,3,1,4,1]
       psegstarts: [0,4,6,9,10,14]
       psegsrcs:   [0,0,0,0,0,0]
       PNested
           vsegids:    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
           pseglens:   [7,1,3,1,1,3,1,3,5,5,5,3,7,3,5]
           psegstarts: [0,7,8,11,0,1,0,1,4,0,0,5,8,15,0]
           psegsrcs:   [0,0,0,0,1,1,2,2,2,3,4,4,4,4,5]
           PInt [7,8,9,10,11,12,13,0,1,2,3,0]
           PInt [0,1,2,3]
           PInt [0,1,2,3,5,6,7,8,9]
           PInt [5,6,7,8,9]
           PInt [1,2,3,4,5,1,2,3,7,8,9,10,11,12,13,1,2,3]
           PInt [5,6,7,8,9]


-- To pack the array we pack the vsegs, then drop the psegs that aren't referenced
-- from any vseg. The pack operation only operates on the outer-most layer,
-- so the inner segmentation isn't touched.

*Test> pprv $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
[[[7,8,9,10,11,12,13],[0],[1,2,3],[0]], [[0],[1,2,3],[5,6,7,8,9]], [[5,6,7,8,9]]]

*Test> pprp $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
PArray  0
   PNested
       vsegids:    [0,1,2]
       pseglens:   [4,3,1]
       psegstarts: [0,6,9]
       psegsrcs:   [0,0,0]
       PNested
           vsegids:    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
           pseglens:   [7,1,3,1,1,3,1,3,5,5,5,3,7,3,5]
           psegstarts: [0,7,8,11,0,1,0,1,4,0,0,5,8,15,0]
           psegsrcs:   [0,0,0,0,1,1,2,2,2,3,4,4,4,4,5]
           PInt [7,8,9,10,11,12,13,0,1,2,3,0]
           PInt [0,1,2,3]
           PInt [0,1,2,3,5,6,7,8,9]
           PInt [5,6,7,8,9]
           PInt [1,2,3,4,5,1,2,3,7,8,9,10,11,12,13,1,2,3]
           PInt [5,6,7,8,9]


-- Applying concat merges the two outer-most layers. For replicated arrays, this
-- has the potential to copy data, but only at the outer-most level.

*Test> pprv $ concatPA $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
[[7,8,9,10,11,12,13],[0],[1,2,3],[0],[0],[1,2,3],[5,6,7,8,9],[5,6,7,8,9]]

*Test> pprp $ concatPA $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
PArray  8
   PNested
       vsegids:    [0,1,2,3,4,5,6,7]
       pseglens:   [7,1,3,1,1,3,5,5]
       psegstarts: [0,7,8,11,0,1,4,0]
       psegsrcs:   [0,0,0,0,2,2,2,3]
       PInt [7,8,9,10,11,12,13,0,1,2,3,0]
       PInt [0,1,2,3]
       PInt [0,1,2,3,5,6,7,8,9]
       PInt [5,6,7,8,9]
       PInt [1,2,3,4,5,1,2,3,7,8,9,10,11,12,13,1,2,3]
       PInt [5,6,7,8,9]


-- Concatenating the above array merges the segmentation with the flat arrays, 
-- producing a flat array. To put this another way, concat performs a 'gather'
-- operation which forces out segmentation information by copying data. 

*Test> pprv $ concatPA $ concatPA $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
[7,8,9,10,11,12,13,0,1,2,3,0,0,1,2,3,5,6,7,8,9,5,6,7,8,9]

*Test> pprp $ concatPA $ concatPA $ packByTagPA' arrM6 [1, 0, 1, 1, 0, 0] 1
PArray  26
   PInt [7,8,9,10,11,12,13,0,1,2,3,0,0,1,2,3,5,6,7,8,9,5,6,7,8,9]
```

## The bigger picture



It makes sense to see this work and the concepts behind the Repa library as part of a bigger picture.  In both cases, we want to avoid the overhead of *index space transformations*.  In Repa, we use *delayed* arrays —i.e., arrays represented as functionals— to delay the execution of index transformations (as well as maps) and to fuse them into consumers.  In Repa, we do that for index transformations explicitly specified by the programmer and we rely on the programmer to be aware of situations, where delayed arrays need to be `forced` into *manifest* form before they are consumed by an array operation that cannot be represented in delayed form — e.g., in matrix-matrix multiplication, we need to `force` the transposed array to improve cache behaviour.



In DPH, we are first of all concerned about chains of index transformations that begin with a lifted replicate as these lead to an asymptotic increase of work complexity as discussed above.  However, other index transformations, such as non-lifted replicate, are of concern as well, and will need to be addressed eventually.



Despite the conceptual similarity, there are two big differences between the situation in Repa and DPH:


1. In Repa, arbitrary user-specified index transformations are being delayed and we rely on the programmer to `force` these explicitly where necessary — i.e., a user needs to be aware of this whole mechanism.  In contrast, in DPH, we aim at eliminating the index transformations introduced by the vectoriser (of which many programmers will not be aware); hence, the elimination also needs to be without user intervention.
1. In Repa, we have no segmented arrays; hence, functions are sufficient to represent delayed arrays.  In contrast, in DPH, segmented arrays are central and we need auxiliary data structures —such as virtual segment descriptors— to delay index transformations efficiently.


 
A question for the future is whether we can find a uniform framework that works for both Repa's regular arrays and DPH's nested arrays. This would provide a key to an integrated system.


## Related work


- The work-efficient vectorisation paper by Prins et al.  Their approach only works in a first-order language.
- Blelloch's work on the space-behaviour of data-parallel programs.

---


# OLD MATERIAL


## A plan to fix the problem



Generally, we don't want to copy replicated data — it's a waste of space!  We definitely don't want to do it for large data structures, and in particular, we don't want to do it for arrays.  After all, that can change the asymptotic work complexity of a program.  So, instead of having `replicateP` allocate and fill a new array with multiple copies of the same data, we use a special array representation that stores the data (once!) together with the replication count.  This is surely the most space efficient representation for a replicated array.



The downside of a special representation is that we now also need to modify all consumers of replicated arrays to accept this new representation and to handle it specially.  This leads to some code blow up (each array consumer needs to be able to dynamically decide between two input array representations), and we need to be careful not to disturb fusion.


### The trouble with indices



Although, a replicated array stores its replicated payload only once, it needs to be handled with care. When indexing into a replicated array, we still use the same indices as if the array data would have been copied multiple times.  That can be a problem in examples, such as `treeLookup` above where the replicated array iteration-space grows exponentially — even 64bit indices will eventually overflow.  However, we can circumvent that problem by taking care in code that consumes replicated arrays.



In the `treeLookup` example, the `table` is replicated and grows exponentially.  But it is a segmented structure (one segment per copy of the original array) and it is accessed in the base case by a lifted index operation.  When you look at the input to that application of lifted indexing, its first argument is huge (the replicated `table`), but the second argument contains the same *data* as the original value of `xx`, albeit segmented into an array with one segment per element.  So we have effectively got


```wiki
 [:table, table, ...., table:] !^ [:[:xx_1:], [:xx_2:], ..., [:xx_n:]:]
```


Note how the `xx_i` are unchanged.  It is only *in the implementation of `(!^)`* that the `xx_i` are blown up to index into the data vector of `[:table, table, ...., table:]` (which is `concatP [:table, table, ...., table:]`).  It is that multiplication of the `xx_i` with the prescaned segment descriptor of `[:table, table, ...., table:]` that will overflow.  Notice how that is internal to the implementation of `(!^)`.  If the first argument of `(!^)` is a replicated structure, there is no need whatsoever to perform that multiplication (and subsequent division) and the indices never overflow!


### Never take the length of a replicated array



Unfortunately, not only indices blow out, the length of replicated arrays may also overflow 64bit integers.  Hence, the consuming code must carefully avoid to take the length of such arrays.  This is only the case for `replicateP`s introduced by the vectoriser.  It is the responsibility of the DPH user to ensure that `replicateP`s that are explicit in the user code do not blow out.  (We may want to switch to 64bit indices —at least on 64bit builds— anyway.)


## Concrete implementation of replicated arrays



The DPH library is built on the [
vector](http://hackage.haskell.org/package/vector) package (that provides high-performance sequential arrays).  This package heavily relies on a cheap representation of sliced arrays — i.e., arrays of which a subarray is extracted.  Such array slices are not copied, but represented by a reference to the original array together with markers for the start and end of the slice.


### Replicating and slicing



When implementing replicated arrays, we need to take into account that (1) a replicated may be a sliced vector and (b) that the partitioning of a parallel array across multiple threads requires to slice that parallel array.



****\******* Is this really an issue?  After all, we don't replicated thread-local slices of parallel arrays (which in turn may be sliced vectors), but replicate parallel arrays (which are already distributed over multiple threads).
**


