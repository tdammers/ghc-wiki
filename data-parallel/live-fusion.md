# Live Fusion: An alternative runtime fusion system (WIP)



**If you came here looking for the description of the compile time Stream Fusion system currently employed in DPH please refer to appropriate papers:**


- [
  Stream Fusion: From Lists to Streams to Nothing at All.](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401) Duncan Coutts, Roman Leshchinskiy and Don Stewart. ICFP 2007.

- [
  Rewriting Haskell Strings.](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.90.3166) Duncan Coutts, Don Stewart and Roman Leshchinskiy. PADL 2007.

- [
  Recycle Your Arrays!](http://www.cse.unsw.edu.au/~rl/publications/recycling.html) Roman Leshchinskiy. PADL 2009.

## Overview



This page presents a fresh approach to array fusion in DPH. It discusses the needs for the research, preliminary design decisions and considerations, as well as some implementation details. This is largely a work in progress at its early stages so none of the design decisions have been finalised.



Although only the section on the *Live Fusion* presents the new and unpublished material, two other fusion systems are described first. There are several reasons for this:


1. to justify the needs for the research by showing the limitations of the current system,
1. to compare the designs of the new and the existing systems,
1. to give credit to the existing systems for the borrowed design decisions.

---


## Stream Fusion (current)



**Skip to [Functional Array Fusion](data-parallel/live-fusion#) or [Compile time fusion drawbacks](data-parallel/live-fusion#) if you are familiar with Stream Fusion.**


### Programming Model



Stream Fusion framework is useful for fusing collective operations that traverse data structures in some uniform fashion. Such data structures are viewed as a continuous stream of values of some type. Both lists and arrays can be presented in such manner, thus the framework is suitable for use in DPH. Stream Fusion introduces two data types: a `Stream` and a `Step`per. 


```wiki
data Stream a = forall s. Stream (s -> Step s a) s
```


A `Stream` is defined by its stepper function and seed. The stepper is used to produce a stream by taking the current seed and yielding the next step. That is, the stepper function (maybe) produces an element and state from the current state. The step may be one of the following:


```wiki
data Step s a = Yield a s
              | Skip s
              | Done
```


`Done` flags the end of the stream, while a `Yield` contains an element and the next seed. If the stream is converted back into the original form of a list of an array, the yielded element will appear in the appropriate position. On the other hand, if a stream `Skip`s the current step does not contain an element (e.g. when a `filter`'s precondition returns false).



Arrays or lists can be converted to and from streams. To save space we will only present a straight forward streaming function for lists.


```wiki
stream :: [a] -> Stream a
stream step0 = Stream next step0
  where next (x:xs) = Yield x xs
        next []     = Done
```


In the above the list tail itself is being used as the seed. Normally, any data structure can be used as the seed/state, though one must be careful not to have any data structures which can be efficiently optimised in a tight loop (TODO give examples of suitable and unsuitable data structures).



Any fusible array combinators could now be defined in terms of `Stream`s, e.g.:


```wiki
mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next0 s0) = Stream next s0
  where next s = case next0 s of
    Done       -> Done
    Skip    s' -> Skip        s'
    Yield x s' -> Yield (f x) s'

map :: (a -> b) -> [:a:] -> [:b:]
map f xs = unstream (mapS f (stream xs))
```


To see how a `Skip` step might be used it might be worthwhile to look at the definition of the `filter` function:


```wiki
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next0 s0) = Stream next s0
  where next s = case next0 s of
    Done                   -> Done
    Skip    s'             -> Skip    s'
    Yield x s' | p x       -> Yield x s'
               | otherwise -> Skip    s'

filter :: (a -> b) -> [:a:] -> [:b:]
filter p xs = unstream (filterS p (stream xs))
```


The fusion opportunity such as `(map f . filter p)` may now be exploited with the following rewrite rule:



`<stream/unstream fusion> forall stream (unstream s) +-> s`



(TODO ppr from latex


1. \\mathmf{(map\\, f\\cdot filter\\, p)}
1. \\mathmf{\\text{⟨}stream/unstream\\, fusion\\text{⟩}\\,\\forall stream(unstream\\, s)\\mapsto s}


)


### Generated Code



In order to completely remove all traces of the helper data structures Stream Fusion relies on several general purpose compiler optimisations. They are discussed in the respective papers referenced at the beginning of this page. The program `TODO: insert program code here` can be desugared and inlined to give


```wiki
TODO: present the program in the desugared/inlined form
```


which can be automatically transformed to a highly efficient:


```wiki
TODO: present the fully optimised program
```


This results in a code a human programmer could have written if efficiency was the goal.


### Drawbacks



Stream Fusion inherits many of the problems associated with compile time fusion. Refer to the appropriate [section](data-parallel/live-fusion#) for an elaborate discussion. However, the approach is very solid otherwise and it might turn out to be yet more effective when combined with a runtime fusion system.



(TODO talk about the limitations of Stream Fusion specifically)


---


## Functional Array Fusion (previous)



**Skip to [Compile time fusion drawbacks](data-parallel/live-fusion#) if you are familiar with Functional Array Fusion.**



The *Functional Array Fusion* system (FAF) previously employed by DPH forms the base for the proposed Live Fusion system. The approach is attractive due to its simplicity, although it's more restrictive than Stream Fusion. FAF has emerged in an attempt to reduce the large list of array combinators to a small fixed number of fusible ones.



In FAF all of the library functions are expressed in terms of two combinators: `loop` and `replicate`. The principle idea behind FAF is to loop over an array, applying a certain *mutator function* to each element possibly producing a new element. Examples of mutator function for well known combinators are presented below:


```wiki
-- map: f is free, accumulator unused
map_mf :: e -> a -> (Maybe e', a)
map_mf x _ = (Just (f x), ())

-- filter: predicate p is free, accumulator unused
filt_mf :: e -> a -> (Maybe e', a)
filt_mf x _ = if (p x) then (Just x,  ())
                       else (Nothing, ())

-- enumFromTo: accumulator stores the value we are up to
enum_mf :: () -> Int -> (Maybe Int, Int)
enum_mf _ curr = (Just curr, curr+1)

-- foldl: f is free, the accumulator stores the partial result
fold_mf :: e -> a -> (Maybe (), a)
fold_mf x acc = (Nothing, f x acc)  -- no element produced
```


Expectedly, a *mutator function* takes an element of type `e` and possibly produces a new element of type `e'`. While `e` is the type of elements of the array currently being iterated, the next intermediate array in a pipeline of operations is of type `e'`. The new element is wrapped in an option type to allow not to produce an element at a particular iteration. This concept is not unlike Steam Fusion's *stepper function*. The result `Just x` corresponds to returning `Yield x`, while `Nothing` is akin to a `Skip`.



Apart from the array element currently being looked at the *mutator function* takes a second argument of an arbitrary type called accumulator. In reduction combinators accumulator it is, while in enumerations, for example, it it used to keep track of the value computed so far.



The folding and the enumeration combinators are also interesting in a sense that semantically the first produces no array, while the second consumes no array. Following the terminology of the original paper, the former is a *pure consumer* while the latter is a *generator*. However, looking at the type signatures of their corresponding mutator functions, they seem to produce/consume arrays of `Unit`s. This is intentional since one of the design goals was to minimise the number of combinators needed to generalise the library interface. Performance-wise, creating these dummy `Unit` arrays is cheap. The parametric representation mechanism used by DPH ([
Instant Generics.](http://www.cse.unsw.edu.au/~chak/papers/CDL09.html) Chakravarty, Ditu, Lshchinskiy. 2009.) only stores the length of such arrays making their creation and iteration very cheap. The `replicate` combinator mentioned previously is used for precisely this purpose: to generate dummy arrays that *array generators* could use.



What has not been shown yet, is the actual `loop` combinator. Its type signature is presented below:


```wiki
loop :: (e -> a -> (Maybe e', a)) -- mutator function
     -> a                         -- accumulator
     -> Array e                   -- input array
     -> (Array e', a)
```


Expectedly, it is given a mutator function, an initial value of the accumulator and an array to iterate. The function returns both the resulting array and the final accumulator value produced in the final iteration. In the case with the pure consumers (like `fold`) this value is the only one we are interested in. Sometimes we may be interested in both results as in the case with the `scan` combinator which would return an array partial results (TODO double check the term) and the final `fold` value that fell off the end of the array.



Note that we are entirely omitting the details of how segmented array combinators are represented. However, the FAF system is capable of doing that too. It is the `loop` combinator that changes to accommodate the segmentation information, the mutator functions, on the other hand, mostly remain unchanged



Lastly, we look at how multiple consequent loops are fused together. The basic idea is similar to the way two maps may be fused together:


```wiki
map f (map g xs) = map (f . g) xs
```


Likewise, FAF composes multiple mutator functions together. It then passes the resulting function to a single `loop` combinator to produce the desired result without the intermediate steps. It get a little more complicated that that since we have to deal with the accumulators, as well as the fact that a mutator function is not obliged to produce an element at every iteration. We will present the code for the composition operator in the section on [Live Fusion](data-parallel/live-fusion#flattening).


---


## Compile time fusion drawbacks



(TODO IMPORTANT Add concrete examples)


### Inlining and Rewriting



The current project is carried out completely within the context of the Data Parallel Haskell project. Therefore, the fusion frameworks designed specifically for Haskell were the first to turn to. We have discussed two of them here: Stream Fusion and Functional Array Fusion. Glasgow Haskell Compiler's rewrite rules functionality plays crucial rule in all three of them. This is not coincidental:


- Compile-time term rewriting is a fundamental optimisation technique in the implementation of functional programming languages. Exposing it to the user ([
  Playing by the rules.](http://research.microsoft.com/en-us/um/people/simonpj/papers/rules.htm) Peyton Jones, Tolmach, Hoare. 2001) makes it an attractive way to expose compiler functionality to pure library code

- Inlining ([
  Secrets of the GHC inliner.](http://research.microsoft.com/~simonpj/papers/inlining) Peyton Jones, Marlow. 2001) is another technique which is crucial in compilers like GHC which heuristically removes superfluous levels of indirection in the original code. As a side effect it provides more opportunities for term rewriting to happen

- Haskell is a purely functional language therefore valid term rewriting can be done without a sophisticated analysis of side effects. Rewriting would generally be unsafe in a non-pure context

- (In the case with Stream Fusion) it is reasonable to rely on the existence of certain compiler optimisations since DPH relies on GHC and is not designed for other Haskell implementations


The above statements suggest that compile time equational fusion seems like a natural choice for the Haskell programming language. This is especially valid for Stream Fusion where the authors were able to achieve, through inlining, rewriting and compiler optimisations, the speed of the handwritten code. However, the strong dependence on the optimisation systems of such a complex system as GHC makes the fusion frameworks fragile and non-portable.


### Let-Floating



One of the cases in which fusion breaks is when two array operation do not end up being adjacent after inlining. This may happen due to the so called Let-Floating optimisation in GHC ([
Let-Floating.](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.30.9079) Peyton Jones, Partain, Santos. 1996.). This optimisation is designed to avoid duplicating work as well as reduce the amount of thunk allocation. (TODO give an example where it poses a problem) This optimisation is the opposite of inlining. If the term that was forcibly floated in or out would have otherwise completed a pattern for a fusion rule, that fusion opportunity is missed.


### Sharing



The other problem with equational fusion is that sharing is not clearly defined. This is related to the above problem. Sharing prevents large amounts of work to be duplicated. Aggressive unconditional inlining would have introduced a major inefficiency for programs in which the result of a pipeline of costly array operations is independently used in more than on place. Recomputing the shared portion may result in a noticeable performance hit.


---


## Live Fusion (proposed)



The above suggests that correct inlining plays a major part in the process of fusion. It also suggests that the decisions taken by the inliner and other optimisations do not always result in the optimal code for exploiting fusion. 



One of the goals of the current work is to reduce the dependency of successfully exploited fusion opportunities on the behaviour of the inliner. It was decided to explore the possibility of performing fusion at runtime of the program. That would eliminate the need for the inliner at least for the part of decision making when fusing array operations together. The DESOLA library ([
DESOLA.](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.142.1454) Russell, Mello Kelly, Backmann.), while designed for C++, serves as a starting point for performing fusion at runtime. The new framework reuses its approach of constructing a dependence tree. When an array computation is later forced by a pure consumer, the tree can be optimised yielding an equivalent computation in a fused form.


### Delayed Array Computation Handles



The very first design decision to make was the way to represent delayed array operations as nodes in the tree. Haskell's ability to delaying function application and effortless function composition to create new functions has lead us to the following GADT definition for a tree node:


```wiki
data DelayedH a acc where
  LoopH      :: (IElt a0, IElt a)
             => (acc -> a0 -> ((Maybe a), acc))
             -> acc
             -> DelayedH a0 acc0
             -> DelayedH a  acc
  ReplicateH :: IElt a => Int -> a -> DelayedH a ()
  VectorH    :: IElt a => Vector a -> DelayedH a ()
```


A *Delayed Handle* represents an array computation. It may be one of the following:


1. a handle to a loop based computation for which we store an appropriate mutator function, the initial accumulator value and the parent node in the tree. It represents an operation which would have been implemented with a `loop` combinator in FAF.

1. a handle to a delayed replication, i.e. an array of a given length which has the same value in each position.

1. a handle to a physical array data (Data.Vector) for the cases where the array has been computed previously by means other than `loop/replicate`.

### Flattening



A tree represented in this manner may be flattened at any time by composing all of the `Loop`'s mutator functions and nesting their respective accumulators. Thus, any computation is flattend to a single `LoopH` wrapping either a `ReplicateH` or a `VectorH`.



(TODO insert image before and after flattening a toy example)



The following presents the code for the "composition" operator of mutator functions. It essentially flattens two loops into one.


```wiki
(-<>-) :: (acc2         -> a1 -> (Maybe a2, acc2))
       -> (acc1         -> a0 -> (Maybe a1, acc1))
       -> ((acc1, acc2) -> a0 -> (Maybe a2, (acc1, acc2)))
(-<>-) mf2 mf1
  = let mf3 (z1, z2) x
          = case mf1 z1 x of
            (Nothing, z1') = (Nothing, (z1', z2))
            (Just x', z1') = case mf2 z2 x' of
                               (Nothing,  z2') = (Nothing,  (z1', z2'))
                               (Just x'', z2') = (Just x'', (z1', z2'))
    in mf3
```


Given The resulting mutator proceeds in two steps:


1. It first applies the first mutator function `mf1` to an element producing a new accumulator and *possibly* a new element.
1. If an element has been produced by the first mutator function, only then it is run through the second mutator function `mf2`. Otherwise the computation is short-circuited and a `Nothing` value is retured in place of an element and the old accumulator is reused.


Note that composing two mutator function results in a tuple of accumulators `(acc1, acc2)`. If the pipline of array operations consisted of four combinators, each having its own mutator function, we would end up with `(((acc1, acc2), acc3), acc4)` accumulator nesting. Clearly this changes the result type of the original topmost loop, more presicely the expected accumulator component is no longer `acc4` but the nested accumulator.



To mitigate this, the original FAF system uses additional helper functions with additional rewrite rules. Bringing fusion into the runtime domain, provides us with extra flexibility. We are able to simply extract the required accumulator from the nesting before returning it to the user. **(TODO This provides no real explanation. Do we need to be more precise about what extra flexibility we have at runtime and why we had to treat the nesting at every step in FAF?. Decide after completing the example above.)**


### Implementation



Many of the library's interface functions can be implemented in terms of the handles to the delayed array operations. Thus, using the mutator function definitions from above, the corresponding array combinators can be implemented as follows:


```wiki
replicate :: IElt a => Int -> a -> Array a
replicate n x = Array (ReplicateH n x)

map :: (IElt a, IElt b) => (a -> b) -> Array a -> Array b
map f (Array arrh) = Array loopH 
  where loopH  = LoopH mf () arrh
        mf _ x = (Just (f x), ())

filter :: IElt a => (a -> Bool) -> Array a -> Array a
filter p (Array arrh) = Array loopH
  where loopH  = LoopH mf () arrh
        mf _ x = if p x then (Just x,  ())
                                     else (Nothing, ())

scan :: IElt a => (a -> a -> a) -> a -> Array a -> Array a
scan f z (Array arrh) = Array loopH
  where loopH  = LoopH mf z arrh
        mf z x = (Just z, z `f` x)

enumFromStepLen :: Int -> Int -> Int -> Array Int
enumFromStepLen start step len = Array loopH
  where loopH = LoopH mf start (ReplicateH len ())
        mf curr _ = let next = curr + step
                    in (Just curr, next)
```


In the above the `Array` type is simply a wrapper for the `DelayedH` tree node handles. It conveniently hides the accumulator type from the user (though this interface is still not for the general use by the client programmer):


```wiki
data Array a = forall acc . Array (DelayedH a acc)
```

### Evaluation (`loop` combinator)



As a special case, the catamorphic functions (pure consumers), not only obtain a new delayed array handle but also force the evaluation of the tree:


```wiki
fold :: IElt a => (a -> a -> a) -> a -> Array a -> a
fold f z (Array arrh) = snd $ eval loopH
  where 
    loopH  = LoopH mf z arrh
    mf z x = (Nothing :: Maybe (), z `f` x)
```


In the above the `eval` function has the following type signature and (a slightly simplified) implementation:


```wiki
eval :: IElt a => DelayedH a acc -> (Vector a, acc)
eval = eval' . flatten
  where
    eval' (ReplicateH n x) = (replicate n x, ())
    eval' (LoopH mf z (ReplicateH n x)) = loop (\z _ -> mf z x) z (replicate n ())
    ...
```


It flattens the delayed evaluation tree to a single loop as described in the appropriate section above. It then calls the actual `loop` combinator which performs the allocation of the new array and the traversal computation:


```wiki
loop :: (IElt a, IElt b)
     => (acc -> a -> (Maybe b, acc))     -- mutator function
     -> acc                              -- accumulator
     -> Vector a                         -- input array(s)
     -> (Vector b, acc)                  -- resulting array and final accumulator value
loop mf z arr = runST ..
```


In the definition of `fold` we only take the second component of the result - the actual fold value. It is the accumulator value returned in the final iteration. We disregard the resulting array though as it is simply an array of units. An empty one, in fact, since the mutator function for a `fold` never produces an array element.



While the implementation of `eval` presented above has been oversimplified, it carries another very important responsibility. It is the `eval` function that drops the accumulator nesting in favour of a single accumulator mentioned in the type signature of the outermost `LoopH`. The implementation is a little crude at the moment since the flattened tree reuses the same GADT as the unflattened tree accumulated at runtime. Indeed, semantically the flattened tree uses the very same `LoopH`, `ReplicateH` and `VectorH` handles but since these can be mixed in several flavours, it makes the pattern matching excessive and error-prone. When this is fixed, explaining the dropping of the accumulator nesting would be straight forward.


### Performance



TODO


### Discussion



TODO


