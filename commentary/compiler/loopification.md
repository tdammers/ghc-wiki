# Loopification



Loopification is a C-- optimisation pass that turns tail recursion into proper loops.



Here is a summary of relevant links and tickets


- [
  Krzysztof Wos's project](http://research.microsoft.com/en-us/um/people/simonpj/tmp/wos-diss-draft.pdf) in which he reports great performance improvements by turning tail recursion into loops in C--. 

- Tickets:

  - [\#8285](http://gitlabghc.nibbler/ghc/ghc/issues/8285)
  - [\#8793](http://gitlabghc.nibbler/ghc/ghc/issues/8793), [\#11372](http://gitlabghc.nibbler/ghc/ghc/issues/11372); see comment 15 of [\#8793](http://gitlabghc.nibbler/ghc/ghc/issues/8793)) etc, where it seems that we are missing loopification for a simple IO function
  - [\#8585](http://gitlabghc.nibbler/ghc/ghc/issues/8585) concerned getting the loop to start *after* the stack check
  - [\#13567](http://gitlabghc.nibbler/ghc/ghc/issues/13567): Do loopification using join points

## Current implementation (Apr 17)



Loopification was implemented by Jan Stolarek:


```wiki
commit d61c3ac186c94021c851f7a2a6d20631e35fc1ba
Author: Jan Stolarek <jan.stolarek@p.lodz.pl>
Date:   Thu Aug 29 10:57:04 2013 +0100

    Optimize self-recursive tail calls

    This patch implements loopification optimization. It was described
    in "Low-level code optimisations in the Glasgow Haskell Compiler" by
    Krzysztof Woś, but we use a different approach here. Krzysztof's
    approach was to perform optimization as a Cmm-to-Cmm pass. Our
    approach is to generate properly optimized tail calls in the code
    generator, which saves us the trouble of processing Cmm. This idea
    was proposed by Simon Marlow. Implementation details are explained
    in Note [Self-recursive tail calls].

    Performance of most nofib benchmarks is not affected. There are
    some benchmarks that show 5-7% improvement, with an average improvement
    of 2.6%. It would require some further investigation to check if this
    is related to benchamrking noise or does this optimization really
    help make some class of programs faster.

    As a minor cleanup, this patch renames forkProc to forkLneBody.
    It also moves some data declarations from StgCmmMonad to
    StgCmmClosure, because they are needed there and it seems that
    StgCmmClosure is on top of the whole StgCmm* hierarchy.
```


It's not a massive patch (580 lines of diff).



There is a succession of small follow-up patches. It's controlled by `-loopification`, which on by default with `-O` and `-O2`.


## New idea: use join points



The basic idea is this: instead of doing loopification in the
code generator, express it directly in Core using join points.



Consider


```wiki
  f = \x. letrec g n y = if n>x then y
                                  else g (n+1) (y+n)
          in g 0 0
```


GHC will spot g as a join point, and we'll get a nice tight loop with a direct jump.
No need for the loopification patch!



But suppose the program was instead


```wiki
  f = \x xs. letrec g n y = if n>x then y
                                   else g (n+1) (y+n)
             in map (g 0) xs
```


Now g is not tail called, so GHC won't turn it ito a join point.
We'll get a heap allocated closure for g (unless we lambda lift it, which is a separate matter), and things are Not So Good.



Idea: transform the program thus:


```wiki
  f = \x xs. let g n y = joinrec gj n y = if n>x then y
                                                 else gj (n+1) (y+n)
                         in gj n y
             in map (g 0) xs
```


Now we can (and in fact do) generate a nice loop:


```wiki
f = \ (x_aSH :: Int) (xs_aSI :: [Int]) ->
      map @ Int @ Int
        (\ (y_aSL :: Int) ->
           case y_aSL of { GHC.Types.I# ww1_s2aH ->
           joinrec {
             $wgj_s2aJ [InlPrag=[0], Occ=LoopBreaker]
               :: GHC.Prim.Int# -> GHC.Prim.Int# -> Int
             [LclId[JoinId(2)], Arity=2, Str=<S,U><S,U>m, Unf=OtherCon []]
             $wgj_s2aJ (ww2_s2aD :: GHC.Prim.Int#) (ww3_X2aY :: GHC.Prim.Int#)
               = case x_aSH of { GHC.Types.I# y1_a27U ->
                 case GHC.Prim.tagToEnum# @ Bool (GHC.Prim.># ww2_s2aD y1_a27U) of {
                   False ->
                     jump $wgj_s2aJ
                       (GHC.Prim.+# ww2_s2aD 1#) (GHC.Prim.+# ww3_X2aY ww2_s2aD);
                   True -> GHC.Types.I# ww3_X2aY
                 }
                 }; } in
           jump $wgj_s2aJ 0# ww1_s2aH
           })
        xs_aSI
```

### The main transformation



The main transformation is this.  If we have a recursive binding


```wiki
  rec { f x = ...f... }
```


where the recursive calls to `f` are all saturated tail calls, then replace the binding with


```wiki
  nonrec { f x = join
                   f x = ...f...
                 in f x }
```


(We are using shadowing here, to avoid making up a new variable name.)


### Top level



We can do this at top level too.


```wiki
   g n y = if n>x then y
                  else g (n+1) (y+n)
===>
   g n y = joinrec gj n y = if n>x then y
                                   else gj (n+1) (y+n)
```


Now when g is called it'll run in a tight local loop with good code generation.
This is definitely better.  Again, it means that we don't need the loopifiation patch.



**Side note**: if you try this today, you'll actually find that GHC


- Spots the join point
- But then full laziness floats it to top level so it's not a join point.


We should stop making full laziness float join points!


### Mutual recursion



All this works for mutually recursive join points.  (The loopification patch does not.)


```wiki
letrec
   f x = ....g e1....
   g y = ....f e2...
in
   ...f...

===>

letrec
   f x = joinrec fj x = ....gj e1....
                 gj y = ....fj e2....
         in fj x
in
   ...f...
```


(assuming of course that the uses of f,g in their own RHSs
are all saturated tail calls).



The body of the letrec must use only *one* of the functions
in the recursive group (although it does not need to use
it in a join-pointy way).  I can't see how to make this work
if both `f` and `g` are used. ** see below for an idea **



At top level, a "use" includes exports.



Here's an idea for handling multiple entry points into a mutually recursive loop without copying the body of one of the functions. There is a slight penalty when initially entering the loop due to the case, but it may pay off since the looping would be more efficient:


```wiki
letrec
   f x = ....g e1....
   g y = ....f e2...
in
   if ...
      ... f ...
    else
      ... g ...

===>

data FG a b = DoF a | DoG b

letrec
   combined z = 
     joinrec fj x = ...gj e1...
             gj y = ...fj e2...
     in
       case z
         of DoF x => fj x
          | DoG y => gj y

   f x = combined (DoF x)
   g y = combined (DoG y)
in
   if ...
      ... f ...
    else
      ... g ...
```


This requires a new data type, which is annnoying (and not easy in GHC). But perhaps we can use an unboxed sum type.  (And an unboxed tuple for the arguments.)


