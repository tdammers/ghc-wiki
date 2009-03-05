# Desugaring instance declarations



These notes compare various ways of desugaring Haskell instance declarations.
The tradeoffs are more complicated than I thought!


## Basic stuff


```wiki
class C a where
  opF :: a -> Int
  opG :: a -> Bool
  opG x = True

instance C Int where
  opF n = n*2
```


These desugar to the following Core:


```wiki
data C a = DC (a->Int) (a->Bool)

opF :: C a -> a -> Int
opF d = case d of { DC opf _ -> opf }

opG :: C a -> a -> Bool
opG d = case d of { DC _ opg -> opg }

$dmopG :: C a -> a -> Bool
$dmopG d x = True

Rec {
  dCInt :: C Int
  dCInt = DC opFI opGI
  
  opFI :: Int -> Int
  opFI n = n*2

  opGI :: Int -> Bool
  opGI = $dmopG dCInt
}
```


(Notation: I am omitting foralls, big lambdas, and type arguments.
I'm also using `f x = e` rather than `f = \x.e`.)



Points worth noting:


- The class gives rise to an eponymous data type (in GHC it is actually
  called `:TC`), the dictionary. 

- There is an eponymous top-level selector function for each class method, 
  `opF` and `opG` in this case.

- The default method for `opG` becomes a top-level function `$dmopG`.
  It takes the `(C a)` dictionary a argument because the RHS is allowed to call
  other methods of C.

- The instance declaration defines a dictionary `dCInt`.  Notice  
  that it's recursive, because we must pass `dCInt` to `opGI`.

- Crucially, the simplifier is careful not to choose `dCInt` as
  a loop breaker, and hence if it sees `case dCInt of ...` it
  can simplify the `case`. 

- If `$dmopG` is inlined, the recursion is broken anyway.

## Dictionary functions



Now consider an instance declaration that has a context:


```wiki
instance C a => C [a] where
  opF xs = case xs of 
             []     -> 0
             (y:ys) -> opF y + opf ys
```


Here is one way to desugar it.


```wiki
dCList :: C a -> C [a]
dCList d_a = letrec {
	       d_as = DC opfl opgl

               opfl xs = case xs of
                          [] -> 0
                          (y:ys) -> opF d_a y + opF d_as ys

               opgl = $dmopG d_as }

             } in d_as
```


Notice that


- If we inline the selector `opF` in `opF d_as`, then
  we can simplify `opfl` to give a directly-recursive function:

  ```wiki
                 oplf xs = case xs of
                            [] -> 0
                            (y:ys) -> opF d_a y + opf ys
  ```

  This is important.

- The BAD THING is that `dCList` is big, and hence won't be inlined.
  That's bad because it means that if we see

  ```wiki
     ...(opF (dCList dCInt))...
  ```

  we don't get to call `opfl` directly. Instead we'll call `dCList`, build 
  the dictionary, do the selection, etc.  So specialiation won't happen,
  even when all the types are fixed.

## The INLINE strategy



An obvious suggestion, which GHC implemented for a long time, 
is to give `dCList` an INLINE pragma.  Then it'll inline at every call
site, the dictionary will be visible to the selectors, and good things happen.



But it leads to a huge code blow-up in some cases.
We call these dictionary functions a lot, often in a nested way, and
we know programs for which the INLINE-all-dfuns approach generates
gigantic code.  (Example: Serge's DoCon.)


## The out-of-line (A) strategy



The INLINE strategy would make sense if `dCList` could be guaranteed small.
Suppose the original instance declaration had been like this:


```wiki
instance C a => C [a] where
  opF = opF_aux

opF_aux :: C a => a -> Int
opF_aux xs = case xs of 
               []     -> 0
               (y:ys) -> opF y + opf ys
```


This is exactly what GHC 6.10 now does, behind the scenes.
Desugaring just as above, we'd get the following:


```wiki
Rec {
  dCList :: C a -> C [a]
  dCList d_a = letrec {
	         d_as = DC opfl opgl
                 opfl = opF_aux d_a
                 opgl = $dmopG d_as
               } in d_as

  opF_aux :: C a -> a -> Int
  opF_aux d_a xs = let { d_as = dCList d_a } in
                   case xs of
                     []     -> 0
                     (y:ys) -> opF d_a y + opF d_as ys
}
```


Notice that


- `dCList` is guaranteed small, and could reasonably be INLINEd
  at every call site.  This good because it exposes the dictionary
  structure to selectors.

- `dCList` and `opF_aux` are mutually recursive.  But if we 
  avoid choosing `dCList` as the loop breaker we can inline
  `dCList` into `opF_aux`, and then the `opF` selector
  can "see" the dictionary structure, and `opF_aux` simplifies, thus:

  ```wiki
    let { d_as = dCList d_a } in
    case xs of
      []     -> 0
      (y:ys) -> opF d_a y + opF d_as ys

  =                  { Inline dCList }
    let { d_as = letrec {
  	         d_as = DC opfl opgl
                   opfl = opF_aux d_a
                   opgl = $dmopG d_as
                 } in d_as
     } in
    case xs of
      []     -> 0
      (y:ys) -> opF d_a y + opF d_as ys

  =                  { Float the letrec }
    letrec { d_as = DC opfl opgl
             opfl = opF_aux d_a
             opgl = $dmopG d_as
     } in
    case xs of
      []     -> 0
      (y:ys) -> opF d_a y + opF d_as ys

  =                  { Inline opF and d_as }
    case xs of
      []     -> 0
      (y:ys) -> opF d_a y + opF_aux d_a ys
  ```

  Good!  Now `opF_aux` is self-recursive as it should be.
  The same thing happens with two mutually recursive methods

- BUT notice that we reconstruct the `(C [a])` dictionary on
  each iteration of the loop.  As Ganesh points out in [\#3073](http://gitlabghc.nibbler/ghc/ghc/issues/3073), that
  is sometimes bad.

## The out-of-line (B) strategy



We can avoid reconstructing the dictionary by passing it to `opF_aux`, 
by recasting latter thus:


```wiki
instance C a => C [a] where
  opF = opF_aux

opF_aux :: (C [a], C a) => a -> Int
opF_aux xs = case xs of 
               []     -> 0
               (y:ys) -> opF y + opf ys
```


Notice the extra `C [a]` in the context of `opF_aux`.
(Remember this is all internal to GHC.)
Now the same desugaring does this:


```wiki
  dCList :: C a -> C [a]
  dCList d_a = letrec {
	         d_as = DC opfl opgl
                 opfl = opF_aux d_a
                 opgl = $dmopG d_as
               } in d_as

  opF_aux :: C [a] -> C a -> a -> Int
  opF_aux d_as d_a xs = case xs of
                     	  []     -> 0
                     	  (y:ys) -> opF d_a y + opF d_as ys
```


The two definitions aren't even recursive.  BUT now that `d_as` is
an *argument* of `opF_aux`, the latter can't "see" that it's 
always a dictionary!  Sigh.  As a result, the recursion in `opF_aux`
always indirects through the (higher order) dictionary argument, using
a so-called "unknown" call, which is *far* less efficient than 
direct recursion.



Note also that


- Typechecking `opF_aux` is a bit fragile; see [\#3018](http://gitlabghc.nibbler/ghc/ghc/issues/3018).  Trouble is that
  when a constraint `(C [a])` arises in its RHS there are two ways
  of discharging it: by using the argument `d_as` directly, or by
  calling `(dCList d_a)`.  As [\#3018](http://gitlabghc.nibbler/ghc/ghc/issues/3018) shows, it's hard to guarantee that
  we'll do the former.

## User INLINE pragmas and out-of-line (A)



There is another difficulty with the out-of-line(A) strategy,
that is currently unsolved.  Consider something like this:


```wiki
  instance C a => C (T a) where
     {-# INLINE opF #-}
     opF x = if opG x then 1 else 2
```


Then we'll desugar to something like this:


```wiki
Rec {
  dCT :: C a -> C (T a)
  {-# INLINE dCT #-}
  dCT d_a = ...opF_aux...

  {-# INLINE opF_aux #-}
  opF_aux d_a = ...dCT...
}
```


The INLINE on `dCT` is added by the compiler; the INLINE on `opF_aux` is
just propagated from the users's INLINE pragma... maybe the RHS is big.



Now the difficulty is that we GHC currently doesn't inline into the RHS
of an INLINE function (else you'd get terrible code blowup). So the 
recursion between `dCT` and `opF_aux` is not broken.  One of the two must be chosen
as loop breaker, and the simplifier chooses `opF_aux`.  Ironcially, therefore
the user INLINE pragma has served only to guarantee that it *won't* be inlined!!



(This issue doesn't arise with out-of-line(B) because (B) doesn't make
`dCT` and `opF_aux` mutually recursive.)


## Summary



Here are the current (realistic) options:


- Out-of-line(A): GHC 6.10 does this. 

  - Good: recursive methods become directly mutually-recursive
  - Bad: lack of memoisation
  - Bad: difficulty with user INLINE pragmas

- Out-of-line(B)

  - Good: memoisation works
  - Very bad: recursive methods iterate only via "unknown" calls.
  - Good: no difficulty with user INLINE pragmas


My current difficulty is that I see no way to get all the good things
at once.



PS: see also the comments at the start of `compiler/typecheck/TcInstDcls.lhs`, which cover some of the same ground.


