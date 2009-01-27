## Desugaring of array comprehensions



Wadler's desugaring for list comprehensions is not suitable for arrays, as we need to use collective operations to get good parallel code.  The build/foldr desugaring, although using collective operations, isn't a good match for how the array operations are implemented.  In fact, the *naive* desugaring from the H98 report is a much better fit:


```wiki
(1) [: e | :] 	         = [:e:]
(2) [: e | b, qs :]      = if b then [: e | qs :] else [::]
(3) [: e | p <- a, qs :] = let ok p = [: e | qs :]
		               ok _ = [::]
		           in concatMapP ok a
(4) [: e | let ds, qs :] = let ds in [: e | qs :]
(5) [: e | qs | qss   :] = 
      [: e | (XS, XSS) <- zip [: XS | qs :] [: XSS | qss :] :]
      where XS & XSS are the bound variables in qs & qss
```


In particular, `concatMapP f a` essentially implies to apply the lifted version of `f` directly to `a` and then the concat strips of one level of segment descriptors; i.e., both the `concatP` and the `mapP` vanish due to vectorisation.


## Problem with the naive rules



Nevertheless, these rules are not entirely satisfactory.  For example, `[:e | x <- a, b:]` turns into


```wiki
concatMap (\x -> if b then [:e:] else [::]) a
```


which is a fairly complicated way to perform 


```wiki
mapP (\x -> e) . filterP (\x -> b) $ a
```


even when taking vectorisation into account.  Under vectorisation, the conditional implies `filterP (\x -> b)`, but adds an expensive, and here useless, merge operation.  Maybe these overheads can be optimised away.  However, for the moment, we use a desugaring that is based on the above rules, but generates code that should be better suited to array processing.


## Modified rules



The idea is to flatten out the processing of comprehensions to some degree by defining a transformation function `<< . >>` that gets two arguments: a pattern `pa` and a desugared expression `ea`, where we are guaranteed that `ea` is array valued and all its elements match `pa`.  The semantics of the transformation function is given by


```wiki
<<[: e | qs :]>> pa ea = [: e | pa <- ea, qs :]
                       = concatMap (\pa -> [: e | qs :]) ea
```


We have the second line by applying Rule (3).



Using this definition of `<< . >>`, we can derive a new set of desugaring rules.  The derivation proceeds by unfold/fold transformations and some properties of the involved combinators.  The resulting rules are the following:


```wiki
(1') <<[: e |            :]>> pa ea = mapP (\pa -> e) ea
(2') <<[: e | b, qs      :]>> pa ea = 
  <<[: e | qs :]>> pa (filterP (\pa -> b) ea)
(3') <<[: e | p <- a, qs :]>> pa ea =
  let ok p = True
      ok p = False
  in
  <<[: q | qs :]>> (pa, p) (crossMapP ea (\pa -> filterP ok a))
(4') <<[: e | let ds, qs :]>> pa ea =
  <<[: e | qs :]>> (pa, XS) (mapP (\v@pa -> let ds in (v, XS)) ea)
  where XS are the variables bound by ds
(5') <<[: e | qs | qss   :]>> pa ea =
  <<[: e | qss :]>> (pa, XS) (zipP ea [: XS | qs :])
  where XS are the variables bound by qs
```


The typical array processing comprehensions containing only generators, guards, and parallel comprehensions (but not cross-products and lets) are translated into a straight combination of `mapP`, `filterP`, and `zipP` by these rules, which is exactly what we want.


