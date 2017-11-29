# Nested closure representation in STG



Progess on this is tracked by Trac [\#14461](http://gitlabghc.nibbler/ghc/ghc/issues/14461).


## The problem



Consider this function (taken from comment:87 of [\#7258](http://gitlabghc.nibbler/ghc/ghc/issues/7258)):


```wiki
f10 :: A (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
f10 =
  A (\i0 ->
  A (\i1 ->
  A (\i2 ->
  A (\i3 ->
  A (\i4 ->
  A (\i5 ->
  A (\i6 ->
  A (\i7 ->
  A (\i8 ->
  A (\i9 ->
  N (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9)
  ))))))))))
```


In STG form the body of `f10` looks like this


```wiki
let f0 = [] \[i0].
         let f1 = [i0] \[i1].
                  let f2 = [i0,i1] \[i2].
                           let f3 = [i0,i1,i2] \[i3].
                                    N (i0,i1,i2,i3)
                           in A f3
                  in A f2
         in A f1
in A f0
```


(I have shortened it to n=3, but you can see the pattern.)  Reminder: the
form


```wiki
   let f = [fv1,...,fvn] \[arg1,...,argm]. body
```


binds `f` to a heap-allocated function closure with free variables `fv1,...,fvn`,
and arguments `arg1,..., argm]`.



Consider the code for function `f2`:


- On entry R0 points to the closure, and R1 points to the argument `i2`
- The first thing it does is to allocate a closure for `f3`, which has free vars `[i0,i1,i2]`
- So it fetches the first two by indirection from R0; the third, `i2` is already in R1.
- It allocates a 4-word closure, andn stores the three free varaiables in it. (The fourth word is the info pointer.)


So the closure for `f3` looks like


```wiki
--------------------------
| f3_info | i0 | i1 | i2 |
--------------------------
```


Clearly we generate a quadratic amount of code, and execute a quadratic number of loads
and stores.  This is not good.


## A solution



An obvious solution is to share the already-allocated closures.  For example,
the closure for `f3` could point to the (already-allocated) closure for `f2`, thus


```wiki
--------------------
| f3_info | . | i2 |
------------|-------
            |
            V
            ---------------------
            | f2_info | i0 | i1 |
            ---------------------
```


If we did this all the way down we'd get a linear amount of code, we'd execute
a linear number of loads and stores.  Function closures would no longer be "flat", but
that's ok.


# Problem: Updatable thunks



While this solution is quite simple it poses some problems with respect to updatable thunks: If an `inner` closure refers to its `outer` closure, which happens to be an updatable thunk, we can't guarantee safe access to `outer`s free variables for `inner`! When `outer` is forced its value will be written back to `outer`s closure and `outer`s info table is updated to be an indirection instead of a thunk. `outer`s free variables are now considered garbage and we might segfault when trying to access these in `inner`.


## When exactly can we do this?



The basic setup is this:


```wiki
  let p = [a,b,c] \[x,y].
          .....(let { q = [a,b,c,d,e,f] \[r,s]. blah } in ...)...
```


That is:


- Start with some "parent" closure `p`, with free variables `[a,b,c]`
- At some arbitary place in `p`'s code we see a binding for `q`, with free variables
  `[a,b,c,d,e,f]`: a superset of the free-vars of `p`.
- In that case we can replace the free vars `a,b,c` of `q`'s closure with `p`.


We might express the transformation like this:


```wiki
  let p = [a,b,c] \[x,y].
          .....(let { q = [p{a,b,c},d,e,f] \[r,s]. blah } in ...)...
```


Here the notation `p{a,b,c}` in `q`'s free-var list indicates that we store a single pointer (to `p`) in `q`'s closure; but that makes available the variables `a,b,c` in `q`'s RHS `blah`.



This can be nested.  In original example we'd get


```wiki
let f0 = [] \[i0].
         let f1 = [i0] \[i1].
                  let f2 = [i0,i1] \[i2].
                           let f3 = [f2{i0,i1},i2] \[i3].
                                    let f4 = [f3{f2{i0,i1},i2},i3] \[i4].
                                             N (i0,i1,i2,i3,i4)
                           in A f3
                  in A f2
         in A f1
in A f0
```


I took it up to n=4 to illustrate.  Notice that


- For `f2` there was no point in storing a pointer to `f1`; that's no more compact than storing `i0` directly.
- For `f4` we have this structure, reflected in its free-var list:

  ```wiki
  --------------------
  | f4_info | . | i3 |
  ------------|-------
              |
              V
              --------------------
              | f3_info | . | i2 |
              ------------|-------
                          |
                          V
                          ---------------------
                          | f2_info | i0 | i1 |
                          ---------------------
  ```

## Things to think about


- In the above example I have used a pointer to the parent closure as soon as doing so saves a single word.  But we could choose to do so only when it saves, say, three words.  Then in small (common) cases we would get no nesting at all; but for large cases we woudl nest in "chunks" of three.  Sounds easy and attractive.

- You might wonder if you could use a pointer to a parent closure even if the child doesn't use *all* the free vars of the parent. E.g

  ```wiki
  let f = [i1, ..., i20] \[v].
          let g = [i2, ... i20] \[w]. blah
          in ...
  in ...
  ```

  Here `g` shares all of `f`'s free variables except `i1`.  Making `g`'s closure point to `f`'s would be sound, but would risk a space leak by keeping `i1` alive for too long.

>
>
> This would be fixable, by keeping a liveness bit-map in `g`'s info-table, and using it when scavenging `g`'s closure.
> But that would be new work.
>
>

- There may be many candidate parents of `q`, but there is always a "best" one (i.e. with the maximum number of free vars).

- One could be more ambitious and look for "peer" sharing too

  ```wiki
    let f = [] \[x].
            case x of (c,d,,e) ->
               let g = [c,d,e] \[r]. blah1
                   h = [c,d,e] \[r]. blah2
               in ...
  ```

  Here perhaps `h` could point to `g`.  Or maybe they can both point to a shared record.  There is *plenty* of material in the literature describing schemes like this, dating back at least thirty years.   But our goal for now is simply to gather the low-hanging fruit; hence the "parent" idea.


  


- All this affects what free variables to save across a case. For example

  ```wiki
    let p = [a,b,c] \[x,y].
            case x of K d e _ f) ->
            let { q = [p{a,b,c},d,e,f] \[r,s]. blah } in ...
  ```

  Here we must save `p` (but not `a,b,c`) across the `case`.

## Implementation



Because of the business about what to save across a case expression,
I think we should do the analysis on STG, and record the results in STG free-var lists.


