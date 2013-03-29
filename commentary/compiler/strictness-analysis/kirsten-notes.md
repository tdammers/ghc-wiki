# Linearity



The solution is to distinguish call demands from product demands. Consider again:


```wiki
let f = \ x. \ y. ... in
  ...(f 1 2)...(f 3 4)...
```


The demands placed on `f` by the first and second call get bothed together to yield `SM(SM(T))`. But this is incorrect. Consider:


```wiki
let f = \ x. \ y. ... 
    frob = f 1 in
  ...(f 1 2)...(frob 2)...(frob 3)...
```


Here, the demands placed on `f` by the body of `frob` and by the call to `f` in the `let`-body get bothed together: `S1(T) & S1(S1(T)) = SM(SM(T))`. Note that this is the same as the demand placed on `f` above, yet we want to distinguish between the two situations, because in the first example, the inner lambda in `f`'s rhs is only called once. 



The solution is to treat call demands and product demands differently, and to define the `both` function for call demands to have the same behavior as `lub`. Then in the first example, `f` has demand `SM(S1(T))` placed on it, and in the second, `SM(T)`. This is what we want; now, if `f` has demand `D(D(T)` placed on it, that implies `f` is always called with two arguments.



Why does this make sense? Consider what it means if we see an example like:


```wiki
let baz = lazyF p in
  case p of
    (a,b) -> strictF a b
```


(where `lazyF` is lazy in `p`, and `strictF` is strict in `a` and `b`). `p` is used both with demand `L` (in the call to `lazyF` and with demand `S(SS)` (in the call to `strictF`). This means it's perfectly same to strictly evaluate `p`, so when we both together the two demands, we should get `S(SS)`. On the other hand, if a function is *called* once with one argument and once with two, we don't want to treat it as a function that's always called with two arguments; we're only interested in functions that are *always* called with *n* arguments for a given *n*. Hence, both should behave the same way as lub for call demands.


# Ticky



(NB out-of-date, but maybe historically useful; cf [Debugging/TickyTicky](debugging/ticky-ticky))



The following code inserts extra fields into closures when ticky is enabled (and so had to be commented out):


```wiki
staticTickyHdr :: [CmmLit]
-- The ticky header words in a static closure
-- Was SET_STATIC_TICKY_HDR
staticTickyHdr = 
  | not opt_DoTickyProfiling = []
  | otherwise		     = [zeroCLit]
```


in [compiler/codeGen/CgTicky.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgTicky.hs).



Other relevant functions: `emitTickyCounter` in [compiler/codeGen/CgTicky.hs](/trac/ghc/browser/ghc/compiler/codeGen/CgTicky.hs) (called by `closureCodeBody` in [compiler/codeGen/CgClosure.lhs](/trac/ghc/browser/ghc/compiler/codeGen/CgClosure.lhs)).



Argh! I spent days tracking down this bug: `idInfoLabelType` in [compiler/cmm/CLabel.hs](/trac/ghc/browser/ghc/compiler/cmm/CLabel.hs) needs to return `DataLabel` for labels of type `RednCount` (i.e., labels for ticky counters.) By default, it was returning `CodeLabel`, which caused the ticky counter labels to get declared with the wrong type in the generated C, which caused C compiler errors.


## Declarations for ticky counters



`emitTickyCounter` spits out C declarations that look like this:


```wiki
static char c16O_str[] = "main:Main.$wrepeated{v r4}";

static char c16Q_str[] = "i";

StgWord Main_zdwrepeated_ct[] = {
0x0, 0x1U, 0x1U, 0x0, (W_)&c16O_str, (W_)&c16Q_str, 0x0, 0x0, 0x0
};
```


Here, `Main_zdwrepeated_ct` is actually an `StgEntCounter` (this type is declared in [includes/StgTicky.h](/trac/ghc/browser/ghc/includes/StgTicky.h)). The counters get used by `printRegisteredCounterInfo` in [rts/Ticky.c](/trac/ghc/browser/ghc/rts/Ticky.c), which prints out the ticky reports. The counter fields are accessed using offsets defined in [includes/GHCConstants.h](/trac/ghc/browser/ghc/includes/GHCConstants.h) (`oFFSET_StgEntCounter_*`), which in turn get generated from [includes/mkDerivedConstants.c](/trac/ghc/browser/ghc/includes/mkDerivedConstants.c) (change it and then run `make` in `includes/`. 



\<s\>Note that the first 3 fields of the counters are 16-bit ints and so the generated ticky-counter registration code has to reflect that (I fixed a bug where the first field was getting treated as a 32-bit int.)\</s\> I modified the `StgEntCounter` type so that all fields are `StgWord`s, because it seems that the code generator can't cope with anything else anyway (i.e., in the declaration above, `Main_zdwrepeated_ct[]` is an array of `StgWord`s, even though the C type declaration implies that some fields are halfwords.)



In `emitBlackHoleCode` in [compiler/codeGen/CgClosure.lhs](/trac/ghc/browser/ghc/compiler/codeGen/CgClosure.lhs), "eager blackholing" was getting employed in the case where ticky was turned on; this was causing programs to `<<loop>>` when they wouldn't with ticky disabled, so I turned that off.


# Strictness and let-floating



We run into the following problem in the `transform` nofib benchmark: suppose we have:


```wiki
f x = 
  let foo = stuff in
     foo + x
```


where `stuff` doesn't depend on `x`. Demand analysis says that `foo` has a strict demand placed on it. Later, `foo` gets floated to the top level because it doesn't depend on `x` (in reality it's more complicated because in this case `foo` probably would have gotten floated out before demand analysis, but bear with me). `foo` still has a strict demand signature, which a top-level binding isn't allowed to have. Currently this manifests itself as an assertion failure in [compiler/simplCore/SimplEnv.lhs](/trac/ghc/browser/ghc/compiler/simplCore/SimplEnv.lhs).



There are two possible easy solutions: don't float out bindings for strict things, or "both" the demand for a binder with Lazy when its binding gets floated out. The question is, is it better to do the let-floating and lose the strictness into or to evaluate something strictly but lose sharing?


# Coercions



When we run into an expression like `(Cast e co)` that we're placing demand `d` on, we analyze `e` to get `dmd_ty`, then check whether the depth of `e` is equal to the depth of `dmd_ty` or not. This is necessary because we might be casting a function to a non-function type. So, if `d` and `dmd_ty` have equal depth, we return `dmd_ty` as is; if `d`'s arity is less, we drop the appropriate number of args from `dmd_ty`; if `dmd_ty`'s arity is less, we add the appropriate number of dummy argument demands to it.


# WARN: arity /= dmdTypeDepth rhs\_dmd\_ty && not (exprIsTrivial rhs)



This warning was happening for (at least) two reasons:


- lambdas with a strict non-call demand placed on them were being handled wrong (see the first two examples in [Commentary/Compiler/StrictnessAnalysis/Examples](commentary/compiler/strictness-analysis/examples))
- coercions were being handled wrong, resulting in a demand type with depth 0 being assigned to an rhs consisting of a cast from/to a function type

# Explaining demand transformers



For those who, like me, are a little slow, this example might go in section 5.1 of the paper:



(a):


```wiki
f = \ x -> 
      case x of
        ... -> \ y -> ...
        ... -> \ y -> ...
```


(b):


```wiki
f = \ x ->
     \ y ->
        case x of
           ...
```


In both (a) and (b), `f`'s rhs places a strict demand on `x`. So if we see:


```wiki
(f x)
```


with a strict demand placed on it, it wouldn't be sound to look at `f`'s demand signature and say that `(f x)` places a strict demand on `x` under `f` -- because we don't know whether `f` is like (a) or like (b). This is why when we see a partial application of `f`, we discard all of the argument information in `f`'s demand type.


# Nofib stuff



I've had weird problems with the `time` and `sed` commands under MSYS but I think it's just when running nofib. At some point I wrote down:



TIME needs to be `time` not `/bin/time`



and



MSYS `sed` does not work, use cygwin `sed`



but of \*course\* I no longer remember what I meant.


