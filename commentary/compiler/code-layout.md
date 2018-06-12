# Block Layout



As expected GHC's code generator eventually produces a list of basic blocks
which are then layout out sequentially before finally being translated to
a binary.


## GHCs current block layout algorithm.



GHC's current way of doing so is very basic.


- Build a graph where basic blocks are nodes.
- Add a edge for each jump at the end of a basic block.
- Find sets of [
  strongly connected components](https://en.wikipedia.org/wiki/Strongly_connected_component).
- For each set flatten it resulting in a list of basic blocks.
- Finally place all of these lists after each other.


This works well when all important jumps are at the end of a block.
However it comes with two problems:


- There is no way to incorporate edge weight information directly.
- Important edges might be represented by conditional jumps not at the end of an block
  or calls. Both of which are not considered at all.

## Impact on performance



Modern CPUs are complicated so predicting performance is difficult
at the best of times. After all we want to optimize for:


- Efficiently using L1 cache (64byte lines)
- Efficiently using the fetch window (16Byte)
- Efficiently using the DSB.
- Match alignment with instruction boundries.


We can't really do much about alignments past the first block. GHC doesn't know about the length of instructions and only emits assembly. So we can't calculate where the alignment of blocks/instructions ends up. Changing this would be a major change.



For the other factors there a lot of ways to improve things and even more to make things worse. But the simplest thing we CAN do is to try and place blocks which are executed consecutively in execution order.


## Factorial example



For highlighting the basic issues we don't have to look far. The factorial function already shows some of the problems with ghcs algorithm. Even if they are NOT an issue for this function,



Consider the following Cmm code for factorial:


### Cmm Code


```wiki
       c4Ba: stack check
           if ((Sp + -16) < SpLim) (likely: False) goto c4Bb; else goto c4Bc;
       c4Bb: run gc if stack exhausted
           R2 = R2;
           R1 = $wfactorial_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
       c4Bc: compare against zero
           if (R2 != 0) goto c4B8; else goto c4B9;
       c4B8: call factorial
           I64[Sp - 16] = c4Bh;
           _s4A8::I64 = R2;
           R2 = R2 - 1;
           I64[Sp - 8] = _s4A8::I64;
           Sp = Sp - 16;
           call $wfactorial_info(R2) returns to c4Bh, args: 8, res: 8, upd: 8;
       c4Bh: x * factorial (x-1)
           R1 = I64[Sp + 8] * R1;
           Sp = Sp + 16;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c4B9: x = 1
           R1 = 1;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
```

### Control Flow


```wiki
digraph {
	check_stack -> run_gc[label="exhausted",weight="10"];
	check_stack -> "x/=0"[label="ok",weight="90"];
	".. factorial(x-1)" -> "x*call_result"[label="10",weight="10"];
	"x/=0" -> ".. factorial(x-1)"[label="True",weight="49"];
	"x/=0" -> "ret 1"[label="False",weight="51"];
}
```

### Actual Control Flow


- Repeat:

  - check\_stack
  - x/=0
  - call factorial
- ret 1
- repeat

  -  ret (x\*call\_result)
- done

### Generated Layout


- check\_stack
- x/=0
- ret 1
- ret (x\*call\_result)
- run\_gc
- call factorial

## Possible improvements:



Cmm does not know that the argument is usually not null so placing `ret 1` after the comparison is bad but the best we can do.



However the current algorithm also can't be easily expanded even if we had this information. After all, only one jump per block is ever considered. And even after that hurdle we still have no way to assign weights to edges.



Ideally we would also want to place `call factorial` next to `ret (x*call_result)`. After all there is clearly an edge between these blocks.
However if we return from a large function by the time we return the cache is likely invalidated already.



In general it seems advantageous to NOT treat call returns as viable edges.


## What I looked into:



If we want to use the information present in Cmm we have to make it available when it matters. Which is a few steps down where
cmm has been turned into platform dependent instructions already.



So I started out with building a CFG which is essentially the CmmGraph stripped of the actual code.


### Building a CFG from Cmm



We simply start at the entry block and recursivly add all edges.
But further we also weigh them by the type of jump.



Example Heuristic:


<table><tr><th> Construct    </th>
<th> Weight(s)     
</th></tr>
<tr><th> goto           </th>
<th> 100             
</th></tr>
<tr><th> If/Else        </th>
<th> 49/51           
</th></tr>
<tr><th> If/Else with likelyhood </th>
<th> 10/90  
</th></tr>
<tr><th> Call with return label  </th>
<th> 0     
</th></tr></table>



The main issue with this is that we have to update the cfg with any changes to
the control flow the asm passes introduce after we have created the initial cfg.
While it was quiet annoying to track down all the places where this happens in the
end with some persistence I found all of these.\<sup\>At least for x64 ...\<sup\>I hope ...\</sup\> \</sup\>



For x64 these passes change the cfg:


#### Linear register allocation



Here we add blocks when we join two control flow paths. I solved this by tracking inserted nodes in the register allocator state and updating the cfg afterwards.


#### Generating instructions from Cmm



Initially surprisingly when we have code like below operating on floats.


```wiki
  if (a < b) then { goto L1; } else { goto L2; }
```


We insert a new block `L3: goto L2` and the generated code jumps to L3 if either (a \< b) or if the floats are unordered. Floating point is truely a headache.



The reason is that if (a,b) is unordered we consider a \< b to be false.
So we have to check if they are unordered BEFORE we check if a \< b.



To make it even more complicated we also can't generate a jump directly to the false branch for the parity check.
In the place where we generate the floating point comparison we don't know the label of the false branch.



Long story short. It's a mess.


#### Shortcutting



Shortcutting is removing blocks which only transfer controlflow to another block.
So we reduce:


```wiki
A: ... ; goto B;
B: goto C;
C: ...
```


to `A: goto C; C: ...`



This was obvious and the easiest place to adjust.


### Place blocks based on this graph



We can then construct placements based on this graph.



The basic idea is to find many long sequences of blocks which should be placed
in series. Then place these next to each other. Very much alike the current
algorithm does but based on a weighted digraph instead.



The best sequences intuitively have heavy edges.
So what I ended up doing is visiting each block in reverse postorder and either
append it to an existing sequence if it's a heavy edge. Or create a new sequence consisting of
just this block.



Since this is a very greedy algorithm it misses some cases. So there is some more edge case handling
involved. But it doesn't change the fundamental approach.



To improve on this a bit more we first chain blocks together where it's clear that there is no better edge.



We then go through all edges by weight and check if we find two chains connected by the current edge.
If so we combine them.



And at last we do the same thing but we don't limit ourselves to chains which are connected at the ends by the given
edge. It's good enough if the edge connects one of the first few blocks in one chain with one of the last few blocks
in another chains.



If we have the end of one chain:


```wiki
L1: cmp 1,reg
    jne L2
L3: jmp (*rbx)
```


It's clear that we would like a chain of blocks starting with `L2` to be placed right after `L3`.
The last pass catches these cases.


### Results:



I've combined a version of my layout branch with my patch to [
detect error branches as unlikely and recursion as likely](https://phabricator.haskell.org/D4327) which gave a bigger improvement which is nice.
IgnoreCal is the new codelayout.
Likely04 is new codelayout combined with the likelyhood patch.


```wiki
---------------------------------------------------------------------------------------------------------------------------
        Program           Size      Size    Allocs    Allocs   Runtime   Runtime   Elapsed   Elapsed  TotalMem  TotalMem
                     IgnoreCal  Likely04 IgnoreCal  Likely04 IgnoreCal  Likely04 IgnoreCal  Likely04 IgnoreCal  Likely04
---------------------------------------------------------------------------------------------------------------------------
            Min          -0.1%     -0.1%     -0.0%      0.0%     -3.1%     -5.3%     -3.0%     -6.2%     -1.7%     -0.7%
            Max          +0.0%     +0.0%      0.0%     +0.0%     +1.6%     +7.3%     +1.6%     +7.0%     +0.5%     +0.8%
 Geometric Mean          -0.0%     +0.0%     -0.0%     -0.0%     -0.3%     -1.1%     -0.5%     -1.2%     -0.0%     +0.0%
```

### Things left to do:



A question is how calls would be best handled.
If we have a small function f we really want to keep a sequence like 


```wiki
A: ...;
B: ...;
   call f returns to C;
C: ...;
```


a sequence when laying out the code since there is a good chance blocks B and C will overlap in the cache.



On the other hand if f is a large function then by the time we return any benefit we can gain by putting C
right after B is gone. (Cache lines have been evicted, buffers invalidated, ...).



For now it's seems ignoring call edges leads to better performance.


### Conclusion



After some tweaking this patch [
https://phabricator.haskell.org/D4726](https://phabricator.haskell.org/D4726) lowered runtime on Haswell and Skylake.



The primary change made compared to the above was to ignore edges based on call returns for code layout.
It also special cases the typical "check tag and evaluate if required" control flow by putting the evaluation
code right after the check.



This isn't much but it allows us to take advantage of things like likelihood information so it opens GHC up to new optimizations.


