


# Debugging GHC-compiled code with gdb



If you're working on a GHC port, tweaking the runtime, or modifying
the code generator, at some point you will have to become familiar
with how to use [ gdb](http://www.gnu.org/software/gdb/) for debugging
crashes in GHC-compiled programs.



Debugging of Haskell code in gdb got better recently, as you can ask GHC to generate DWARF annotations in the compiled Haskell code, which gdb can use to provide useful backtraces in Haskell code and to map symbol names to Haskell source code location.  To get this, compile with the `-g` option. (for the best results, use GHC 8.4.1 or later).



The DWARF annotations can be a bit flaky still, and then we might be left with having to debug the compiled Haskell code at the assembly level.  This can seem quite scary, but there are lots of resources on this wiki to help you understand what's going on.


## The recipe



Usually you want to do something along these lines:


- Compile all the Haskell code (including libraries) `-g` to get DWARF annotations.

- Compile with `-dcore-lint` to make sure the crash isn't caused by the compiler
  generating incorrect code in a way that can be detected statically.

- Compile with `-debug`!  This links the program with a debugging
  version of the runtime, that includes extra assertions, debugging
  code, and debugging info for gdb (if you compile using cabal, use the unstripped binary from dist/build/).


 


- Don't compile with `-threaded` (assuming the bug still happens
  with the non-threaded runtime).

- Use runtime debugging options to help narrow down the fault (see also the [relevant User Manual section](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime_control.html#rts-options-debugging)).

- Grab our [gdb macros](/trac/ghc/attachment/wiki/Debugging/CompiledCode/.gdbinit)[](/trac/ghc/raw-attachment/wiki/Debugging/CompiledCode/.gdbinit).

- Run the program in gdb until it crashes, type `where` to find
  out whether the crash is in the RTS or in Haskell code.  Hopefully
  you'll get a useful backtrace through both Haskell and C code, and
  if you compiled with `-g` you should see Haskell source code
  locations.  If the crash was in the runtime, then you can use gdb's
  full C debugging facilities to track down the problem.

- Make the crash happen as early as possible.  `-debug` already
  turned on lots of assertions, one of which might trigger.  Also,
  try `+RTS -DS` which turns on a bunch of extra "sanity
  checking", i.e. expensive assertions about the state of the runtime
  at regular points.  One thing this enables is a full sweep of the
  heap after each garbage collection to make sure there are no
  dangling references.  Also, it fills all free memory with the value
  `0xaaaaaaaa` (the sound of GHC disappearing down a hole).

- Look at the fragment of code that crashed: `disassemble`.  It
  should be pretty small (code blocks in GHC-generated code usually
  are).  Look at the instruction that caused the crash - the address
  is given in the `where` output earlier.  If you can tell what
  the problem is right there, then great.

- Poke around in the heap and stack and see if you can figure out
  what has gone wrong.  See "poking around", below.

- Make the crash happen repeatably, if possible.  That means after
  the exact same number of garbage collections and context switches
  each time. Use `+RTS -C0` to turn off timer-based context switches.
  Setting `+RTS -V0` disables the RTS timer entirely.  Disabling the RTS
  timer altogether can make the point at which the program crashes reproducible,
  and is very helpful when using the "going back in time" method described below.
  If you're debugging heap profiling, add `-i0` too.

- Similarly, if possible, turn off address space layout randomization (ASLR).
  On Linux you can do this with "echo 0 \> /proc/sys/kernel/randomize\_va\_space".

- If you still haven't figured it out, you probably need to go back in
  time a bit to find out what was happening just before the crash.
  This is the tricky bit, because going back in time isn't directly
  supported by gdb, we need to find a way to hit a breakpoint a short
  time before the crash happened.  See "going back in time", below.

## Debugging stack overflows


- Set a breakpoint on `threadStackOverflow`, and when it gets hit
  `p *tso.stackobj` will tell you the stack pointer, e.g.
  `sp = 0x7ffff69fe228`. Then `pmem 0x7ffff69fe228 64` will show you
  what's at the top of the stack.

## Going back in time



There's a great tool called `rr` for reverse debugging. Try it out - if you're lucky it will work and you'll be able to single-step backwards and set breakpoints and watchpoints in the past.  If `rr` doesn't work for you for whatever reason, try the following tricks.


- Set a breakpoint on a code fragment that is regularly executed.
  Some good examples are `stg_upd_frame_info` (the standard
  update code), `stg_upd_frame_1_info` (the update code for a
  1-tagged object, eg. a cons cell), `stg_ap_p_info` (the apply
  code for a single pointer argument).

>
> >
> >
> > `break stg_upd_frame_info`
> >
> >
>

- Ignore that breakpoint for ever

>
> >
> >
> > `ignore 1 9999999`
> >
> >
>

- Run the program

>
> >
> >
> > `run`
> >
> >
>

- Find out how many times the breakpoint was hit

>
> >
> >
> > `info break`
> >
> >
>

- Suppose it was hit 4325 times, then next time we'll ignore it
  for 4324 times (i.e. subtract one), which will stop on the 4325th
  time, just before the crash.

>
> >
> >
> > `ignore 1 4324`
> >
> >
>

- Run the program

>
> >
> >
> > `run`
> >
> >
>

- Single step until the crash happens

>
> >
> >
> > `si`...
> >
> >
>

- If the crash doesn't happen for a long time, try picking another
  break point (e.g. something you stepped through this time). You
  might have to repeat this a few times to get to a useful point.

- Sometimes the crash is a long way into the program, and setting a
  frequently accessed breakopint like `stg_upd_frame_info` means
  the program just runs too slowly; in this case you might have to
  use a 2-stage strategy instead.  Set a breakpoint on a less
  frequently accessed point (a good one I use for this is
  `GarbageCollect`, which stops at each GC), do the ignore thing
  on this, and then switch to a more frequently accessed breakpoint
  when you are closer to the crash.

## Mapping symbol names to source code



Gdb's `list` command is useful for this, if the code was compiled with `-g`:


```wiki
(gdb) list *s5aO_info
0x7ffff5762138 is in s5aO_info (compiler/basicTypes/NameCache.hs:95).
90	        Just occ_env -> lookupOccEnv occ_env occ
91	
92	extendOrigNameCache :: OrigNameCache -> Name -> OrigNameCache
93	extendOrigNameCache nc name
94	  = ASSERT2( isExternalName name, ppr name )
95	    extendNameCache nc (nameModule name) (nameOccName name) name
96	
97	extendNameCache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
98	extendNameCache nc mod occ name
99	  = extendModuleEnvWith combine nc mod (unitOccEnv occ name)
```


The `list` command can also be used with a raw address, like `list *0x7ffff5762138`.  This is often better, because a symbol name can be ambiguous, whereas an address is unique.



To go in the opposite direction, use `info line`:


```wiki
(gdb) info line compiler/basicTypes/NameCache.hs:95
Line 95 of "compiler/basicTypes/NameCache.hs"
   starts at address 0x7ffff5762138 <s5aO_info>
   and ends at 0x7ffff5762141 <s5aO_info+9>.
```

## Decoding symbol names



Symbols in GHC are encoded using something called the [Z-encoding](commentary/compiler/symbol-names) (see
[compiler/utils/Encoding.hs](/trac/ghc/browser/ghc/compiler/utils/Encoding.hs)).  Basically special symbols are replaced by sequences
beginning with `z` or `Z`.  eg. `state#` becomes
`statezh`.  The letter `z` itself is replaced by `zz`.



External symbols follow the pattern
*module*\_*identifier*\_*kind*.  Where *module* is the module
name, *identifier* is the identifier in the STG code that this
symbol relates to, and *kind* is the kind of symbol:


<table><tr><th>`closure`</th>
<th>a static closure
</th></tr>
<tr><th>`info`</th>
<th>an info table, also entry code or return address
</th></tr>
<tr><th>`con_info`</th>
<th>an info table for a constructor
</th></tr>
<tr><th>`fast`</th>
<th>a primitive
</th></tr></table>



(see [compiler/cmm/CLabel.hs](/trac/ghc/browser/ghc/compiler/cmm/CLabel.hs)
for a table of these).  Note that if you're matching up assembly with
C-- and (info) tables next to code is enabled (as it is by default),
then code that is named `entry` is equivalent to `info` symbols
in the final binary.



For example:


>
>
> `GHCziBase_ZMZN_closure`
>
>


Means the static closure for `[]` in module `GHC.Base`.


>
>
> `DataziList_foldlzq_info`
>
>


Means the entry code (or info table) for `Data.List.foldl'`.



Internal symbols are of the form *identifier*\_*kind*, i.e. the
same as external symbols but without the module name.


## Poking around



There are two things you usually want to inspect inside a code
fragment: the contents of heap objects, and the contents of the
stack.  A full description of the layout of these things is here: [Commentary/Rts/Storage](commentary/rts/storage); what follows is a basic introduction to
looking at heap & stack objects in a running program.



You can display memory in gdb with something like `x/4a` to
display 4 words of memory, or using our [gdb macros](/trac/ghc/attachment/wiki/Debugging/CompiledCode/.gdbinit)[](/trac/ghc/raw-attachment/wiki/Debugging/CompiledCode/.gdbinit) you get slightly
nicer output:


```wiki
(gdb) p4 $rbx
0x2b35cc6c99a0: 0x4482d8 <stg_IND_1_info>
0x2b35cc6c9998: 0x2b35cc6c9978
0x2b35cc6c9990: 0x5
0x2b35cc6c9988: 0x404e30 <GHCziBase_Izh_con_info>
```


`p4` displays 4 words of memory backwards, i.e. lower addresses at
the bottom.  In this case I'm displaying memory pointed to by the
register `rbx`, which corresponds to the STG register `R1` on
a recent x86\_64 build.  Check
[includes/stg/MachRegs.h](/trac/ghc/browser/ghc/includes/stg/MachRegs.h) to
see which machine registers correspond to which STG registers on your
platform.



In the example above, I can see that `$rbx` points to a heap
closure for the `Int` value 5.  Closures always consist of an info
pointer (`GHCziBase_Izh_con_info` in this case, the `I#`
constructor), followed by any number of payload words (just one word
containing the value 5, here).  Full details on closure layouts are in
[includes/rts/storage/Closures.h](/trac/ghc/browser/ghc/includes/rts/storage/Closures.h).



It looks like the next word contains garbage, probably because it is
the next free location in the heap.



We can see the info tables using the `pinfo` macro:


```wiki
(gdb) pinfo &GHCziBase_Izh_con_info
$2 = {layout = {payload = {ptrs = 0, nptrs = 1}, bitmap = 4294967296, 
    large_bitmap_offset = 0, __pad_large_bitmap_offset = 0, 
    selector_offset = 4294967296}, type = 3, srt_bitmap = 0, 
  code = 0x404e30 "ÿe"}
```


Info tables tell the garbage collector and other parts of the system
about the layout of closures, they are rarely used during actual
execution.  The *info pointer* of a closure actually points to the
*entry code* for the closure; the info table lives just before the
entry code (this is a trick used by GHC so that the common operation
of jumping to the entry code for a closure can be done with a single
indirection).  The layout of info tables is defined in
[includes/rts/storage/InfoTables.h](/trac/ghc/browser/ghc/includes/rts/storage/InfoTables.h).



To display the stack, you need to know what the `Sp` register is
mapped to on this platform.  On x86\_64 you'll find it in `$rbp`:


```wiki
(gdb) p8 $rbp
0x2ae9697ccc00: 0x1
0x2ae9697ccbf8: 0x40b8c8 <GHCziBase_plusInt_info+32>
0x2ae9697ccbf0: 0x2ae9696c9868
0x2ae9697ccbe8: 0x44a2d8 <stg_upd_frame_info>
0x2ae9697ccbe0: 0x1
0x2ae9697ccbd8: 0x40b8c8 <GHCziBase_plusInt_info+32>
0x2ae9697ccbd0: 0x2ae9696c98a8
0x2ae9697ccbc8: 0x44a2d8 <stg_upd_frame_info>
```


It looks like the stack has an update frame on the top, followed by
some stack frame from `GHC.Base`, followed by another update
frame.



Return addresses on the stack have info tables just like heap
closures, and you can display them with `pinfo` in the same way:


```wiki
(gdb) pinfo 0x40b8c8
$4 = {layout = {payload = {ptrs = 65, nptrs = 0}, bitmap = 65, 
    large_bitmap_offset = 65, __pad_large_bitmap_offset = 65, 
    selector_offset = 65}, type = 36, srt_bitmap = 0, 
  code = 0x40b8c8 "I\203Ä\020M9üw$H\213E\bI\003E\bIÇD$ø0N@"}
```


However, return addresses have special info tables with more
information; to display the whole thing, use `prinfo`


```wiki
(gdb) prinfo 0x40b8c8
$5 = {srt_offset = 4241688, __pad_srt_offset = 6684481, i = {layout = {
      payload = {ptrs = 65, nptrs = 0}, bitmap = 65, large_bitmap_offset = 65, 
      __pad_large_bitmap_offset = 65, selector_offset = 65}, type = 36, 
    srt_bitmap = 0, code = 0x40b8c8 "I\203Ä\020M9üw$H\213E\bI\003E\bIÇD$ø0N@"}}
```


The `type` field tells us what kind of object this is, in this
case `36`}, which means a `RET_SMALL` stack frame (see
[includes/rts/storage/ClosureTypes.h](/trac/ghc/browser/ghc/includes/rts/storage/ClosureTypes.h)
for a list of closure types, but make sure you are
looking at the right version of this file for the build you're using,
because the types do change).


## Useful flags



See `-ddump-stg, -ddump-simpl, -ddump-cmm, -dppr-debug`.  For a really close correspondence to the assembly, see `-ddump-opt-cmm`.  You can also use `-ddump-to-file` to have this output be put into a file rather than to standard output.


## Useful hints


- Use the `dpc` macro if you want to avoid the pain of matching the `pc` address with instructions at every program step:

  ```wiki
  (gdb) dpc
  (gdb) si
  0x0000000000408a37 in base_GHCziTopHandler_lvl9_info ()
  1: x/i $pc  0x408a37 <base_GHCziTopHandler_lvl9_info+7>:        jb     0x408a55 <base_GHCziTopHandler_lvl9_info+37>
  (gdb)
  0x0000000000408a39 in base_GHCziTopHandler_lvl9_info ()
  1: x/i $pc  0x408a39 <base_GHCziTopHandler_lvl9_info+9>:        mov    $0x5cf248,%ebx
  (gdb)
  0x0000000000408a3e in base_GHCziTopHandler_lvl9_info ()
  1: x/i $pc  0x408a3e <base_GHCziTopHandler_lvl9_info+14>:       movq   $0x408a80,0xfffffffffffffff8(%rbp)
  (gdb) 
  ```

  Also note that an empty gdb request repeats the previous command, in this case `si`.
- Sometimes you need to look and step through the compiled code of Haskell libraries. (ToDo: the following is out of date, and refers to the old build system.  We should now use `-ddump-to-file`.)  I find it useful to change the lines 43 and 48 of file `mk/suffix.mk` as follows:

  ```wiki
  43:  $(HC) $(HC_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi -ddump-simpl -ddump-cmm -ddump-stg > $@.output
  48:  $(HC) $(HC_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi -ddump-simpl -ddump-cmm -ddump-stg > $@.output
  ```

  Now, I can use file `libraries/base/GHC/TopHandler.o.output` to make sense of what is going on in `TopHandler.o`.

## Mapping back to the STG code



To figure out which bit of Haskell code corresponds to the assembly
fragment you're looking at, you need to look at the STG intermediate
code generated by GHC.  Use the `-ddump-stg` flag.  The reason we
have to look at the STG is because this is the last phase before code
generation, after all the transformations have happened, and the
symbol names in STG correspond pretty directly to the symbols you see
in the object code.



For example, if you're stopped in a code fragment `s28a_info`,
then hopefully you can use `info line` to see what `s28a_info` is. You should also be able to search for `s28a` in the STG output and
find it - but you first have to find which module it comes from, and
the best way to do that is to grep for `s28a` in all the modules
of your program.


## Setting up gdbinit



As GHC's allocation of registers will vary depending on what architecture
you're building for, you'll need to modify some values in your gdbinit to
make things work for you.  In particular, you need to know what values
`BaseReg` and `Sp` are.  You can find them out by looking at
`includes/stg/MachRegs.h` (as well as all of the other registers.)
In particular, on x86-32, `BaseReg` is in `ebx` and `Sp` is in `ebp`, while on
x84-64, `BaseReg` is in `r13` and `Sp` is in `rbp`.


## When you've found & fixed it


- Tell someone, they'll probably be impressed :-)

- Try to think about whether adding an assertion somewhere might have
  helped you find the bug quicker, and if so, add one.  Test it.  Add
  some commentary next to the assertion to help someone else
  understand the problem later.


  


- Add a test to the testsuite for your bug.
