# The GHC Commentary: Coding Style Guidelines for RTS C code


## Comments



These coding style guidelines are mainly intended for use in
`rts/` and `includes/`.  See [Coding Style Guidelines](commentary/coding-style) for code in `compiler/`.



These are just suggestions.  They're not set in stone.  Some of
them are probably misguided.  If you disagree with them, feel free to
modify this document (and make your commit message reasonably
informative) or mail someone
(eg. The GHC mailing list)


## References



If you haven't read them already, you might like to check the following.
Where they conflict with our suggestions, they're probably right.


- The C99 standard.  One reasonable reference is ~~here~~ (this link is out-of-date).

- Writing Solid Code, Microsoft Press. (Highly recommended.)

- Autoconf documentation. See also
  The autoconf macro archive (this link is out-of-date)
  and [
  Cyclic Software's description](http://www.cyclic.com/cyclic-pages/autoconf.html).

- [
  Indian Hill C Style and Coding Standards](http://www.cs.arizona.edu/~mccann/cstyle.html)

- A list of C programming style links (this link is out-of-date)

- [
  A very large list of C programming links](http://www.lysator.liu.se/c/c-www.html)

## Portability issues


### Which C Standard?



We try to stick to C99 where possible.  We use the following
C99 features relative to C89, some of which were previously GCC
extensions (possibly with different syntax):


- Variable length arrays as the last field of a struct.  GCC has
  a similar extension, but the syntax is slightly different: in GCC you
  would declare the array as `arr[0]`, whereas in C99 it is
  declared as `arr[]`.

- Inline annotations on functions (see later)

- Labeled elements in initialisers.  Again, GCC has a slightly
  different syntax from C99 here, and we stick with the GCC syntax
  until GCC implements the C99 proposal.

- C++-style comments.  These are part of the C99 standard, and we
  prefer to use them whenever possible.


In addition we use ANSI-C-style function declarations and prototypes
exclusively.  Every function should have a prototype; static
function prototypes may be placed near the top of the file in which
they are declared, and external prototypes are usually placed in a
header file with the same basename as the source file (although
there are exceptions to this rule, particularly when several source
files together implement a subsystem which is described by a single
external header file).


- We use the following GCC extensions, but surround them with `#ifdef__GNUC__`:

  - Function attributes (mostly just `no_return` and `unused`)
  - Inline assembly.

### Other portability conventions


- char can be signed or unsigned - always say which you mean

- Our POSIX policy: try to write code that only uses POSIX
  ([ IEEE Std 1003.1](http://www.opengroup.org/onlinepubs/009695399/toc.htm))
  interfaces and APIs.  We used to define `POSIX_SOURCE` by
  default, but found that this caused more problems than it solved, so
  now we require any code that is POSIX-compliant to explicitly say so
  by having `#include "PosixSource.h"` at the top.  Try to do this
  whenever possible.

- Some architectures have memory alignment constraints.  Others don't
  have any constraints but go faster if you align things.  These
  macros (from `ghcconfig.h`) tell you which alignment to use

  ```wiki
  	  /* minimum alignment of unsigned int */
  	  #define ALIGNMENT_UNSIGNED_INT 4
  	
  	  /* minimum alignment of long */
  	  #define ALIGNMENT_LONG 4
  	
  	  /* minimum alignment of float */
  	  #define ALIGNMENT_FLOAT 4
  	
  	  /* minimum alignment of double */
  	  #define ALIGNMENT_DOUBLE 4
  ```

- Use `StgInt`, `StgWord` and `StgPtr` when
  reading/writing ints and ptrs to the stack or heap.  Note that, by
  definition, `StgInt`, `StgWord` and `StgPtr` are the
  same size and have the same alignment constraints even if
  `sizeof(int) != sizeof(ptr)` on that platform.

- Use `StgInt8`, `StgInt16`, etc when you need a certain
  minimum number of bits in a type.  Use `int` and `nat` when
  there's no particular constraint.  ANSI C only guarantees that ints
  are at least 16 bits but within GHC we assume they are 32 bits.

- Use `StgFloat` and `StgDouble` for floating point values
  which will go on/have come from the stack or heap.  Note that
  `StgDouble` may occupy more than one `StgWord`, but it will
  always be a whole number multiple.

- Use `PK_FLT(addr)`, `PK_DBL(addr)` to read `StgFloat`
  and `StgDouble` values from the stack/heap, and
  `ASSIGN_FLT(val,addr)` / `ASSIGN_DBL(val,addr)` to assign
  StgFloat/StgDouble values to heap/stack locations.  These macros
  take care of alignment restrictions.

- Heap/Stack locations are always `StgWord` aligned; the
  alignment requirements of an `StgDouble` may be more than that
  of `StgWord`, but we don't pad misaligned `StgDoubles`
  because doing so would be too much hassle (see `PK_DBL` & co
  above).

-  Avoid conditional code like this:

  ```wiki
    #ifdef solaris_host_OS
    // do something solaris specific
    #endif
  ```

  Instead, add an appropriate test to the configure.ac script and use
  the result of that test instead.

  ```wiki
    #ifdef HAVE_BSD_H
    // use a BSD library
    #endif
  ```

>
> >
> >
> > The problem is that things change from one version of an OS to
> > another - things get added, things get deleted, things get broken,
> > some things are optional extras.  Using "feature tests" instead of
> > "system tests" makes things a lot less brittle.  Things also tend to
> > get documented better.
> >
> >
>

## Debugging/robustness tricks



Anyone who has tried to debug a garbage collector or code generator
will tell you: "If a program is going to crash, it should crash as
soon, as noisily and as often as possible."  There's nothing worse
than trying to find a bug which only shows up when running GHC on
itself and doesn't manifest itself until 10 seconds after the actual
cause of the problem.



We put all our debugging code inside `#ifdef DEBUG`.  The
general policy is we don't ship code with debugging checks and
assertions in it, but we do run with those checks in place when
developing and testing.  Anything inside `#ifdef DEBUG` should
not slow down the code by more than a factor of 2.



We also have more expensive "sanity checking" code for hardcore
debugging - this can slow down the code by a large factor, but is only
enabled on demand by a command-line flag.  General sanity checking in
the RTS is currently enabled with the `-DS` RTS flag.



There are a number of RTS flags which control debugging output and
sanity checking in various parts of the system when `DEBUG` is
defined.  For example, to get the scheduler to be verbose about what
it is doing, you would say `+RTS -Ds -RTS`.  See
`includes/RtsFlags.h` and `rts/RtsFlags.c` for the full
set of debugging flags.  To check one of these flags in the code,
write:


```wiki
  IF_DEBUG(gc, fprintf(stderr, "..."));
```


would check the `gc` flag before generating the output (and the
code is removed altogether if `DEBUG` is not defined).



All debugging output should go to `stderr`.



Particular guidelines for writing robust code:


- Use assertions.  Use lots of assertions.  If you write a comment
  that says "takes a +ve number" add an assertion.  If you're casting
  an int to a nat, add an assertion.  If you're casting an int to a
  char, add an assertion.  We use the `ASSERT` macro for writing
  assertions; it goes away when `DEBUG` is not defined.

- Write special debugging code to check the integrity of your data
  structures.  (Most of the runtime checking code is in
  `rts/Sanity.c`) Add extra assertions which call this code at
  the start and end of any code that operates on your data
  structures.

- When you find a hard-to-spot bug, try to think of some assertions,
  sanity checks or whatever that would have made the bug easier to
  find.

- When defining an enumeration, it's a good idea not to use 0 for
  normal values.  Instead, make 0 raise an internal error.  The idea
  here is to make it easier to detect pointer-related errors on the
  assumption that random pointers are more likely to point to a 0
  than to anything else.

  ```wiki
  typedef enum
      { i_INTERNAL_ERROR  /* Instruction 0 raises an internal error */
      , i_PANIC           /* irrefutable pattern match failed! */
      , i_ERROR           /* user level error */

      ...
  ```

- Use `#warning` or `#error` whenever you write a piece of
  incomplete/broken code.

- When testing, try to make infrequent things happen often.  For
  example, make a context switch/gc/etc happen every time a context
  switch/gc/etc can happen.  The system will run like a pig but it'll
  catch a lot of bugs.

## Syntactic details


- Please keep to 80 columns: the line has to be drawn somewhere, and
  by keeping it to 80 columns we can ensure that code looks OK on
  everyone's screen.  Long lines are hard to read, and a sign that
  the code needs to be restructured anyway.

- An indentation width of 4 is preferred (don't use actual tab characters, use spaces).

- **Important:** Put "redundant" braces or parens in your code.

>
> >
> >
> > Omitting braces and parens leads to very hard to spot bugs -
> > especially if you use macros (and you might have noticed that GHC
> > does this a lot!)
> >
> >
>

>
> >
> >
> > In particular, put braces round the body of for loops, while loops,
> > if statements, etc.  even if they "aren't needed" because it's
> > really hard to find the resulting bug if you mess up.  Indent them
> > any way you like but put them in there!
> >
> >
>

- When defining a macro, always put parens round args - just in case.
  For example, write:

  ```wiki
    #define add(x,y) ((x)+(y))
  ```

  instead of

  ```wiki
    #define add(x,y) x+y
  ```

- Don't declare and initialize variables at the same time.
  Separating the declaration and initialization takes more lines, but
  make the code clearer.

- Don't define macros that expand to a list of statements.  You could
  just use braces as in:

  ```wiki
    #define ASSIGN_CC_ID(ccID)              \
          {                                 \
          ccID = CC_ID;                     \
          CC_ID++;                          \
          }
  ```

  (but it's usually better to use an inline function instead - see above).

- Don't even write macros that expand to 0 statements - they can mess
  you up as well.  Use the `doNothing` macro instead.

  ```wiki
    #define doNothing() do { } while (0)
  ```

- This code

  ```wiki
  int* p, q;
  ```

  looks like it declares two pointers but, in fact, only p is a pointer.
  It's safer to write this:

  ```wiki
  int* p;
  int* q;
  ```

  You could also write this:

  ```wiki
  int *p, *q;
  ```

  but it is preferrable to split the declarations.

- Try to use ANSI C's enum feature when defining lists of constants
  of the same type.  Among other benefits, you'll notice that gdb
  uses the name instead of its (usually inscrutable) number when
  printing values with enum types and gdb will let you use the name
  in expressions you type.

>
> >
> >
> > Examples:
> >
> >
> > ```wiki
> >     typedef enum { /* N.B. Used as indexes into arrays */
> >      NO_HEAP_PROFILING,		
> >      HEAP_BY_CC,		
> >      HEAP_BY_MOD,		
> >      HEAP_BY_GRP,		
> >      HEAP_BY_DESCR,		
> >      HEAP_BY_TYPE,		
> >      HEAP_BY_TIME		
> >     } ProfilingFlags;
> > ```
> >
> >
> > instead of
> >
> >
> > ```wiki
> >     # define NO_HEAP_PROFILING	0	/* N.B. Used as indexes into arrays */
> >     # define HEAP_BY_CC		1
> >     # define HEAP_BY_MOD	2
> >     # define HEAP_BY_GRP	3
> >     # define HEAP_BY_DESCR	4
> >     # define HEAP_BY_TYPE	5
> >     # define HEAP_BY_TIME	6
> > ```
> >
> >
> > and 
> >
> >
> > ```wiki
> >     typedef enum {
> >      CCchar    = 'C',
> >      MODchar   = 'M',
> >      GRPchar   = 'G',
> >      DESCRchar = 'D',
> >      TYPEchar  = 'Y',
> >      TIMEchar  = 'T'
> >     } ProfilingTag;
> > ```
> >
> >
> > instead of
> >
> >
> > ```wiki
> >     # define CCchar    'C'
> >     # define MODchar   'M'
> >     # define GRPchar   'G'
> >     # define DESCRchar 'D'
> >     # define TYPEchar  'Y'
> >     # define TIMEchar  'T'
> > ```
>
>

- When commenting out large chunks of code, use `#ifdef 0 ... #endif` 
  rather than `/* ... */` because C doesn't have
  nested comments.

- When declaring a typedef for a struct, give the struct a name as
  well, so that other headers can forward-reference the struct name
  and it becomes possible to have opaque pointers to the struct.  Our
  convention is to name the struct the same as the typedef, but add a
  leading underscore.  For example:

  ```wiki
    typedef struct _Foo {
      ...
    } Foo;
  ```

- Do not use `!` instead of explicit comparison against `NULL`
  or `'\0'`; the latter is much clearer.

- Please write comments in English.  Especially avoid Klingon.

## Inline functions



Use inline functions instead of macros if possible - they're a lot
less tricky to get right and don't suffer from the usual problems
of side effects, evaluation order, multiple evaluation, etc.


- Inline functions get the naming issue right.  E.g. they
  can have local variables which (in an expression context)
  macros can't.

- Inline functions have call-by-value semantics whereas macros are
  call-by-name.  You can be bitten by duplicated computation if you
  aren't careful.

- You can use inline functions from inside gdb if you compile with
  -O0 or -fkeep-inline-functions.  If you use macros, you'd better know
  what they expand to.

>
> >
> >
> > However, note that macros can serve as both l-values and r-values and
> > can be "polymorphic" as these examples show:
> >
> >
> > ```wiki
> >   // you can use this as an l-value or an r-value
> >   #define PROF_INFO(cl) (((StgClosure*)(cl))->header.profInfo)
> >
> >   // polymorphic case
> >   // but note that min(min(1,2),3) does 3 comparisons instead of 2!
> >   #define min(x,y) (((x)<=(y)) ? (x) : (y))
> > ```
>
>


There are three macros to do inline portably.  Don't use `inline` directly, use these instead:



`INLINE_HEADER`


>
>
> An inline function in a header file.  This is just like a macro.  We never emit
> a standalone copy of the function, so it *must* be inlined everywhere.
>
>


`STATIC_INLINE`


>
>
> An inline function in a C source file.  Again, it is always inlined, and we never
> emit a standalone copy. 
>
>


`EXTERN_INLINE`


>
>
> A function which is optionally inlined.  The C compiler is told to inline if possible,
> but we also generated a standalone copy of the function just in case (see [source:rts/Inlines.c](/trac/ghc/browser/rts/Inlines.c)[](/trac/ghc/export/HEAD/ghc/rts/Inlines.c)).
>
>

## Source-control issues


- Don't be tempted to re-indent or re-organise large chunks of code -
  it generates large diffs in which it's hard to see whether anything
  else was changed, and causes extra conflicts when moving patches to
  another branch.

  If you must re-indent or re-organise, don't include any functional
  changes that commit and give advance warning that you're about to do
  it in case anyone else is changing that file.  For more details on
  source control conventions, see [WorkingConventions/Git](working-conventions/git).
