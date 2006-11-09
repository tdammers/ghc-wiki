# Working notes on spark profiling



This page is just working notes, not really intended for public consumption (though you are welcome to peek). It records work on spark profiling, led by Tim Harris.


- Add ticky calls, expressed as macros in HC files, which either
  generates instrumentation calls, or calls to spark things.
  The Haskell program isn't recompiled.

- The instrumentation calls record, in an in-memory log, the following
  events, each time-stamped (allocation bytes or processor time)

  - thunk allocation( address of thunk, alloc site-id )
    \[hack to ensure site-id is unique across compilation units\]
  - thunk entry( address )
  - thunk update( address )
  - start running thread( thread id, time )
  - stop running thread( thread id )

- At GC time, process the in-memory log, generate unique ids for
  each thunk (stable across the whole run), output text version
  into log file

- Off-line processing consumes log file and decides which allocation 
  site should be a spark.  Heuristics:

  - decent delay between allocation and entry
  - decent granularity
  - most instances are entered (e.g. 7/8)

- Take log, use thread-switching info to generate a log
  for each individual thread, with time stamps virtualised to
  that thread's life only. Now we have a per-thread log.

- Also use log to generate parallelism profiles under wildly
  optimistic assuptions of cost

- Consider

  ```wiki
  let x = z+1
      y = z+1
  in p + x + y
  ```

  If (+) evaluates left-right, the log will show that 

  - x is expensive but y is cheap (because z is evaluated by x)
  - x isn't need until some time after its allocation
    (because of the cost of evaluating p)

  Hence we'll decide to spark x but not y... which is precisel
  what we want!!

>
> >
> >
> > But if (+) evaluates the right-left, we won't spark either x or y!
> > (You can use seq to control this, of course.)
> >
> >
>

- Note \[Tension between thunks and parallelism\]

  ```wiki
        let t2 = fib 40
        in case fib 40 of r1 -> case t2 of r2 -> r1+r2
    vs
        case fib 40 of r1 -> case fib 40 of r2 -> r1+r2
  ```

  GHC will convert the former to the latter, which will
  lose sparking opportunities

- Idea

  ```wiki
  	case f x of (,) -> let y = e in body
    ==>   
  	let y = e in case f x of (,) -> body
  ```

  This affects (up or down) the number of variables
  saved across the case.  But it's GOOD for parallelism
  because y is allocated sooner.

- Idea: display info about thunks that are

  - always entered
  - never entered

  Feedback to programmer; give more clues to strictness
  analyser.

## Challenges


- Making the profile information robust, that it can be used
  on the \*source\* program, perhaps after small modifications

- Address Note \[Tension between thunks and parallelism\]
