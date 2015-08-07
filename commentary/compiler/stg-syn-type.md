
Video: [
STG language](http://www.youtube.com/watch?v=v0J1iZ7F7W8&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) (17'21")


# The STG syntax data types



Before code generation, GHC converts the Core-language program into `StgSyn`.  The basic ideas are still pretty much exactly as described in the paper [
Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz).



The best way to think of STG is as special form of [Core](commentary/compiler/core-syn-type).  Specifically, the differences are these (see [compiler/stgSyn/StgSyn.hs](/trac/ghc/browser/ghc/compiler/stgSyn/StgSyn.hs)):


- Function arguments are atoms (literals or variables), of type `StgArg`.
- The right hand side of a let-binding, `StgRhs`, is either

  - `StgRhsCon`: a constructor application, or 
  - `StgRhsClosure`: **lambda-form** (possibly with zero arguments, in which case it's a thunk).
- Constructor applications are saturated.
- Applications of primitive operators are saturated.
- Lambdas can only appear the right-hand side of a let-binding.  (There is an expression form `StgLam`, but it is only used during the Core-to-STG transformation, not in a valid STG program.)
- Types have largely been discarded, retaining only enough type information as is needed to guide code generation. There is an `StgLint` checker, which makes some consistency checks, but the CoreLint guarantee that "if the program passes Lint it cannot crash" has been lost.


In addition, the STG program is decorated with the results of some analyses:


- Every lambda-form (`StgRhsClosure`) lists its free variables.  These are the variables that are in the thunk of function closure that is allocated by the let.

- Every lambda-form gives its ['Static Reference Table'](commentary/rts/ca-fs) or **SRT**.  You should think of the SRT as the *top-level* free variables of the body.  They do not need to be dynamically allocated in the heap object, but they do need to be accessible from the object's info-table, so that the garbage collector can find the CAFs kept alive by the object.

- A `StgCase` expression is decorated with its **live variables**; that is, variables reachable from the continuation of the case.  More precisely, two sets of live variables, plus the SRT for the continuation.  Todo: say more.

- The STG program has a new construct called **let-no-escape**, that encodes so-called **join points**. Variables bound by a let-no-escape are guaranteed to be tail-calls, not embedded inside a data structure, in which case we don’t have to construct a closure because the required stack will always be present. Todo: say more.
