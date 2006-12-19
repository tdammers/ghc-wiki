```wiki
ALGORITHM

Ax    axioms
C     locals
W     wanted

The task:

Verify that W follows from C wrt Ax (at least 'reduce' W as much as possible).

For this purpose we normalize C and W, written (C | W) -->* (C' | W'),
by performing the following steps

- axiom step, apply axioms on C and W
- local step, build the closure of C
- wanted step, reduce W wrt C (in the axiom step we reduce W wrt Ax)

We can verify that W follows from C wrt Ax if
all constraints in W' are either True, or are syntactically
present in C'.

Normalization terminates if
- we have exhaustively applied the axiom and wanted steps and
- the set C' of locals remains stable (ie any local step
generates equations which are already present in C').

The individual normalization steps are:
(I'll skip the rigid/wobbly issue)
1) Axiom step (affects C and W)

We rewrite C and W wrt the axioms: C cup W -->Ax C' cup W'

We exhaustively apply rewritings implied by Ax on terms
in C and W and obtain C' and W' (no surprise here).
We also apply the common rules such as

 [t]=[s] --> t=s etc

2) Local step (affects only C)

We build the closure of all locals:  (C | W) --> (C' | W)

  Let s=t or t=s in C such that s \equiv S s1 ... sn where
  S is a type function and si are terms

  a) if s'[s] =t' in C

     then C cup W --> C cup {s'[t]=t'} cup W

  b) if t'=s'[s] in C

     then C cup W --> C cup {t'=s'[t]} cup W
    
  c) if s'=t'[t] in C

     then C cup W --> C cup {s'=t'[s]} cup W

  d) if t'[t]=s' in C

     then C cup W --> C-{t'=s'} cup {t'[t]=s'} cup W


We write t[s] to denote the occurrence of a term s in
a term t.

Point to note:
 We don't care about the orientation of locals.
 The "order issue" is solved by simpling computing the fixpont (ie closure of C).
 Steps a)-d) effectively build the closure of all local assumptions.

3) Wanted step (affects only W)

We rewrite W wrt C:  (C | W) --> (C' | W)

  Let s=t in C such that s \equiv S s1 ... sn where
  S is a type function and si are terms
(NOTE: there was a typo in my earlier email,
I wrote "Let s=t in W ..." but of course we reduce W wrt C)

    a') if s'[s] =t' in W

     then C cup W --> C cup W-{s'=t'} cup {s'[t]=t'} 
                            ^^^^^^^^^^^^^^^^^^^^^^^^
                  we replace s'=t' by s'[t]=t'
    
Point to note:

We actually rewrite s'[s]=t' in W to s'[t]=t'.
The other cases b'), c') and d') are similar to the
above cases b), c) and d).

The above algorithm is inspired by the typefunction vs chr
correspondence.

How to build evidence during the application of
axiom, local and wanted steps is obvious
(but please yell if there's something I've overlooked).


EXAMPLES

Example 1:

forall a. S [a] = [S a]  -- axiom, Ax

T Int = Int         (2)  -- locals, C
T [Int] = S [Int]   (3)
T Int = S Int       (4)


T [Int] = [Int]          -- wanted, W


We'll only show the reduction of W.

    T [Int] = [Int]
--> apply (3) from left to right
    S [Int] = [Int]
--> apply axiom
    [S Int] = [Int]
--> "decompose"
     S Int = Int
--> apply (4) from right to left
    T Int = Int
--> apply (2) from left to right
    Int = Int
--> True

The above normalization steps can be mapped directly to CHR solving
steps. Application of axioms correspond to CHR rule applications.
Application of local assumptions correspond to FD rule applications.


Example 2:

no axioms

Bool = G Int      (1)   -- locals, C
F (G Int) = Int   (2)

F Bool = Int            -- wanted, W

In this example, we actually need to build the closure of C
to verify W.

We find that

     Bool = G Int   
     F (G Int) = Int

-->* Bool = G Int   
     F (G Int) = Int
     F Bool = Int

Thus, we can immediately verify W


TERMINATION OF NORMALIZATION

At this point you may wonder what about termination?
For example, in case of the local assumptions

T Int = Int     (1)
T Int = S Int   (2)

we may repetively apply (2) on (1) and thus we keep adding in
"copies" of S Int = Int. Notice that normalization of C to the form C'
means that we apply the axiom and local steps until the set C' remains
stable. The claim is that the size of C' is bound. This must be the
case because the local steps correspond almost directly to FD rule
applications. For example, the above local assumptions
are represented as the following CHR constraints

T Int Int,
T Int b, S Int b

Application of the FD rule yields

b=Int, T Int Int,
T Int b, S Int b

<--> equivalent to

b=Int, T Int Int, S Int Int

the above can be interpreted as

T Int= Int
T Int = S Int
S Int = Int

The point here is that although the number of local steps may be
infinite, we'll eventually reach a stable constraint store.
As said above, each local step corresponds almost directly to a FD
rule application step.
Guess it may be more appropriate to say that the above method is
specificed declaratively rather than algorithmic.
(more on how to avoid infinite application of local steps below).

Actually, there's another termination issue.
Recall the earlier example


   S [[Int]] = [T Int]
   S [[Int]] = T Int

   -->

   T Int = [T Int]
   S [[Int]] = [T Int]
   S [[Int]] = T Int

It seems we have introduced a non-terminating local rewrite relation
T Int = [T Int]. As observed earlier, such "non-terminating"
relations correspond to inconsistent CHR stores. 
I'm sure the condition that rewrite relations must be "decreasing"
is sufficient to guarantee termination, not sure whether it's also
necessary. The brute force method would be to translate 
C cup W into its CHR form and check the CHR store for consistency.

AVOIDING INFINITE LOCAL STEPS/SMARTER LOCAL STEPS

Let's consider again the example

T Int = Int     (1)
T Int = S Int   (2)

As observed above, normalization yields the fixpoint

T Int = Int
T Int = S Int
S Int = Int

but a naive algorithm may keep applying (2) on (1), thus
adding in the already present equation S Int = Int.

Is there a smart method to avoid infinite local steps?
(thus we don't need to check whether we've reached a fixpoint,
this may be costly).
We may get some insight from the CHR derivation. Eg

T Int = Int     (1)
T Int = S Int   (2)

is represented as the CHR constraint store

T Int Int,
T Int b, S Int b

Application of the FD rule yields

b=Int, T Int Int,
T Int b, S Int b

<--> equivalent to

b=Int, T Int Int, S Int Int

In terms of the type function notation, we can thus argue that

T Int = Int     (1)
T Int = S Int   (2)

is normalized to

T Int = Int
S Int = Int

There's no termination issue anymore. Of course, the details have yet
to be worked out.
```