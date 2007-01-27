## WHY WE NEED TO NORMALIZE EQUATIONS



Consider


```wiki
H (G Int) = Int    (1)
G Int = F (G Int)  (2)
```


where F, G, H are type function constructors.
I will omit evidence (construction steps) for the moment.
The above equations imply


```wiki
H (F .... F (G Int)) = Int  (3)
```


Indeed, we can repeatidly apply the second equation
on the first equation to obtain the third equation.
But this also shows that we cannot naively apply
the following reduction step


```wiki
     T1[F t] = t'
-->  T1[s] = t'
```


where F t = s. We write T1\[\] to denote a type with a hole.
NOTE: The above reduction step is proposed in Plan MC and Plan MS.



Otherwise, we find that


```wiki
    H (G Int) = Int
--> H (F (G Int)) = Int
--> ...
```


So, the question is whether there is a way out to avoid
non-termination.



As argued by Plan MS revised there is a way out
if we "normalize" equations.
"Normalization" of equations works by "flattening" equations
such that all equations are of the form


```wiki
F n = n'
```


where n and n' only refer to types NOT containing
type function constructors.
For example, the equations


```wiki
H (G Int) = Int    (1)
G Int = F (G Int)  (2)
```


are normalized to the form


```wiki
H a = Int
G Int = a
G Int = b
F c = b
G Int = c
```


We can consider variables a, b and c as existentially quantified.
Notice that logically speaking


```wiki
H (G Int) = Int  /\ G Int = F (G Int)

	iff

exists a, b, c.
  H a = Int /\
  G Int = a /\
  G Int = b /\
  F c = b /\
  G Int = c /\
```


On the equations in normal form
we apply the reduction step


```wiki
    F n1 = n2
    F n1 = n3
-->
    F n1 = n2
    F n1 = n3
    n2 = n3
```


Points to note:


- this corresponds exactly to the FD-CHR reduction step
- n2 = n3 is a "pure" unification constraint. Recall that
  by (normalization) assumption, n2 and n3 do NOT refer to
  type function constructors
- we can either discard F n1 = n3 (or F n1 = n2), or we
  remember having applied the reduction step on
  F n1 = n2 and F n1 = n3 to avoid infinite application
  of the same reduction step


Let's apply this reduction step to the above normal form.
We find that


```wiki
H a = Int
G Int = a   *
G Int = b   * we highlight equations involved in the reduction
F c = b
G Int = c

-->

H a = Int
G Int = a   *
b = a
F c = a    -- cause b = a we replace F c = b by F c = a
G Int = c   *

-->

H a = Int
G Int = a
b = a
c = a
F a = a    -- cause c = a we find F a = a
```


At this point no further reductions are possible.
Notice the "cycle" in F a = a. In essence,


```wiki
H a = Int
G Int = a
b = a
c = a
F a = a
```


represents the canonical (ie fully reduced) normal form
of all equations which are derivable


```wiki
H (G Int) = Int
G Int = F (G Int)
```

## HOW DOES NORMALIZATION FIT TOGETHER WITH EVIDENCE CONSTRUCTION



Let's assume that


```wiki
g1 : H (G Int) = Int
g2 : G Int = F (G Int)
```


are given equations where g1 and g2 are the respective evidence values
and we would like to compute g3 (wanted) such that


```wiki
g3 : H (F (G Int)) = Int
```


For the construction of wanted evidence from given evidence it seems
counter-intuitive to normalize (given) equations. Based on the
normalization procedure we turn


```wiki
g1 : H (G Int) = Int
g2 : G Int = F (G Int)
```


into the normal form


```wiki
g1' : H a = Int
g2' : G Int = a
g3' : G Int = b
g4' : F c = b
g5' : G Int = c
```


It's clear how to build g1 and g2 from g1',g2',g3',g4' and g5'.
But the other direction is not obvious.


## EVIDENCE CONSTRUCTION IN THE PRESENCE OF NORMAL FORMS



So much for how we put equations into normal form.  Now we look at the real algorithm.
Let's consider a slightly different example, which we will use from now on.
We are given the equations


```wiki
g1 : F (G Int) = Int
g2 : G Int = F (G Int)
```


It should be clear that the above yields the canonical normal form
**SLPJ: what is a "canonical normal form", and why do we want it?  Our goal, after all, is 
to compute evidence for "wanted" from "given"**


```wiki
g1' : F Int = Int
g2' : G Int = Int
```


where


```wiki
g1' = sym (F g2') trans g1 = sym (F (g2 trans g1)) trans g1
g2' = g2 trans g1
```


**SLPJ: when you say "it should be clear", you mean "the solution we seek is this", right?**



We write 'trans' for the transitive relation on coercions,
'sym' is symmetry etc.



The challenge is how to compute


```wiki
g1' : F Int = Int
g2' : G Int = Int
```


for some g1' and g2' from the normal form?



We apply the following strategy:



(Ia) Put the given equations into normal form.



(Ib) Exhaustively apply reduction steps on these nonrmal-form 


>
>
> equations.
>
>


(II) Then, we normalize the proof terms of the equations


>
>
> in the final store (to obtain proof terms which only
> refer to the proof term variables from the original
> equation set).
>
>


We first consider (Ib).



There are two kind of reduction steps on equations



1) FD-CHR reduction step:


```wiki
g:  F n1 = n2
g': F n1 = n3

-->

g:  F n1 = n3
(sym g) trans g' : n2 = n3
```


We discard g': F n1 = n3 because we can build g'
using `g:  F n1 = n3` and
`(sym g) trans g' : n2 = n3`.



2) Substitution step (replacing variables by equals):


```wiki
g  : a = n1
g' : F a = n2

-->

g  : a = n2
(sym (F g)) trans g' : F n1 = n2
```


We omit the other cases, e.g. `n1 = a`, `F n2 = a` etc, and
reductions such as `[n1] = [n2] --> n1 = n2`.



NOTE: both reduction steps maintain the normal form property



Here's the normal form for our example, attached with evidence.


```wiki
g1' : F a = Int
d1  : G Int = a
g2' : G Int = b
d2  : F c = b
d3  : G Int = c
```


We use the convention that d refers to some "anonymous" evidence.



Notice that we can express g1' and g2' in terms of d1,d2,d3 and g1,g2.


```wiki
g1' = sym (F d1) trans g1
g2' = g2 trans (F d3) trans d2
```


**SLPJ: for me, g1' is like d1, a piece of anonymous evidence.  
The evidence I have is g1.  I can construct g1 from g1' and d1**:


```wiki
  g1 = (F d1) trans g1'
```


**Now that also implies that I can construct g1' from g and d1, as you say;
and that I can construct d1 from g1 and g1'!  But the starting point is surely
g1.  I found this confusing.**



We exhaustively apply reduction steps on the above normal form.


```wiki
g1' : F a = Int
d1  : G Int = a     *
g2' : G Int = b
d2  : F c = b
d3  : G Int = c     *

--> FD-CHR step

g1' : F a = Int
d1  : G Int = a
g2' : G Int = b
d2  : F c = b                *
(sym d3) trans d1 : c = a    *

--> Substitution step

g1' : F a = Int
d1  : G Int = a       *
g2' : G Int = b       *
(sym (F ((sym d3) trans d1))) trans d2  : F a = b
(sym d3) trans d1 : c = a

--> FD-CHR step

g1' : F a = Int
g2' : G Int = b
(sym (F ((sym d3) trans d1))) trans d2  : F a = b   *
(sym d3) trans d1 : c = a
(sym g2') trans d1 : b = a                          *

--> Substitution step

g1' : F a = Int      *
g2' : G Int = b
d4 : F a = a         *
(sym d3) trans d1 : c = a
(sym g2') trans d1 : b = a

  where d4 = (sym (F ((sym d3) trans d1))) trans d2 trans (sym g2') trans d1

--> FD-CHR step

g1' : F a = Int               *
g2' : G Int = b
(sym d3) trans d1 : c = a
(sym g2') trans d1 : b = a
(sym d4) trans g1' : a = Int  *

--> Substitution step

(sym (F d5)) trans g1' : F Int = Int
g2' : G Int = b               *
(sym d3) trans d1 : c = a
(sym g2') trans d1 : b = a    *
(sym d4) trans g1' : a = Int  *

  where d5 = (sym d4) trans g1'

--> Substitution step

(sym (F d5)) trans g1' : F Int = Int
g2' trans ((sym g2') trans d1) trans d5 : G Int = Int
(sym d3) trans d1 : c = a
(sym g2') trans d1 : b = a
(sym d4) trans g1' : a = Int
```


No further reductions are applicable at this point.
Note that


```wiki
d4 = (sym (F ((sym d3) trans d1))) trans d2 trans (sym g2') trans d1
d5 = (sym d4) trans g1'
g1' = sym (F d1) trans g1
g2' = g2 trans (F d3) trans d2
```


Next, we consider (II).



We will use the equation and some laws to "normalize"
the proof terms attached to equations `F Int = Int` and `G Int = Int`
such that the proof terms only mention g1 and g2.



We make use of the following laws:


```wiki
sym (F (g1 trans g2)) = (F (sym g2)) trans (F (sym g1))

sym (F g) = F (sym g)

F (g1 trans g2) = (F g1) trans (F g2)

sym (sym g) = g
```


The above laws imply the following property



Property: For each `g : s = t` we can achieve the normal form


```wiki
  d1 trans .... trans dn : s = t
```


where each di is either of the form
`sym (F1 ... Fn n) or (F1 ... Fn n)`, 
where n is either a variable or a primitive type such as Int etc.



**SLPJ give intuition and/or proof for this property.  Why is the n the last parameter?**



Let's normalize the proof terms attached to the equations
`F Int = Int` and `G Int = Int`.



We have that


```wiki
(sym (F d5)) trans g1' : F Int = Int
g2' trans ((sym g2') trans d1) trans d5 : G Int = Int

where

d4 = (sym (F ((sym d3) trans d1))) trans d2 trans (sym g2') trans d1
d5 = (sym d4) trans g1'
g1' = sym (F d1) trans g1
g2' = g2 trans (F d3) trans d2
```


During the normalization of proof terms process we also
apply the "cancellation" law that


```wiki
g1 trans (sym g2) trans g2 = g1
```


We start off with `(sym (F d5)) trans g1' : F Int = Int`


```wiki
(sym (F d5)) trans g1'

--> d5 = (sym d4) trans g1'

(sym (F ((sym d4) trans g1'))) trans g1'

--> push sym inside

(F (sym g1')) trans (F d4) trans g1'

--> g1' = sym (F d1) trans g1
    and normalize

(sym (F g1)) trans (F (F d1)) trans (F d4) trans g1'

--> d4 = (sym (F ((sym d3) trans d1))) trans d2 trans
         (sym g2') trans d1
       = (sym (F d1)) trans (F d3) trans d2 trans
         (sym g2') trans d1
    and normalize

(sym (F g1)) trans (F (F d1)) trans
(sym (F (F d1)) trans (F (F d3)) trans (F d2) trans
(sym (F g2')) trans (F d1) trans g1'

--> g1' = sym (F d1) trans g1
    g2' = g2 trans (F d3) trans d2
    and normalize

(sym (F g1)) trans (F (F d1)) trans
(sym (F (F d1)) trans (F (F d3)) trans (F d2) trans
(sym (F d2)) trans (sym (F (F d3))) trans (sym (F g2)) trans
(F d1) trans sym (F d1) trans g1

	-- ********* Here a miracle happens
	-- Magically, a (F d2) meets (sym (F d2)) etc

--> "cancellation" law

(sym (F g1)) trans (sym (F g2)) trans g1
```


A similar calculation for
`g2' trans ((sym g2') trans d1) trans d5 : G Int = Int`
yields


```wiki
g2' trans ((sym g2') trans d1) trans d5

-->*

g2 trans g1
```


Hence, we have verified that


```wiki
(sym (F g1)) trans (sym (F g2)) trans g1 : F Int = Int

and

g2 trans g1 : G Int = Int
```


This is the result we were hoping for!



Note that `(sym (F g1)) trans (sym (F g2)) trans g1`
is equivalent to the more "compact" form
`sym (F (g2 trans g1)) trans g1`.


## SUMMARY



The method outlined above performs reductions on equations
in normal form. These equations are logically equivalent
to the original constraint set, for our running example,
we have that


```wiki
F (G Int) = Int  /\ G Int = F (G Int)

iff

exists a, b, c.
  F a = Int /\
  G Int = a /\
  G Int = b /\
  F c = b /\
  G Int = c
```


When reducing the equations in normal form, we obtain
proof terms in terms of


```wiki
g1' : F a = Int
d1  : G Int = a
g2' : G Int = b
d2  : F c = b
d3  : G Int = c

rather than

g1 : F (G Int) = Int
g2 : G Int = F (G Int)
```


The trick is that we yet need to normalize proof terms.
See the above calculations. Thus, we find that


```wiki
(sym (F g1)) trans (sym (F g2)) trans g1 : F Int = Int

and

g2 trans g1 : G Int = Int
```


In general, we will need to include axioms and wanted equations.
But there is really no problem with such extensions. Of course,
the details have yet to be worked out.


