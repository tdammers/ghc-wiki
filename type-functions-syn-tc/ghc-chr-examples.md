```wiki
A rough set of examples (using the refined 'chr-based' algorithm)


Assuming a normal form of equations, it is sufficient to apply
the following 'FD'-rule step.

e1: F t1 = t2

e2: F t1 = t3

==> 

e1: F t1 = t2
e2: F t1 = t3
e3: t2 = t3

where e3 =  (sym e1) trans e2


Here are some examples:

EXAMPLE 1

Assume (given)

g : forall a. S [a] = [S a]   -- axiom

d1 : T Int = Int              -- local equations
d2 : T [Int] = S [Int]
d3 : T Int = S Int


wanted

? : T [Int] = [Int]


Step 1: Normalize (local and wanted) equations
        (also applies to axioms but we skip this step here)

g : forall a. S [a] = [S a]   (0)


g1 : T Int = Int       (1)
g2 : T [Int] = a       (2)
g3 : S [Int] = a       (3)
g4 : T Int = b         (4)
g5 : S Int = b         (5)


? : T [Int] = [Int]    (6)

Reduction steps:


2+6 => ?' : a = [Int]

   ? = g2 trans ?'

03 from 0+3 =>  
    g03 : a = [S Int]
    g03 = (sym g3) trans (g Int)

503 from 5+03 =>
    g503 : a = [b]
    g503 = g03 trans ([] g5)

14 from 1+4 =>
    g14 : b = Int
    g14 = (sym g4) trans g1

Apply 14 on 503 =>
    g' : a = [Int]
    g' = g503 trans ([] g14)

We found a match for the (reduced) wanted constraint.

 ? = g2 trans ?'
   = g2 trans (g503 trans ([] g14))
   = g2 trans ((g03 trans ([] g5)) trans ([] g14))
   = g2 trans ((((sym g3) trans (g Int)) trans ([] g5)) trans ([] g14))

How to map back to the original (given) local equations?


The same calculation using the original (given) local equations.

g : forall a. S [a] = [S a]  (0)   -- axiom

d1 : T Int = Int        (1)      -- local equations
d2 : T [Int] = S [Int]  (2)
d3 : T Int = S Int      (3)

wanted:
d4 : T [Int] = [Int]     (4)

Reduction steps:

24: 2+4 =>
    d24 : S [Int] = [Int]
    d4 = d2 trans d24       -- wanted, we're interested how to construct d4!

024: 0+24 =>
     d024 : [S Int] = [Int]
     d24 = (sym (g Int)) trans d024

deompose =>
     d024' : S Int = Int
     d024 = [] d024'

13 : 1+3 =>
     d13 : S Int = Int
     d13 = (sym d3) trans d1


We found a match for the (reduced) wanted constraint.

    d024' = d13
          = (sym d3) trans d1

Thus,

d4 = d2 trans d24
   = d2 trans ((sym (g Int)) trans d024)
   = d2 trans ((sym (g Int)) trans ([] d024'))
   = d2 trans ((sym (g Int)) trans ([] ((sym d3) trans d1)))


Compare the normalized result 

g2 trans ((((sym g3) trans (g Int)) trans ([] g5)) trans ([] g14))

against the 'unnormalized' result 

d2 trans ((sym (g Int)) trans ([] ((sym d3) trans d1)))


EXAMPLE 2

no axioms involved

given:
d1 : G Int = Bool
d2 : F (G Int) = Int

wanted:

d : F Bool = Int

clearly d = (sym (F d1)) trans d2


The normalized case:

g1 : G Int = Bool   (1)
g2 : G Int = a      (2)
g3 : F a = Int      (3)

Reductions:

12 from 1 + 2 =>
   g12 : a = Bool
   g12 = (sym g2) trans g1

apply 12 on g3:

   g3' : F Bool = Int
   g3' = (sym (F g12)) trans g3

Match found, we find

d = (sym (F g12)) trans g3
  = (sym (F ((sym g2) trans g1))) trans g3

compare with 

  (sym (F d1)) trans d2
```