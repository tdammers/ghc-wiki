
(thanks to Bernie Pope for letting me take advantage of his efforts with notation and abstract syntax)
 


### An abstract syntax



For the purpose of exploring the rules we need an abstract syntax. Below is one for a simple core functional language:


```wiki
   Decls(D)        -->   x :: T   |   x = E   |   data f a1 .. an = K1 .. Km

   Constructors(K) -->   k T1 .. Tn

   Types(T)        -->   f   |   a   |   T1 T2

   Expressions(E)  -->   x   |   k   |   E1 E2   |   let D1 .. Dn in E   |   case E of A1 .. An   |   \y1 .. yn -> E

   Alts(A)         -->   p -> E

   Pats(P)         -->   x   |   k P1 .. Pn
```

### Notation



Double square brackets denote the transformation function, which has two arguments: the expression itself and a list of bindings in scope.
For instance:


```wiki
   [[ E ]]_b  ==> [[ E' ]]_b'
```


means transform expression E with b as the list of bindings in scope into E' with the new list l'



Double brackets denote the auxiliary binding capture function, which takes an expression and returns a list of the variables bound in it:


```wiki
   {{ E }}    ==> x1 .. xn  |  []
```

### breakpointJump desugaring



The main role of the desugaring, as shown by the rules, is injecting the explicit list of local bindings. 



In the rules below \<breakpoint\> and \<breakpointJump\> are placeholders for the several breakpoint flavors. Each flavor of breakpoint has a corresponding jump function:


```wiki
 breakpoint      -  breakpointJump
 breakpointCond  -  breakpointJumpCond
 breakpointAuto  -  breakpointJumpAuto
```


The \<ptr b\> placeholder denotes a pointer to the compiler datastructures for b, which in GHC are values of type [compiler/basictypes/Id.hs](/trac/ghc/browser/ghc/compiler/basictypes/Id.hs).
 
The \<srcloc x\> placeholder denotes the source code location information for the expression x.


```wiki
Declarations:

   [[ x :: T ]]_b             ==>   x :: T

   [[ x = E ]]_b              ==>   x = [[ E ]]_b

   [[ data f a1 .. an = K1 .. Km ]]_b 
                              ==>   data f a1 .. an = K1 .. Km

   {{ x = E }}                ==>   x 

Expressions:

   [[ <breakpoint> x ]]_b     ==>   <breakpointJump> <ptr b> b <srcloc x> [[x]]_b

   [[ x ]]_b                  ==>   x
   
   [[ k ]]_b                  ==>   k

   [[ E1 E2 ]]_b              ==>   [[ E1 ]]_b [[ E2 ]]_b

   [[ let D1 .. Dn in E ]]_b  ==>   let [[ D1 ]]_b .. [[ Dn ]]_b in  [[ E ]]_b'
                                    where b' = {{ D1 }} ++ .. ++ {{ Dn }}

   [[ case E of A1 .. An ]]_b ==>   case [[ E ]]_b of [[ A1 ]]_b .. [[ An ]]_b

   [[ \y1 .. yn -> E ]]_b     ==>   \y1 .. yn -> [[ E ]]_(y1 .. yn ++ b)


   {{ E }}                    ==>   [] 

Alternatives:

   [[ p -> E ]]_b             ==>   p -> [[ E ]]_({{p}} ++ b)

 
   {{ A }}                    ==>   []

Pats:

   {{ x }}                    ==>   x
   
   {{ k P1 .. Pn }}           ==>   {{ P1 }} ++  ...  ++ {{ Pn }}

```