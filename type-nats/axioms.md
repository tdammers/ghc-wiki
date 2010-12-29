
(NOTE: This is work in progress)



These axioms are used by GHC's solver to construct proofs/evidence for various predicates involving type-level naturals.



The actual algorithm for constructing the evidence is implemented as set of rules (interactions) which are described separately.



The "\*Def" axioms bellow look a bit odd but all they are saying is that the predicates which are being defined behave like their corresponding mathematical operations.



Notation:


```wiki
k,m,n:  literals of kind Nat
r,s,t:  arbitrary terms of kind Nat
```


Comparison:


```wiki
leqDef:      m <= n    -- if "m <= n"
leqLeast:    0 <= t
leqRefl:     t <= t
leqTrans:    (r <= s, s <= t) => r <= t
leqAntiSym:  (s <= t, t <= s) => s ~ t
```


Addition:


```wiki
addDef:      m + n ~ k     -- if "m + n == k"
addUnit:     0 + t ~ t
addAssoc:    (r + s) + t ~ r + (s + t)                   | (r + s = u, s + t = v, r + v = w1, u + t = w2) => w1 ~ w2
addCommutes: t + s ~ s + t                               | (r + s ~ t) => (s + r ~ t)
addCancel:   (r + s ~ r + t) => s ~ t                    | (r + s ~ u, r + t = u) => s ~ t
```


Multiplication:


```wiki
mulDef:      m * n ~ k   -- if "m * n == k"
mulUnit:     1 * t ~ t
mulAssoc:    (r * s) * t ~ r * (s * t)
mulCommutes: t * s ~ s * t
mulCancel:   (r * s ~ r * t, 1 <= r) => s ~ t
```


Exponentiation:


```wiki
expDef:      m ^ n ~ k    -- if "m ^ n == k"
exp0:        a ^ 0 ~ 1
exp1:        a ^ 1 ~ a
log1:        1 ^ a ~ 1

XXX:
m ^ a ~ a => m ~ 1
a ^ m ~ a => a <= 1        -- 2 <= m
0 ^ a ~ b => b <= 1
```


Interactions:


```wiki
addMulDistr: r * (s + t) = (r * s) + (r * t)
```


References:


- [
  http://en.wikipedia.org/wiki/Semiring](http://en.wikipedia.org/wiki/Semiring)
- [
  http://en.wikipedia.org/wiki/Cancellation\_property](http://en.wikipedia.org/wiki/Cancellation_property)
