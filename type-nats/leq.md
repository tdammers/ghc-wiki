
One step relation (in a model M):


```wiki
(a + b ~ c) => (a <= c, b <= c)
(a * b ~ c) => (b <= c)                   -- M |- 1 <= a
(a * b ~ c) => (a <= c)                   -- M |- 1 <= b
(0 ^ b ~ c) => (c <= 1)
(a ^ b ~ a) => (a <= 1)
(a ^ b ~ c) => (b <= c)                   -- M |- 2 <= a
(a ^ b ~ c) => (a <= c)                   -- M |- 1 <= b
```


Transitivity and Cycles...


