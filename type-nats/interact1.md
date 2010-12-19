
(NOTE: The rules involving constants but no computation could be generalized
to work for variables as well, as long as we know that the variables
are in the acceptable ranges.  Such information could be computed
from the \<= model, perhaps.)



Top-level interactions for `TypeNat`:


```wiki
TypeNat m
```


Top-level interactions for \<=


```wiki
m <= n   <=> {m <= n}
0 <= a   <=> True
a <= 0   <=> a ~ 0
```


Top-level interactions for +.


```wiki
(m + n ~ k) <=> {m + n == k}

(m + a ~ n) <=> a ~ {n - m}    -- n >= m
(0 + a ~ b) <=> a ~ b

(a + b ~ a) <=> (b ~ 0)
(a + b ~ b) <=> (a ~ 0)

(a + a ~ b) <=> (2 * a ~ b)
(a + b ~ 0) <=> (a ~ 0, b ~ 0) -- XXX: Drop this, follows from <= rules?

(a + m ~ b) <=> (m + a ~ b)    -- simple normalization cuts down on some rules
```


Top-level interactions for \*.


```wiki
(m * n ~ k) <=> {m * n == k}
(m * a ~ n) <=> a ~ { n / m }    -- m `divides` n, False otherwise

(0 * a ~ b) <=> b ~ 0
(1 * a ~ b) <=> a ~ b
(m * a ~ a) <=> a ~ 0            -- 2 <= m

(a * b ~ 1) <=> (a ~ 1, b ~ 1)
(a * a ~ b) <=> a ^ 2 ~ b

(a * m ~ b) <=> (m * a ~ b)      -- simple normalization cuts down on some rules
```


Top-level interactions for `^`


```wiki
(m ^ n ~ k) <=> {m ^ n == k}

(m ^ a ~ n) <=> a ~ {log m n}   -- log (base m) of n exists, False otherwise
(1 ^ a ~ b) <=> b ~ 1
(m ^ a ~ a) <=> False           -- m /= 1

(a ^ m ~ n) <=> a ~ {root m n}  -- m-th root of n exists, False otherwise
(a ^ 0 ~ b) <=> b ~ 1
(a ^ 1 ~ b) <=> a ~ b
(a ^ m ~ a) <=> (a <= 1)        -- 2 <= m

```