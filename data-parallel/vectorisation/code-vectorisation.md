## Code Vectorisation


```wiki
(\x -> e)*
  | isUbxFun (\x->e) =      (\x_v -> e*) :|| (\x_v -> e^)
  | otherwise        = Fun ((\x_v -> e*) :|| (\x_v -> e^))
(e1 e2)*
  | isUbxFun e1      = vfunS e1* e2*
  | otherwise        = funS e1* e2*
```


A function fulfills `isUbxFun` if either its argument or its result type is an unboxed type.


