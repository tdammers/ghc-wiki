# Alternative view patterns: lightweight views for Haskell



This is a copy of the [ViewPatterns](view-patterns) page, but with a different syntax.


## Basic view patterns



View patterns are a convenient way of pattern-matching against values of abstract types.  For example, in a programming language implementation, we might represent the syntax of the types of the language as follows:


```wiki
   type Typ
 
   data TypView = Unit
                | Arrow Typ Typ

   view :: Typ -> TypView

   -- additional operations for constructing Typ's ...
```


The representation of `Typ` is held abstract, permitting implementations to use a fancy representation (e.g., hash-consing to manage sharing).



In current Haskell, using this signature is a little inconvenient:


```wiki
   size :: Typ -> Integer
   size t = case view t of
     Unit -> 1
     Arrow t1 t2 -> size t1 + size t2
```


It is necessary to iterate the case, rather than using an equational function definition.  And the situation is even worse when the matching against `t` is buried deep inside another pattern.
In response, programmers sometimes eschew type abstraction in favor of revealing a concrete datatype that is easy to pattern-match against.



View patterns permit calling the view function inside the pattern and matching against the result:


```wiki
   size (x | Unit <- x) = 1
   size (x | Arrow t1 t2 <- x) = size t1 + size t2
```


That is, we add a new form of pattern, written 


```wiki
   pattern | qual1 , ..., qualn
```


This has the same meaning as pattern guard at the top level, but here they are allowed to be part of a pattern, and thus nested inside other patterns.
So first the pattern is matched, then the qualifiers are matched in order.  An expression qualifier has to evaluate to `True`, and for a *pat `<-` exp* the pattern must match the expression.



The key feature of this proposal is its modesty, rather than its ambition:


- There is no new form of declaration (e.g. 'view' or 'pattern synonym').  
- No new syntax, the existing pattern guard syntax is simply generalized to be allowed inside patterns.
- No changes are needed to import or export mechanisms.
- Both static and dynamic semantics are extremely simple.


It is essentially some simple syntactic sugar for patterns.
However, sometimes modest syntactic sugar can have profound consequences. In this case, it's possible that people would start routinely hiding the data representation and exporting view functions instead, which would be an excellent thing.


### Semantics



**Scoping** for *pat* `|` *quals*:


- The variables bound by the view pattern are the variables bound by *pat* and *quals*.
- Variables bound by patterns to the left of a view pattern expression are in scope.  For example:

  - In function definitions, variables bound by matching earlier curried arguments may be used in view pattern expressions in later arguments.

    ```wiki
       example :: (String -> Integer) -> String -> Bool
       example f (x | f x == 4) = True
    ```
  - Variables can be bound to the left in tuples and data constructors:

    ```wiki
       example :: ((String -> Integer,Integer), String) -> Bool
       example ((f,_), x | f x == 4) = True
    ```


There is no way to *localise* how many variables are brought into scope.  For example, consider


```wiki
  f (x | x>=k, let y = x-k) = rhs
```


Here, both `x` and `y` are in scope in `rhs`; and there is no way to say "only bind `y` in this pattern".



**Typing**
If *pat* `|` *quals* has same type as *pat*, and the "quals" must be well typed in the same way as for pattern guards.



**Evaluation**
To match a value *v* against a pattern (*pat* `|` *quals*), match "v" against *pat* and then match the *quals*.


### Examples



We discuss some examples of pattern-matching abstract types and of other uses of view patterns here.


#### Join lists



The requisite join-list example:


```wiki
   data JList a = Empty 
                | Single a
                | Join (JList a) (JList a)
 
   data JListView a = Nil
                    | Cons a (JList a)
```


Here we've chosen that the view type only exposes the cons/nil structure one level at a time: the second argument to `Cons` is a join-list, not a view of it---but that's of course up to the programmer.  



The implementation of the view:


```wiki
  view :: JList a -> JListView a
  view Empty = Nil
  view (Single a) = Cons a Empty
  view (Join (l | Cons xh xt <- view l) y) = Cons xh $ Join xt y
  view (Join (l | Nil <- view) l) = view y
```


Note the recursive uses of the view function in view patterns within its own definition.



An example of using it:


```wiki
  length :: JList a -> Integer
  length (l | Nil <- view l) = 0
  length (l | Cons x xs <- l) = 1 + length xs
```


For more general sequences, `Data.Sequence` already defines the views from the left and from the right


```wiki
   data ViewL a
   = EmptyL
   | (:<) a (Seq a)

   viewl :: Seq a -> ViewL a

   data ViewR a
   = EmptyR
   | (:>) (Seq a) a

   viewr :: Seq a -> ViewR a
```


that now may be used in view patterns.


#### Partial views



Here's an alternate style of view definition: rather than mapping the abstract type to a single sum type, you provide outjections optionally inverting each constructor:


```wiki
   type Typ

   outUnit  : Typ -> Maybe ()
   outArrow : Typ -> Maybe (Typ, Typ)

   -- additional operations for constructing Typ's ...
```


This view is used as follows:


```wiki
   size (t | Just _ <- outUnit t) = 1
   size (t | Just (t1, t2) <- outArrow t) = size t1 + size t2
```


This style should be discouraged when the view is in fact a total operation, as it moves the documentation of this fact out of the type system, making it harder for both people and the compiler to check exhaustiveness.  However, it may be a useful idiom for defining a partial matching function with several constructors (e.g., in XML processing).


#### Sets



Here is a small module that allows to decompose sets with respect to a given element, deleting it hereby.


```wiki
module Set(Set, empty, insert, delete, has) where

  newtype Set a = S [a]
  
  has :: Eq a => a -> Set a -> Maybe (Set a)
  has x (S xs) | x `elem` xs = Just (xs \\ x)
               | otherwise   = Nothing
  
  delete :: a -> Set a -> Set a
  delete x (r | Just s <- has r) = s
  delete x s                     = s
  
  insert :: a -> Set a -> Set a
  insert x (s | Just _ <- has x s) = s
  insert x (S xs)                = S (x:xs)
```


Note the use of the previous argument `x` in later view patterns.


#### Erlang-style parsing



Sagonas et al describe an extension to Erlang that supports pattern-matching on bit-strings ([
"Application, implementation and performance evaluation of bit-stream programming in Erlang", PADL'07](http://user.it.uu.se/~kostis/Papers/index.html#Conference)).  Suppose we had a parsing function thus:


```wiki
  bits :: Int -> ByteString -> Maybe (Word, ByteString)
  -- (bits n bs) parses n bits from the front of bs, returning
  -- the n-bit Word, and the remainder of bs
```


Then we could write patterns like this:


```wiki
  parsePacket :: ByteString -> ...
  parsePacket (p1 |  Just (n, (p2 | Just (val, bs) <- bits n p2)) <- bits 3 p1) = ...
```


This parses 3 bits to get the value of `n`, and then parses `n` bits to get the value of `val`.  Note that this example uses the left-to-right scoping in the inner tuple: the first component is jused in the view expression in the second.


#### N+k patterns



`(n+k)` patterns use the following view function:


```wiki
   np :: Num a => a -> a -> Maybe a
   np k n | k <= n = Just (n-k)
 	   | otherwise = Nothing
```


They are used as follows:


```wiki
   fib :: Num a -> a -> a
   fib 0 = 1
   fib 1 = 1
   fib (n2 | Just n <- np 2 n2) = fib (n + 1) + fib n
```


Note the integration with type classes: the view function can be overloaded, and its use in a view pattern gives rise to a type-class constraint (in this case, that in turn makes `fib` overloaded).



Alternatively it can be written without an auxiliary function


```wiki
   fib :: Num a -> a -> a
   fib 0 = 1
   fib 1 = 1
   fib (n2 | let n = n2-2, n >= 0) = fib (n + 1) + fib n
```


`n+k` patterns are another a good opportunity for passing view data at run-time, as in:


```wiki
   example k (m | m > k) = ...
```

#### Named constants



View patterns can be used to pattern match against named constants:


```wiki
    errorVal :: Int -> Bool
    errorVal = (== 4)
    f (x | errorVal x) =  ...
```

#### Both patterns



A "both pattern" `pat1 & pat2` matches a value against both `pat1` and `pat2` and succeeds only when they both succeed.  A special case is as-patterns, `x@p`, where the first pattern is a variable.  Both patterns can be programmed using view patterns:



And used as follows:


```wiki
   f (x | xs <- x, h : t <- x) = h : (xs ++ t)
```

## Comparison with existing view patterns



There are straightforward translations between old and new style view patterns:

*pat* `<-` *exp* 

translates to

*x* `|` *pat* `<-` *exp x*

where *x* is a fresh variable.



And in the other direction

*pat* `|` *quals*

translates to

`Just` *vs* `<- let` *f* *pat* `|` *quals* `= Just` *vs*`;` *f* `_ = Nothing in` *f*

where *vs* are all the variables bound in *pat* and *quals* and *f* is fresh.



Notable differences:


- The new style is a straightforward extension of existing syntax (pattern guards).
- The new style allows more powerful matching without introducing an auxiliary type (typically a `Maybe`).  This leads to a more straightforward translation of pattern matching and better efficiency without sophisticated transformations.
- The new style often has to introduce an extra variable to match the entire expression; a variable that is not needed on the right hand side.  This small pollution of the namespace can be avoided if the [PatternSynonyms](pattern-synonyms) proposal is also implemented.
