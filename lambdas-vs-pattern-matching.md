
This page is a summary of proposals from [\#4359](http://gitlabghc.nibbler/ghc/ghc/issues/4359)


## The problem



The current lambda abstraction syntax allows us to conveniently bind parts of the arguments by using patterns, but does not provide a way to branch quickly (without naming the argument). Usually we just cringe a bit and write


```wiki
\tmp -> case tmp of
  Pat1 -> ...
  Pat2 -> ...
```


or even


```wiki
\meaningfulName -> case meaningfulName of
  Pat1 -> ...
  Pat2 -> ...
```


However, when this situation nests (e.g. monadic bind) and variables have the same type, naming becomes painful and often degrades to not-so-meaningful ```meaningfulName1```, ```meaningfulName2```, ...



A similar problem exists with `proc` expressions from arrow notation, which can be regarded as generalized lambda expressions. We sometimes have expressions of the following structure:


```wiki
proc meaningfulName -> case meaningfulName of
    Pat1 -> ...
    Pat2 -> ...
```


Here, the dots stand for arrow expressions, not ordinary expressions.


## The proposals


### LambdaCase: ```case of```



A simple sugar for one-argument lambda abstractions.


```wiki
case of
  Pat1 -> Expr1
  Pat2 -> Expr2
```


desugars to


```wiki
\freshName -> case freshName of
  Pat1 -> Expr1
  Pat2 -> Expr2
```

- Pros

  - No conflicts with the current syntax (the sequence ```case of``` is illegal)
- Cons

  - Looks weird (no hint of being a lambda abstraction)
  - Single-argument solution (see the note)
  - Cannot be generalized to cover also `proc` expressions

### LambdaCase: ```\case```



A "less weird" version of ```case of```. As above,


```wiki
\case
  Pat1 -> Expr1
  Pat2 -> Expr2
```


desugars to


```wiki
\freshName -> case freshName of
  Pat1 -> Expr1
  Pat2 -> Expr2
```


(```\case``` is a layout herald).


- Pros

  - No conflicts with the current syntax (the sequence ```\ case``` is illegal)
  - An analog syntax for `proc` expressions can be gained by replacing `\` with `proc`
- Cons

  - Single-argument solution (see the note). One way to extend it to support multiple arguments is

    ```wiki
    \case
      Pat1_1, Pat1_2, ... -> Expr1
      Pat2_1, Pat2_2, ... -> Expr2
    ```

    (separation with commas is supposed to preserve case-like feel, e.g. ```Just x, Just y ->``` vs ```(Just x) (Just y) ->```) which is considered unorthodox by GHC HQ.

### MultiClauseLambdas



Extend the current syntax with alternative clauses:


```wiki
\Pat1_1 Pat1_2 ... -> Expr1
 Pat2_1 Pat2_2 ... -> Expr2
 ...
```


(```\``` becomes a layout herald)


- Pros

  - Multi-argument solution (see the note)
  - An analog syntax for `proc` expressions can be gained by replacing `\` with `proc`
- Cons

  - Breaks current idioms. For example,

    ```wiki
    mask $ \restore -> do
      stmt1
      stmt2
    ```

    becomes illegal because ```stmt1``` is indented less than ```restore```. One way to avoid this is to not make ```\``` a herald, forcing users to use explicit layout for multi-clause abstractions, i.e.

    ```wiki
    \ { Pat1_1 Pat1_2 ... -> Expr1
      ; Pat2_1 Pat2_2 ... -> Expr2 }
    ```

    Another is to start each clause with a ```\```:

    ```wiki
    \ Pat1_1 Pat1_2 ... -> Expr1
    \ Pat2_1 Pat2_2 ... -> Expr2
    \ ...
    ```

### MultiClauseLambdas with a keyword



Addresses the layout problem of MultiClauseLambdas. Requires multi-clause abstractions to have a keyword after ```\```:


```wiki
\KEYWORD Pat1_1 Pat1_2 ... -> Expr1
         Pat2_1 Pat2_2 ... -> Expr2
         ...
```


(```\KEYWORD``` is a layout herald)


- Pros

  - No conflicts with the current syntax
  - Multi-argument solution (see the note)
  - An analog syntax for `proc` expressions can be gained by replacing `\` with `proc`
- Cons

  - Deciding on the keyword may take years

### Extra: LambdaMatch



A full revamp of pattern matching: [
Haskell' ticket](http://hackage.haskell.org/trac/haskell-prime/ticket/114).


## Notes


### Single vs multi-argument



(field report by Mikhail Vorozhtsov) I've been using ```\case``` for over a year and tried MultiClauseLambdas for about two months. In my code base ```\case``` seems to cover 99% of cases (no pun intended) and ```curry $ \case ...``` does the job for the rest, so having only a single-argument solution may be not as restrictive as it seems. On the other hand, I had a hard time with MultiClauseLambdas extra clauses: I just kept writing ```Just x -> Expr``` instead of the correct ```(Just x) -> Expr```. It seems that lines like ```[spaces]Pat -> Expr``` are just hardwired to case-expressions in my brain.


