
This page outlines a design for the "unresolved infix expressions" feature requested in [\#4430](http://gitlabghc.nibbler/ghc/ghc/issues/4430). Parts of this page are copied from the ticket.


## Motivation



Consider writing a quasiquoter to parse haskell (for example, the `parseHaskell` quasiquoter mentioned in Part D of Simon's [
New directions for Template Haskell](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)).



How is the quasiquoter supposed to handle infix expressions, such as


```wiki
foo = [parseHaskell| 3 * 5 + 4 |]
```


In order to parse this expression properly, the quasiquoter needs to know the fixities of the operators mentioned, and this information is only available at the call site. 



The solution proposed in this page is to extend the TH syntax datatypes to support "unresolved infix expressions" -- so that a quasiquoter can essentially say, "I have done as much parsing as possible, but you (GHC) will have to handle the fixities".


## The API change



We add the following constructor (or similar) to template haskell's `Exp` datatype:


```wiki
data Exp = 
     ...
     | UnresolvedInfixE [(Exp, Op)] Exp Section
     ...
```


and we add the following type synonym and datatype:


```wiki
type Op = Exp
data Section = NoSection
             | LeftSection Exp
             | RightSection Exp
```


with the understanding that `UnresolvedInfixE [("a","+"), ("b", "*")] "c" NoSection` (here, and later, I write strings in place of syntax trees) denote the (unparenthesised) expression ` a + b * c ` with the expectation that GHC will apply the correct fixities when splicing (this will be explained in more detail later). The analogous interpretations for other values of the `Section` field are:


```wiki
 UnresolvedInfixE [(e1,op1),(e2,op2),...,(en,opn)] efinal (LeftSection op)  ~~~ op e1 op1 e2 op2 ... en opn efinal
 UnresolvedInfixE [(e1,op1),(e2,op2),...,(en,opn)] efinal (RightSection op) ~~~ e1 op1 e2 op2 ... en opn efinal op
```

## The meaning of splices containing `UnresolvedInfixE`



Suppose we have a splice containing an `UnresolvedInfixE`, such as


```wiki
foo = $( ... (UnresolvedInfixE exprs expr) ... )
```


As with all splices, GHC will evaluate the TH syntax tree and then convert this to the type of syntax tree that GHC uses internally. When GHC comes across a constructor 


```wiki
UnresolvedInfixE [(e1,op1),(e2,op2),...,(en,opn)] efinal section
```


we use the following procedure:


1. First, look up the operators `op1,...,opn` (and the operator in `section` if it exists) **in the context where the `UnresolvedInfixE` occurs**, and find out their fixities.
1. Now look at the "expression" 

  ```wiki
      e1 op1 e2 op2 e3 op3 ... en opn efinal         (if section = NoSection)
   op e1 op1 e2 op2 e3 op3 ... en opn efinal         (if section = LeftSection op)
      e1 op1 e2 op2 e3 op3 ... en opn efinal op      (if section = RightSection op)
  ```

  and disambiguate it using the fixities of the `op1,...,opn,op` (following the Haskell rules for parsing unparenthesised infix expressions). The result will be a parenthesised (unambiguous) tree of infix expressions, for example any of the following:

  ```wiki
   e1 op1 (e2 op2 (e3 op3 ... (en opn efinal)...)) -- if all operators were right-associative of equal precedence
   (...((e1 op1 e2) op2 e3) ... en) opn efinal     -- if all operators were left-associative of equal precedence
  ```

  This is the syntax tree that results.


Some special cases are worth pointing out.


- The bold phrase, above, "in the context where the `UnresolvedInfixE` occurs" simply means that the operators are looked up by following all the lexical scoping rules, so that the correct fixities are found even in the presence of name shadowing. So, our hypothetical `parseHaskell` quasiquoter would behave as expected on

  ```wiki
   [parseHaskell| let (++) = ... in x ++ y ++ z |]
  ```

  in the sense that the infix expression `x ++ y ++ z` is resolved using the fixity of the locally-defined `(++)` operator, rather than the fixity of `(Prelude.++)`
- There is no special treatment of trees of `UnresolvedInfixE`. For example, the tree

  ```wiki
   UnresolvedInfixE
    (UnresolvedInfixE e1 op1 e2)
      op2
    (UnresolveInfixE e3 op3 e4)
  ```

  will resolve to

  ```wiki
   (e1 op1 e2) op2 (e3 op3 e4)
  ```

  no matter what the fixities of the operators are. In particular, this means that the tree just listed above behaves differently from the tree

  ```wiki
   UnresolvedInfixE e1 op1 e2 op2 e3 op3 e4
  ```

  It is crucial that these two trees are treated differently, because the `parseHaskell` quasiquoter needs to distinguish between the following two expressions:

  ```wiki
   [parseHaskell| (a + b) + (c + d) |]
   [parseHaskell| a + b + c + d     |]
  ```

  In the first expression, we do not want the compiler to reassociate the infix expression (because it is already parenthesised), but in the second expression, we do.
- We have the following equivalences of syntax trees, as far as they are treated when splicing:

  ```wiki
   InfixE (Just e1) op (Just e2)    <--->    UnresolvedInfixE [(e1,op)] e2 NoSection
   InfixE Nothing   op (Just e2)    <--->    UnresolvedInfixE []        e2 (LeftSection op)
   InfixE (Just e1) op Nothing      <--->    UnresolvedInfixE []        e1 (RightSection op)
   InfixE Nothing   op Nothing   <------------ this will be rejected when splicing.
   e                                <--->    UnresolvedInfixE []        e NoSection
  ```

  In view of the previous dot point, we see that there is no special treatment for trees of `InfixE` and `UnresolvedInfixE` constructors. 

  These equivalences tell us that `UnresolvedInfixE` expressions are unambiguous if either:

  - the list has length 0; or
  - the list has length 1 and we have `NoSection`

  The redundancy introduced by these equivalences is discussed further down.
- The following two syntax trees:

  ```wiki
   UnresolvedInfixE [(e1,op1)] e2 (RightSection op)               say, generated by [parseHaskell| (2 + 3 +) |]
  ```

  and 

  ```wiki
   InfixE                                                             
     (Just $ UnresolvedInfixE [(e1,op1)] e2 NoSection)           say, generated by [parseHaskell| ((2 + 3) +) |]
     op                                                               
     Nothing                                                          
  ```

  are treated *differently* when splicing. If `(+)` is left-associative, then they will resolve the same way; if `(+)` is right-associative, then the first syntax tree will result in an error, whereas the second syntax tree will not.

  This demonstrates that it is necessary to include sectioning information in the `UnresolvedInfixE` constructor, as we do not want to silently convert illegal section expressions (such as `(2 + 3 +)` when `(+)` is right-associative) into legal, but different, section expressions (say `((2 + 3) +)`).

## Template Haskell Quotations and `UnresolvedInfixE`



If we have a Template Haskell quote containing infix expressions, for example


```wiki
 foo = [| x + y * z |]
```


then how will these infix expressions be represented?



The important point is: **the infix expressions will have already been resolved**. (This is because GHC knows that `(+)` and `(*)` in the above example refer to the in-scope ones, so the fixities are known when the quote is constructed -- so we should certainly communicate this information.)



The precise representation of the (already-resolved) infix expressions will depend on whether the `InfixE` constructor will be removed, as discussed in the following section. At present, we can say the following:


- if the `InfixE` constructor is *not* removed, then (already-resolved) infix expressions will be written in terms of the `InfixE` constructor, as is currently the case.
- if the `InfixE` constructor *is* removed, then infix expressions will be written in terms of unambiguous `UnresolvedInfixE` expressions (see above for discussion of when `UnresolvedInfixE` expressions are unambiguous).

## Redundancy; should `InfixE` be removed?



Because of the equivalences discussed earlier, we see that the `InfixE` constructor will be completely redundant after these changes. It is not clear to me what this means for design. The following are some options.


- We could remove the `InfixE` constructor. Unfortunately, this means that every TH program which generates infix expressions would have to be changed.
- We could modify the `UnresolvedInfixE` constructor to remove the ambiguity -- the idea being that `UnresolvedInfixE` is reserved for infix expressions which are truly ambiguous. This would involve requiring the list to be of length \>=2 when we have `NoSection`, and length \>=1 when we have `LeftSection` or `RightSection`. To do this the most obvious way ("unrolling" part of the list) unfortunately makes the constructors very large:

  ```wiki
   data Exp = 
      ...
      | UnresolvedInfixE       [(Exp,Op)] Exp Op Exp Op Exp
      | UnresolvedLeftSection  Op Exp Op Exp [(Op,Exp)]    
      | UnresolvedRightSection [(Exp,Op)] Exp Op Exp Op
  ```
- We could put up with the redundancy, and perhaps write a `convertUnambiguousToInfixE :: Exp -> Exp` which converts all unambiguous `UnresolvedInfixE`s to `InfixE`s.

## Is there a better definition of `UnresolvedInfixE`?



The definition of `UnresolvedInfixE` I have given above is somewhat ugly, because it suggests an asymmetry which is not actually present. However, it manages to enforce the precise relationship that must hold between the number of operators and the number of operands -- which, for example, `InfixE` does not currently enforce.



Is there a better definition of this constructor?


