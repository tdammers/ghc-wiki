# View patterns: lightweight views for Haskell




    

1. 
1. 
          [Basic view patterns](#Basicviewpatterns)
          

  1. 
  1. 
                [Semantics](#Semantics)
              
  1. 
  1. 
                [Examples](#Examples)
              
  1. 


        
1. 
1. 
          [Further Syntactic Extensions](#FurtherSyntacticExtensions)
          

  1. 
  1. 
                [Implicit Maybe](#ImplicitMaybe)
              
  1. 
  1. 
                [Implicit View Functions](#ImplicitViewFunctions)
              
  1. 


        
1. 
1. 
          [Compilation](#Compilation)
        
1. 
1. 
          [Features views can have](#Featuresviewscanhave)
        
1. 
1. 
          [Related work](#Relatedwork)
        
1. 





View patterns were introduced in GHC 6.10.  This page has been revised to reflect what we've implemented.  


- For the previous discussion, see [ViewPatternsArchive](view-patterns-archive).
- For an assessment of view patterns in practice, see [
  Neil Mitchell's blog entry](http://neilmitchell.blogspot.com/2009/11/reviewing-view-patterns.html)
- For some discussion of implicit view patterns see [
  GHC feature request 3583](http://hackage.haskell.org/trac/ghc/ticket/3583)
- For a discussion of pattern bindings and view patterns see [\#4061](http://gitlabghc.nibbler/ghc/ghc/issues/4061)
- For an alternative syntax for view patterns: [ViewPatternsAlternative](view-patterns-alternative).

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
   size (view -> Unit) = 1
   size (view -> Arrow t1 t2) = size t1 + size t2
```


That is, we add a new form of pattern, written 


```wiki
   expression -> pattern
```


that means "apply the expression to whatever we're trying to match against, and then match the result of that application against the pattern".  The expression can be any Haskell expression of function type, and view patterns can be used wherever patterns are currently used.



The key feature of this proposal is its modesty, rather than its ambition:


- There is no new form of declaration (e.g. 'view' or 'pattern synonym').  
- The functions used in view patterns are ordinary Haskell functions, and can be called from ordinary Haskell code.
- No changes are needed to import or export mechanisms.
- Both static and dynamic semantics are extremely simple.


It is essentially some simple syntactic sugar for patterns.
However, sometimes modest syntactic sugar can have profound consequences. In this case, it's possible that people would start routinely hiding the data representation and exporting view functions instead, which would be an excellent thing.


### Semantics



**Scoping** for *expr `->` *pat:


- The variables bound by the view pattern are the variables bound by *pat*.
- Any variables in *expr* are bound occurrences.  Variables bound by patterns to the left of a view pattern expression are in scope.  For example:

  - In function definitions, variables bound by matching earlier curried arguments may be used in view pattern expressions in later arguments.

    ```wiki
       example :: (String -> Integer) -> String -> Bool
       example f (f -> 4) = True
    ```
  - Variables can be bound to the left in tuples and data constructors:

    ```wiki
       example :: ((String -> Integer,Integer), String) -> Bool
       example ((f,_), f -> 4) = True
    ```


**Typing**
If *expr* has type *t1* `->` *t2* and  *pat* matches a *t2*,  then the whole view pattern has type *t1*.



**Evaluation**
To match a value *v* against a pattern (*expr* `->` *pat*), evaluate *(expr v)* and match the result against *pat*.  


### Examples



We discuss some examples of pattern-matching abstract types and of other  uses of view patterns here.


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
  view (Join (view -> Cons xh xt) y) = Cons xh $ Join xt y
  view (Join (view -> Nil) y) = view y
```


Note the recursive uses of the view function in view patterns within its own definition.



An example of using it:


```wiki
  length :: JList a -> Integer
  length (view -> Nil) = 0
  length (view -> Cons x xs) = 1 + length xs
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
   size (outUnit -> Just _) = 1
   size (outArrow -> Just (t1, t2)) = size t1 + size t2
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
  delete x (has x -> Just s) = s
  delete x s                 = s
  
  insert :: a -> Set a -> Set a
  insert x s@(has x -> Just _) = s
  insert x (S xs)              = S (x:xs)
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
  parsePacket (bits 3 -> Just (n, (bits n -> Just (val, bs)))) = ...
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
   fib :: Num a => a -> a
   fib 0 = 1
   fib 1 = 1
   fib (np 2 -> Just n) = fib (n + 1) + fib n
```


Note the integration with type classes: the view function can be overloaded, and its use in a view pattern gives rise to a type-class constraint (in this case, that in turn makes `fib` overloaded).



`n+k` patterns are another a good opportunity for passing view data at run-time, as in:


```wiki
   example k (np k -> Just n) = ...
```

#### Named constants



View patterns can be used to pattern match against named constants:


```wiki
    errorVal :: Int -> Bool
    errorVal = (== 4)
    f (errorVal -> True) =  ...
```

#### Both patterns



A "both pattern" `pat1 & pat2` matches a value against both `pat1` and `pat2` and succeeds only when they both succeed.  A special case is as-patterns, `x@p`, where the first pattern is a variable.  Both patterns can be programmed using view patterns:


```wiki
   both : a -> (a,a)
   both x = (x,x)
```


And used as follows:


```wiki
   f (both -> (xs, h : t)) = h : (xs ++ t)
```


(However, this might cause a loss of sharing.)


#### Iterator Style



View patterns permit programming in an iterator style, where you name the result of a recursive call but not the term the call was made on.  E.g.:


```wiki
   length [] = 0
   length (_ : length -> n) = 1 + n
   
   map f [] = []
   map f (x : map f -> xs) = f x : xs
   
   foldr f z [] = z
   foldr f z (x : foldr f z -> xs) =  x `f` xs

   unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
   unfoldr f (f -> Just (a, unfoldr f -> b)) = a : b
   unfoldr f other         = []
```

## Further Syntactic Extensions



Next, we describe two further syntactic extensions that we will implement.  


### Implicit Maybe



Above, we saw several examples of view functions that return a `Maybe`, including:


```wiki
   np :: Num a => a -> a -> Maybe a
   np k n | k <= n = Just (n-k)
 	   | otherwise = Nothing
```


which were used as follows:


```wiki
   fib (np 2 -> Just n) = fib (n + 1) + fib n
```


We may implement a special syntax that makes the `Just` implicit, using *expr* `=>` *pat* for *expr* `-> Just` *pat*.  An example use: 


```wiki
   fib (np 2 => n) = fib (n + 1) + fib n
```


This syntax works very nicely with partial views:


```wiki
   size (outUnit => _) = 1
   size (outArrow => (t1, t2)) = size t1 + size t2
```

#### Implicit Tupling



A further syntactic extension would be to have implicit Maybes with implicit tupling: multiple patterns after the `=>` are implicitly tupled.  Then you could write:


```wiki
   size (outArrow => t1 t2) = size t1 + size t2
```

### Implicit View Functions



Total views have one syntactic disadvantage relative to the iterated-case style definition that we started with: you have to repeat the name of the view function in each clause!  We now describe a method for eliding the name of the view function.  



The idea is that we distinguish a particular type class as a hook into the pattern compiler.  The class has the following interface:


```wiki
   class View a b where
      view :: a -> b
```


Then, you can leave off the expresion in a view pattern, writing (`->` *pat*), to mean `view -> ` *pat*.  For example:


```wiki
   size (-> Unit) = 1
   size (-> Arrow t1 t2) = size t1 + size t2
```


means 


```wiki
   size (view -> Unit) = 1
   size (view -> Arrow t1 t2) = size t1 + size t2
```


for the overloaded `view`.  



To use this mechanism, you add instances to `view`, as in:


```wiki
   instance View Typ TypView where
     view = (the view function from above)
```


This way, you don't have to write the name of the view function in each case.  However, there is a still a syntactic marker saying that the case isn't an ordinary pattern match, which may be useful in understanding the performance of the code.  



Of course, you can only use one view function for each hidden-type/view-type pair this way, since you can only have one instance of the class.


#### Functional dependencies



The above implementation of `size` is given the following type:


```wiki
   size :: View a TypView => a -> Int
```


which may or may not be what you want.  (For example, with nested view patterns, you can get into situations where the middle type connecting two view patterns is not determined.)  



Thus, it may be better to make one parameter of the type class determine the other (using associated type synonyms):


```wiki
   class View a where
      type View a 
      view :: a -> View a
```


or 


```wiki
   class View b where
      type Hidden b 
      view :: Hidden b -> a
```


Of course, a programmer can always use all three type classes explicitly; it's just a question of which one should be the default.  We plan to try them out before deciding.
The downside of these versions is that you can only have one view for a type (when `a` determines `View a`) or you can only use a type as a view of one type (when `b` determines `Hidden b`) with the implicit syntax.


## Compilation



**Efficiency**  View patterns can do arbitrary computation, perhaps expensive. It's reasonable to expect the compiler to avoid repeated computation when pattern line up in a column, as in `size` at the top of the page.  In pattern-guard form, common sub-expression should achieve the same effect, but it's quite a bit less obvious.  We should be able to give clear rules for when the avoidance of repeat computation is guaranteed.



Due to type classes, checking for the "same" view pattern must be type-aware; the same source syntax cannot necessarily be commoned up:


```wiki
class View a b where
    view :: a -> b

instance View Int Int where
    view x = x

instance View Int String where
    view _ = "hi"

-- should not be commoned, even though they're syntactically the same,
-- because they are different uses of the overloaded function
f (view -> 1) = 1
f (view -> "hi") = 2
```


**Exhaustiveness/Redundancy.**  It is hard to check for completeness of pattern matching; and likewise for overlap.  But guards already make both of these hard; and GADTs make completness tricky too. So matters are not much worse than before.


## Features views can have



In comparing the different views proposals below, it will be useful to have terminology for some features of views.


#### Value input feature



Our proposal has the *value input* feature: the view function can be passed parameters; and those those parameters can mention variables bound by patterns to the left.  For example, this permits a view function itself to be passed as an argument, so patterns, in a sense, become first class.


#### Implicit `Maybe` feature



Our proposal has the *implicit `Maybe`* feature: the syntax *expr* `=>` *pat* permits the programmer to elide the `Just`, for example when using partial views.  


#### Transparent ordinary Patterns



Our proposal does not have the *transparent ordinary patterns* feature: view patterns are written differently than ordinary patterns.
There are pros and cons both ways:
The advantage of having transparent ordinary patterns is that you can replace a concrete datatype with an abstract type and a view without changing client code.  A disadvantage is that view patterns can do arbitrary computation, perhaps expensive, so it's good to have a syntactic marker that some computation beyond ordinary pattern matching may be going on.  Another disadvantage is that transparent ordinary patterns require a larger language extension than just a new form of pattern, so that certain names may be declared to be view constructors for a type.  We consider our proposal's implicit-view-function syntax `(->` *pat*`)` to be a nice compromise between the two alternatives.  


#### Nesting



Our proposal has the *nesting* feature: view patterns nest inside other patterns, and other patterns nest inside them. Nesting is perhaps the biggest practical difference between view patterns and pattern guards.


#### Integration with type classes



Our proposal *integrates with type classes*: an single view function can decompose multiple different data types, and the type class constraints are propagated to the user of the view.  


## Related work


#### [
Wadler's original paper (POPL 1987)](http://homepages.inf.ed.ac.uk/wadler/papers/view/view.ps)



Wadler's paper was the first concrete proposal.  It proposed a top-level view
declaration, together with functions *in both directions* between the view type
and the original type, which are required to be mutually inverse.  
This allows view constructors to be used in expressions
as well as patterns, which seems cool. Unfortunately this dual role proved
problematic for equational reasoning, and every subsequent proposal restricted
view constructors to appear in patterns only.


#### [ Burton et al views (1996)](http://haskell.org/development/views.html)



This proposal is substantially more complicated than the one above; in particular it
requires new form of top-level declaration for a view type. For example:


```wiki
  view Backwards a of [a] = [a] `Snoc` a | Nil
     where
     backwards [] = Nil
     backwards (x:[]) = [] `Snoc` x
     backwards (x1:(xs `Snoc` xn)) = (x1:xs) `Snoc` xn
```


Furthermore, it is in some ways less expressive than the proposal here;
the (n+k) pattern, Erlang `bits` pattern, and `regexp` examples are not
definable, because all rely on the value input feature.



I think this proposal is substantially the same as "Pattern matching and
abstract data types", Burton and Cameron, JFP 3(2), Apr 1993.


#### [
Okasaki: views in Standard ML](http://citeseer.ist.psu.edu/okasaki98view.html)



Okasaki's design is very similar to Burton et al's, apart from differences due
to the different host language.  Again, the value input feature is not supported.


#### [ Erwig: active patterns](http://citeseer.ist.psu.edu/erwig96active.html)



Erwig's proposal for active patterns renders the Set example like this:


```wiki
data Set a = Empty | Add a (Set a)

pat Add' x _ =
  Add y s => if x==y then Add y s
             else let Add' x t = s
                  in Add x (Add y t)

delete x (Add' x s) = s
delete x s          = s
```


This requires a new top-leven declaration form `pat`; and I don't think it's nearly 
as easy to understand as the current proposal.  Notably, in the first equation for
`delete` it's hard to see that the second `x` is a bound occurrence; this somehow
follows from the `pat` declaration.



Still the proposal does support the value input feature.


#### [
Palao et al: active destructors (ICFP'96)](http://portal.acm.org/citation.cfm?id=232641&coll=portal&dl=ACM)



Active Destructors (ADs) are defined by a new form of top-level declaration.  



Where we'd write


```wiki
   sing :: [a] -> a option
   sing [x] = x 
```


The equivalent active destructor would be


```wiki
  Sing x match [x]
```


Here **match** is the keyword, and `Sing` is the AD.  ADs are quite like view patterns:
the can do computation, and can fail to match.  But they are definitely not normal 
Haskell functions, and need their own form of top-level declaration.  They even have
a special form of type to describe them.



The value-input feature is supported, but only via a sort of mode declaration (indicated by a down-arrow) on the new form of type.



They also introduce a combining form for ADs, to make a kind of and-pattern.  For
example, suppose we had


```wiki
  Head x match (x:_)
  Tail x match (_:xs)

  f :: [a] -> [a]
  f ((Head x)@(Tail ys)) = x:x:ys
```


Here `(Head x)@(Tail ys)` is a pattern that matches *both* `(Head x)` and `(Tail ys)` against the argument, binding `x` and `ys` respectively.  We can model that with view patterns:


```wiki
  headV (x:xs) = Just x
  headV []     = Nothing

  tailV (x:xs) = Just xs
  tailV []     = Nothing

  f (both -> (headV => x, tailV => ys)) = x:x:ys
```


An alternative to duplicating the value is to compose the functions:


```wiki
  (@) :: (a -> Maybe b) -> (a -> Maybe c) -> a -> Maybe (b,c)
  (f @ g) x = do { b <- f x; c <- g x; return (b,c) }

  f :: [a] -> [a]
  f (headV @ tailV -> (x,ys)) = x:x:ys
```


This is a little clumsier: the "`@`" combines functions, with a kind of positional binding; the pattern `(x,ys)` is separated from the combiner so that it's less clear that `headV` binds `x` and `tailV` binds `y`.


#### [
Erwig/Peyton Jones: transformational patterns](http://citeseer.ist.psu.edu/erwig00pattern.html)



This paper describes pattern guards, but it also introduces **transformational patterns**.  (Although
it is joint-authored, the transformational-pattern idea is Martin's.)  Transformational patterns
are very close to what we propose here.  In particular, view functions are ordinary Haskell functions,
so that the only changes are to patterns themselves.



There are two main differences (apart from syntax). 
First, transformational patterns didn't have the value input feature, althought it'd be easy 
to add (indeed that's what we've done). Second, transformational patterns as described by
Erwig do no stripping of the `Maybe` (see "Possible extension 2" above).


#### [
F\# Active Patterns](http://blogs.msdn.com/dsyme/archive/2006/08/16/ActivePatterns.aspx)



Simon started this design discussion after talking to Don Syme about F\#'s **active patterns**, which serve a very similar purpose. These combine both “total” discrimination (views) and “partial” discrimination (implicit maybe) into one mechanism. It does this by embedding the names of discriminators in the names of matching functions, via “values with structured names”.  Sample uses include matching on .NET objects and XML.



Here is [
a full paper describing the design](http://blogs.msdn.com/dsyme/archive/2007/04/07/draft-paper-on-f-active-patterns.aspx), by Don Syme, Gregory Neverov, and James Margetson (April 2007).



The feature is implemented in F\# 1.9. Some code snippets are below.


```wiki
    let (|Rect|) (x:complex) = (x.RealPart, x.ImaginaryPart)
    let (|Polar|) (x:complex) = (x.Magnitude , x.Phase)

    let mulViaRect c1 c2 = 
        match c1,c2 with 
        | Rect(ar,ai), Rect(br,bi) -> Complex.mkRect(ar*br - ai*bi, ai*br + bi*ar)

    let mulViaPolar c1 c2 = 
        match c1,c2 with 
        | Polar (r1,th1),Polar (r2,th2) -> Complex.mkPolar(r1*r2, th1+th2)

    let mulViaRect2  (Rect(ar,ai))   (Rect(br,bi))   = Complex.mkRect(ar*br - ai*bi, 
                                                                      ai*br + bi*ar)
    let mulViaPolar2 (Polar(r1,th1)) (Polar(r2,th2)) = Complex.mkPolar(r1*r2, th1+th2)
```


And for views:


```wiki
    open System
    
    let (|Named|Array|Ptr|Param|) (typ : System.Type) =
        if typ.IsGenericType        then Named(typ.GetGenericTypeDefinition(), 
                                               typ.GetGenericArguments())
        elif not typ.HasElementType then Named(typ, [| |])
        elif typ.IsArray            then Array(typ.GetElementType(), 
                                               typ.GetArrayRank())
        elif typ.IsByRef            then Ptr(true,typ.GetElementType())
        elif typ.IsPointer          then Ptr(false,typ.GetElementType())
        elif typ.IsGenericParameter then Param(typ.GenericParameterPosition, 
                                               typ.GetGenericParameterConstraints())
        else failwith "MSDN says this can't happen"

    let rec freeVarsAcc typ acc =
        match typ with
        | Named (con, args) -> Array.fold_right freeVarsAcc args acc
        | Array (arg, rank) -> freeVarsAcc arg acc
        | Ptr (_,arg)       -> freeVarsAcc arg acc
        | Param(pos,cxs)    -> Array.fold_right freeVarsAcc cxs (typ :: acc) 
```

#### [
Emir, Odersky, Williams: Matching objects with patterns](http://lambda-the-ultimate.org/node/1960)



Scala is an OO language with lots of functional features.  It has algebraic data types and
pattern matching.  It also has a form of view called **extractors**, which are
pretty similar to view patterns, albeit in OO clothing.  Notably, by packaging a constructor
and an extractor in a class, they can use the same class name in both expressions and terms, 
implicitly meaning "use the constructor in expressions, and use the extractor in patterns".



The paper does a comparative evaluation of various OO paradigms for matching, and 
concludes that case expressions and extractors work pretty well.


#### Pattern synonyms



[
Pattern synonyms](http://hackage.haskell.org/trac/haskell-prime/wiki/PatternSynonyms) 
are a requested Haskell Prime feature. John Reppy had the same idea years ago for Standard ML; see 
[
Abstract value constructors](http://people.cs.uchicago.edu/~jhr/papers/1992/tr-sml-const.pdf), 
Reppy & Aiken, TR 92-1290, Cornell, June 1992.



The one way in which pattern synonyms are better than view patterns is thatn they define by-construction bi-directional maps.  Example


```wiki
  data Term = Var String | Term String [Term]
  
  -- 'const' introduces a pattern synonym
  const Plus a b = Term "+" [a,b]

  f :: Term -> Term
  f (Plus a b) = Plus (f a) (f b)
  f ... = ...
```


With pattern views, we'd have to write two functions for the "plus" view:


```wiki
  plus :: Term -> Term -> Term
  plus a b = Term "+" [a,b]

  isPlus :: Term -> Maybe2 Term Term
  isPlus (Term "+" [a,b]) = Just2 a b
  isPlus other		  = Nothing

  f :: Term -> Term
  f (isPlus -> a b) = plus (f a) (f b)
```


But perhaps that is not so bad.  Pattern synonyms also require a new form of top level declaration; and are much more limited than view patterns (by design they cannot do computation).


#### [
Tullsen: First Class Patterns](http://citeseer.ist.psu.edu/tullsen00first.html)



First Class Patterns is an approach that attempts to
add the minimum of syntax to the language which---in combination with
pattern combinators written within the language---can achieve everything
and more that Haskell patterns can do.  They have the value-input feature.



The advantages are  1) They are simpler than Haskell's patterns;  2) Patterns are first class.
3) The binding mechanism (the pattern binder) is orthogonal to the the pattern combinators:
the hope is that one can stop changing the syntax/semantics of patterns and concentrate on writing the
combinators (as Haskell functions).



The disadvantages are as follows: 1) An extra syntactic construct that binds variables, the pattern binder, is required.
2) Even with pattern binders, simple patterns look clunkier than Haskell's patterns.
3) No attempt is made to check for exhaustiveness of patterns.
4) No attempt is made to integrate with Haskell's patterns, the idea is a proposal for an alternative when one needs more than simple patterns.



The singleton example above would like this:


```wiki
  f = {%sing n} .-> n+1
                |>> 0

  g =  {%sing True}  .-> 0
    .| {%sing False} .-> 1
                     |>> 2  
```

#### First class abstractions



Several proposals suggest first class *abstractions* rather that first-class *patterns*.  By a "first class abstraction" I mean a value of type
(*something* `->` *something*)
with a syntax something like
(`\` *pattern* `->` *result*).
The abstraction includes both the pattern and the result.  In contrast, view patterns tackle only the syntax of patterns; the pattern of a first-class abstraction.  



Here are the ones I know of


- [
  Claus Reinke's lambda-match proposal](http://hackage.haskell.org/trac/haskell-prime/ticket/114)
- [
  Matthias Blume: Extensible programming with first-class cases](http://ttic.uchicago.edu/~blume/pub-cat.html) (ICFP06)


All these proposals are more or less orthogonal to this one. For example, Reinke
proposes a compositional syntax for lambda abstractions 
`(\p -> e)` where pattern matching failure on `p` can be caught and composed
with a second abstraction. Thus


```wiki
   (| Just x -> x+1 ) +++ (| Nothing -> 0 )
```


combines two abstractions, with failure from the first falling through to the seoond.  



None of these proposals say
anything about the patterns themselves, which in turn is all this
proposal deals with.  Hence orthgonal.


#### Barry Jay: First class patterns



A yet more ambitious scheme is to treat patterns themselves as first class, even though they have free (binding) variables.  This is the approach that Barry Jay has taken in his very interesting project on the *pattern calculus*.  His [
home page](http://www-staff.it.uts.edu.au/~cbj) has more info.


#### Uses of Views



The observations from [
Okasaki: Breadth-First Numbering - Lessons ... ](http://citeseer.ist.psu.edu/356396.html) suggest that not having abstract pattern matching (for sequences) can indeed have great impact on the abstractions functional programmers can think of.



The abstractional power views offer can also be put to good use when designing data structures, as the following papers show


- [
  R.Hinze: A fresh look on binary search trees](http://www.informatik.uni-bonn.de/~ralf/publications/SearchTree.ps.gz).
- [
  R.Hinze:  A Simple Implementation Technique for Priority Search Queues](http://www.informatik.uni-bonn.de/~ralf/publications/ICFP01.pdf)
- The the key idea of [
  M.Erwig: Inductive Graphs and Functional Graph Algorithms](http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.ps.gz) is to introduce a suitable view of graphs. This way, graphs can be liberated from their notoriously imperative touch.
