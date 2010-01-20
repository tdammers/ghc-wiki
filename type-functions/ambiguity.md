# Ambiguity



The question of *ambiguity* in Haskell is a tricky one.  This wiki page is a summary of thoughts and definitions, in the hope of gaining clarity.  I'm using a wiki because it's easy to edit, and many people can contribute, even though you can't typeset nice rules.  



\[Started Jan 2010.\]  **Please edit to improve.**


## Terminology



A type system is usually specified by


- A **specification**, in the form of some **declarative typing rules**.  These rules often involve "guessing types".  Here is a typical example, for variables:

  ```wiki
    (f : forall a1,..,an. C => tau) \in G
    theta = [t1/a1, ..., tn/an]        -- Substitution, guessing ti
    Q |= theta( C )
    ------------------------- (VAR)
    Q, G |- f :: theta(tau)
  ```

  The preconditions say that f is in the environment G with a suitable polymorphic type.
  We "guess" types t1..tn, and use them to instantiate f's polymorphic type variables 
  a1..an, via a substitution `theta`.  Under this substitution f's instantiated constraints
  `theta(C)` must be *satisfiable* (using `|=`) from the ambient constraints Q.

>
>
> The point is that we "guess" the ai.
>
>

- An **inference algorithm**, often also presented using similar-looking rules, but in a form that can be read as an algorithm with no "guessing".  Typically 

  - The "guessing" is replaced by generating fresh unification variables. 
  - The algorithm carries an ever-growing substitution that instantiates these unification variables.


We want the inference algorithm to be 


- **sound** (if it succeeds, then the program is well typed according to the specification) and 
- **complete** (if the program is well typed according to the specification, the algorithm succeeds).

## Coherence



Suppose we have (I conflate classes `Read` and `Show` into one class `Text` for brevity):


```wiki
class Text a where
  read :: String -> a
  show :: a -> String

x :: String
x = show (read "3.7")
```


The trouble is that there is a constraint `(Text t)`, where `t` is a type variable that is otherwise unconstrained.  Moreover, the type that we choose for `t` affects the semantics of the program.  For example, if we chose `t = Int` then we might get `x = "3"`, but if we choose `t = Float` we might get `x = "3.7"`.  This is bad: we want our type system to be **coherent** in the sense that every well-typed program has but a single value.



In practice, the Haskell Report, and every Haskell implementation, rejects such a program saying something like


```wiki
Cannot deduce (Text t) from ()
```


In *algorithmic* terms this is very natural: we indeed have a constraint `(Text t)` for some unification variable `t`, and no way to solve it, except by searching for possible instantiations of `t`. So we simply refrain from trying such a search.



But in terms of the type system *specification* it is harder.  We can simply guess `a=Int` when we instantiate `read` and `show` and lo, the program is well typed.  But we do not *want* this program to be well-typed.



**Problem 1**: How can we write the specification so as to reject programs such as that above. 


## Digression: open and closed world



Suppose there was precisely one instance for `Text`:


```wiki
instance Text Char where ...
```


Then you might argue that there is only one way for the algorithm to succeed, namely by instantiating `read` and `show` at `Char`.



It's pretty clear that this is a Bad Idea:


- In general it is hard to say whether there is a unique substitution that would make a collection of constraints satisfiable.
- If you add just one more instance, the program would become untypeable, which seems fragile.


To avoid this nasty corner we use the **open-world assumption**; that is, we assume that the programmer may add new instances at any time, and that doing so should not make a well-typed program become ill-typed.  (We ignore overlapping instances for now.


## Early detection of errors



Suppose, with the above class `Text` I write


```wiki
f x = show (read x)
```


What type should we infer for `f`?  Well, a simple-minded inference algorithm works as follows for a let-definition `f=e`: typecheck `e`, collecting whatever constraints it generates.  Now simply abstract over them.  



In this example we'd get


```wiki
f :: (Text a) => String -> String
```


And indeed this is a perfectly fine type for 


