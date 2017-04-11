# Arrow notation



Arrow notation ([current documentation](http://www.haskell.org/ghc/docs/latest/html/users_guide/arrow-notation.html)) has been in GHC for many years, but it lacks a maintainer.  Even Simon PJ is very hazy about how it all works, and even toys with the idea of taking it out altogether.



Apart from the tickets below, there are a number of things that need doing


- There no single place that describes all the moving parts of the arrow implementation, including both the typechecking and desugaring rules -- and it's very hard to work it out from the code.  Simply writing out the typechecking and desugaring rules, with some commentary, would be really helpful.  There is a tiny start in `Note [Arrow overview]` in `TcArrows`.

- [\#13547](http://gitlabghc.nibbler/ghc/ghc/issues/13547): work out if existential and GADT pattern matches are allowed. If so, fix the desugarer; if not, reject them with a decent error message, not a Core Lint crash.

- See the discussion on this [
  Dec 16 ghc-devs thread](https://mail.haskell.org/pipermail/ghc-devs/2016-December/013317.html).  It started with a desire to support rebindable syntax.

- Lower down this page are a couple of proposed changes to the notation enabled with `-XArrows`.  I'm not sure of their status.

## Tickets



Use Keyword = `Arrows` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th>[\#344](http://gitlabghc.nibbler/ghc/ghc/issues/344)</th>
<td>arrow notation: incorrect scope of existential dictionaries</td></tr>
<tr><th>[\#5267](http://gitlabghc.nibbler/ghc/ghc/issues/5267)</th>
<td>Missing type checks for arrow command combinators</td></tr>
<tr><th>[\#5777](http://gitlabghc.nibbler/ghc/ghc/issues/5777)</th>
<td>core lint error with arrow notation and GADTs</td></tr>
<tr><th>[\#7828](http://gitlabghc.nibbler/ghc/ghc/issues/7828)</th>
<td>RebindableSyntax and Arrow</td></tr>
<tr><th>[\#9985](http://gitlabghc.nibbler/ghc/ghc/issues/9985)</th>
<td>GHC panic with ViewPatterns and GADTs in a proc pattern</td></tr>
<tr><th>[\#10582](http://gitlabghc.nibbler/ghc/ghc/issues/10582)</th>
<td>Tiny bug in lexer around lexing banana brackets</td></tr>
<tr><th>[\#13547](http://gitlabghc.nibbler/ghc/ghc/issues/13547)</th>
<td>Lint error in arrows program</td></tr>
<tr><th>[\#15175](http://gitlabghc.nibbler/ghc/ghc/issues/15175)</th>
<td>ghc: panic! (the 'impossible' happened)</td></tr></table>




**Closed Tickets:**

<table><tr><th>[\#2722](http://gitlabghc.nibbler/ghc/ghc/issues/2722)</th>
<td>\<\<loop\> when compiling with -O option with ghc-6.10.0.20081019</td></tr>
<tr><th>[\#3822](http://gitlabghc.nibbler/ghc/ghc/issues/3822)</th>
<td>guards in arrow notation (Arrows extension) case statement cause compiler panic</td></tr>
<tr><th>[\#5022](http://gitlabghc.nibbler/ghc/ghc/issues/5022)</th>
<td>Core Lint error from polymorphic definitions inside arrow rec</td></tr>
<tr><th>[\#5333](http://gitlabghc.nibbler/ghc/ghc/issues/5333)</th>
<td>Arrow command combinators and infixr cause the desugarer to fail</td></tr>
<tr><th>[\#5609](http://gitlabghc.nibbler/ghc/ghc/issues/5609)</th>
<td>Type checking arrow notation in the presence of deferred constraints</td></tr>
<tr><th>[\#7071](http://gitlabghc.nibbler/ghc/ghc/issues/7071)</th>
<td>Refactoring arrows</td></tr>
<tr><th>[\#8505](http://gitlabghc.nibbler/ghc/ghc/issues/8505)</th>
<td>Arrows example error</td></tr></table>



##
Changing the types of arrow operators (implemented in March 2013, for GHC 7.8)



Currently, the type of each argument of an operator (and its result) is required to have the form


```wiki
a (...(e,t1), ... tn) t
```


where `e` is a polymorphic type variable shared by all these types, but the arrow types `a` can vary.  The *User's Guide* has these examples:


```wiki
ArrowPlus a => (<+>) :: a e c -> a e c -> a e c
untilA :: ArrowChoice a => a e () -> a e Bool -> a e ()
handleA :: ... => a e c -> a (e,Ex) c -> a e c
bracketA :: ... => a e b -> a (e,b) c -> a (e,c) d -> a e d
runReader :: ... => a e c -> a' (e,State) c
runState :: ... => a e c -> a' (e,State) (c,State)
bind :: Arrow a => a e b -> a (e,b) c -> a e c
bind_ :: Arrow a => a e b -> a e c -> a e c
cond :: ArrowChoice a => a e b -> a e b -> a (e,Bool) b
```


The problem is that to work out how many `ti`s there are, the type checker needs to be able to determine whether a type is a pair type or this Skolem variable `e`, and this can't be done with GHC's new constraint-based type system.



The plan is to re-arrange the shapes of the argument and result types to


```wiki
a (e, (t1, ... (tn, ())...)) t
```


For the above examples, the new types will be


```wiki
ArrowPlus a => (<+>) :: a (e,()) c -> a (e,()) c -> a (e,()) c
untilA :: ArrowChoice a => a (e,()) () -> a (e,()) Bool -> a (e,()) ()
handleA :: ... => a (e,()) c -> a (e,(Ex,())) c -> a (e,()) c
bracketA :: ... => a (e,()) b -> a (e,(b,())) c -> a (e,(c,())) d -> a (e,()) d
runReader :: ... => a (e,()) c -> a' (e,(State,())) c
runState :: ... => a (e,()) c -> a' (e,(State,())) (c,State)
bind :: Arrow a => a (e,()) b -> a (e,(b,())) c -> a (e,()) c
bind_ :: Arrow a => a (e,()) b -> a (e,()) c -> a (e,()) c
cond :: ArrowChoice a => a (e,()) b -> a (e,()) b -> a (e,(Bool,())) b
```


Now in the cases of `(<+>)`, `untilA` and `bind`, the new types are specializations of the old, so those operators will still work, but the others will need to be re-defined with the new types.


## Generalizing the types of commands



The translation of many of the different varieties of command does not require the full `Arrow` class, but rather just


```wiki
premap :: Arrow a => (b -> b') -> a b' c -> a b c
premap f g = arr f >>> g
```


So the proposal is to introduce a superclass of `Arrow` with just this:


```wiki
class PreArrow a where
    premap :: Arrow a => (b -> b') -> a b' c -> a b c
```


and require that class instead of `Arrow` for the types of those constructs. ([
libraries proposal](http://thread.gmane.org/gmane.comp.lang.haskell.libraries/17609))



This shouldn't break any code that uses arrows, but will require rewriting of instances of `Arrow`.


