# Design discussion for improvements to GHCi's `:type` command



This page, created on April 25, 2016, is to discuss the need (if any) to improve GHCi's `:type` command to deal with different use cases and desired output.



Relevant tickets: [\#10963](http://gitlabghc.nibbler/ghc/ghc/issues/10963) (about defaults/specializations), [\#11376](http://gitlabghc.nibbler/ghc/ghc/issues/11376) (about default behavior of `:type`), and [\#11975](http://gitlabghc.nibbler/ghc/ghc/issues/11975) (about printing specified variables).


## Problem 1: variables available for visible type application



As discussed at length in [\#11376](http://gitlabghc.nibbler/ghc/ghc/issues/11376), `:type` instantiates the inferred type before generalizing and reporting to the user. Here is the example: Given `  bar :: forall a b. Show a => a -> b -> a`, what should `:type bar @Int` show (with `-fprint-explicit-foralls`)?


1. `forall b. Show Int => Int -> b -> Int`
1. `forall b. Int -> b -> Int`
1. `forall {b}. Int -> b -> Int`
1. `Int -> b -> Int`


We choose (3), because (1) has an unsolved `Show Int`, (2) is quite hard to arrange for (and may not be fully specified), and (4) is ill-scoped. [\#11376](http://gitlabghc.nibbler/ghc/ghc/issues/11376) has much more discussion.



However, the choice of (3) means that there is no way to discover the specified type variables of a type, where "specified" here refers to the type variables available for visible type application. For example, the type `foldr` is parameterized by three type variables, `t :: * -> *`, `a :: *` and `b :: *`, *in that order*, and these variables are available for visible type application. However, with instantiation and generalization, there is no guarantee that the variables' ordering will be maintained, and GHC reports the variables as unavailable for visible type application.


## Problem 2: types can be too general



With the addition of generalization over `Foldable` and `Traversable`, many common functions have very general types. For example:


```wiki
> :t mapM
mapM
  :: forall {t :: * -> *} {m :: * -> *} {a} {b}.
     (Traversable t, Monad m) =>
     (a -> m b) -> t a -> m (t b)
> :t length
length :: forall {t :: * -> *} {a}. Foldable t => t a -> Int
```


Even for me (Richard E.), a somewhat experienced functional programmer, I find these types hard to think about without instantiating. 


## Design questions



Running example: `foo :: forall a m t. (Show a, Monad m, Foldable t) => t a -> m a`. We assume `-fprint-explicit-foralls` throughout.


1. What should the default behavior of `:type` be? Possible answers:

  1. As it is now, deeply instantiating the type and then generalizing.

    - `:t foo @Int` produces `forall {t :: * -> *} {m :: * -> *}. (Monad m, Foldable t) => t Int -> m Int`.
    - `:t foo @Int []` produces `forall {m :: * -> *}. Monad m => m Int`
    - Pros:

      - The type reported is the type that would be inferred for a variable assigned to the given expression.
      - No unsolved constraints.
      - Fully general.
    - Cons:

      - Loses information about specified type variables. (Note that the variables are listed in braces and out of order in the first example output.)
  1. No deep instantiation of the type, only generalize.

    - `:t foo @Int` produces `forall (m :: * -> *) (t :: * -> *). (Show Int, Monad m, Foldable t) => t Int -> m Int`.
    - `:t foo @Int []` produces `forall {m :: * -> *}. Monad m => m Int`.
    - Pros:

      - The specified variables are correct.
      - Any unspecialized variable (such as `m` in the second example) is still given with its kind, but we can see that, e.g., `foo @Int [] @IO` would not work.
    - Cons:

      - If you've used visible type application (but only if you've used visible type application), there may be unsolved constraints, like `Show Int`.
      - The type shown is not what would be assigned to a variable. This may be surprising to users.
  1. No instantiation or generalization.

    - `:t foo @Int` produces `forall (m :: * -> *) (t :: * -> *). (Show Int, Monad m, Foldable t) => t Int -> m Int`
    - `:t foo @Int []` produces `m Int`.
    - Pros:

      - Shows precisely the type of the expression.
    - Cons:

      - No kind for `m` provided in the second example.
      - No `Monad m` constraint provided in the second example.
  1. Specialize the type, as per Design Question 2, below.

    - Pros:

      - Beginners get easy-to-read types.
    - Cons:

      - The type returned from `:type` is not fully general.

1. If we provide a way for GHC to specialize the types, how should we do so? Possible answers:

  1. Apply the monomorphism restriction.

    - `length`'s type is reported as `[a] -> Int`.
    - `mapM` leads to a type error, because `Monad m` cannot be defaulted.
    - Pros:

      - Dead simple to implement.
      - Brief to specify. (Though users may not fully understand the specification.)
      - Customizable via `default` declarations.
      - Only 1 specialization is reported.
    - Cons:

      - Rather limited in applicability only to those types which contain all defaultable constraints.
      - Only 1 specialization is reported.
  1. Default all so-called "interactive" classes, where the interactive classes are: `Num`, `Show`, `Eq`, `Ord`, `Foldable`, and `Traversable`. The default list of default types in GHCi is `()`, `[]`, `Integer`, `Double`.

    - `length`'s type is reported as `[a] -> Int`.
    - `mapM`'s type is reported as `Monad m => (a -> m b) -> [a] -> m [b]`.
    - Pros:

      - Easy enough to specify and implement.
      - Customizable.
      - Only 1 specialization is reported.
    - Cons:

      - Only 1 specialization is reported.
  1. Provide multiple specializations of the types, in a way yet to be specified.

    - `length`'s type might be reported as `[a] -> Int` or `Maybe a -> Int`.
    - `mapM`'s type might be reported as `(a -> [b]) -> [a] -> [[b]]` or `(a -> Maybe b) -> [a] -> Maybe [b]` or `(a -> IO b) -> [a] -> IO [b]` or `(a -> IO b) -> Maybe a -> IO (Maybe b)` or ...
    - Pros:

      - Lots of examples make it easier for the user to generalize internally and understand what is going on.
    - Cons:

      - Verbose.
      - Potentially a great many types to be reported.
      - Not clear how to specify this.

1. How do we provide the non-default behavior? Possible answers:

  1. Have `:type` provide multiple types.

    - Pros:

      - Easy for users to be aware of this.
      - Easy to remember what command to use.
    - Cons:

      - Verbose output to a common operation.
      - Could possibly break tools that read the output of `:type`.
  1. Use `:info` to provide the extra information.

    - Pros:

      - No new commands.
    - Cons:

      - Requires a redesign of `:info`, as `:info` does not print the full type of, e.g., data constructors, class methods, record selectors, etc.
      - Makes `:info` even more verbose.
      - Unavailable for expressions that aren't a single identifier.
  1. Have `:type` behave one way for single identifiers and a different way for more complex expressions.

    - Pros:

      - No new commands.
      - Many uses of `:type` are (probably, I don't have data) for single identifiers.
    - Cons:

      - No way to get the extra information for compound expressions.
      - The different behaviors might confuse users.
  1. Have multiple commands, each with different behavior.

    - Pros:

      - Users can always say precisely what they mean.
    - Cons:

      - New commands to be aware of.
      - General bloat of GHCi interface.

## The current state of affairs



Choice (A) about the behavior of `:type`, and there is no way to default or access the other possibly desirable behaviors.


## Concrete proposed solutions



In [
Phab:D2136](https://phabricator.haskell.org/D2136), Richard E has written a patch that implements (1A, 2B, 3D).



Simon has suggested ([comment:5:ticket:10963](http://gitlabghc.nibbler/ghc/ghc/issues/10963)) that three `:type` commands is two too many, and suggests (1D, 3B), with no particular vote on issue 2. Simon also suggests that Problem 1 is a non-problem.



User \@takenobu has suggested, most recently in [comment:7:ticket:10963](http://gitlabghc.nibbler/ghc/ghc/issues/10963), that we look into 2C.



On [
Phab:2136](https://phabricator.haskell.org/2136), Herbert has reminded us to make sure that any changes do not invalidate common existing workflows, like typing `:ty<TAB> foo` into GHCi.


## Summary of feedback about this issue



I (Richard) collected feedback on May 2, 2016, from [
ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2016-April/011933.html), [
haskell-cafe 1](https://mail.haskell.org/pipermail/haskell-cafe/2016-April/123792.html), [
haskell-cafe 2](https://mail.haskell.org/pipermail/haskell-cafe/2016-May/123843.html), on the ticket, and via [
reddit](https://www.reddit.com/r/haskell/comments/4gvim4/design_input_opportunity_specializing_types_in/). Here is a summary of the feedback. I have not attached numbers of votes, etc., because the feedback has tended to be unstructured. In my opinion, attaching a number of votes would imply more significant figures than we really have.


- 1A is the most popular option among issue 1.
- A minority of people suggested adding specializations right to `:type`, alongside the full type. More people seemed to counter this than support it, however.
- Use flag to control `:type`. This idea was seconded a few times.

  - Also, specific suggestion for "newbie mode"
- For 3D, we could use a `+` suffix to indicate that we want more information.
- On issue 2, the preponderance of respondents want more than one specialization. It was suggested that we specialize w.r.t. all instances that are in scope. No one suggested a concrete algorithm for this, though, especially in the presence of multiple specializable constraints.
- We should have a `:doc` command, like Idris. This got support both via email and on reddit.
- Specializations could be included by the library writer, via a new pragma.
- Change the behavior of `:set +t`.
- A reddit post suggesting (1A, 2C, 3D) got the most upvotes, suggesting `:examples` as the name of the command to get the specializations. That post also suggests instantiating non-constrained type variables.


My (Richard's) reaction: I think the clearest information I can glean from all of this is that (1A, 2C, 3D) is a good way forward. 1A was a pretty consistent vote-getter (though by no means unanimous). Many also supported seeing multiple specializations, in 2C, suggesting that users can generalize a pattern more easily if they see multiple types. I do wish someone had a good idea of how to specify this. And many voted for 3D in a variety of ways, if we include ideas about `:doc`, or the `+` suffix, etc. as votes for 3D.



Separately, there is a real groundswell of support for `:doc`, but I consider that to be beyond the scope of this proposal. A future `:doc` may subsume what we're seeing here.



Concrete proposal going forward:


1. Seek a bit more input about the names of the new commands.
1. Implement 2B using the new command name.
1. Post 2C as a feature request.


2C, multiple specializations, is actually a generalization of 2B. Given the lack of specification of 2C, 2B does exactly 2C, if we choose to print only 1 specialization. Furthermore, my time budget on this point is more than exhausted, and this is as easy to extend later as it is today. It would be wonderful for someone to come along and implement 2C. In the meantime, I think having 2B is better than nothing (and no comment posted would suggest disagreement on this point).


