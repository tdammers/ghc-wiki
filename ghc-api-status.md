


# GHC API Improvement Status



This Wiki page shall serve as a central place to collect all issues and ideas related to the GHC API.  If you feel that something is missing from this page, please add a comment to the comment section below.


- [ Source code](http://code.haskell.org/~nominolo/src/ghc/)
- [
  Haddock docs](http://code.haskell.org/~nominolo/html/ghc/) (may occasionally be out of date with the source)

---


# Current GHC API design



There were two main issues which were large cross-cutting concerns and, unfortunately, took up most of my time: (a) consistent error handling and (b) explicit passing around of a `Session`.



The latter issue is rather straightforward to fix by introducing a monad, however, functions expecting `IO` callbacks complicated things a little bit.  Consistent error handling is trickier.  I decided to use extensible exceptions which required some `#ifdef`s to properly bootstrap and some special `handle*` type functions to portably handle certain exceptions (akin to `handleGhcException`.)  But first, an overview:



A `GhcMonad` is a class, and a default datatype which implements it is the `Ghc` monad.  Most interface functions will be of type `GhcMonad m => ... -> m a`.  This should make it easier to use these functions in custom monads which most non-trivial clients will likely need.  Very simple clients can just reuse the `Ghc` monad.  The `GhcMonad` class is defined as follows:


```wiki
class (MonadIO m, WarnLogMonad m, ExceptionMonad m) => GhcMonad m where
  getSession :: m HscEnv
  setSession :: HscEnv -> m ()
```


The three required classes ensure that: we can use `liftIO` to call `IO` actions inside the GHC monad, accumulate warnings, and handle (extensible) exceptions in that monad.  The three classes are defined as follows:


```wiki
class Monad m => MonadIO m where   -- util/MonadUtils.hs
  liftIO :: IO a -> m a

type WarningMessages = Bag WarnMsg
class Monad m => WarnLogMonad m where -- main/HscTypes.lhs
  setWarnings  :: WarningMessages -> m ()
  getWarnings :: m WarningMessages
  -- An alternative interface with 'addWarnings' and 'clearWarnings' 
  -- instead of 'setWarnings' may be nicer if we just want to dump
  -- warnings somewhere and not accumulate (in which case 
  -- 'getWarnings' would always return the emptyBag)

class Monad m => ExceptionMonad m where -- utils/Exception.hs
  gcatch :: Exception e => m a -> (e -> m a) -> m a
  gbracket :: m a -> (a -> m b) -> (a -> m c) -> m c
  gfinally :: m a -> m b -> m a
  -- 'gfinally' and 'gbracket' may be implemented in terms of 
  -- 'gcatch' if we add 'gblock' and 'gunblock'.
  -- The version for GHC < 6.9 also contains 'gcatchDyn'.
```


There are two GHC-API-specific exceptions:


```wiki
data SourceError = SourceError ErrorMessages
data GhcApiError = GhcApiError SDoc

mkSrcErr :: ErrorMessages -> SourceError
srcErrorMessages :: SourceError -> ErrorMessages
mkApiErr :: SDoc -> GhcApiError
```


A source error corresponds to a problem with the compiled code and contains all accumulated error messages (but no warnings).  An API error is used to signal failure of an API call and replace many 'Maybe' results.  The choice wasn't always obvious.  In general API errors should be seldom, but catchable, i.e., they should be rare, but not entirely unexpected.  I guess, the choice which functions return which error needs some fine tuning.



The 'WarnLogMonad' does what it's name says.  It accumulate warnings, which can be queried with 'getWarnings' and 'clearWarnings'.  Deciding when to clear warnings is a bit delicate.  ATM, I provide a default function that should be invoked in case of a source error (i.e., compilation failure) which prints all errors and warnings and clears the accumulated warnings.


```wiki
printExceptionAndWarnings :: GhcMonad m => SourceError -> m ()
printExceptionAndWarnings err = do
    let errs = srcErrorMessages err
    warns <- getWarnings
    dflags <- getSessionDynFlags
    liftIO $ printErrorsAndWarnings dflags (warns, errs)
    clearWarnings
```


GHC internally uses `handleSourceError`, but this is only to avoid bootstrapping issues.  API clients can just use the standard extensible exception handling mechanism, but should use `gcatch`, `gtry`, etc. as these work in any monad with the proper instance.



To start a 'Ghc' session use:


```wiki
withGhc :: Maybe FilePath  -- path to GHC library
        -> Ghc a           -- ^ The action(s) to perform.
        -> IO a
```


The first parameter can be determined automatically with the ghc-path
package.  



TODO We miss a `GhcT` monad transformer and a init function for custom monads `initSession :: GhcMonad m => Maybe FilePath -> m a`.


## Callbacks



Most of GHC's internal IO callbacks have been changed to use the proper class, e.g., `(MonadIO m, ExceptionMonad m) => ... -> m a -> m a`.  However, this cannot be done for external functions with callbacks.  A particularly complicated case are the asynchronous (FFI-initiated) callbacks of the Readline package.  For this case I added two functions to reflect into and reify from the `IO` monad:


```wiki
-- | Reflect a computation in the 'Ghc' monad into the 'IO' monad.
--
-- You can use this to call functions returning an action in the 'Ghc' monad
-- inside a 'IO' action.  This is needed for some (too restrictive) callback
-- arguments of some library functions:
--
-- > libFunc :: String -> (Int -> IO a) -> IO a
-- > ghcFunc :: Int -> Ghc a
-- >
-- > ghcFuncUsingLibFunc :: String -> Ghc a -> Ghc a
-- > ghcFuncUsingLibFunc str =
-- >   reifyGhc $ \s ->
-- >     libFunc $ \i -> do
-- >       reflectGhc (ghcFunc i) s
-- 
reflectGhc :: Ghc a -> Session -> IO a
reflectGhc m = unGhc m

-- > Dual to 'reflectGhc'.  See its documentation.
reifyGhc :: (Session -> IO a) -> Ghc a
reifyGhc act = Ghc $ act
```

## Interface Changes



**Most (all?) GHC API clients will break**, since any function that uses a Session now has a different type.  However, these changes are usually rather straightforward.  The tricky parts are what to do with error messages.  So far, messages could be intercepted by overriding `log_action`.  This was awkward, comparable to Unix calls returning that an error code and storing it in some global variable.  OTOH, the new method requires that errors are explicitly dealt with--otherwise, exceptions will propagate and quit the program.



`load` and `setTarget` work like before.  `checkModule`
has been split up into:


```wiki
parseModule :: ModuleName -> Ghc ParsedModule
typecheck :: ParsedModule -> Ghc TypecheckedModule
desugarModule :: TypecheckedModule -> Ghc DesugaredModule
loadModule :: TypecheckedMod m => m -> Ghc ()
```


`TypecheckedMod` has two instances `TypecheckedModule`, and
`DesugaredModule`.  A `DesugaredModule` is
very similar to a `CoreModule`, but the latter contains less
information, while the former also contains all the information from
the parser and typechecker.  Users should therefore be careful about
memory leaks and discard old intermediate results as soon as
possible.



Compile errors are thrown inside the `Ghc` monad and can be
caught with `onCompileError`.  `IO` errors can also be caught
using `ghcCatch`.  (If we get a type class that allows handling exceptions in any monad, we should add instances in a wrapper package outside of the `GHC` package.) 
Warnings are logged automatically and can be
retrieved using `getWarnings` or discarded using
`clearWarnings`.  An exception currently only contains the 
name of the phase it occurred in and the error message(s).


## Current Work



I am currently evaluating what information can be added to the output
of the various phases and how hard it would be to modify or create
intermediate types (e.g., typecheck a "hand-crafted" module).



Additionally, logging of errors and warnings is somewhat spread-out,
so I try to find the places where that happens and incorporate that
into the `Ghc` monad.


---


# Project Overview



Simon Marlow reported that the last time he tried to work on the GHC API, it turned into a huge refactoring task.  This could mean that it may take a while until bigger changes are visible and it is very likely that programs that currently use the GHC API will break.  Hopefully, though, it will lead to a more usable GHC API and maybe to a more hackable code base in general.  



I plan to work with a use case and try to make it as clear and simple as possible by adapting the GHC API.  The use case will be a program that generates HTML pages for Haskell programs which will give access to more and more semantic information.  E.g. it will start out with linking references of names to their definitions (possibly to import statement or link to other module), then give typing information, and later give types for sub-expressions.  A lot of this is already possible, but some may not.  



This use case will hopefully reflect many features an IDE or code analysis tools need.  Once this is working, we can move on to performing various transformations on the given code, or only a selected part of it.  This will should help figure out what information needs to be available to work with exposed, separate compilation phases.



There are also a few non-functional requirements:


- Using the API should be fairly self-explainatory and safe.  I.e., where necessary inputs are checked for invariants and there should be no implicit dependencies.  More concretely:

  - If several phases use the same AST, the AST will contain a type parameter which corresponds to the phases that have been performed with it.  Hence, if a function requires input of type `AST Phase3` then it is clear that the phases with types `AST Phase2 -> AST Phase3` and `AST Phase1 -> AST Phase2`  must be performed first.
  - GHC uses some evil hacks to simulate global variables but has some implicit assumptions when those are actually accessible.  If you call a function too early, ie., before a certain variable is initialised, GHC will die with a very unhelpful error message.  I hope to make as many of those dependencies explicit and encode those dependencies via the API (mostly via types).


   


## Trac Tickets Related to the GHC API


- [\#1467](http://gitlabghc.nibbler/ghc/ghc/issues/1467) - GHC API: expose separate compilation stages
- [\#1886](http://gitlabghc.nibbler/ghc/ghc/issues/1886) - GHC API should preserve and provide access to comments
- [\#654](http://gitlabghc.nibbler/ghc/ghc/issues/654) - Cabalization of the GHC library.

### Possibly Related


- [\#2159](http://gitlabghc.nibbler/ghc/ghc/issues/2159) - Use a more efficient representation than `[DynFlag]`
- [\#1631](http://gitlabghc.nibbler/ghc/ghc/issues/1631) - Make the External Package Table contain `ModDetails` not `ModIface`

## Related Documents and Discussions


- [
  The GSoC proposal](http://code.google.com/soc/2008/haskell/appinfo.html?csaid=4189AF2C8AE5E25A)
- Related GSoC project [
  Dynamically Loaded Plugins for the Glasgow Haskell Compiler](http://code.google.com/soc/2008/haskell/appinfo.html?csaid=69C2ABD1E013EE0C)
- Haskell-cafe question: [
  How to get the typechecked AST?](http://www.haskell.org/pipermail/haskell-cafe/2008-May/042616.html)
- [
  Porting HaRe to the GHC API](http://www.cs.kent.ac.uk/pubs/2005/2266/) Technical report describing some difficulties with the current API.
- [
  GHC as a Library](http://www.haskell.org/haskellwiki/GHC/As_a_library), the Haskell Wiki page
- [
  GHC Commentary on the GHC API](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/API) (may be outdated)
- [
  hint, an attempt to provide a simplified and stable subset of the GHC API](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hint-0.2.1)

## Various Ideas, Comments, Questions


- **Interface Stability** 

  - Is there a way to reduce version-skew for clients of the GHC API (currently, there is no stability guaranteed at all, so if you don't want to live with lots of \#ifdefs and breakage, you keep delaying your fantastic GHC API-base projects "until the dust settles") (Claus Reinke)
  - Would it be possible to separate the monolithic GHC API into two parts, one providing a simplified and stable subset/wrapper of commonly used functionality (as in Hint, hs-plugins, GHCi), the other providing all the rest, with no stability guarantees? (Claus Reinke)
- **Ast Traversals/Queries**, see: [GhcApiAstTraversals](ghc-api-ast-traversals)

  - Is it possible to use standalone deriving to get a **generic programming framework over the ASTs** without blowing up GHC's code for its own use (deriving Data, etc.)? (Claus Reinke)
  - David Waern mentions [
    deriving \`Data.Traversable\`](http://www.haskell.org/pipermail/haskell-cafe/2008-May/042961.html) for GHC's AST
- **GHC library directory in GHC API clients**

  - the need to hardcode the libdir is very fragile and troublesome (cf. the [
    Haddock version during build](http://www.haskell.org/pipermail/cvs-libraries/2008-June/008942.html) thread on `cvs-libraries` for just one example). would it be possible to integrate the path for the compiling GHC as a default, so that one only needs to specify an explicit path if the default doesn't work (compiling GHC moved/unavailable)? (Claus Reinke) 
  - this has been addressed by the new [
    ghc-paths](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/ghc-paths) package
- **binary incompatibility of GHC versions**

  - this also affects GHC Api clients, see [
    the Haddock 2 in GHC build issues](http://www.haskell.org/pipermail/cvs-ghc/2008-July/043568.html) for an infamous example
- From `compiler/main/GHC.hs`:

  ```wiki
  -- NOTE:
  --   - things that aren't in the output of the typechecker right now:
  --     - the export list
  --     - the imports
  --     - type signatures
  --     - type/data/newtype declarations
  --     - class declarations
  --     - instances
  --   - extra things in the typechecker's output:
  --     - default methods are turned into top-level decls.
  --     - dictionary bindings
  ```
- is there a way to make all the useful functionality of GHCi more easily available from the GHC API? ie, refactoring GHCi so that both it and other GHC API clients can use the same collection of useful functionality? (Claus Reinke)
- dynamic loading of Haskell code, ala hs-plugins, but without
  the version/platform issues (GHCi has to be able to do this
  anyway, but it would be nice to have the ugly bits hidden,
  such as `unsafeCast#`, or whatever it was). that might require
  a standard for typeReps, if I recall correctly.. (Claus Reinke)
- since the refactoring ideas below mention error handling: it appears that some GHC Api functions output error messages directly, without providing a means to handle/capture them in callers. I ran into one such instance a while ago ([\#1463](http://gitlabghc.nibbler/ghc/ghc/issues/1463), comments 8, 10, 11)
- ... more comments here ...


 


## Refactoring Ideas



There follow some notes about desirable refactorings, mainly around [compiler/main/HscMain.lhs](/trac/ghc/browser/ghc/compiler/main/HscMain.lhs).  These will be important when looking at how to modify the GHC API to expose the individual compilation stages.  At the moment, the compilation stages are all hidden behind the `HscMain` interface, which in turn is hidden behind the `DriverPipeline` module, which is used by the code in `GHC`.  In order to untangle things, we need to make some changes.  Not all of these are essential, and some of them might not even end up being good ideas at all; this is just a list of things we (Simon M & Simon PJ) noticed while doing a code walkthrough.


- We should separate the action of reading the old interface from checking its usages.  Currently the two
  are mixed up in `checkOldIface`.  (in the new story with fingerprints instead of versions, we also want
  to discard the old interface as soon as we decide to recompile, because it isn't necessary for calculating
  the new versions now).

- Perhaps `HsModule` should indicate whether the module is an `hs-boot` module or not.  That would reduce the
  number of arguments to `tcRnModule` by one.

- It would be nicer to return error messages from each phase directly rather than invoking the `log_action` callback
  in `DynFlags`.

- What is currently called `RenamedStuff` should be `HsModule Name`.  Hence, the `tcg_rn_` files in `TcGblEnv`
  can be merged into a single `tc_rn_module`.

- instead of passing a `Bool` to `tcRnModule` to ask for the renamed syntax, use a flag in `DynFlags`?

- `mi_globals` is in the wrong place: it is not part of the interface.  The reason it is where it is is because
  we need to keep it when a module is considered for compilation but not recompiled; when we generate the
  `ModDetails` from the `ModIface`.  ToDo: find a better place to put it.


 


## Open Issues


- Which operations should automatically call `clearWarnings`?
- Remove `compileToCore` (it says it's there for backwards compatibility only)
- What's the deal with `SuccessFlag`?  Shouldn't that be `Either SomeError ()`?
- Comment in HscTypes:

  ```wiki
  -- If the module does come from the home package, why do we look in the PIT as well?
  -- (a) In OneShot mode, even home-package modules accumulate in the PIT
  -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
  --     module is in the PIT, namely GHC.Prim when compiling the base package.
  -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
  -- of its own, but it doesn't seem worth the bother.
  ```

  We now have a separate `ghc-prim` package.  Should we eliminate (b) then?
