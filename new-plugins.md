
See also the section on [Compiler Plugins in GHC's User's Guide](http://www.haskell.org/ghc/docs/7.2.1/html/users_guide/compiler-plugins.html).


# New Plugins work



Plugins are a new feature in GHC 7.2.1 that allows users to write compiler passes (for things like optimizations) over GHC's internal intermediate language, [Core](commentary/compiler/core-syn-type).



GHC understands the `-fplugin` and `-fplugin-arg` options. You essentially install plugins for GHC by `cabal install`ing them, as they expose a module implementing an interface, and then calling GHC in the form of:


```wiki
$ ghc -fplugin=Some.Plugin.Module -fplugin-opt=Some.Plugin.Module:no-fizzbuzz a.hs
```


Warning: this can fail with non obvious error messages if in the same directory as the Some.Plugin.Module source; seems GHC tries to use the source instead of the installed package.



`Some.Plugin.Module` should export a symbol named 'plugin' - see the following repositories for examples that do Common Subexpression Elimination, turn Haskell into a strict language, and implement a loop unroller:



[
https://github.com/thoughtpolice/cse-ghc-plugin](https://github.com/thoughtpolice/cse-ghc-plugin)



[
https://github.com/thoughtpolice/strict-ghc-plugin](https://github.com/thoughtpolice/strict-ghc-plugin)



[
https://github.com/thoughtpolice/unroll-ghc-plugin](https://github.com/thoughtpolice/unroll-ghc-plugin)


### Basic overview of the plugins API for Core



Modules can be loaded by GHC as compiler plugins by exposing a declaration called 'plugin' of type [Plugin](http://www.haskell.org/ghc/docs/7.4.2/html/libraries/ghc-7.4.2/CoreMonad.html#t:Plugin), which is an ADT containing a function that installs a pass into the Core pipeline.


```wiki
module Some.Plugin.Module (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}


-- type CommandLineOption = String

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options passes = do
  ...


```


We can think of `CoreToDo` as being a type synonym for `(Core -> Core)` - that is, the `install` function just inserts its own `CoreToDo` into the list of compiler passes. For example, the CSE pass actually couples a simplification pass, followed by CSE itself into the front of the compilation pipeline:


```wiki
module CSE.Plugin where

...

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ xs = return $ CoreDoPasses [defaultGentleSimplToDo, cse] : xs
  where cse = CoreDoPluginPass "Common Subexpression Elimination" (bindsOnlyPass cseProgram)

cseProgram :: [CoreBind] -> CoreM [CoreBind]
cseProgram binds = do
  ...
```


More specifically, a `CoreToDo` describes some sort of particular pass over a Core program that can be invoked as many times as you like. For reference, `defaultGentlSimplToDo` is constructed using `CoreDoSimplify`. In this case, `cse_pass` is constructed using `CoreDoPluginsPass`, which takes a name and a function of type `ModGuts -> CoreM ModGuts` - `ModGuts` is a type that represents the 1 module GHC is compiling at any time. You normally want to manipulate the field `mg_binds` of a `ModGuts`, which contains all the top-level bindings for the module.



`bindsOnlyPass` is a function that merely lifts a function over binders to a function over [ModGuts](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-7.2.2/HscTypes.html#t:ModGuts). It's the simple case where nothing else from the `ModGuts` is needed.



More details on plugins for end-users can be found in the users guide.


# The Future


## Plugins for Cmm



Aside from manipulating the core language, we would also like to manipulate the C-- representation GHC generates for modules too.



TODO fixme


### Rough API possibilities



TODO fixme


