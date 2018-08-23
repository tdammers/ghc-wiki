# Unused imports



GHC has a series of bugs related to the "report unused imports"
flags, including [\#1148](http://gitlabghc.nibbler/ghc/ghc/issues/1148), [\#2267](http://gitlabghc.nibbler/ghc/ghc/issues/2267), [\#1074](http://gitlabghc.nibbler/ghc/ghc/issues/1074), [\#2436](http://gitlabghc.nibbler/ghc/ghc/issues/2436), [\#10117](http://gitlabghc.nibbler/ghc/ghc/issues/10117), [\#12067](http://gitlabghc.nibbler/ghc/ghc/issues/12067).



This page describes the current design (GHC 8.4 onwards, or thereabouts).



NB: GHC 8.4 and 8.6 had a bug (Trac [\#13064](http://gitlabghc.nibbler/ghc/ghc/issues/13064)) which meant that GHC did not implement the design advertised below.



See also 


- [Commentary/Compiler/RelaxedUnusedImports](commentary/compiler/relaxed-unused-imports) for a proposed, more sophisticated, design.

## The current story



Currently (GHC 6.10) we report three different things:


- warnUnusedModules: import M, where nothing is used from M
- warnUnusedImports: import M(f), where f is unused, and M doesn't fall under warnUnusedModules
- warnDuplicateImports: import M + import M(f), even when f is used complain about duplicate import of f

## Examples



The hard bit is to specify what the warning should do.
Consider these examples, where `Foo` exports `x` and `y`, and `FooPlus` 
re-exports all of `Foo`, plus `z`:


```wiki
  module X0 where            	   module X1 where	
    import Foo	             	     import Foo		
    import Foo( x )          	     import Foo( x )	
    bar = x	             	     bar = x+y		

  module X2 where            	   module X3 where	
    import Foo( x, y )	     	     import Foo( x, y )	
    import Foo( x )	     	     import Foo( x )	
    bar = x		     	     bar = x + y         
 
  module X4 where            	   module X5 where	      
    import Foo( x, y ) 	     	     import Foo( x, y ) as Bar 
    import Foo( x, y )	     	     import Foo( x, y )	      
    bar = x + y		     	     bar = x + Bar.y           
 
  module X6 where                  module X7 where	
    import Foo( x, y ) as Bar	     import FooPlus(x,y)	
    import Foo( x, y ) 		     import FooPlus(y,z)	
    bar = Foo.x + Bar.y		     import FooPlus(z,x)	
				     bar = (x,y,z)       

  module X8
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
	-- NB : Control.Monad.State re-exports all of Control.Monad
	--      so the first decl is actually redundant
```


Which import is redudant, in each case?



Also: we might warn if you import the same module more than once, and the
imports can be combined (ie they have the same 'qualified' and 'as'
attributes)


```wiki
  module Y1 where
    import Foo(x)
    import Foo(y)
    bar = (x,y)
```


Here both are used, but we might want to suggest combining them.


## Specfication



We can at least agree on this:


- If the warning suggests that an import can be omitted, and you omit it,
  the program should still compile.
- It's not worth trying to be too subtle.  The 90% case is very simple.


Say that an *import-item* is either an entire import-all decl (eg `import Foo`),
or a particular item in an import list (eg `import Foo( ..., x, ...)`).  
The general idea is that for each use of an imported name, we will attribute
that use to one (or possibly more) import-items. Then, any import items with no
uses attributed to them are unused, and are warned about.
More precisely:


1.  For every `RdrName` in the program text, find all the import-items that brought it     into scope.  The lookup mechanism on `RdrNames` already takes account of whether the `RdrName` was qualified, and which imports have the right qualification etc, so this step is very easy.

1. Choose one of these, the "chosen import-item", and mark it "used".  

1.  Now bleat about any import-items that are unused.  For a decl
  `import Foo(x,y)`, if both the `x` and `y` items are unused, it'd be better
  to bleant about the entire decl rather than the individual items.


The import-item choosing step 2 implies that there is a total order on 
import-items.  We say import-item A *dominates* import-item B if we choose
A over B.  Here is one possible dominance relationship:


>
>
> (a) `import Foo` dominates `import qualified Foo`, regardless of all-or-none.
> (b) `import Foo` dominates `import Foo(x)`.
> (c) Otherwise choose the textually first one.
>
>


Rationale for (a).  Consider


```wiki
   import qualified M  -- Import #1
   import M( x )       -- Import #2
   foo = M.x + x
```


The unqualified `x` can only come from `import #2`.  The qualified `M.x`
could come from either, but `bestImport` picks `import #2`, because it is
more likely to be useful in other imports, as indeed it is in this
case (see Trac [\#5211](http://gitlabghc.nibbler/ghc/ghc/issues/5211) for a concrete example).



Other notes:


- The algorithm chooses exactly one import-item in step 2.  It would
  also be sound to choose more than one if there was a tie, but then completely-duplicate
  imports might not be reported.

- Note that if we have an import item `import Foo (Bar(bar))`, then
  it's marked as used if either `Bar` or `bar` are used.  We could have yet finer
  resolution and report even unused sub-items.

- We should retain the special case of not warning about `import Foo ()`, which implies "instance declarations only".

---


## Implementation



We want to collect the set of all `RdrNames` that are mentioned in the
program.  We must collect **`RdrNames`** not `Names`:


```wiki
   import Foo( x ) as Bar
   import Foo( x )
   q = (Foo.x, Bar.x)
```


Here both imports are required, but you can only tell that by
seeing the `RdrNames`, not by knowing that the name `x` is used.



I think that all lookups go through either, `RnEnv.lookupGreRn_maybe` or `RnEnv.lookup_sub_bndr`.
So in `RnEnv.lookupGreRn_maybe`, if `(gre_prov gre)` is `(Imported _)`,
and in `RnEnv.lookup_sub_bndr`,
put `rdr_name` in a new


```wiki
tcg_used_rdrnames :: TcRef (Set RdrName)
```


in `TcGblEnv`.  All the `tcg_used_rdrnames` are in scope; if not,
we report an error and do not add it to `tcg_used_rdrnames`.



Other notes


- Any particular (in-scope) used `RdrName` is bought into scope by
  one or more `RdrName.ImportSpec`'s.  You can find these `ImportSpecs`
  in the GRE returned by the lookup.

- The unit of "unused import" reporting is one of these `ImportSpecs`.

- Suppose that `rn` is a used, imported `RdrName`, and `iss` is 
  the `[ImportSpecs]` that brought it into scope.  Then, to a first 
  approximation all the `iss` are counted 'used'.

- We can compare `ImportSpecs` for equality by their `SrcSpans`.

- In `TcRnDriver.tcRnImports`, save import\_decls in a new
  `tcg_rn_rdr_imports :: Maybe [LImportDecl RdrName]`
  in `TcGblEnv`

---


## Algorithm



The algorithm for deciding which imports have been used is based around this datatype:


```wiki
data ImportInfo = ImportInfo SrcSpan
                             SDoc
                             (Maybe ModuleName) -- The effective module name
                             [RdrName] -- The names the import provides
                             Bool -- Has it been used yet?
                             [ImportInfo] -- Child import infos
```


We convert import declarations into trees of `ImportInfo`s, e.g.


```wiki
import Foo (a, D(c1, c2))
```


becomes (only the `SDoc` and `[RdrName]` fields are given, as that's the interesting bit)


```wiki
ImportInfo "Foo" []
    ImportInfo "a" ["a", "Foo.a"]
    ImportInfo "D" ["D", "Foo.D"]
        ImportInfo "c1" ["c1", "Foo.c1"]
        ImportInfo "c2" ["c2", "Foo.c2"]
```


If a node in the tree is marked as used, then so are all nodes above it. For example, given the tree
a use of `"D"` marks both the first and third lines as used.



When we come to giving warnings, if a node is unused then we warn about it, and do not descend into the rest of that subtree, as the node we warn about subsumes its children. If the node is marked as used then we descend, looking to see if any of its children are unused.



Here are how some example imports map to trees of `ImportInfo`, assuming `Foo` exports `a`, `b`, `D(c1, c2)`.


```wiki
import Foo
->
ImportInfo "Foo" ["a", "b", "D", "c1", "c2", "Foo.a", "Foo.b", "Foo.D", "Foo.c1", "Foo.c2"]

import qualified Foo as Bar
->
ImportInfo "Foo" ["Bar.a", "Bar.b", "Bar.D", "Bar.c1", "Bar.c2"]

import qualified Foo (a, D)
->
ImportInfo "Foo" []
    ImportInfo "a" ["Foo.a"]
    ImportInfo "D" ["Foo.D"]

import qualified Foo hiding (a, D(..))
->
ImportInfo "Foo" ["Foo.b"]

import Foo (D(c1, c2))
->
ImportInfo "Foo" []
    ImportInfo "D" ["D", "Foo.D"]
        ImportInfo "c1" ["c1", "Foo.c1"]
        ImportInfo "c2" ["c2", "Foo.c2"]

import qualified Foo (D(..))
->
ImportInfo "Foo" []
    ImportInfo "D(..)" ["Foo.D", "Foo.c1", "Foo.c2"]
```


These trees are built by `RnNames.mkImportInfo`. In `RnNames.warnUnusedImportDecls` we make two lists of `ImportInfo`s; one list contains all the explicit imports, e.g.


```wiki
import Foo (a, b)
```


and the other contains the implicit imports, e.g.


```wiki
import Foo
import Foo hiding (a, b)
```


Then `RnNames.markUsages` is called for each `RdrName` that was used in the program. The current implementation marks all explicit import as used unless there are no such imports, in which case it marks all implicit imports as used. A small tweak to `markUsages` would allow it to mark only the first import it finds as used.



As well as the `RdrName`s used in the source, we also need to mark as used the names that are exported. We first call `RnNames.expandExports` to expand `D(..)` into `D(c1, c2)`, and then call `RnNames.markExportUsages`. Normally this just marks the `RdrName`s as used in the same way that uses in the module body are handled, but it is also possible for an entire module to be "used", if `module Foo` is in the export list. In this case `RnNames.markModuleUsed` does the hard work, marking every module imported with that name as used.


