# Module Types



Here we attempt to describe some of the main data structures involved in GHC's representation and handling of Haksell modules. GHC uses a number of different data types to represent modules, for efficiency (some types load less information) and categorising how other modules relate to the one being compiled. Most these types are defined in [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs).


## Module



Location: [compiler/basicTypes/Module.hs](/trac/ghc/browser/ghc/compiler/basicTypes/Module.hs)



The **Module** data type is simply an identifier of a module; its fully qualified name.


```wiki
-- | A Module is a pair of a 'PackageId' and a 'ModuleName'.
data Module = Module {
   modulePackageId :: !PackageId,  -- pkg-1.0
   moduleName      :: !ModuleName  -- A.B.C
  }
  deriving (Eq, Ord)

newtype ModuleName = ModuleName FastString
```

## ModIface



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



The **ModIface** data type is one of the fullest representations of a module. It is a complete representation of a modules interface file (**.hi**). It is this data structure that is serialised to produce a modules **.hi** file.


## ModDetails



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



**ModDetails** is essentially a cache for information in the **ModIface** for home modules only. It stores information about a module after linking has taken place. **ModIface** stores information about a module before linking. Information stored in a **ModDetails** is created from a **ModIface**, typically during type checking.


### ModGuts



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



A **ModGuts** is carried through the compiler, accumulating stuff as it goes. There is only one **ModGuts** at any time, the one for the module being compiled right now.  Once it is compiled, a **ModIface** and **ModDetails** are extracted and the **ModGuts** is discarded.


## ModSummary



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



A **ModSummary** stores a summary of a module that is suitable for recompilation checking. A **ModSummary** is a node in the compilation manager's dependency graph.


## HomeModInfo



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



A **HomeModInfo** stores information about a module in the package being compiled. It simply stores for the **ModIface**, **ModDetails** and linkage information about a single module.


## HomePackageTable



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



The home package table describes already-compiled home-package modules, /excluding/ the module we are compiling right now.


## ExternalPackageState



Location: [compiler/main/HscTypes.hs](/trac/ghc/browser/ghc/compiler/main/HscTypes.hs)



Stores information about other packages that we have pulled in while compiling the current module.


