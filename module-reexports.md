## Module reexports



This is has been implemented and is supported starting with Cabal 1.22 & GHC 7.10, see also the [
Cabal documentation about \`reexported-modules\`](http://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-library-reexported-modules)



(Trac’ed as [\#8407](http://gitlabghc.nibbler/ghc/ghc/issues/8407)).


### Goal



This proposal aims to introduce re-exports at the package/module level (similar to the symbol re-exporting at the module/symbol level), to make reorganization of the package structure, such as moving a module from package-a to package-b, easier on the users of these packages.


### Motivation



Occasionally, a whole module `Data.Foo` needs to be moved from one package (`package-a`) to another (`package-b`), keeping its name. There are two possibilities:


- The module is provided by both packages (possibly with its symbols re-exported by one of them). Then a user cannot easily (i.e. without [PackageImports](package-imports)) depend on both packages.
- The module is removed in `package-a`. This then requires a major API bump and downstream packages likely have to update their dependencies. Also, if they did not use `package-b` before, they’d have to do that now.


One use-case in particular would be to turn `base` into a pure module-rexporting package, exporting a selection of modules from other packages. In that case, this proposal would allow the other package to provide additional (less commonly used, less stable or internal modules) and those users who need these can build-depend on `base` and the implementing package, and can still use the re-exported modules without further ado.


### Semantics



To combine the best of both possibilities, it will be possible for package-a to explicitly re-export `Data.Foo` from `package-b`. This information is noted in the package data base, and has the following effects


- a module using "`import Data.Foo`", "`import "package-a" Data.Foo`" or "`import "package-b" Data.Foo`" imports the real `package-b:Data.Foo`, if `package-a` or `package-b` is in scope.
- even if both `package-a` and `package-b` are in scope, the compiler does not complain about an ambiguous import.


A feature which is not currently implemented is the ability to annotate re-exports with deprecation messages.  If this were implemented, if `Data.Foo` is imported without package-b being in scope, the warning is printed. It is not printed if the module is imported only via `package-b`, or if both are in scope. "Importing Data.Foo via package-a is deprecated, please build-depend on package-b"


### Syntax



The re-export will need to be given in the `.cabal` file:


```wiki
name: package-a
...

library:
  build-depends:
    package-b
  exposed-modules:
    Data.Bar
  reexported-modules:
    Data.Foo
```


If, for some reason, the reexport itself needs to be qualified, then a package name can be given by prefixing the name with a colon:


```wiki
name: package-a
...

library:
  build-depends:
    package-b
  exposed-modules:
    Data.Bar
  reexported-modules:
    package-b:Data.Foo
```


We can also rename the module as another name:


```wiki
name: package-a
...

library:
  build-depends:
    package-b
  exposed-modules:
    Data.Bar
  reexported-modules:
    Data.Foo as OurFoo
```