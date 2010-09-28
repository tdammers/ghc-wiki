


## Alternative Proposal for Packages (with explicit namespaces)



This proposal is an alternative to [Commentary/Packages/GhcPackagesProposal](commentary/packages/ghc-packages-proposal).  Large parts overlap with that
proposal.  To motivate this new proposal, let's consider another
proposed and desirable feature of the import/export language, which may
interact in interesting ways with packages.


## A different, but related, problem



A problem that has been mentioned several times on mailing lists, is
grafting part of a directory hierarchy into an arbitrary location
elsewhere in the hierarchy.
(See [
http://www.haskell.org/pipermail/libraries/2005-June/004009.html](http://www.haskell.org/pipermail/libraries/2005-June/004009.html))



Another way of expressing a similar wish is the ability to re-export
imports with a different qualified name, as in the scenario suggested by
the developers of the package gtk2hs:
[
http://www.haskell.org/pipermail/libraries/2004-December/002800.html](http://www.haskell.org/pipermail/libraries/2004-December/002800.html)



There are several desires in play here:


- a desire to minimise typing of long qualified names
- a desire to refer to "leaf" nodes of the hierarchy in a way that makes it easy to relocate those modules in the hierarchy, without needing to edit every import declaration that uses them
- a desire to partially-qualify names for disambiguation

## Proposal



We introduce the new concept of *namespace* as something that can be
declared in source code.  A namespace can contain only module names.
(The specification of what module names are contained in a namespace is
rather like our current concept of a package, i.e. not declared in the
source code, but rather by some external mechanism e.g. grouping of
files in a filesystem hierarchy.)



There are now two separate kinds of `import`.


- `import namespace "foo-1.3" Data.Foo`
- `import Bar`


The new semi-reserved word `namespace` is introduced, having special
meaning only directly after the `import` keyword.  There is a
*level* difference in what this new form of import means.  The
declaration `import namespace` brings into availability the subset of
the hierarchy of *module* names rooted in the package `"foo-1.3"`,
at the position `Data.Foo`.  That is, if the package `foo`
version `1.3` contains the modules


- Data.Foo.Bar
- Data.Foo.Baz
- Data.Bar


then the namespace import brings into the "importable" namespace only
the modules


- Data.Foo.Bar
- Data.Foo.Baz


However, for the program to use those modules, it is still necessary to
go ahead and actually `import` them in the normal way, although the
names used to import them will now be *relative* to the available
namespaces, rather than absolute.  So the declaration `import Bar`
brings into scope all the entities defined in `Data.Foo.Bar`.  Like
any normal import, these can be qualified or hidden.



Thus,


- `import namespace` brings into scope a bunch of names for modules
  from the given provenance.
- `import` brings into scope a bunch of entities from the given
  module.

### Naming a namespace



Are namespaces first class?  Can we give them a name?  Indeed, why not?


- `import namespace "foo-1.3" Data.Foo as OldFoo`
- `import OldFoo.Bar`


Here, we have declared that we want to be able to refer to the namespace
as `OldFoo`, and so, a subsequent `import OldFoo.Bar`
specifically asks for the `Data.Foo.Bar` from the package
`foo-1.3`, just in case there might be a `Bar` module also
available from another namespace.


### What namespaces are available by default?



If no namespaces are explicitly brought into scope, what modules are
implicitly available?


- Anything in the *current* package, i.e. the executable or library
  whose modules are all physically rooted at the same location in the
  filesystem as this module.

- Is there an implicit `import namespace "base"`, just as there is an
  implicit `import Prelude`?

### Namespace resolution



In essence, namespaces take over the role formerly played by commandline
arguments like `-Iproject` and `-package foo`.  The search path
used by the compiler for finding modules is now partially declared in
the source code itself.  (Note however that that the search path is
declared symbolically, involving package names, not directories.  This is a very important
separation of the thing itself from where it is stored.)



Resolution of which module is referred to by an import statement (taking
into account the namespaces) is just like the current process of
resolving which entity is referred to by program text (taking into
account the imported modules).  The source text may import multiple
namespaces.  If any module import is ambiguous (i.e. the module exists
in more than one namespace), it is a static error.  Resolution is lazy,
in the sense that there is no error if namespaces contain the same
module name, only if the program tries to import that module name.



So when you say "import A.B.C", from what package does A.B.C come?



There must be a single namespace in scope containing a module called
`A.B.C`.  (Sidenote: or in fact a namespace called `A`, containing a module
named `B.C`)


### Syntax



The precise syntax can be debated.  New keywords like `use` or
`from` could be substituted for `import namespace`.  The key
important features however are the inclusion of:


- the package name (mandatory)
- an optional package version, if several are available
- an optional path to use as the root of the available namespace
- an optional renaming

### Exports



One might wonder whether it is now either necessary or desirable to
permit *namespaces* to be re-exported in the same way that *modules*
can be?  For instance:


```wiki
module Aggregate
  ( module Aggregate
  , namespace OldFoo
  ) where
import namespace "foo-1.3" Data.Foo as OldFoo
```


The idea is that any module saying `import Aggregate` would thereby
implicitly open the namespace of package `"foo-1.3"` at the root
`Data.Foo`, in addition to having access to entities defined in
`Aggregate` itself.



Note that, just as with a current module re-export it is no longer
possible for the importing location to use the original module name as a
qualifier; so with a namespace re-export, there is no way to refer to
the namespace in the importing location either.  It is purely a signal
to the compiler telling it where to look for modules when resolving
imports.



I argue that namespace export *is* desirable, because it allows (but
does not require) all package (namespace) dependencies to be gathered
together in a single module for an entire project.  With such an
organising principle, when dependencies change, there is only one source
file to update.  But without namespace re-exports, it would be
impossible to localise those dependencies to a single file.



Note how this feature addresses several of the initial stated desires,
of reducing the verbosity of imports, and of referring to leaf modules
conveniently.  For instance:


```wiki
module Gtk (namespace AllOfGtk) where
import namespace "gtk-2.4" Graphics.UI.Gtk as AllOfGtk

module MyGUI where
import Gtk
import Button
..... Button.label .....
```

### Implicit imports



One could go further.  If I write a qualified name `M.e` in the
source text, must I also write `import M` at the top?  The qualified
entity is unambiguous, whether or not there is an explicit import for
it, because the module qualification `M` must be unambiguous within
the current namespaces.  In the Gtk example above, this would eliminate
the need for `import Button`, and who knows how many other imports,
leaving a single `import Gtk` to bring all of the qualified entities
into scope.


### Exposed vs Hidden packages



GHC's scheme of exposed vs hidden packages can now be replaced with full
source-code control of namespace visibility.  To setup a default set of
exposed packages, you just write a module to export their namespaces:


```wiki
module ExposedPackages
  ( namespace FGL
  , namespace Parsec
  , namespace HaXml
  ) where

import namespace "fgl" as FGL
import namespace "parsec-0.1" as Parsec
import namespace "HaXml" as HaXml
```


and import it in every module of your project.  Or if importing it
everywhere sounds too painful, one can even imagine that a compiler
might provide a command-line option (or use a configuration file) to
specify one distinguished module to be implicitly imported everywhere:


```wiki
$ ghc --make -implicit-prelude=ExposedPackages MyProject.hs

$ cat .ghci
set -implicit-prelude ExposedPackages
```

###
What if you wanted to import A.B.C from P1 and A.B.C from P2 into the *same* module?


```wiki
module C1 (module A.B.C) where
import namespace "P1"
import A.B.C

module C2 (module A.B.C) where
import namespace "P2"
import A.B.C

module User where
import qualified C1
import qualified C2
```