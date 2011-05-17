# Commentary: The Package System



See also: [Packages](commentary/compiler/packages), where we describe how this is implemented in GHC.


## Architecture



GHC maintains a package database, that is basically a list of `InstalledPackageInfo`.  The `InstalledPackageInfo` type is defined in `Distribution.InstalledPackageInfo` in Cabal, and both `ghc-pkg` and GHC itself import it directly from there.



There are four main components of the package system:


<table><tr><th>Cabal</th>
<td>
Cabal is a Haskell library, which provides basic data types for the package system, and support for building,
configuring, and installing packages.
</td></tr></table>


<table><tr><th>GHC itself</th>
<td>
GHC reads the package database(s), understands the flags `-package`, `-hide-package`, etc., and uses the package database
to find `.hi` files and library files for packages.  GHC imports modules from Cabal.
</td></tr></table>


<table><tr><th>`ghc-pkg`</th>
<td>
The `ghc-pkg` tool manages the package database, including registering/unregistering packages, queries, and
checking consistency.  `ghc-pkg ` also imports modules from Cabal.
</td></tr></table>


<table><tr><th>`cabal-install`</th>
<td>
A tool built on top of Cabal, which adds support for downloading packages from Hackage, and building and installing
multiple packages with a single command.
</td></tr></table>



For the purposes of this commentary, we are mostly concerned with GHC and `ghc-pkg`.


## Identifying Packages


<table><tr><th>`Cabal.PackageName` ("base")</th>
<td>
A string.  Defined in `Distribution.Package`.  Does not uniquely identify a package: the package
database can contain several packages with the same name.
</td></tr></table>


<table><tr><th>`Cabal.PackageId` ("base-4.1.0.0")</th>
<td>
A `PackageName` plus a `Version`.  A `PackageId` names an API.  If two `PackageId`s are
the same, they are assumed to have the same API.

`InstalledPackageInfo` contains the field `sourcePackageId :: PackageId`.

In GHC 6.11, the `PackageId` also uniquely identifies a package instance in the package database, but
only by convention (we may lift this restriction in the future, and allow the database to contain
multiple package instances with the same `PackageId` (and different `InstalledPackageId`s).
</td></tr></table>


<table><tr><th>`Cabal.InstalledPackageId` ("base-4.1.0.0-1mpgjN")</th>
<td>
(introduced in GHC 6.12 / Cabal 1.7.2) A string that uniquely identifies a package instance in the database.
An `InstalledPackageId` identifies an ABI: if two `InstalledPackageIds` are the same, they have the
same ABI.

`InstalledPackageInfo` contains the field `installedPackageId :: InstalledPackageId`.

Dependencies between installed packages are identified by the `InstalledPackageId`.  An `InstalledPackageId` is
chosen when a package is registered. It is chosen by calling `ghc --abi-hash` on the compiled modules and appending
the hash as a suffix to the string representing the `PackageIdentifier`.
</td></tr></table>


<table><tr><th>`GHC.PackageId` (these currently look like "base-4.1.0.0" in GHC 6.12)</th>
<td>
Inside GHC, we use the type `PackageId`, which is a `FastString`.  The (Z-encoding of) `PackageId` prefixes each
external symbol in the generated code, so that the modules of one package do not clash with those of another package,
even when the module names overlap.
</td></tr></table>


## Design constraints


1. We want [recompilation avoidance](commentary/compiler/recompilation-avoidance) to work.  This means that symbol names should not contain any information that varies too often, such as the ABI hash of the module or package.  The ABI of an entity should depend only on its definition, and the definitions of the things it depends on.

1. We want to be able to detect ABI incompatibility.  If a package is recompiled and installed over the top of the old one, and the new version is ABI-incompatible with the old one, then packages that depended on the old version should be detectably broken using the tools.

1. ABI compatibility:

  - We want repeatable compilations.  Compiling a package with the same inputs should yield the same outputs.
  - Furthermore, we want to be able to make compiled packages that expose an ABI that is compatible (e.g. a superset)
    of an existing compiled package.
  - Modular upgrades: we want to be able to upgrade an existing package without recompiling everything that depends
    on it, by ensuring that the replacement is ABI-compatible.
  - Shared library upgrades.  We want to be able to substitute a new ABI-compatible shared library for an old one, and all the existing binaries linked against the old version continue to work.
  - ABI compatibility is dependent on GHC too; changes to the compiler and RTS can introduce ABI incompatibilities.  We
    guarantee to only make ABI incompatible changes in a major release of GHC.  Between major releases, ABI compatibility
    is ensured; so for example it should be possible to use GHC 6.12.2 with the packages that came with GHC 6.12.1.


Right now, we do not have repeatable compilations, so while we cannot do (3), we keep it in mind.


## The Plan



We need to talk about some more package Ids:


- `PackageSymbolId`: the symbol prefix used in compiled code.
- `PackageLibId`: the package Id in the name of a compiled library file (static and shared).

### Detecting ABI incompatibility


- in the package database, dependencies specify the `InstalledPackageId`.

- The package database will contain at most one instance of a given package/version combination.  The tools
  are not currently able to cope with multiple instances (e.g. GHC's -package flag selects by name/version).

- If, say, package P-1.0 is recompiled and re-installed, the new instance of the package will almost
  certainly have an incompatible ABI from the previous version.  We give the new package a distinct
  `InstalledPackageId`, so that packages that depend on the old P-1.0 will now be detectably broken.

- `PackageSymbolId`: We do not use the `InstalledPackageId` as the symbol prefix in the compiled code, because 
  that interacts badly with [recompilation avoidance](commentary/compiler/recompilation-avoidance).  Every time we pick a
  new unique `InstalledPackageId` (e.g. when reconfiguring the package), we would have to recompile
  the entire package.  Hence, the `PackageSymbolId` is picked deterministically for the package, e.g.
  it can be the `PackageIdentifier`.

- `PackageLibId`: we do want to put the `InstalledPackageId` in the name of a library file, however.  This allows
  ABI incompatibility to be detected by the linker.  This is important for shared libraries too: we
  want an ABI-incompatible shared library upgrade to be detected by the dynamic linker.  Hence,
  `PackageLibId` == `InstalledPackageId`.

### Allowing ABI compatibilty


- The simplest scheme is to have an identifier for each distinct ABI, e.g. a pair of the package name and an integer
  that is incremented each time an ABI change of any kind is made to the package.  The ABI identifier
  is declared by the package, and is used as the `PackageSymbolId`.  Since packages with the same ABI identifier
  are ABI-compatible, the `PackageLibId` can be the same as the `PackageSymbolId`.

- The previous scheme does not allow ABI-compatible changes (e.g. ABI extension) to be made.  Hence, we could
  generalise it to a major/minor versioning scheme.

  - the ABI major version is as before, the package name + an integer.  This is also the `PackageSymbolId`.
  - the ABI minor version is an integer that is incremented each time the ABI is extended in a compatible way.
  - package dependencies in the database specify the major+minor ABI version they require, in addition to the
    `InstalledPackageId`.  They may be satisfied by a greater minor version; when upgrading a package with an 
    ABI-compatible replacement, ghc-pkg updates dependencies to point to the new `InstalledPackageId`.
  - `PackageLibId` is the major version.  In the case of shared libraries, we may name the library using the
    major + minor versions, with a symbolic link from the major version to major+minor.
  - the shared library `SONAME` is the major version.

- The previous scheme only allows ABI-compatible changes to be made in a linear sequence.  If we want a tree-shaped
  compatibility structure, then something more complex is needed (ToDo).

- The previous schemes only allow compatible ABI changes to be made.  If we want to allow incompatible changes to be
  made, then we need something like ELF's symbol versioning.  This is probably overkill, since we will be making
  incompatible ABI changes in the compiler and RTS at regular intervals anyway, so long-term ABI compatibility is
  impractical at this stage.
