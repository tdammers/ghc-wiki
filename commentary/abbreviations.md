# GHC Source Code Abbreviations



Certain abbreviations are used pervasively throughout the GHC source code. This page gives a partial list of them and their expansion:


- **ANF**: A-normal form

- **BCO**: Byte-code object

- **CAF**: Constant Applicative Form

- **Class**: Type Class

- **Cmm**: The final IR used in GHC, based on the C-- language

- **Core**: GHC core language. Based on System FC (variant of System F). Represents a type-checked and desugared program in some (out of several) intermediate compilation step

- **CoreFV**: Free variables in core

- **CoreLint**: Type and sanity-checking of core. (Lint: Jargon for a program analysis that looks for bug-suspicious code.)

- **CoreSubst**: Substitution in core

- **CoreSyn**: Core abstract syntax

- **DataCon**: Data constructor

- **Ds**: Desugarer

- **Gbl**: Global

- **Hs**: Haskell Syntax (generally as opposed to Core, for example, Expr vs HsExpr)

- **Hsc**: Haskell compiler. Means it Deals with compiling a single module and no more.

- **HsSyn**: Haskell abstract syntax

- **Id**: Synonym for Var, but indicating a term variable

- **Iface**: Interface, as in Haskell interface (.hi) files

- **IfaceSyn**: Interface abstract syntax

- **LHs**: Located Haskell something

- **Loc**: Location, as in SrcLoc

- **Located**: Something annotated with a SrcSpan

- **Lcl**: Local

- **nativeGen**: Native code generator (generates assembly from Cmm)

- **Occ**: Occurrence

  - However, in the context of [
    OccName](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RdrNameType#TheOccNametype), "occurrence" actually means "classified (i.e. as a type name, value name, etc) but not qualified and not yet resolved"

- **PId**: Package ID

- **PprCore**: Pretty-printing core

- **Rdr**: Parser (or reader)

- **Rn**: Rename or Renamer

- **Rts**: Run Time System

- **SimplCore**: Simplify core (the so-called simplifier belongs to this, as does the strictness analyser)

- **SrcLoc**: Source location (filename, line number, character position)

- **SrcSpan**: Source location span (filename, start line number and character position, end line number and character position)

- **STG**: [Spineless Tagless G-machine](commentary/compiler/stg-syn-type)

- **Tc**: TypeCheck{ing,er}

- **TSO**: [
  Thread State Object](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#ThreadStateObjects)

- **TyCon**: Type constructor

- **TyThing**: Something that is type-checkable

- **Ty**: Type

- **TyVar**: Synonym for Var, but indicating a type variable

- **Var**: A variable with some information about its type (or kind)
