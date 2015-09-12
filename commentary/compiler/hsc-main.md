# Compiling one module: HscMain



Here we are going to look at the compilation of a single module.
There is a picture that goes with this description, which appears at the bottom of this page, but you'll probably find it easier to open [this link](commentary/compiler/hsc-pipe) in another window, so you can see it at the same time as reading the text.



You can also watch a **video** of Simon Peyton-Jones explaining the compilation pipeline here: [
Compiler Pipeline II](http://www.youtube.com/watch?v=Upm_kYMgI_c&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) (10'16")



Look at the picture first.  The yellow boxes are compiler passes, while the blue stuff on the left gives the data type that moves from one phase to the next.  The entire pipeline for a single module is run by a module called HscMain ([compiler/main/HscMain.hs](/trac/ghc/browser/ghc/compiler/main/HscMain.hs)).  Each data type's representation can be dumped for further inspection using a `-ddump-*` flag. (Consider also using `-ddump-to-file`: some of the dump outputs can be large!)  Here are the steps it goes through:


- The **Front End** processes the program in the [big HsSyn type](commentary/compiler/hs-syn-type). `HsSyn` is parameterised over the types of the term variables it contains.  The first three passes (the front end) of the compiler work like this:

  - The **[Parser](commentary/compiler/parser)** produces `HsSyn` parameterised by **[RdrName](commentary/compiler/rdr-name-type)**.  To a first approximation, a `RdrName` is just a string. (`-ddump-parsed`) 

  - The **[Renamer](commentary/compiler/renamer)** transforms this to `HsSyn` parameterised by **[Name](commentary/compiler/name-type)**.  To a first appoximation, a `Name` is a string plus a `Unique` (number) that uniquely identifies it.  In particular, the renamer associates each identifier with its binding instance and ensures that all occurrences which associate to the same binding instance share a single `Unique`. (`-ddump-rn`)  

  - The **[Typechecker](commentary/compiler/type-checker)** transforms this further, to `HsSyn` parameterised by **[Id](commentary/compiler/entity-types)**.  To a first approximation, an `Id` is a `Name` plus a type. In addition, the type-checker converts class declarations to `Class`es, and type declarations to `TyCon`s and `DataCon`s.  And of course, the type-checker deals in `Type`s and `TyVar`s. The [data types for these entities](commentary/compiler/entity-types) (`Type`, `TyCon`, `Class`, `Id`, `TyVar`) are pervasive throughout the rest of the compiler. (`-ddump-tc`)

>
>
> These three passes can all discover programmer errors, which are sorted and reported to the user.
>
>


 


- The **Desugarer** ([compiler/deSugar/Desugar.hs](/trac/ghc/browser/ghc/compiler/deSugar/Desugar.hs)) converts from the massive `HsSyn` type to [GHC's intermediate language, CoreSyn](commentary/compiler/core-syn-type).  This Core-language data type is unusually tiny: just eight constructors.) (`-ddump-ds`)

  Generally speaking, the desugarer produces few user errors or warnings. But it does produce *some*.  In particular, (a) pattern-match overlap warnings are produced here; and (b) when desugaring Template Haskell code quotations, the desugarer may find that `THSyntax` is not expressive enough.  In that case, we must produce an error ([compiler/deSugar/DsMeta.hs](/trac/ghc/browser/ghc/compiler/deSugar/DsMeta.hs)).

  This late desugaring is somewhat unusual.  It is much more common to desugar the program before typechecking, or renaming, because that presents the renamer and typechecker with a much smaller language to deal with.  However, GHC's organisation means that

  - error messages can display precisely the syntax that the user wrote; and 
  - desugaring is not required to preserve type-inference properties.

- The **SimplCore** pass ([compiler/simplCore/SimplCore.hs](/trac/ghc/browser/ghc/compiler/simplCore/SimplCore.hs)) is a bunch of Core-to-Core passes that optimise the program; see [
  A transformation-based optimiser for Haskell (SCP'98)](http://research.microsoft.com/%7Esimonpj/Papers/comp-by-trans-scp.ps.gz) for a more-or-less accurate overview.  See [Commentary/Compiler/Core2CorePipeline](commentary/compiler/core2-core-pipeline) for an overview of the Core-to-Core optimisation pipeline. The main passes are:

  - The **Simplifier**, which applies lots of small, local optimisations to the program.  The simplifier is big and complicated, because it implements a *lot* of transformations; and tries to make them cascade nicely.  The transformation-based optimiser paper gives lots of details, but two other papers are particularly relevant: [
    Secrets of the Glasgow Haskell Compiler inliner (JFP'02)](http://research.microsoft.com/%7Esimonpj/Papers/inlining/index.htm) and [
    Playing by the rules: rewriting as a practical optimisation technique in GHC (Haskell workshop 2001)](http://research.microsoft.com/%7Esimonpj/Papers/rules.htm).  (`-ddump-simpl`)

  - The **float-out** and **float-in** transformations, which move let-bindings outwards and inwards respectively.  See [
    Let-floating: moving bindings to give faster programs (ICFP '96)](http://research.microsoft.com/%7Esimonpj/papers/float.ps.gz).

  - The **strictness analyser**.  This actually comprises two passes: the **analyser** itself and the **worker/wrapper** transformation that uses the results of the analysis to transform the program. (Further described in [Demand analysis](commentary/compiler/demand).) The same analyser also does [
    Constructed Product Result analysis](http://research.microsoft.com/%7Esimonpj/Papers/cpr/index.htm) and [
    Cardinality analysis](http://research.microsoft.com/en-us/um/people/simonpj/papers/usage-types/cardinality-extended.pdf). (`-ddump-stranal`)

  - The **liberate-case** transformation.

  - The **constructor-specialialisation** transformation.

  - The **common sub-expression eliminiation** (CSE) transformation. (`-ddump-cse`)

- Then the **CoreTidy pass** gets the code into a form in which it can be imported into subsequent modules (when using `--make`) and/or put into an interface file.  


 


>
>
> It makes a difference whether or not you are using `-O` at this stage.  With `-O` (or rather, with `-fomit-interface-pragmas` which is a consequence of `-O`), the tidied program (produced by `tidyProgram`) has unfoldings for Ids, and RULES.  Without `-O` the unfoldings and RULES are omitted from the tidied program.  And that, in turn, affects the interface file generated subsequently.
>
>

>
>
> There are good notes at the top of the file [compiler/main/TidyPgm.hs](/trac/ghc/browser/ghc/compiler/main/TidyPgm.hs); the main function is `tidyProgram`, documented as "Plan B" ("Plan A" is a simplified tidy pass that is run when we have only typechecked, but haven't run the desugarer or simplifier).
>
>

- At this point, the data flow forks.  First, the tidied program is dumped into an interface file.  This part happens in two stages:

  - It is **converted to `IfaceSyn`** (defined in [compiler/iface/IfaceSyn.hs](/trac/ghc/browser/ghc/compiler/iface/IfaceSyn.hs) and [compiler/iface/IfaceType.hs](/trac/ghc/browser/ghc/compiler/iface/IfaceType.hs)).
  - The `IfaceSyn` is **serialised into a binary output file** ([compiler/iface/BinIface.hs](/trac/ghc/browser/ghc/compiler/iface/BinIface.hs)).

>
> >
> >
> > The serialisation does (pretty much) nothing except serialise.  All the intelligence is in the `Core`-to-`IfaceSyn` conversion; or, rather, in the reverse of that step.
> >
> >
>

- The same, tidied Core program is now fed to the Back End.  First there is a two-stage conversion from `CoreSyn` to [GHC's intermediate language, StgSyn](commentary/compiler/stg-syn-type).

  - The first step is called **CorePrep**, a Core-to-Core pass that puts the program into A-normal form (ANF).  In ANF, the argument of every application is a variable or literal; more complicated arguments are let-bound.  Actually `CorePrep` does quite a bit more: there is a detailed list at the top of the file [compiler/coreSyn/CorePrep.hs](/trac/ghc/browser/ghc/compiler/coreSyn/CorePrep.hs).
  - The second step, **CoreToStg**, moves to the `StgSyn` data type ([compiler/stgSyn/CoreToStg.hs](/trac/ghc/browser/ghc/compiler/stgSyn/CoreToStg.hs)).  The output of CorePrep is carefully arranged to exactly match what `StgSyn` allows (notably ANF), so there is very little work to do. However, `StgSyn` is decorated with lots of redundant information (free variables, let-no-escape indicators), which is generated on-the-fly by `CoreToStg`.

- Next, the **[Code Generator](commentary/compiler/code-gen)** converts the STG program to a `C--` program.  The code generator is a Big Mother, and lives in directory [compiler/codeGen](/trac/ghc/browser/ghc/compiler/codeGen)  

- Now the path forks again:

  - If we are generating GHC's stylised C code, we can just pretty-print the `C--` code as stylised C ([compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs))
  - If we are generating native code, we invoke the native code generator.  This is another Big Mother ([compiler/nativeGen](/trac/ghc/browser/ghc/compiler/nativeGen)).
  - If we are generating LLVM code, we invoke the LLVM code generator. This is a reasonably simple code generator ([compiler/llvmGen](/trac/ghc/browser/ghc/compiler/llvmGen)).

# The Diagram



This diagram is also located [here](commentary/compiler/hsc-pipe), so that you can open it in a separate window.



[](/trac/ghc/attachment/wiki/Commentary/Compiler/HscPipe/HscPipe2.png)


