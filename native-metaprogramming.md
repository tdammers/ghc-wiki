## Native Metaprogramming


### Definition



Metaprogramming is the act of writing programs (in a meta language) that
generate and manipulate programs (in an object language).
Native metaprogramming is a form of metaprogramming where the meta languages's
own infrastructure is directly employed to generate and manipulate object
programs.
Native metaprogramming naturally arises in homogeneous metaprogramming (where
meta language and object language are nearly identical).


### Goal



The goal is to allow metaprograms to directly access and reuse the "native"
machinery inside the compiler. There is no need to have a separate
representation of syntax, and the associated sets of tools, as in Template
Haskell (TH), or even as in third-party libraries like Haskell-Src-Exts (HSE)
and others in Haskell-Suite. GHC has a "native" representation of terms, with
many tools already built on top of that, including the compiler passes like the
parser, renamer, or the typechecker. We would like to unify these
representations and tools as much as possible, and allow metaprograms to
directly access these internal machineries.



The eventual goal is indeed more than sole reuse of the AST in GHC (HsSyn) and
its tools; metaprograms should also be able to reuse the "infrastructure" like
the different environments and monads used for name resolution or
typechecking. For example, we may want to treat certain surface language
constructs simply as metaprograms (as opposed to them being built-in into the
compiler). It is helpful in simplifying both the front-end (i.e., how users
perceive constructs in the language), and the back-end (i.e., how the compiler
implements them). However doing so, \*sometimes\* requires access to the type of
terms: such metaprograms describe a type-directed elaboration process for the
surface constructs they represent. This amounts to, for example, splices
(anti-quotations) in Haskell be able to accept terms wrapped in GHC's internal
typechecking monad (for this particular use of internal typechecking monad, see
David Christiansen's thesis).


### Approach



The general approach is to gradually refactor GHC into a set of smaller reusable
packages, e.g., an AST, a parser, a renamer, a type-checker, a desugarer, an
evaluator (out of GHCi), and possibly series of packages for the intermediate
languages.



But, as you may have guessed already, there are challenging problems on the way
that should be addressed first.


### Problems


#### Annotations



The immediate problem with reusing GHC AST is that it comes with a large set of
extra fields and constructors carrying the information only necessary for the
passes inside GHC. Users do not want to, and do not need to, deal with these
extra fields and constructors (I refer to these as annotations).



For example, compare the following representations of lambda terms with n-ary
tuples.



[
Annotation free variant](https://github.com/shayan-najd/NativeMetaprogramming/blob/master/Comparison/Unannotated.hs):


```
data Exp id
  = Var id
  | Abs id (Exp id)
  | App (Exp id) (Exp id)
  | Tpl [Exp id]
```


[
Annotated Variant](https://github.com/shayan-najd/NativeMetaprogramming/blob/master/Comparison/Annotated.hs):


```
data Exp id
  = Var            id
  | Abs Typ SrcLoc id (Exp id)
  | App Typ        (Exp id) (Exp id)
  | Tpl [Typ]      [Exp id]
  | Out Typ        (Exp id)
```


We would like the AST exposed to the users to be represented like the former,
while a representation like the latter is what it is needed for different use
cases.


#### All the Fun with Macros



Once we manage to refactor and expose the internal machinery, we may, for
instance, want to treat certain surface language constructs as
metaprograms. However, there is a huge, often underestimated, gap between having
these constructs as built-in, and having them as yet another metaprogram. The
gap is both in theory, e.g., the equational and algebraic properties of such
constructs, and in practice, e.g., handling the error messages. These problems
arise basically in any macro/metaprogramming system, and many researchers have
been working on these problems and there is still much work to be done.  Indeed,
GHC has its own set of solutions to these problems. For instance, GHC uses
instances of `Outputable`, a large set of customisation options (i.e.,
`DynFlags`), and a specific exception handling and error reporting mechanism
(i.e., `Panic`). But, this solution is specific to GHC, and how to port it to be
used for user-define code generating and code manipulating programs is not
clear.


### Progress


#### Summer of Haskell



This summer (2016), I (Shayan Najd) stepped up to address some of these problems
by proposing a Summer of Haskell project, with Simon Peyton Jones and Jacques
Carette as my mentors. My proposal got accepted and thanks to the community
support and the organisers (specially Edward Kmett, and Ryan Trinkle), I started
working on the project nearly full-time.



I soon faced the problem of decorating an AST with arbitrary set of annotations.
I, with my mentors (Simon and Jacques), Richard Eisenberg, and Alan Zimmerman
analysed the design space considerably.


#### Annotations in Practice



We studied and identified different ways that annotations may appear in
practice, specifically in the GHC code base.



The annotated example earlier is designed so that it demonstrates all the
different forms in which annotations may actually appear in the GHC code base:


1. annotations may appear as new fields to existing constructors,
  e.g., the field `Typ` in the constructor `App`

1. annotations may appear as new data constructors to datatypes,
  e.g., the constructor `Out` in the datatype `Exp`

1. type and number of annotations on constructors (or a datatypes) may differ from one to another,
  e.g., the constructor `App` is annotated with one new field of the type `Typ`,
  but the constructor `Abs` is annotated with two fields of the types `Typ` and `SrcLoc`


In addition to above, we identified that


1. Annotations are always stored inside the AST in GHC

1. ASTs are regular (mutually recursive) datatypes, e.g., Haskell98 ADTs
  with no polymorphic recursion, and nodes of function type (see, for example,
  McBride's
  [
  definition](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.22.8611&rep=rep1&type=pdf))

1. Annotated ASTs should be at least as large as their non-annotated variants:
  an AST after adding annotations should be able to carry at least the same
  amount of information as the original AST.
  In above example, the annotated version of `Exp` is able to carry at least
  the same amount of information as the original datatype.

1. Annotated AST should not be too large (which will cause problems with the
  totality checker):
  an AST after adding annotations should not allow storing more information,
  compared to the original AST, other than the exact extra information carried
  by the relevant annotations.

#### Towards Unified AST



We compared multiple different solutions on how to reuse the same parametric
datatype for representing both annotated and annotation-free variants.
(see an old relevant [
wiki entry](https://github.com/shayan-najd/NativeMetaprogramming/wiki/Extensions-&-Annotations)).



When comparing these, soon I realised that the problem is an instance of
row/column extensibility problem, a simple observation that helped us in the
analysis, and finally finding a suitable solution.



Using row/column extensibility, we can define both annotated and annotation-free
variants of ASTs, based on two separate instantiations of the same set
of extensible datatypes.


#### Row/Column Extensibility



Row extensions are the new fields that are added to the constructors.
For example, the constructor `Abs :: id -> Exp id -> Exp id` in the
annotation-free `Exp` above, is row-extended compared to the constructor
`Abs :: Typ -> SrcLoc -> id -> Exp id -> Exp id` in the annotated `Exp` above.
The fields of the type `Typ` and `SrcLoc` are the row extensions for the
constructor `Abs`.



Column extensions are the new constructors that are added to a datatype.
For example, the annotation-free `Exp` above is column-extended with the
constructor `Out :: Typ -> Exp id -> Exp id` compared to the annotated `Exp`.
The constructor `Out :: Typ -> Exp id -> Exp id` is the column extension.



There are multiple solutions to the row/column extensibility problem, and there
are multiple criteria to what solutions are acceptable.


#### Encoding Extensibility



Before explaining the details let us have a look at a definition of extensible
datatypes using a tool that I have developed to help in defining them:


```
desugarExtensible "Ext"
  [d| {-# ANN type Exp Extensible #-}
      data Exp id
        = Var id
        | Abs id       (Exp id)
        | App (Exp id) (Exp id)
        | Tpl [Exp id]

      {-# ANN type ExpAS (Extends "Exp") #-}
      data ExpAS id
        = VarAS (Extends "Var")
        | AbsAS (Extends "Abs") Typ SrcLoc
                -- (Extends ...) is a dummy field
                -- that I used for simulating syntax.
        | AppAS (Extends "App") Typ
        | TplAS (Extends "Tpl") [Typ]
        | OutAS  Typ (ExpAS id)
  |]
```


It defines an extensible datatype `Exp`, by annotating it.
Then it defines an extention to it, named `ExpAS`, which represent the annotated
variant of `Exp` from earlier.



Above produces the following code:


```
   data Exp ext id
      = ExpExt (ext "ExpExt")
      | Var (ext "Var") id
      | Abs (ext "Abs") id (Exp ext id)
      | App (ext "App") (Exp ext id) (Exp ext id)
      | Tpl (ext "Tpl") [Exp ext id]

    data family Ext id (lbl :: Symbol)
    type ExpAS id = Exp (Ext id) id

    data instance Ext id "Var" = VarX
    pattern VarAS :: id -> ExpAS id
    pattern VarAS      x   = Var    VarX x

    data instance Ext id "Abs" = AbsX Typ SrcLoc
    pattern AbsAS :: Typ -> SrcLoc -> id -> ExpAS id -> ExpAS id
    pattern AbsAS t  s x n = Abs    (AbsX t s) x n

    data instance Ext id "App" = AppX Typ
    pattern AppAS :: Typ -> ExpAS id -> ExpAS id -> ExpAS id
    pattern AppAS t    l m = App    (AppX t) l m

    data instance Ext id "Tpl" = TplX [Typ]
    pattern TplAS :: [Typ] -> [ExpAS id] -> ExpAS id
    pattern TplAS ts   ms  = Tpl    (TplX ts) ms

    data instance Ext id "ExpExt" = OutASX Typ (ExpAS id)
    pattern OutAS :: Typ -> ExpAS id -> ExpAS id
    pattern OutAS t    m   = ExpExt (OutA t m)
```


Basically, what the extensible encoding does is to add a new parameter to the
datatype to stand for extensions, and each extension is projected out of it by a unique label.



We explain details of the extensible datatypes, the tool, and the
possible encoding separately [
here](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf) and [
here](https://arxiv.org/abs/1610.04799).


#### Current Status



I have implemented an extensible variant of HsSyn AST in GHC, which can be found
[
here](https://github.com/shayan-najd/NativeMetaprogramming/blob/master/HsSyn/SyntaxExtensibleAutoSplitted.hs).



I have also extracted a stand-alone parser for Haskell from GHC, and from that
I have extracted a stand-alone implementation of Haskell AST.  They can both be
found [ here](https://github.com/shayan-najd/HsParser).


#### Next Steps



We, the GHC developers, have to discuss the details of the work in details, as changes to the `HsSyn` AST affect the entire code base.
I present the work at Haskell Implementors Workshop at Nara, Japan (recorded [
here](https://www.youtube.com/watch?v=DSWoGdfYt68)), and from then, I hope we can start further serious discussions about this.
Specifically, we have to find a simple way to do all this massive refactoring required for extracting the `HsSyn` AST and the parser as separate packages, 


1. without risking introducing bugs,
1. while keeping git history clean and relevant, and
1. keeping the changes minimal (small commits at the time), to avoid conflicts at the repository.


Having extracted the `HsSyn` AST and the parser as separate packages, and changed GHC to depend on them; I am planing to focus on refactoring Template Haskell based on this. Backwards compatibility is a big debate awaiting us!


