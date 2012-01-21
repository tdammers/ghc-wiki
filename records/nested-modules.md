
Proposal for A:



Allow nested modules. A nested module can occur in any
position that a data declaration can occur. The syntax of
a nested module is the same as the syntax of a conventional
module.



A nested module must have the same hierarchical name as
the module in which it is nested, followed by a single additional
component. A name with a single component can be specified
in the declaration of a nested module; this is syntactic sugar for
the fully qualified name of the enclosing module followed by that
single additional component.



When a module M.A is directly nested in module M, there is
an implied import in the enclosing module M as follows:



import qualified M.A as A



and an implied import in the nested module M.A as follows:



import M



These implied imports may optionally be specified explicitly
with no effect, or overridden with other explicit imports,
similarly to the usual implied import of Prelude.



When modules are nested to a depth greater than one,
similar implied imports exist for all recursively enclosing
and enclosed modules, with the same rules about
overriding.



If an enclosing module M has an export list, a nested
module N at any depth recursively cannot be imported
by modules not nested inside M unless N is included in
the export list of M. If M does not have an export list,
N can be imported by any other module as usual.



In every other respect, a nested module declaration has
exactly the same effect as any other module declaration.
In particular, the behavior of nested modules in the
presence of all corner cases such as data families, etc.,
is specified by this rule.



The effect of a nested module on the behavior of
ghc --make is left unspecified as of now, until
there is feedback from the GHC team. This would
probably involve GHC looking for A.B and then
A in turn when it fails to find A.B.C. Or perhaps
even when A.B.C is found, to identify erroneous
duplication. Or GHC could stay pretty much as
it is now, relying on the user to ensure that GHC
finds the nested module; that would certainly
be fine for an initial implementation.



Usage example:



module Library where


>
>
> import Data.Text (Text)
>
>


...


>
>
> type ISBN = Text
> module Book where
>
>
> >
> >
> > import Data.Text (Text)
> > data T = New { name :: Text, iSBN :: ISBN }
> >
> >
>
>
> module Checkout where
>
>
> >
> >
> > import Data.Time
> > import qualified Library.Book as Book
> > import qualified Library.Borrower as Borrower
> > data T = New
> >
> >
> > >
> > >
> > > { book :: Book.T, borrower :: Borrower.T, dueDate :: Day }
> > >
> > >
> >
>
>
> module Borrower where
>
>
> >
> >
> > import Data.Text (Text)
> > data T = New { name :: Text, address :: Text }
> >
> >
>
>
> module History where
>
>
> >
> >
> > import qualified Library.Borrower as Borrower
> > import qualified Library.Checkout as Checkout
> > data T = New { borrower :: Borrower.T, checkouts :: \[Checkout.T\] }
> >
> >
>


This makes available in the module Library the
record types:



Book.T, Checkout.T, Borrower.T, History.T



with constructors:



Book.New, Checkout.New, Borrower.New, History.New



and record accessors:



Book.name, Book.iSBN,
Checkout.book, Checkout.borrower, Checkout.dueDate,
Borrower.name, Borrower.address,
History.borrower, History.checkouts



I believe this specification should be very simple to
implement and describe. There are some obvious
shortcomings. But it does provide basic namespacing
of records with almost no change to Haskell and GHC.



Note also that you need to be careful to avoid mutually
recursive imports. That is really more of a limitation
of GHC than a limitation of the specification itself.
Other compilers might not require that.



I'd be happy to hear ideas about how to develop this
simple idea further and eliminate some of the
shortcomings, on condition that it doesn't lead to
further bikeshedding and significant delay.



One obvious enhancement would be for the
implied import of the enclosing module to
include also all names imported into
the enclosing module, unlike the usual
convention for imports. I'm not sure if
there are complications to that though.



====================



Whenever any module E imports M unqualified without an
import list, as in:



import M



then the following implied imports would be added to E:



import qualified M.T as T
import qualified M.S as S



and whenever E imports M qualified without an
import list, as in:



import qualified M as Q



then the following implied imports would be
added to E:



import qualified M.T as Q.T
import qualified M.S as Q.S



Similarly, if M also contains more deeply nested
modules and E imports M either qualified or
unqualified without an import list, the corresponding
implied imports of the deeply nested modules would
also be added to E. But in fact, this is nothing
more than a recursive application of the previous
rule.



Note that an import statement with an import list
will never generate any automatic import of
a nested module.


