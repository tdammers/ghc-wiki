## CHR-style simplification for GHC



The main problem with regarding given equations as rewrite rules (used in much  the same way as toplevel axioms) is termination of the rewrite system.  In particular, we need to use a crude syntactic criterion (namely, decreasingness) to reject systems of equations that *might* lead to nontermination, and hence, reject some good programs.  This is in contrast to CHRs, where termination does not depend on the form of given equations.  If we ensure that all simplification of types wrt. to type equations happens during simplification (i.e., unification etc. does not rewrite type terms), we can use CHR-style simplification in GHC for type equalities.  The challange is keeping track of the coercions witnessing the various simplified equalities.


### Representation of equalities



We use the equational syntax `F t1 .. tn = t` for relations that take the form `F t1 tn t` with CHRs.  CHR's decomposition of expressions to conjunctions of relations promotes every nested indexed synonym application to a toplevel relation by introducing new variables.  In the equational syntax that means that indexed synonym symbols occur only as the outermost symbol in the left-hand side of equations.  In the `t1` to `tn` and `t`, there are no further occurences of indexed synonyms.  We call such equations *atomic*.



It appears attractive to represent a conjunction of atomic equations as a map of indexed synonym symbol to list of atomic equation.  This makes it easy to apply the FD rule and to find matching atomic equations of the wanted set in the given set.


### Coercions


