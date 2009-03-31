# Idiom: the vanilla way



Libraries can be built in several different "ways", for example
"profiling" and "dynamic" are two ways.  Each way has a short tag
associated with it; "p" and "dyn" are the tags for profiling and
dynamic respectively.  In previous GHC build systems, the "normal" way
didn't have a name, it was just always built.  Now we explicitly call
it the "vanilla" way and use the tag "v" to refer to it.  



This means that the `GhcLibWays` variable, which lists the ways in
which the libraries are built, must include "v" if you want the
vanilla way to be built (this is included in the default setup, of
course).


