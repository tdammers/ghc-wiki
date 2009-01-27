## Other work on nested data parallelism


- There is obviously all the work of Blelloch's group on [
  NESL](http://en.wikipedia.org/wiki/NESL) including an implementation based on flattening.

- Later Blelloch worked on multi-threaded implementations of nested parallelism (without vectorisation, but by having a thread per loop iteration).  This was mostly theoretical work [
  http://www.cs.cmu.edu/\~guyb/research.html\#scheduling](http://www.cs.cmu.edu/~guyb/research.html#scheduling). Blelloch's PhD student Girija Narlikar did some experiments with a scheduler using Pthreads [
  http://www.cs.cmu.edu/\~scandal/papers/spaa99.html](http://www.cs.cmu.edu/~scandal/papers/spaa99.html), but there was no real implementation.

- The Manticore project of Reppy's group at Chicago includes nested data parallelism and is based on SML: [
  http://manticore.cs.uchicago.edu/](http://manticore.cs.uchicago.edu/).

- Intel's Microsystems Programming Lab is working on an extension to their C/C++ compiler, called Ct, which adds nested data parallelism as a library (and special compiler support for NDP intrinsics).

- Then there is the work of Jan Prins' group on Proteus [
  http://www.cs.unc.edu/Research/proteus/](http://www.cs.unc.edu/Research/proteus/).  I think they had some kind of vector library hooked up to an interpreter for experiments, but again nothing usable.  Jan also implemented a couple of algorithms, which he vectorised manually, in imperative languages (eg, Fortran); in particular, the study about manual vectorisation in Fortran [
  http://www.cs.unc.edu/\~prins/Publications/SciProg99.pdf](http://www.cs.unc.edu/~prins/Publications/SciProg99.pdf).


 


- Modula-2\* of Tichy's group also recognised the value of nested data parallelism.  They added it in the form of a FORALL construct to Modula-2: [
  http://www2.cs.fau.de/download/Papers/CstarCritique.pdf?language=en](http://www2.cs.fau.de/download/Papers/CstarCritique.pdf?language=en), [
  http://citeseer.ist.psu.edu/philippsen91modula.html](http://citeseer.ist.psu.edu/philippsen91modula.html) and [
  http://citeseer.ist.psu.edu/397019.html](http://citeseer.ist.psu.edu/397019.html).  They started on a compiler that targeted the [
  MasPar](http://en.wikipedia.org/wiki/Maspar) (one of these SIMD machines), but I think didn't get any further.

- Using nested DO or FORALL constructs, nested data parallelism can be expressed in Fortran 95 and beyond.  However, parallelising Fortran compilers cannot exploit such parallelism properly.  For example, they may only parallelise the inner or outer loop.  Loop parallelisation of Fortran programs is however a broad topic and there are all kinds of approaches aiming to broaden the classes of loop nests that can be parallelised.  It's been a while since I looked at that stuff last.


Further interesting information about recent work on data parallelism is at [
http://groups.google.com/group/dataparallel](http://groups.google.com/group/dataparallel).


