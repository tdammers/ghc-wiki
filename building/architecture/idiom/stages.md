# Idiom: stages



What do we use to compile GHC?  GHC itself, of course.  In a complete build we actually build GHC twice: once using the GHC version that is installed, and then again using the GHC we just built.  To be clear about which GHC we are talking about, we number them:


- **Stage 0** is the GHC you have installed.  The "GHC you have installed" is also called "the bootstrap compiler".
- **Stage 1** is the first GHC we build, using stage 0.  Stage 1 is then used to build the packages.
- **Stage 2** is the second GHC we build, using stage 1.  This is the one we normally install when you say `make install`.
- **Stage 3** is optional, but is sometimes built to test stage 2.


Stage 1 does not support interactive execution (GHCi) and Template Haskell.  The reason being that when running byte code we must dynamically link the packages, and only in stage 2 and later can we guarantee that the packages we dynamically link are compatible with those that GHC was built against (because they are the very same packages).


