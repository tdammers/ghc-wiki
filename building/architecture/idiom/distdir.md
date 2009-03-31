# Idiom: distdir



Often we want to build a component multiple times in different ways.  For example:


- certain libraries (e.g. Cabal) are required by GHC, so we build them once with the
  bootstrapping compiler, and again with stage 1 once that is built.

- GHC itself is built multiple times (stage 1, stage 2, maybe stage 3)

- some tools (e.g. ghc-pkg) are also built once with the bootstrapping compiler,
  and then again using stage 1 later.


In order to support multiple builds in a directory, we place all generated files in a subdirectory, called the "distdir".  The distdir can be anything at all; for example in `compiler/` we name our distdirs after the stage (`stage1`, `stage2` etc.).  When there is only a single build in a directory, by convention we usually call the distdir simply "dist".



There is a related concept called *ways*, which includes profiling and dynamic-linking.  Multiple ways are currently part of the same "build" and use the same distdir, but in the future we might unify these concepts and give each way its own distdir.


