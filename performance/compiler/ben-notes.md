# Ben's notes on compiler performance


## Callstack solver and local lets



[3ec8288a18d57fb856e257905897daae237a1d5d](/trac/ghc/changeset/3ec8288a18d57fb856e257905897daae237a1d5d/ghc) (Rework the Implicit CallStack solver to handle local lets) decreased compiler allocations on some tests.
Later [7407a66d5bd29aa011f5a4228c6e2b2f7f8ad3f8](/trac/ghc/changeset/7407a66d5bd29aa011f5a4228c6e2b2f7f8ad3f8/ghc) (Don't infer CallStacks) undid many of these gains. TODO Why?
See, for instance, `anna/Dependency`, where the former decreases allocations by 38%, and the latter increases them by 25%, with a corresponding \~5% increase in compile time.


## nokinds



See [\#11196](http://gitlabghc.nibbler/ghc/ghc/issues/11196). Unfortunately, the commit of nokinds merge ([6746549772c5cc0ac66c0fce562f297f4d4b80a2](/trac/ghc/changeset/6746549772c5cc0ac66c0fce562f297f4d4b80a2/ghc)) had a few issues rendering it untestable. This means that the regressions are attributed to an unrelated commit ([b5d5d83122c93c2a25839127edfd6b2df7ed6928](/trac/ghc/changeset/b5d5d83122c93c2a25839127edfd6b2df7ed6928/ghc), Revert .gitmodules changes from 6746549772c5).



Along with introducing runtime-rep polymorphism, the `nokinds` merge  eliminated the `FunTy` constructor in favor of a (likely) larger representation. This was undone by SPJ in [77bb09270c70455bbd547470c4e995707d19f37d](/trac/ghc/changeset/77bb09270c70455bbd547470c4e995707d19f37d/ghc). This commit was sadly untestable but allocations didn't change terribly much between the two surrounding commits ([e368f3265b80aeb337fbac3f6a70ee54ab14edfd](/trac/ghc/changeset/e368f3265b80aeb337fbac3f6a70ee54ab14edfd/ghc) and [e33ca0e54f3c20a8b233a3f7b38e4968a4955300](/trac/ghc/changeset/e33ca0e54f3c20a8b233a3f7b38e4968a4955300/ghc)).


## Adding instances



There are a few examples where adding instances to `base` modules increases compiler allocations, and often compile time, remarkably.


- [673efccb3b348e9daf23d9e65460691bbea8586e](/trac/ghc/changeset/673efccb3b348e9daf23d9e65460691bbea8586e/ghc) (Add more type class instances for GHC.Generics): See [\#12367](http://gitlabghc.nibbler/ghc/ghc/issues/12367)
- [4e6bcc2c8134f9c1ba7d715b3206130f23c529fb](/trac/ghc/changeset/4e6bcc2c8134f9c1ba7d715b3206130f23c529fb/ghc) (Add various instances to newtypes in Data.Monoid)


I suspect these will both be addressed by ed480981/ghc, which fixes a bug (introduced by [4c834fdddf4d44d12039da4d6a2c63a660975b95](/trac/ghc/changeset/4c834fdddf4d44d12039da4d6a2c63a660975b95/ghc) and perhaps worsened by later commits) in instance resolution which makes interface loading significantly more eager than necessary.


## Tuple constraint size



Rather significant regression due to [dd3080fe0263082f65bf2570f49189c277b12e28](/trac/ghc/changeset/dd3080fe0263082f65bf2570f49189c277b12e28/ghc). Ended up being due to a redundant lookup in the original name cache. See [\#12357](http://gitlabghc.nibbler/ghc/ghc/issues/12357).


## fltk



See [\#12506](http://gitlabghc.nibbler/ghc/ghc/issues/12506).


