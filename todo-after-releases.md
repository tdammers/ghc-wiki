## Things to do after releases are made


- Update default GHC version for new Trac tickets. 

- Update [Commentary/Libraries/VersionHistory](commentary/libraries/version-history).

- Delete `__GLASGOW_HASKELL__` ifdefs for versions of ghc older than the [previous 2 major releases](building/preparation/tools). Something like (add versions which you want to keep the ifdefs for, remove versions you want to delete them for):

  ```
  git grep -l GLASGOW_ | grep -v 'users_guide' | xargs grep GLASGOW_ | grep -v '712\|711\|710\|709\|708'
  ```