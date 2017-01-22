# Checklist for submitted patches



This is the beginning of a checklist to check for patches submitted for merge.


- Includes regression test?

- Does patch add a user-visible command-line flag?

  - Document in users guide (`docs/users_guide`) and `utils/mkUserGuidePart/Options/`

- Does patch change a core library (e.g. `base`)?

  - Has the Core Libraries Committee approved?
  - Document change in migration guide on Wiki
  - Document change in the library's changelog
  - Document change in the users guide release notes

- Introduces new syntax?

  - Document in `docs/users_guide/glasgow_exts.rst`
  - Document in `docs/users_guide/*-relnotes.rst`
  - Add Template Haskell support

- Builds with last two releases of GHC?
