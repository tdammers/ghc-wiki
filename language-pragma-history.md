# `{-# LANGUAGE #-}` Pragma History



Here is the list of language extensions added (and sometimes removed) by the Glasgow Haskell Compiler (GHC) for each major version release.  This may be useful for those interested in seeing whether an extension is available on an older version of GHC for compatibility reasons.



See also [GHC Boot Library Version History](commentary/libraries/version-history), which tabulates the GHC versions against the versions of the `base` library and other boot libraries.



See also [
http://damianfral.github.io/ghcaniuse/](http://damianfral.github.io/ghcaniuse/).


## Changes in GHC 8.6



Added LANGUAGE extensions:


- `[No]BlockArguments`
- `[No]DerivingVia`
- `[No]NumericUnderscores`
- `[No]QuantifiedConstraints`
- `[No]StarIsType`

## Changes in GHC 8.4



Added LANGUAGE extensions:


- `[No]EmptyDataDeriving`
- `[No]HexFloatLiterals`

## Changes in GHC 8.2



Added LANGUAGE extensions:


- `[No]DerivingStrategies`
- `[No]UnboxedSums`

## Changes in GHC 8.0



Added LANGUAGE extensions:


- `[No]ApplicativeDo`
- `[No]DeriveLift`
- `[No]DuplicateRecordFields`
- `[No]TypeFamilyDependencies`
- `[No]MonadFailDesugaring`
- `[No]OverloadedLabels`
- `[No]Strict`
- `[No]StrictData`
- `[No]TemplateHaskellQuotes`
- `[No]TypeApplications`
- `[No]TypeInType`
- `[No]UndecidableSuperClasses`

## Changes in GHC 7.10



Added LANGUAGE extensions:


- `[No]BinaryLiterals`
- `[No]DeriveAnyClass`
- `[No]NamedWildCards`
- `[No]PartialTypeSignatures`
- `[No]StaticPointers`

## Changes in GHC 7.8



Added LANGUAGE extensions:


- `[No]AllowAmbiguousTypes`
- `[No]AutoDeriveTypeable`
- `[No]EmptyCase`
- `[No]JavaScriptFFI`
- `[No]NegativeLiterals`
- `[No]NullaryTypeClasses`
- `[No]NumDecimals`
- `[No]OverloadedLists`
- `[No]PatternSynonyms`
- `[No]RoleAnnotations`

## Changes in GHC 7.6



Added LANGUAGE extensions:


- `[No]ExplicitNamespaces`
- `[No]InstanceSigs`
- `[No]LambdaCase`
- `[No]MultiWayIf`

## Changes in GHC 7.4



Added LANGUAGE extensions:


- `Unsafe`
- `[No]CApiFFI`
- `[No]ConstraintKinds`
- `[No]DataKinds`
- `[No]PolyKinds` (Warning: `PolyKinds` yields broken interface files in 7.4! This was fixed in 7.6)
- `[No]TraditionalRecordSyntax`


Removed LANGUAGE extensions:


- `SafeImports`

## Changes in GHC 7.2



Added LANGUAGE extensions:


- `Safe`
- `SafeImports`
- `Trustworthy`
- `[No]DefaultSignatures`
- `[No]DeriveGeneric`
- `[No]GADTSyntax`
- `[No]InterruptibleFFI`
- `[No]MonadComprehensions`
- `[No]NondecreasingIndentation`
- `[No]ParallelArrays`
- `[No]RelaxedLayout`


Removed LANGUAGE extensions:


- `[No]Generics`
- `[No]NewQualifiedOperators`
- `[No]PArr`

## Changes in GHC 7.0 (since GHC 6.12)



Added LANGUAGE extensions:


- `Haskell2010`
- `Haskell98`
- `[No]AlternativeLayoutRule`
- `[No]AlternativeLayoutRuleTransitional`
- `[No]DatatypeContexts`
- `[No]DoAndIfThenElse`
- `[No]RebindableSyntax`

## Language pragmas existing prior to GHC 7.0



TODO


