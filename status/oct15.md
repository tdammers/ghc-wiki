# GHC Status Report, October 2015



GHC development spurs on, with an exciting new announcement - the next release will be a super-major one, culminating in **GHC 8.0**. There are many reasons for this change, but one of the most exciting is that GHC is getting a **completely new core language**. While this adds a bit of complexity to the compiler, it paves the way to implement **Dependent Haskell** over the course of the next few years.



On top of that, we've also done a ton of other work over the past half year, including...


# Major changes in GHC 8.0.1


- **Support for simple, implicit callstacks with source locations** \[ImplicitCallstacks\] implicit parameters providing callstacks/source locations\], allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([
  Phab:D861](https://phabricator.haskell.org/D861))

- **Improved optimization diagnostics**. The compiler is now more liberal about issues warnings of potentially non-firing rewrite rules and other potential gotchas.

- Support for wildcards in data and type family instances ([
  Phab:D1092](https://phabricator.haskell.org/D1092))

- **Injective type families** \[[InjectiveTypeFamilies](injective-type-families)\]. Injective TFs allow you to specify type families which are injective, i.e. have a one-to-one relationship. ([
  Phab:D202](https://phabricator.haskell.org/D202)).

- **Applicative do notation** \[[ApplicativeDo](applicative-do)\]. With the new `-XApplicativeDo`, GHC tries to desugar do-notation to `Applicative` where possible, giving a more convenient sugar for many common Applicative expressions. ([
  Phab:D729](https://phabricator.haskell.org/D729))

- **Support for deriving the `Lift` typeclass** - a very common need when working with Template Haskell. ([
  Phab:D1168](https://phabricator.haskell.org/D1168))

- **A PowerPC 64bit code generator**. The new native codegen supports Linux/ppc64 in both big endian and little endian mode. ([
  Phab:D629](https://phabricator.haskell.org/D629)).

- **A beautiful new users guide**. Now rewritten in reStructured Text, and with significantly improved output and documentation.

- **Visible type application** - \[[ExplicitTypeApplication](explicit-type-application)\]. This allows you to say, for example `id @Bool` to specialize `id` to `Bool -> Bool`. With this feature, proxies are never needed.

- **Kind Equalities**, which form the first step to building Dependent Haskell. This feature enables promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. ([
  Phab:D808](https://phabricator.haskell.org/D808))

- **Strict Haskell support**. This includes new `-XStrictData` and `-XStrict` language extensions.

- Support for record pattern synonyms ([
  Phab:D1152](https://phabricator.haskell.org/D1152))

- **Implement phase 1 of the `MonadFail` proposal**. ([\#10751](http://gitlabghc.nibbler/ghc/ghc/issues/10751))

- **Overloaded record fields** \[[OverloadedRecordFields](overloaded-record-fields)\]. At long last, ORF will finally be available in GHC 8.0, allowing multiple uses of the same field name and a form of type-directed name resolution.

- A huge improvement to pattern matching (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [
  their paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf).

- **More Backpack improvements**. There's a new user-facing syntax which allows multiple modules to be defined a single file, and we're hoping to release at least the ability to publish multiple "units" in a single Cabal file.

- **Support for DWARF based stacktraces** \[DWARF\]. from Peter Wortmann, Arash Rouhani, and Ben Gamari with backtraces from Haskell code.

- **A better LLVM backend** \[ImprovedLLVMBackend\]. We're planning on a major build system change that will ship GHC 8.0 with a pre-baked copy of LLVM 3.7.0, that ships with every major Tier 1 platform.

# Upcoming post-8.0 plans



Naturally, there were several things we didn't get around to this cycle, or things which are still in flight and being worked on. (And you can always try to join us if you want something done!)


## Libraries, source language, type system


- A new, type-indexed type representation, `data TTypeRep (a :: k)`. See [TypeableT](typeable-t).
- Support for **Type Signature Sections**, allowing you to write `(:: ty)` as a shorthand for `(\x -> x :: ty)`.
- A (possible) overhaul of GHC's build system to use **Shake** instead of Make.
- A `DEPRECATED` pragma for exports ([\#4879](http://gitlabghc.nibbler/ghc/ghc/issues/4879))

## Back-end and runtime system



Lorem ipsum...


## Frontend, build system and miscellaneous changes



Lorem ipsum...


# Development updates and "Thank You"s



Lorem ipsum...


# References


- \[[ExplicitTypeApplication](explicit-type-application)\] [
  https://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication](https://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication)
- \[ImplicitCallstacks\] [
  https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations](https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations)
- [\[ImprovedLLVMBackend](improved-llvm-backend)\] [
  https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend](https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend)
- \[[InjectiveTypeFamilies](injective-type-families)\] [
  https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies)
- \[KindEqualities\] [
  https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1)
- \[[OverloadedRecordFields](overloaded-record-fields)\] [
  https://ghc.haskell.org/trac/ghc/wiki/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/OverloadedRecordFields)

- \[[ApiAnnotations](api-annotations)\] [
  https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations](https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations)
- \[[ApplicativeDo](applicative-do)\] [
  https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo)
- \[Backpack\] TODO FIXME
- \[[DistributedHaskell](distributed-haskell)\] [
  https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell](https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell)
- \[DWARF\] [
  https://ghc.haskell.org/trac/ghc/wiki/DWARF](https://ghc.haskell.org/trac/ghc/wiki/DWARF)
- [
  FCkinds](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf) System FC with explicit kind equality, Weirich, Hsu, and Eisenberg, ICFP 13. [
  http://www.seas.upenn.edu/\~eir/papers/2013/fckinds/fckinds-extended.pdf](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf)
- \[[GhcApi](ghc-api)\] [
  https://ghc.haskell.org/trac/ghc/wiki/GhcApi](https://ghc.haskell.org/trac/ghc/wiki/GhcApi)
- [\[ImprovedLLVMBackend](improved-llvm-backend)\] [
  https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend](https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend)
- \[[InjectiveTypeFamilies](injective-type-families)\] [
  https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies)
- \[KindEqualities\] TODO FIXME
- [Records/OverloadedRecordFields](records/overloaded-record-fields) [
  https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields)
- \[[PartialTypeSignatures](partial-type-signatures)\] [
  https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures](https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures)
- [ \[PPC64-NCG](https://phabricator.haskell.org/D629)\] [
  https://phabricator.haskell.org/D629](https://phabricator.haskell.org/D629)
- [\[Prelude710](prelude710)\] [
  https://ghc.haskell.org/trac/ghc/wiki/prelude710](https://ghc.haskell.org/trac/ghc/wiki/prelude710)
- [\[Shake](building/shake)\] [
  https://ghc.haskell.org/trac/ghc/wiki/Building/Shake](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake)
- \[[StaticPointers](static-pointers)\] [
  https://ghc.haskell.org/trac/ghc/wiki/StaticPointers](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers)
- [\[TCPlugins](plugins/type-checker)\] [
  https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker](https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker)
- [ \[TCSMT](https://github.com/yav/type-nat-solver)\] [
  https://github.com/yav/type-nat-solver](https://github.com/yav/type-nat-solver)
- [
  \[TCSMT\_paper](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)\] [
  https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)
- [Typeable](typeable) [
  https://ghc.haskell.org/trac/ghc/wiki/Typeable](https://ghc.haskell.org/trac/ghc/wiki/Typeable)
