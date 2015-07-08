



Video: [
Abstract Syntax Types](http://www.youtube.com/watch?v=lw7kbUvAmK4&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) (1hr03')


# The `HsSyn` types



The program is initially parsed into "**`HsSyn`**", a collection of data types that describe the full abstract syntax of Haskell.  `HsSyn` is a pretty big collection of types: there are 52 data types at last count.  Many are pretty trivial, but a few have a lot of constructors (`HsExpr` has 40).  `HsSyn` represents Haskell in its full glory, complete with all syntactic sugar.



The `HsSyn` modules live in the [compiler/hsSyn](/trac/ghc/browser/ghc/compiler/hsSyn) directory.  Each module declares a related group of declarations, *and* gives their pretty-printer.


- [compiler/hsSyn/HsSyn.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.hs): the root module.  It exports everything you need, and it's generally what you should import.
- [compiler/hsSyn/HsBinds.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsBinds.hs): bindings.
- [compiler/hsSyn/HsImpExp.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsImpExp.hs): imports and exports.
- [compiler/hsSyn/HsDecls.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsDecls.hs): top-level declarations.
- [compiler/hsSyn/HsExpr.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsExpr.hs): expressions, match expressions, comprehensions.
- [compiler/hsSyn/HsLit.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsLit.hs): literals.
- [compiler/hsSyn/HsPat.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsPat.hs): patterns.
- [compiler/hsSyn/HsTypes.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsTypes.hs): types.
- [compiler/hsSyn/HsUtils.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsUtils.hs): utility functions (no data types).


There is significant mutual recursion between modules, and hence a couple of `hs-boot` files. Look at [ModuleDependencies](module-dependencies) to see the dependencies.


## Decorating `HsSyn` with type information



The type checker adds type information to the syntax tree, otherwise leaving it as undisturbed as possible.  This is done in two ways:


- Some constructors have a field of type `PostTcType`, which is just a synonym for `Type`. For example:

  ```wiki
  data HsExpr id = ... | ExplicitList PostTcType [LHsExpr id] | ...

  type PostTcType = Type

  placeHolderType :: PostTcType
  placeHolderType = panic "Evaluated the place holder for a PostTcType"
  ```

  An `ExplicitList` represents the explicit list construct in Haskell (e.g. "`[2, 4, 1]`"). The parser fills the `PostTcType` field with an error thunk `HsTypes.placeHolderType`; and the renamer does not touch it.  The typechecker figures out the type, and fills in the value.  So until the type checker, we cannot examine or print the `PostTcType` fields.

>
>
> The error thunks mean that we can't conveniently pretty-print the `PostTcType` fields, because the pretty-printer would poke the error thunks when run on pre-typchecked code.  We could have defined `PostTcType` to be `Maybe Type`, but that would have meant unwrapping lots of `Just` constructors, which is messy.  It would be nicer to parameterise `HsSyn` over the `PostTcType` fields.  Thus:
>
>
> ```wiki
>   type RnHsBinds = HsBinds Name ()   -- After renaming
>   type TcHsBinds = HsBinds Id Type   -- After type checking
> ```
>
>
> This would be a Good Thing to do.
>
>

- In a few cases, the typechecker moves from one constructor to another.  Example:

  ```wiki
  data HsPat id
    = ...
    | ConPatIn	(Located id)
  		(HsConDetails id (LPat id))

    | ConPatOut	(Located DataCon)
  		[TyVar]			-- Existentially bound type variables
  		[id]			-- Ditto dictionaries
  		(DictBinds id)		-- Bindings involving those dictionaries
  		(HsConDetails id (LPat id))
  		Type    		-- The type of the pattern
    ...
  ```

  The parser and renamer use `ConPatIn`; the typechecker generates a `ConPatOut`. This naming convention is used consistently.

- There are a few constructors added by type checker (rather than replacing an input constructor), particularly:

  - `HsWrap`, in the `HsExpr` type.
  - `AbsBinds`, in the `HsBinds` type.

>
>
> These are invariably to do with type abstraction and application, since Haskell source is implicitly generalized and instantiated, whereas GHC's intermediate form is explicitly generalized and instantiated.
>
>

## Source Locations



`HsSyn` makes heavy use of the `Located` type ([compiler/basicTypes/SrcLoc.hs](/trac/ghc/browser/ghc/compiler/basicTypes/SrcLoc.hs)):


```wiki
data Located e = L SrcSpan e
```


A `Located t` is just a pair of a `SrcSpan` (which describes the source location of `t`) and a syntax tree `t`.  The module `SrcLoc` defines two other types:


- `SrcLoc` specifies a particular source location: (filename, line number, character position)
- `SrcSpan` specifes a range of source locations: (filename, start line number and character position, end line number and character position)


More details in [compiler/basicTypes/SrcLoc.hs](/trac/ghc/browser/ghc/compiler/basicTypes/SrcLoc.hs).



Naming convention within the code: "`LHs`" means located Haskell, e.g.


```wiki
type LHsBinds n = Located (HsBinds n)
```