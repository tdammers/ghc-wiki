# Unpacking primitive fields



This page describes a proposal to automatically unpack (strict) primitive fields. A primitive fields is a field that when unpacked has a pointer-sized representation. Examples include `Int`, `Word`, `Float`, and `newtype`s thereof.


## Goals and non-goals



This proposal is about changing the default behavior of GHC, not changing expressiveness. Users can still use `UNPACK` and `NOUNPACK` to explicitly control the memory representation of fields.



There are two goals:


1. Reduce the amount of boilerplate experienced programmers have to write: As of Feb 18th 2012, the [
  bytestring](http://hackage.haskell.org/package/bytestring), [
  text](http://hackage.haskell.org/package/text), and [
  containers](http://hackage.haskell.org/package/containers) packages had 46 fields that matched the definition of primitive given above. 43 of these had an explicit `UNPACK` pragma (and the remaining 3 could have had one without changing the performance of the program.)

1. To provide better defaults for beginner and intermediate level Haskellers. Not unpacking e.g. `Int` fields can have a large, negative effect on performance and many beginner and intermediate level Haskellers are bitten by this.

## Detailed design


## Benchmarks


