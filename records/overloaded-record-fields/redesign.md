# `OverloadedRecordFields`: design overview






This is an attempt to redesign and clarify the design of the [OverloadedRecordFields](records/overloaded-record-fields) extension, in order to develop a plan for implementation.  It has benefited from the extensive discussion surrounding [Nikita Volkov's record library](records/volkov).  The following design choices are not set in stone, but are intended as a concrete proposal for further discussion.  For reference, here is the [previous design](records/overloaded-record-fields/design).



See also the [
high-level summary of the current plan on the Well-Typed blog](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/).


## Part 1: Records with duplicate fields



The `DuplicateRecordFields` extension permits existing Haskell records to declare duplicate field labels.  Thus the following is legal in a single module:


```wiki
data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }
```


We defer the ambiguity check to use sites. See [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) for more details.


## Part 2: Overloaded labels



Bare field names in expressions refer to the selector function only if unambiguous, so how are we want to select and update overloaded fields?  We need some way to refer to write fields in expressions in a way that allows them to be overloaded.  This is provided by the [OverloadedLabels extension](records/overloaded-record-fields/overloaded-labels).


## Part 3: Polymorphism over record fields



Once we have overloaded labels, we need [a little typeclass magic](records/overloaded-record-fields/magic-classes) to allow overloaded labels to refer to record fields.


## Summary



We propose three essentially orthogonal additions to GHC:


1. an extension `DuplicateRecordFields` to permit the same field name to be used multiple times in the same module;
1. an extension `OverloadedLabels` to enable the `#x` syntax, interpreted with the `IsLabel` typeclass;
1. typeclasses with special-purpose constraint solving behaviour to enable polymorphism over record fields. 


The `OverloadedRecordFields` extension is then defined as the combination of `OverloadedLabels` and `DuplicateRecordFields`.



These are all useful independently, but complement each other:


- `DuplicateRecordFields` is perfectly sensible without `OverloadedLabels`: it allows duplicate field names provided they are not used ambiguously.
- `OverloadedLabels` uses the special typeclasses through the instance for `IsLabel x (r -> a)`, but is also useful when used at other types (e.g. we could give an instance `IsLabel x (Proxy x)` to allow implicit values to represent Symbol proxies).
- Without either of the extensions, the special typeclasses allow users to write code that works for all datatypes with particular fields (albeit without a nice built-in syntax).

### Anonymous records



While we might choose to add anonymous records later, they are not central to the design.  In particular, this means that


- all existing features of Haskell datatypes, such as multiple constructors, strictness and unpacking, are supported unchanged;

- abstraction and representation hiding work just as in normal Haskell: if a field selector is not exported, client code cannot observe it;

- application code can use `DuplicateRecordFields` even with libraries that do not;

- no new declaration syntax is added.
