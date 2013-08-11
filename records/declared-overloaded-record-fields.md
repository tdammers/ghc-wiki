# Declared Overloaded Record Fields (DORF)


## Thumbnail Sketch



This proposal is addressing the narrow issue of **namespacing for record field names** by allowing more than one record in the same module to share a field name. Specifically each field name is overloaded so that:


- Within the same module, many record types can be declared to share the field name.
- The field name can be exported so that records in other modules can share it.
- Furthermore, other modules can create records using that field name, and share it.


The export/import is under usual H98 namespace and module/qualification control, so that for the record type in an importing module:


- Some fields are both readable and updatable;
- Some are read-only;
- Some are completely hidden.


In case of 'unintended' clash (another module using the same name 'by accident'), usual H98 controls apply to protect encapsulation and representation hiding.



This proposal introduces several new elements of syntax, all of which desugar to use well-established extensions of ghc. The approach has been prototyped in ghc v 7.2.1. In particular:


- The field name overloading is implemented through usual class and instance mechanisms.
- Field selectors are ordinary functions named for the field (but overloaded rather than H98's monomorphic), so field selection is regular function application. (There is no need for syntactically-based disambiguation at point of use.)

### Implementation: the `Has` class, with methods `get` and `set`



Record declarations generate a `Has` instance for each record type/field combination. As well as type arguments for the record and field, there is a third argument for the field's resulting type. This is set at the instance level using equality constraints in a functional-dependencies style. Here is the `Has` class (`r` is the record, `fld` is the proxy type for the field, `t` is the field's type), with an example record declaration, its `Has` instance, and examples of use:


```wiki
    class Has r fld t                                             where
        get :: r -> fld -> t                                            -- simplified form
        set :: fld -> t -> r -> r                                       -- where not changing record's type

    data Customer = Cust{ customer_id :: Int, ... }                     -- declaration syntax same as H98

    instance (t ~ Int) => Has Customer Proxy_customer_id t        where -- Has instance generated, with ~ constraint
        get Cust{ customer_id } _ = customer_id                         -- DisambiguateRecordFields pattern
        set _ x Cust{ .. }        = Cust{ customer_id = x, .. }         -- RecordWildCards and NamedFieldPuns

    myCust :: Customer                                                  -- usual record decl
    ... myCust{ customer_id = 27 }                                      -- polymorphic record update
    ... (customer_id myCust) ...                                        -- field selection is func apply, or:
    ... myCust.customer_id ...                                          -- dot notation is sugar for reverse func apply
```


Note that the** `Has` mechanism** uses a Proxy as the type 'peg' for a field (this is the wildcard argument to `get` and `set`):


- There must be a Proxy\_type declared for each distinct field name.
- The Proxy must be declared once, and the Proxy is then under regular name control.
- The field selector function also must be declared once, defined using the Proxy.

>
> >
> >
> > It is an error to declare a record field without there being a Proxy in scope. The desugar for the data decl would create the instance to use the Proxy, but then the instance would fail.
> >
> >
>


To generate the correct declarations, there is to be a new `fieldLabel` sugar:


```wiki
    fieldLabel customer_id :: r -> Int                                  -- new declaration, desugars to Proxy and func:
    data Proxy_customer_id                                              -- phantom
    customer_id :: r{ customer_id :: Int } => r -> Int                  -- r{ ... } is sugar for Has constraint
    customer_id r = get r (undefined :: Proxy_customer_id)

    set (undefined :: Proxy_customer_id) 27 myCust                      -- record update desugarred from above example
```

- (Admittedly, this could get onerous to declare a `fieldLabel` for every field, even the ones that appear in a single record type. See "Option Three: Mixed In-situ and DeclaredORF: " further down this page for a suggestion of using the DORF mechanism to generate one-off H98-style fields.)


**Virtual** or **pseudo-** fields are easy to create and use, because field selection is merely function application. Virtual fields look like ordinary fields (but can't be updated, because there is no `Has` instance):


```wiki
    fullName r = r.firstName ++ " " ++ map toUpper r.lastName           -- example adapted from SPJ
                                                                        -- dot notation binds tighter than func apply
    fullName :: r{ firstName :: String, lastName :: String} => r -> String
                                                                        -- type inferred for fullName
                                                                        -- the Has constraints use elided syntax
```


**Technical capabilities** and limitations for the `Has` class:


- Monomorphic fields can be `get` and `set`.
- Parametric polymorphic fields can be applied in polymorphic contexts, and can be `set` including changing the type of the record.
- Higher-ranked polymorphic fields can be applied in polymorphic contexts, but cannot be set -- for the same reasons as under SORF.
  The instances use equality constraints to 'improve' types up to polymorphic.
- `Has` uses type family functions to manage type-changing update, which adds complexity -- see Implementer's view.
- Multiple fields can be updated in a single expression (using familiar H98 syntax), but this desugars to nested updates, which is inefficient.
- Pattern matching and record creation using the data constructor prefixed to { ... } work as per H98 (using `DisambiguateRecordFields` and friends).


.


## DORF Full motivation and examples



Explained in 5 wiki pages (these proposals are linked but somewhat orthogonal):


- ** [No Record Selector Functions](records/declared-overloaded-record-fields/no-mono-record-fields) **   (precursor to DORF)
- ** DORF -- Application Programmer's view **     (this page)
- ** [DORF -- Implementer's view](records/declared-overloaded-record-fields/implementors-view) **
- ** [DORF -- Comparison to SORF (and TDNR)](records/declared-overloaded-record-fields/c-ompare-sorf) **
- ** [Dot as Postfix Function Apply](records/declared-overloaded-record-fields/dot-postfix) **   (***optional*** syntactic sugar)
- ** [Polymorphic Record Patterns](records/declared-overloaded-record-fields/poly-record-pattern) **   (***speculative*** future)

## Application Programmer's view



This proposal is addressing the "narrow issue" of namespacing for record field names.
[Records](records)



I'm avoiding giving implementation details here -- see:


>
>
> The Implementer's view; and Comparison to SORF   (links above)
>
>


I'm not saying anything about field selection via pattern matching or record construction using explicit data constructors -- those are to behave as currently (using the approach per ‑XDisambiguateRecordFields and friends).



Currently in Haskell two records in the same module can't share a field name. This is because declaring a field name within a data decl creates a selector function bound to that record type; and if it's single-record, we can only have one. I think the wiki is characterising the problem incorrectly:


- it's not that the field name appearing in different record decls is ambiguous between the two record types
  so we need some (syntactical) way of choosing between the different definitions;

- rather, we have one field name, and we lack the syntax/semantics for sharing it between different records.


An example: let's say I have a database application with a field (meaning type) `customer_id`. Then it appears in records for name and address, pricing, order entry, etc. This is not a name 'clash', it's 'intended sharing'. (It really galls me to even put it that way for explanatory purposes. Really it's the **same** `customer_id`.)



In data model design you'd typically go about identifying all the fields (types aka attributes) and putting them in a data dictionary. Then you'd construct your records from them. You might (possibly) put the data dictionary in a distinct module, for easy maintenance. But you'd certainly want all the customer-related records in the same module. So a data decl:


```wiki
    data Customer_NameAddress = Cust_NA{ customer_id :: Int, ... } 
```


is not declaring `customer_id`, it's using (or instancing) an already-declared field for `customer_id`.
Similarly, if I have a family of objects, all with a `reset` method, that's not umpteen methods with a 'clash' of names, it's one method with umpteen instances. (And I might create a family of record structures to describe each object, and store the `reset` method within it.)



What's more, the Haskell 98 field selector (auto-created from the data decl) is half-way to what we want. It's a function:


```wiki
    customer_id :: Customer_NameAddress -> Int
```


The DORF proposal generalises that signature: if you want to share a field across different records, its selector function needs to be overloaded to this type:


```wiki
    customer_id :: r{ customer_id :: Int } => r -> Int
```


The `r{ ... }` is syntactic sugar for the constraint meaning "record `r` has field `customer_id` at type `Int`".



We need a way to declare that a name is available as an overloadable field name (roughly speaking, a class/method definition), proposed syntax:



**Option One: new `fieldLabel` style of declaration:**


```wiki
    fieldLabel customer_id :: r -> Int
```

>
>
> (The `r{ ... }` is added by the desugarer.)
>
>


**Option Two: explicit record constraint on the function:**


```wiki
    customer_id :: r{ customer_id :: Int} => r -> Int          -- field name same as the declared function
```

>
>
> (See discussion at [Wild afterthought](records/declared-overloaded-record-fields/c-ompare-sorf#the-string-type-parameter-to-has-,-and-scope-control).)
>
>


**Option Three: Mixed In-situ and Declared ORF:**


>
>
> \[Added 3-March in response to concerns at the extra effort needed to declare a `fieldLabel` for every field, not just the shared ones.\]
>
>


Provide a way of 'flagging' in the record declaration whether field names are intended to be shared. Possible syntax:


```wiki
    data Cust_AdHoc = CustAH{ customer_id :: Int, x, y :: String } sharing (customer_id) deriving (...)
```

- Fields listed as `sharing` must have the `fieldLabel` declared separately (per Option One or Two).
- Fields not `sharing` will get a fieldLabel declared for them,
   and it will be bound to a single record type.


Or perhaps:


```wiki
    data Customer_Order = Cust_Order { customer_id :: Int, order_num :: Int, ... }
                          sharing (customer_id) share (order_num) deriving (...)
```


That is:


- for `share` fields, this is declaring them as sharable.


** [Option Four: Type Punning on the \`fieldLabel\`](records/declared-overloaded-record-fields/option-four-type-punning) ** q.v.


>
>
> \[End of 3-March addition.\]
>
>


The field selector's result type `-> Int` means the field's domain (type) is `Int` -- it's just a type.
We might also want to constrain the record -- for example to be sure it is savable to persistent storage:


```wiki
    fieldLabel unitPrice :: (Save r, Num t) => r -> t    -- again the `r{ ... }` gets added as a further constraint
-- or
    unitPrice :: (r{ unitPrice :: t}, Save r, Num t) => r -> t     -- again repeated field name
```


Now we can use the field in a record, and that in effect declares an instance for the field/record. All these definitions are in the same module:


```wiki
    data Customer_NameAddress = ... (as above)
    data Customer_Price a = Num a => Cust_Price {
                                       customer_id :: Int,
                                       product_id  :: Int,
                                       unit_Price  :: a,
                                       ... }
    data Customer_Order = Cust_Order { customer_id :: Int, ... }
```

### Field Selection



With those records declared, a field selection expression like:


>
>
> `... (customer_id r) ...`          -- H98 style field application
>
>


uses familiar type instance resolution to figure out from record type `r` how to extract the `customer_id`.



\[Possibly that expression could be:


>
>
> `... r.customer_id ...`
>
>


See [Dot as Postfix Function Apply](records/declared-overloaded-record-fields/dot-postfix) for that dot notation, but note that nothing in this proposal assumes dot notation will be needed.\]



From here upwards, the `r{ ... }` constraint is just a constraint, and gets merged with other constraints. For example, you could define a function:


```wiki
    fullName r = (firstName r) ++ " " ++ (lastName r)  -- per SPJ
```


The type inferred would be:


```wiki
    fullName :: r{ firstName, lastName :: String} => r -> String               -- could declare this for yourself
                                                 -- note this is __not__ like a field label decl (Option Two)
                                                 -- because the function's name is different to the field(s)
```


which is eliding:


```wiki
    fullName :: (r{ firstName :: String}, r{ lastName :: String })
                 => r -> String
```


And if you think that's very close to the type of a field selector function, you'd be right. Here's some more examples of field selection using **pseudo-** or** 'virtual' **fields, with dot notation:


```wiki
    customer.fullName
    shape.area
    date.dayOfWeek        -- not a field: calculated from the date
    name.middleInitial    -- extract from the name field
    tuple.fst             -- Prelude functions
    list.head
    list.length
```


\[Since they're just functions, they can use dot notation -- or not: personal preference.\]


### Modules and qualified names for records



Do these field selector functions have a special scope in some way? No! They're just functions. They can be exported/imported.



We can't stop some other developer creating an application/package with a field `customer_id` which is incompatible with ours. (Say a Sales Order entry application where `customer_id` is a `String`, to merge with our Accounts Receivable.) So do we have a problem if someone wants to import both?



No! This is regular business-as-usual familiar name clash, and it's what the module system is designed to handle. The field selectors are just functions, we can use them qualified:


```wiki
    (My.customer_id myCust)        <===> myCust.My.customer_id
    (Their.customer_id theirCust)  <===> theirCust.Their.customer_id
    (My.customer_id r)       -- fails if r is from the 'other' module
```

### Import/Export and Representation hiding



\[See [No Record Selector Functions](records/declared-overloaded-record-fields/no-mono-record-fields), which is implied by DORF.\]



Since there is only a single (overloaded) field selector function created, we either have to export it always, or hide it always (that is, we can't control which record instances get exported).



But we can control at a record and field level how much of the representation gets revealed.



The field selector function is separately declared vs. the records and their fields, so must be exported separately. For example:


```wiki
{-# OPTIONS_GHC -XDeclaredOverloadedRecordFields             #-}
module M( x, T )       where
    fieldLabel x,y :: r -> Int
    data T = MkT { x, y :: Int }
```


Here only the field selector function `x` and type `T` are exported. The representation is abstract, the client can't construct or dismantle a record type `T`;


>
>
> The existence of field `y` is hidden altogether.
>
>


If you say:


```wiki
{-# OPTIONS_GHC -XDeclaredOverloadedRecordFields
                -XNoRecordSelectorFunctions              #-}
module M( T( x ) )       where
    fieldLabel x,y :: r -> Int
    data T = MkT { x, y :: Int }
```


then you are exporting the `x` field within record type `T`, but not the field selector `x` (nor the generated type 'peg' `Proxy_x`).



Type `T` and field label `x` are exported, but not data constructor `MkT`, so `x` is unusable. (It can't even be used to update an existing record using syntax: `r{ x = 57 }`, because that syntax now has a different semantics.)


>
>
> The existence of field `y` is hidden altogether.
>
>


With:


```wiki
module CRM               where
    import CUST hiding (firstName, lastName)          -- note import is __not__ qualified

    fieldLabel firstName :: r -> String
    fieldLabel lastName :: r -> String

    data Customer_Contact = Cust_Cont { customer_id :: Int, firstName, lastName :: String }

```


We're sharing fieldLabel `customer_id`, but we've got local fieldLabels for the names. There is no name clash! If you want to use the imported name labels, you have to qualify as `CUST.lastName`.



Then this works:


```wiki
    contact1 :: Customer_Contact
    custAddr1 :: Customer_NameAddress
    ...
    ... contact1.customer_id ...                         -- shared fieldLabel
    ... custAddr1.customer_id ...                        --
    ...
    ... contact1.firstName ...                           -- local fieldLabel
    ... custAddr1.CUST.firstName ...                     -- imported fieldLabel used qualified
    ... 
    ...
    localfullName r = r.firstName ++ " " r.lastName
```


This doesn't:


```wiki
    ... custAddr1.firstName ... -- ==> No instance for (Has Customer_Contact Proxy_firstName t0)
                                --     tried to use the local fieldLabel against an imported record type
    ... contact1.fullName ...   -- ==> No instances for (Has Customer_Contact CUST.Proxy_firstName t0,
                                --                       Has Customer_Contact CUST.Proxy_lastName t10)
                                --        arising from a use of `fullName'
                                --     tried to use an imported virtual field (used unqualified) against a local record type
```


because `fullName` is overloaded against the fieldLabel in module `CUST`, not the local module.



Absolutely nothing magical going on here: all standard module/namespace control. Move along please.



\[There's a working example of all this importing, as an attachment to the implementor's page.\]


### Field Update for Overloadable Record Fields



You can (continue to) use pattern matching and data constructor tagging for record update:


```wiki
    case r of {
      Cust_Price{ unit_Price, .. }    -> Cust_Price{ unit_Price = unit_Price * 1.05, .. }
      }                                          -- increases Price by 5%
```


(This uses ‑XDisambiguateRecordFields, -XRecordWildCards and ‑XNamedFieldPuns -- all mature GHC extensions.)



The new part is polymorphic record update:


```wiki
    myPrice{ unit_Price = 72 :: Int }
```


Returns a record with same fields as `myPrice`, except a different `unit_Price`. Note that the update can change the type of a field (if the record declaration is polymorphic).



Upon first encountering that expression, we don't know the record types (because `unit_Price` is overloaded). So the types initially inferred are:


```wiki
    <expr>  :: r { unit_Price :: Int } => r
    myPrice :: _r{ unit_Price :: t }   => _r
```


That is, the update might be changing the record type as well as the field type -- in case that the record type is parametric over the field type.



Behind the scenes, the update syntax with an expression prefix `e{ ... }` (as opposed to a data constructor `MkT{ .. }`) is syntactic sugar for a call to the polymorphic record update method `set`:


```wiki
    set (undefined :: Proxy_unit_Price) (72 :: Int) myPrice
```


\[See [DORF -- Implementor's view](records/declared-overloaded-record-fields/implementors-view) for what the Proxy is doing.\]



Normal type inference/instance resolution will find the record type for `myPrice`, and therefore the correct instance to apply the update.



You can update multiple fields at the same time:


```wiki
    myCustNA{ firstName = "Fred", lastName = "Dagg" }
```

>
>
> \[There's a poor story to tell here in implementation terms: we split into two calls to `set`, one nested inside the other. It's wasteful to build the intermediate record. Worse, the two fields' types might be parametric in the record type or polymorphically related (perhaps one is a method to apply to the other), then we get a type failure on the intermediate record.\]
>
>


Note that where there is a genuine business-as-usual name clash you'd need qualified names in polymorphic update syntax, as currently:


```wiki
    someCust2 = someCust{ My.customer_id = 57, ... }
```


That is, there'd be no inference from the type of `someCust` to figure out which field label you're using. (That's because in general we can't infer the type of the expression prefixing the `{ ... }` update.)



Some discussion threads have argued that Haskell's current record update syntax is awkward. The DORF proposal is to implement field update using a polymorphic function. Once this is implemented, alternative syntax could be explored, providing it desugars to a call to `set`.



Posted 18-Feb-2012, Anthony Clayden. \[Apologies for my wiki formatting and cross-linking -- in haste! and a novice to trac.\] 


