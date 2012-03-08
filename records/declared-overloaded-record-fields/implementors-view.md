# DORF -- Implementer's view



The implementation has been prototyped in GHC 7.2.1, see
[
http://www.haskell.org/pipermail/glasgow-haskell-users/2011-December/021298.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-December/021298.html), and SPJ's observations/possible improvements and caveats
[
http://www.haskell.org/pipermail/glasgow-haskell-users/2012-January/021744.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2012-January/021744.html)



A cut-down prototype is attached to this page. (The ugliest hacks removed.)



The fact that DORF has been 'faked' in existing GHC is good evidence that it's a modest change. Furthermore, we can implement H98-style records/fields using the same mechanism.



DORF is to be enabled by a compiler flag **‑XDeclaredOverloadedRecordFields**, which implies flag **[‑XNoMonoRecordFields](records/declared-overloaded-record-fields/no-mono-record-fields)**, which in turn implies ‑XDisambiguateRecordFields and -XNamedFieldPuns with ‑XRecordWildCards.



Note we do not assume flag **[‑XDotPostfixFuncApply](records/declared-overloaded-record-fields/dot-postfix)**; dot notation is not needed by DORF, it's purely syntactic sugar to suit the taste of the programmer.



DORF is implemented through a class `Has` with methods `get` and `set`. (Very similar in principle to SORF.) There's an instance of `Has` for each record/field combination, with the instance generated from the record declaration.



Within each instance, `get/set` are defined in terms of the record's data constructors, using ‑XDisambiguateRecordFields and friends.


### Option One: fieldLabel declaration (data dictionary)



There is to be a new declaration type, examples:


```wiki
    fieldLabel customer_id :: r -> Int
    fieldLabel unitPrice :: (Save r, Num t) => r -> t
    fieldLabel monoField :: Rec -> String   -- equiv to H98 
```


\[`fieldLabel` is rather long as reserved words go. I'm guessing that field or label would already be heavily used in existing code. Suggestions welcome!\]



`fieldLabel` is not some new ontology in Haskell, it's only sugar. The `fieldLabel` declaration desugars to:


```wiki
    data Proxy_customer_id                  -- phantom, a type 'peg'
    customer_id :: r{ customer_id :: Int } => r -> Int
    customer_id r = get r (undefined :: Proxy_customer_id)

    unit_Price :: (r{ unit_Price :: t}, Save r, Num t) => r -> t
```


That is: the `r{ ... }` constraint is added by the desugarer (and will be further desugarred to a `Has` constraint).


### Option Two: explicit record constraint


>
>
> \[Or perhaps the new `fieldLabel` declaration isn't needed. See a *very speculative* discussion at [Wilder aftererthought](records/declared-overloaded-record-fields/c-ompare-sorf#the-string-type-parameter-to-has-,-and-scope-control) \]
>
>

>
>
> Declaring:
>
>
> ```wiki
>         customer_id :: r{ customer_id :: Int } => r -> Int     -- explicit record constraint
>                                                                -- field name same as the function name
> ```
>
>
> Desugars to the same as for `fieldLabel`. That is the proxy type and the binding:
>
>
> ```wiki
>         data Proxy_customer_id
>         customer_id r = get r (undefined :: Proxy_customer_id)
> ```
>
>
> **Note:** the desugarring only applies where the field and function are the same name (and record type argument and result type). Otherwise this syntax is declaring a regular function with a record constraint (could be a 'virtual' field).
>
>

### Option Three: Mixed In-situ and Declared ORF


>
>
> (See discussion at [
> http://www.haskell.org/pipermail/glasgow-haskell-users/2012-March/022061.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2012-March/022061.html) "My main complaint against DORF is that having to write fieldLabel declarations for every field you want to use is onerous.")
>
>


There may be some (perhaps most) of the field names in a record type that appear only in that record type. Then this:


```wiki
data Cust_AdHoc = CustAH{ customer_id :: Int, x, y :: String } sharing (customer_id) deriving (...)
```


For the non-`shareing` `x` and `y`, saves the burden of a `fieldLabel`, by declaring it for you:


```wiki
        data Proxy_x
        x :: Cust_AdHoc{ x :: String } => Cust_AdHoc -> String    -- Note: monomorphic record type
        x r = get r (undefined :: Proxy_x)
```


So field selector function `x` has the same type (in effect) as would have the H98 field selector.



For this variation:


```wiki
        data Customer_Order = Cust_Order { customer_id :: Int, order_num :: Int, ... }
                          sharing (customer_id) share (order_num) deriving (...)
```


The `share` fields are declared at this point -- generate the Proxy and the (overloadable) field selector.



Pros, compared with Option One or Two:


- No extra declaration style; nor innocuous-looking declaration with unobvious effects


Cons:


- This obscures the approach of declaring a data dictionary first.
- May lead to 'dependency hell' between record types.
- Prevents putting class constraints on the record type argument to the selector function.
- Prevents putting class constraints on the field type result from the function.


Perhaps the data dictionary/dependency hell could be avoided with a dummy record type to declare all the shared fields:


```wiki
        data Customer_fieldLabels = Cust_fLabs{ customer_id :: Int, lastName, firstName :: String,
                                                product_id :: Int, ...
                                                order_num :: Int, ...
                                              }                          share (All)
```


(This was a typical kludge on the System/38.)


###
[Option Four: Type Punning on the \`fieldLabel\`](records/declared-overloaded-record-fields/option-four-type-punning) === q.v.


### Syntactic sugar for `Has`



DORF steals from SORF:


```wiki
    r{ fld :: t }  <===> Has r Proxy_fld t
```


Using the sugar in the surface syntax (representation) allows for some freedom in the design space 'behind the scenes'.


### Should `get` have a Proxy argument?



I've used a phantom/proxy type (in GHC v 7.2.1) to drive type instancing for `Has`.



SORF uses a `String` Kind (which is only partially available with GHC v 7.4.1), with implicit type application (so `get` does not have a proxy argument).
I'll leave it to the implementors to determine which works best.



`get` is a method of the `Has` class:


```wiki
    get :: (Has r fld t) => r -> fld -> t
```

### Record declaration


```wiki
    data Customer_NameAddress = Cust_NA { customer_id :: Int, ... } 
```


Does not create a field selector function `customer_id`. Instead it creates a `Has` instance:


```wiki
    instance (t ~ Int) 
       => Has Customer_NameAddress Proxy_customer_id t where
               get Cust_NA{customer_id} _ = customer_id
```


Note the bare `t` with type equality constraint. This is unashamedly stolen from SORF's "functional-dependency-like mechanism (but using equalities) for the result type". So type inference binds to this instance based only on the record type and field (type 'peg'), then 'improves' the type of the result.
The definition of `get` uses ‑XDisambiguateRecordFields style (with ‑XNamedFieldPuns).


>
>
> \[It's a wart that in the record declaration, we've had to repeat the type of `customer_id` when the `fieldLabel` decl has already stipulated `Int`. It is legal syntax to omit the type in the record decl, but that currently has a different effect:
>
>
> ```wiki
>         data ... = Cust_NA { customer_id, custName :: String, ... }
> ```
>
>
> currently means `customer_id` is to be same type as `custName`.
>
>

>
>
> On the other hand, the advantage of repeating the type (from an implementation point of view) is that the desugarrer doesn't have to look for the `fieldLabel` to generate the `Has` instance.
>
>

>
>
> Opportunity for improvement! \]
>
>

### Record/field update



Update uses method `set` from the `Has` class:


```wiki
        set :: (Has r fld t) => fld -> t -> r -> r
```


`set`'s instances are defined using explicit data constructor:


```wiki
    instance (t ~ String) =>
        Has Customer_NameAddress Proxy_firstName t where
            set _ x (Cust_NA{..}) = Cust_NA{firstName = x, ..}
```


The definition of `set` uses ‑XDisambiguateRecordFields style (with ‑XNamedFieldPuns and ‑XRecordWildCards to fill in the un-named fields).
Haskell's existing update syntax is desugarred to a call to `set`:


```wiki
    myCustNA{ firstName = "John" }
===>  set (undefined :: Proxy_firstName) "John" myCustNA
```


(That is, with a name or expression preceeding the `{ ... }`. A data constructor prefix continues to use  -XDisambiguateRecordFields.)



It is crucial to this proposal that we can implement a polymorphic field update function (`set`). There's a number of tricky requirements considered below.



In amongst the discussion of dot notation for field selection, there have been aspersions cast on Haskell's current record update syntax.  [
http://www.haskell.org/pipermail/haskell-cafe/2012-February/099314.html](http://www.haskell.org/pipermail/haskell-cafe/2012-February/099314.html)



If we can support update as just a function, there's a chance we can then turn to the syntactic sugar. (For example, the application programmer can develop update idioms to suit their purpose, as just overloaded functions.)


### Updating multiple fields



The syntax already supports updating multiple fields:


```wiki
    myCustNA { firstName = "Fred", lastName = "Dagg" }
```


The simplest implementation is to turn this into two (nested) updates, but that makes it inefficient generating then discarding the interim result. It may also block updates of linked fields that share a type parametric in the record type.
The prototype for this proposal has explored updating with a type instance pairing the two fields:


```wiki
    instance (t ~ (String, String)) =>
        Has Customer_NameAddress (Proxy_firstName, Proxy_lastName) t     where ...
```


but in general, this would need instances for each perm/comb of fields.


### Changing the record constructor



The `set` method continues with the as-is record constructor, with no attempt to figure out the 'appropriate' constructor for the fields names presented. This follows H98 behaviour.



So if in update syntax you present field names which are not in the current constructor (but are in other constructors for the same type), you'll get pattern match failure (`Non-exhaustive patterns in record update`). For example:


```wiki
    data Txyz	=   Txyz { x :: Int, y :: String, z :: Bool }
		  | Tyx  { y :: String, x :: Int }  
             deriving (Show, Read, Eq)

    tyx  = Tyx{ y = "hello", x = 72 }

    txyz = tyx{ z = False }                              -- run-time fail: Non-exhaustive patterns in record update
                                                         -- no attempt to change to constructor Txyz
```


To achieve this, you'd need to put an explicit data constructor (presumably using punning/wildcards):


```wiki
    case tyx of { Tyx{..} -> Txyz{ z = False, .. } }
```

### Type-changing update



Haskell 98's record update can change the type of the record, by changing the type of a field that is parametric in the record's type.



There has been some feedback that there are legitimate use-cases for type-changing update -- for example traversing a record structure applying some type-changing transformation.



This proposal does support type changing, but at cost of considerable extra complexity.



So the earlier definitions of `Has/get/set` have been "economical with the truth". Instead:


```wiki
    class Has r fld t	where
        get  :: r -> fld -> GetResult r fld t
        set  :: (Has (SetResult r fld t) fld t) =>
                   fld -> t -> r -> SetResult r fld t
```


The type functions are to handle the possibly-changing types:


```wiki
    type family GetResult  r fld t   :: *  -- result from get
    type family SetResult  r fld t   :: *  -- result from set
```


For monomorphic (non-changing) fields, `GetResult` returns `t` and `SetResult` returns `r`, so this amounts to the simpler definitions for `Has/get/set` given earlier.



These are type families, not associated types, because in many cases, the result from `get` depends only on `fld` (not `r`), and the result from `set` depends only on the record type `r` (not `t`). In a few cases, the type function must be sensitive to the combination of field type and record type.



The extra `Has` constraint on `set`'s result is to 'improve' `t` by gathering constraints from the type of `set`'s resulting record type.



Note that the field value's type `t` is the type to-be in the result, not the type as-was in the record being updated.
So the result from set has that type 'inserted'.



Example, based on field `unit_Price`:


```wiki
    data Customer_Price a = Num a => Cust_Price {
                                       ...,
                                       unit_Price  :: a,
                                       ... }
    type instance GetResult (Customer_Price a) Proxy_unit_Price t
           = a           -- type as is
    type instance SetResult (Customer_Price _a) Proxy_unit_Price t
           = Customer_Price t      -- set record type per arg to set
    instance (Num t) =>
        Has (Customer_Price a) Proxy_unitPrice t        where
            get Cust_Price{unit_Price} _ = unit_Price
            set _ x Cust_Price{..} = Cust_Price{ unit_Price = x, .. }
```


(The method definitions are 'uninteresting', compared to the drama to get the types right.)



The extra complexity to support changing type could be somewhat reduced using a separate `Set` class with four type parameters, including both as-was and resulting record types, and equality constraints to improve them (and to improve the result from `get`) -- rather than type family `SetResult` and `GetResult`.



This would mean, though, that the type sugar for `Has` constraints would not be adequate. Since that sugar is to be visible but the instance definitions are to be 'internal', this proposal prefers to support the sugar.


### Selecting polymorphic/higher-ranked fields



Note that initialising records with polymorphic fields (using record constructor syntax) is not affected. This proposal implements selecting/applying those fields in polymorphic contexts. This includes fields with class-constrained types 'sealed' within the record.



To support higher-ranked fields, this proposal follows SORF's approach (with three parameters to Has) to obtain a polymorphic type:


```wiki
    data HR	= HR {rev :: forall a_. [a_] -> [a_]}   -- per SPJ
    fieldLabel rev :: r -> (forall a_. [a_] -> [a_])
    ===>
    data Proxy_rev
    rev :: Has r Proxy_rev t => r -> t
    rev r = get r (undefined :: Proxy_rev)
    type instance GetResult r Proxy_rev t = t     -- plain t
                   -- field's type is whatever's there (it's opaque)
                   -- improved by the instance constraint
    type instance SetResult HR Proxy_rev t = HR
                   -- the higer-ranked type is hidden inside HR
    instance (t ~ ([a_] -> [a_])) =>              -- same as SORF
        Has HR Proxy_rev t    where
            get HR{rev} _ = rev
            -- set _ fn HR{..} = HR{rev = fn, ..}  -- compile fails: can't match fn's type to rev's
```

### Updating polymorphic/higher-ranked fields



The prototype for this proposal does include a method of updating Higher-ranked fields. SPJ has quickly reviewed the prototype:


>
>
> "Your trick with `SetTy` to support update of polymorphic fields is, I belive, an (ingenious) hack that does not scale. I think it works only for fields that are quantified over one type variable with no constraints.
>
> So, I think that update of polymorphic fields remains problematic. "
>
>


Note that the "(ingenious)" and unscalable "hack" appears only in compiler-generated code.



Is it a requirement to be able to update polymorphic fields? Is it sufficient to be able to update other (monomorphic) fields in records that also contain poly fields?


### Representation hiding/import/export



See the discussion under [Application Programmer's view Import/Export](records/declared-overloaded-record-fields#)  and [No Mono Record Fields](records/declared-overloaded-record-fields/no-mono-record-fields). 


>
>
> \[The following re the Proxy\_type changed/added 1st March.\]
>
>


When import/exporting we need to separately control exporting the Proxy\_type:


- The original plan was to control import/export shadowing the field selector function.

>
> >
> >
> > (As an afterthought: this would treat the function differently to usual functions.
> >
> >
> > >
> > >
> > > On the other hand, no more special than exporting the function along with the H98 record label.)
> > >
> > >
> >
>

- If not exported, update syntax cannot be desugarred to use it, so this hides the representation.

>
> >
> >
> > (So any records become in effect read-only, using the exported field selector function.)
> >
> >
>

- Furthermore, it prevents the client declaring records to 'share' the fieldLabel.

>
> >
> >
> > (Because the `Has` instance generated will try to use the Proxy\_type.)
> >
> >
>


Drat! I was trying to keep the Proxy\_type hidden from the programmer.



It's possible we might want to hide the representation (prevent update), but allow sharing with a locally-declared record type/field??


>
>
> \[Using a `String Kind` a la SORF would not help: can't control sharing nor hide the representation.
>  End of Proxy\_type addition.\]
>
>


See also the attached `DORF Prototype Importing 29Feb2012.lhs`, which selectively imports some fieldLabels, and declares local versions of others. This shows that within a single record decl:


1. You can create fields that share Labels with imports.
1. You can create fields that don't share, even with the same Label name.

>
> >
> >
> > (That is, the module system continues to control the namespace.)
> >
> >
>

1. You can prevent using the wrong field selector with the wrong record type,

>
> >
> >
> > even if they have the same Label name.
> >
> >
>


(Apologies for labouring the point: it seems to be widely mis-understood, and it's a point of difference compared to SORF.)


### Should application programmers declare instances for `Has/set`?



Nothing so far suggests they should. (And there's obvious dangers in allowing it.)



But updating through virtual fields might need it. See [Virtual Record selectors](records/declared-overloaded-record-fields/c-ompare-sorf#virtual-record-selectors).


