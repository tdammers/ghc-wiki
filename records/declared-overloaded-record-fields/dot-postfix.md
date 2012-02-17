# Dot as Postfix Function Apply


## Exploiting the power of the dot -- or not?



Currently (in Haskell 98), declaring a record field generates a field selector function, for example:


```wiki
    data Rec = Rec { f :: String }   -- generates:
    f :: Rec -> String
```


And `f` is 'just a function', to be applied in prefix form.



The DORF Overloaded Record Fields proposal also generates field selectors as 'just functions' (to be overloaded for all the record types thay appear in).



As with TDNR, I propose an additional syntactic form `r.f` for selecting field `f` from record `r` (of type `Rec`). The dynamic syntax is simply reverse application `r.f` \<===\> `(f r)`  (So for overloaded field selectors, we use usual type-directed instance resolution.)



But dot notation must be written with no spaces around the dot -- because it has strange syntactic properties that mean it isn't 'just an operator'.



The reasoning for using dot is exactly as SPJ adumbrates in TDNR ("it is critical to support dot notation") -- essentially, it's standard practice (both for selecting fields from records in data manipulation languages such as SQL, and selecting methods from objects on OO languages). Postfix application supports an object -\> action pattern of thinking: focus on the object, then apply some action; or the UNIX pipes concept of the data 'flowing' left to right.



The dot syntax is to be optional (and is orthogonal to DORF -- you can use DORF purely pre-fix). (There are voiciferous calls from some on the discussion thread not to use dot syntax, and/or not to use postfix function application. I'd rather get DORF adopted, so I don't want to get into a confrontation over lexical syntax.)



Suggested compiler flag **‑XDotPostfixFuncApply**.



The syntax could use some other character than dot (hash \# has been suggested), but there's a danger that is already a user-defined operator in existing code (or in some unrelated GHC extension). There are good syntactic reasons for using dot (see below), besides that it is a convention familiar from other programming paradigms.



Note this proposal differs significantly from others for dot syntax, such as:
[
http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR)
[
http://hackage.haskell.org/trac/ghc/wiki/Records/DotOperator](http://hackage.haskell.org/trac/ghc/wiki/Records/DotOperator)



Dot notation can be used for any function, not just record fields (as with TDNR, but different to SORF). This supports pseudo- or virtual fields. The declaration:


```wiki
    fullName r = r.firstName ++ " " ++ r.lastName   -- SPJ's example
```


creates fullName as just a function, not a field. But we can still use dot syntax, and here are some other similar examples:


```wiki
    customer.fullName
    shape.area            -- also from SPJ
    date.dayOfWeek        -- not a field: calculated from the date
    name.middleInitial    -- extract from the name field
    tuple.fst             -- Prelude functions
    list.head
    list.length
```


That is, we can use this syntax to 'select' attributes or properties from structures. (Because these are 'virtual', there is no update function.)


### Dot notation's "strange syntactic properties"



Dot Apply must bind tighter than function application. This is unlike any other operator. We want:


```wiki
    map toUpper customer.lastName
    ===> map toUpper (lastName customer)

    m.lookup key                  -- method `lookup' from object m
    ===> (lookup m) key
```


Postfix dots can be stacked up, and bind leftwards:


```wiki
    shape.position.xCoord
    ===>  (shape.position).xCoord     -- not! shape.(position.xCoord)
    ===> (xCoord (position shape))
```


But to facilitate postfix style, there are occasions where we want a loose binding form. We could of course use parentheses, but the requirement is just like loose-binding prefix function application provided by Prelude `($)`. Suggested operator:


```wiki
    (.$) = flip ($)
```


(This is an ordinary Haskell operator, but this proposal is asking to reserve the namespace.)



Two examples adapted from SPJ's TDNR wiki, and avoiding the 'something odd' he notices:


```wiki
       m.lookup key
    .$ snd
    .$ reverse

    record.list .$ reverse
                .$ filter isEven
                .$ map double
                .$ foldr (+) 0       -- sum
                .$ (^ 2)             -- square
```

### Using Dot notation amongst qualified names



This must be valid (note no embedded spaces):


```wiki
    MyData.customer.Their.Module.fullName.Prelude.head.Data.Char.toUpper
```


The syntax rule is:


-  A name to the left of a dot starting upper case is a module,

>
> >
> >
> > and the dot binds most tightly.
> >
> >
>

-  A name to the left starting lower case is postfix apply,

>
> >
> >
> > and binds less tightly, but tighter than usual function apply.
> >
> >
>

-  A name at the rightmost end starting upper case is valid,

>
> >
> >
> > it must be a data constructor.
> >
> >
>

-  You can use parentheses to override the binding.

>
> >
> >
> > (And parens would be needed where embedding a data constructor.)
> >
> >
>

### Why no embedded spaces? -- And a code-breaking change



In case anybody's worrying about parsing the long dotted block above, this is already valid Haskell 98:


```wiki
    Data.Char.toUpper.Prelude.head.Prelude.tail
```


"It graphically appeals to the notion of a function composed of several functions", according to a poster resistant to dot as postfix function apply.



(Applying the "function" to "hello" yields 'E'.) This is equivalent:


```wiki
    Data.Char.toUpper . Prelude.head . Prelude.tail
```


That is, there's already a dot operator (function composition), so the 'good syntactic reason' (per above) for using dot as postfix function apply is that we can be sure it's already reserved as Haskell syntax.



The code-breaking change is:


- **Function composition will only work when the dot is surrounded by spaces.**

- Dot with no space either side is to change
  to mean postfix function application (tight-binding).

- Dot with space one side only is to change
  to be invalid syntax -- it's too confusing to assume what's meant.


Note that if 'one-sided' dot were valid as partial application of postfix notation (section style):


```wiki
    (.f)  ===> (\r -> r.f)          -- eta-expand
          ===> (\r -> (f r))        -- desugar the dot
          ===> f                    -- eta-reduce
```


So `map (.f) ts` would mean `map f ts`.


```wiki
    (r.) -- makes no sense as a section, as SPJ points out.
```


If you really, really want a section:


```wiki
    (.$ f)
    (r .$)
```


There has been lengthy discussion about the interaction of dot syntax and record/field selection -- see the thread starting:
[
http://www.haskell.org/pipermail/haskell-cafe/2012-January/098899.html](http://www.haskell.org/pipermail/haskell-cafe/2012-January/098899.html)



Thank you for everybody's contributions, they've significantly tuned the proposal as it went along.


### Relationship to the proposal for Declared Overloaded Record Fields (DORF)



I've concluded that dot notation is controversial (contrary to SPJ's assumption when TDNR started).



So to repeat: DORF does not rely on postfix dot notation, neither does postfix dot notation rely on DORF.



They're related because DORF relies on field selectors being functions; and field selection being function application -- for which postifx dot provides familiar syntax. (That is, familiar from other programming paradigms.)


