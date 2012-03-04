### DORF `fieldLabel` Declaration through Type Punning



DORF's `filedLabel`'s generate a Proxy\_type named for the label. Of course the `Proxy_` prefix to the name is arbitrary -- it just needs to be something to be easily desugarrable-to, and not clash with any other type name in the environment.



Despite all the frenzied comment on ghc-users, nobody's questioned the arbitrariness.


- Why didn't I just name the type as per the label, but up-shifted?


Here's why: the ideas in DORF were triggered from this contribution by Chris Done (on the Records front page):


>
>
> Suppose I have 112 hand-crafted data types in my project (e.g. see attachment 51369.txt), this creates a lot of conflicts in field names and constructor names. For example:
>
>

```wiki
data Comment = Comment {
      commentId           :: CommentId
    , commentContent      :: Content
    , commentReviewId     :: ReviewId
    , commentSubmissionId :: SubmissionId
    , commentConferenceId :: ConferenceId
    , commentDate         :: ISODate
    , commentReviewerNumber :: Int
  } deriving (Show)
```

>
>
> This is a real type in my project. ...
>
>


So for every field name, there's already a type with the same name (upshifted). And this is a standard, disciplined approach to building the naming structure for a large-scale database application.



In the sample, we haven't got the definition for type `CommentId`, so perhaps it's:


```wiki
    type CommentId = Int
```


But that means we could do arithmetic on `CommentId`'s, and add them to `SubmissionId`'s then multiply by `ConfenceId` and put the result in `commentReviewerNumber`.



That sort of nonsense is exactly what SQL lets you do. (It even helpfully(?!) casts your numeric fields to the same format and lines up the decimal positions.) It sucks! It's exactly what strongly typed languages should stop you doing.



So more likely:


```wiki
    newtype CommentId = CommentId Int
```


(Or perhaps it's a regular `data` type?)



So you've done that, then under DORF you need a `fieldLabel`, and then you can declare your record:


```wiki
    fieldLabel commentId :: r -> CommentId

data Comment = Comment {
      commentId           :: CommentId
    , ...
    }
```


That's *how many times* I have to repeat the same name in one shift or the other? (Of course TH or a decent editor might reduce that, but it don't look pretty.) Plus there's a shadowy `Proxy_commentId` I have to worry about exporting/importing/hiding, so I can encapsulate my record properly.



Here's a radical suggestion:


- don't declare your `newtype`
- instead `fieldLabel` declares it for you (rather than generating a `Proxy_type`)
- record declarations go like this:

  ```wiki
  data Comment = Comment {
        commentId                        -- no type declarations at all, at all
      , content                          -- } Note: no `comment` prefix to the field labels
      , reviewId                         -- }
      , ...
      , commentReviewerNumber :: Int     -- Not allowed! Must declare a fieldLabel/newtype ReviewerNumber.
      }                                  -- and put field name reviewerNumber
  ```


The data decl desugars to `Has` instances, using the upshifted field name as the type-level 'peg':


```wiki
    instance (t ~ CommentId) => Has Comment CommentId t where ...
```


And you get a newtype `CommentId` to control the scope and representation -- which you're doing already.



Possible downsides:


- Can't have two fields of the same type -- such as two `ISODate`'s in the example.
  (Would have to be something like `submissionDate`, `conferenceDate`, etc. Applies particularly for 'generic' custom data types such as dates.)
- We can't write polymorphic record types (with type vars), because there's no type declaration against the field name to hold it.
  (Terrific! avoid all that complexity for updates that change the type of the record ;-)
- Your `fieldLabel`'s (or `newtype`'s) can't have type arguments -- which is going to get increasingly irksome if your fields are in fact sub-records.
- Or perhaps we allow type var(s) (or a type decl??) against the field name, but very limited -- still need to validate against the `fieldLabel`/`newtype`.
- Note that the effect of omitting a type decl for the field is different to H98: it doesn't mean this field same type as the next; it means this field's type to be the upshifted name.
- Perhaps to make that clear we use slightly different syntax: instead of comma separator between the fields, put semicolon.
  Makes sense: we're inside a curly bracketed scope.


So the `fieldLabel` and data decl would look like this:


```wiki
fieldLabel commentId :: r -> Int
fieldLabel contentSource a :: (Contentful a) => r -> a

data Comment a = (Contentful a) => Comment {
      commentId
    ; content                          -- semicolon between the fields
    ; reviewId                     
    ; ...
    ; contentSource a                  -- typevar from the record parameter               
    ; ...
    ; commentReviewerNumber :: Int     -- Still not allowed!
    }      

```


For the implementors' benefit the desugar is:


```wiki
newtype CommentId = CommentId Int                                 -- fieldLabel commentId
newtype ContentSource a = (Contentful a) => CommentSource a       -- contentSource

instance (Contenful a, t ~ ContentSource a_, a ~ a_) => 
          Has (Comment a) (ContentSource a_) t           where ...
```


(Note the `a`, `a_` and type equality constraint: this is again using the functional-dependency-like mechanism.)


