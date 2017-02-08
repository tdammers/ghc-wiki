# Proposed move from Trac to Maniphest



This page discusses the various pros and cons for moving from Trac to Phabricator for ticket management, as proposed on [
ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2016-December/013442.html).



If you want to try it yourself then there is a prototype [
http://104.130.13.28/](http://104.130.13.28/)\]


## What does Maniphest do well?


- Actively developed: Phabricator will continue to improve in the
  future.

- Metadata: Custom fields are supported.

- Flexible user interface: Custom fields can be hidden from the new
  ticket form to prevent user confusion.

- Familiarity: Many users may feel more at home in Phabricator's interface;
  reMarkup's similarity to Markdown also helps.

- Integration: Having Phabricator handle code review, release
  management, and issue tracking will hopefully reduce maintenance
  workload.

- Notifications: Herald's rule-based notifications are quite handy.

- Works well on mobile as well as in the browser. 

## What does Maniphest do poorly?


- Flexibility of search: The search options feel a bit more limiting
  than Trac; in particular the ability to show arbitrary columns in
  search results seems conspicuously missing.

- Legibility: This is admittedly to some extent a matter of aesthetics
  but the search results list feels very busy and is quite difficult to
  quickly scan. This is made exacerbated by the fact that some aspects
  of the the color scheme are quite low contrast (e.g. grey against
  white for closed tickets). This hurts quite a bit since a number of
  contributors spend a significant amount of time looking through lists
  of tickets. Perhaps we could convince the Phacility people to provide
  a more legible, compact presentation option.

## What does Trac do well?


- Convenient cross-referencing: while the syntax is a bit odd, once you
  acclimate it is quite liberating to be able to precisely
  cross-reference tickets, wiki documents, and comments without copying
  links around.

- Automation of ticket lifecycle: Trac tickets progress through their
  lifecycle (e.g. from "new" to "patch" to "merge" to "closed"
  statuses) through predefined actions. This means that moving a ticket
  through its lifecycle typically only requires one click and the right
  thing happens with no additional effort. I think this is a great model,
  although in practice it's not clear how much we benefit from it
  compared to a typical Maniphest workflow.

- Rich metadata: Tickets can have a variety of metadata fields
  which can be queried in sophisticated ways .

## What does Trac do poorly?


- Familiarity: Many users feel rather lost in Trac

- Active development: Trac is largely a stagnant project.

- Spam management: Keeping spam at bay has been a challenge. We seem to
  have it under control at the moment, but I wonder how long this will
  last for.

- Safety: I have personally lost probably a half-dozen hours of my life
  to Trac eating comments.

- Integration with code review workflow: We use Phabricator for CI and
  code review; the only thing that remains on Trac are our tickets and
  the Wiki. Keeping these two resources in sync is time-consuming and
  error-prone.

- Full text search: Trac's full text search capabilities are generally
  terrible. While I've tried working around this using PostgreSQL's
  native full text search functionality, the result is poorly
  integrated into Trac and consequently hard to use.

- Customizability of the ticket form: While the rich metadata that Trac
  supports can be very helpful developers, it can also be confusing to
  users. Ideally we would hide these fields but Trac does not give us
  the ability to do so.

- Relations between tickets: Trac has essentially no first-class notion
  of ticket relatedness. Even duplicate tickets need to be manually
  annotated in both directions.

- Keywords are hard to discover and apply

- Fine-grained notification support is nearly non-existent

- Notification emails break links if they word wrap and doesn't apply other markup

- Fields are not validated at all, once you start looking there are many cases where input is malformed (Three recent examples: [\#12623](http://gitlabghc.nibbler/ghc/ghc/issues/12623) (Owner) [\#13100](http://gitlabghc.nibbler/ghc/ghc/issues/13100) (CC field) [\#13194](http://gitlabghc.nibbler/ghc/ghc/issues/13194) (Differential field) [\#11525](http://gitlabghc.nibbler/ghc/ghc/issues/11525) (Differential field, 2x)).

- It is not possible to set the status of a ticket to patch and simultaneously assign an owner to the ticket.
