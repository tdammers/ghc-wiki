
like "computer is a net", nowadays language is a library. there is
nothing exceptional in C++ and Java languages except for their huge
library codebase that makes them so widely appreciated



while it's impossible for Haskell to have the same level of libraries
maturity, we can try to do our best. Libraries was considered so
important, that in H98 report libs required more pages than language
itself. But, really, all libraries described there together is
appropriate only for learning and small programs - to do real work, we
need even much, much more



fortunately, now we have large enough set of libs. moreover, this set
grows each year. but these libs don't have official/recommended
status. now we have two languages - H98 as reported with its bare
libs, which is appropriate only for teaching, and real Haskell
language with many extensions and rich set of libs, used to develop
real programs



with a language itself, now we go to standardize current practice and
include into language definition all popular extensions. this will
close the gap between standard and practice. Haskell' committee also
plan to define new version of standard Haskell library. but what a
library can be defined in this way? slightly extended version of
standard Haskell98 lib? or, if it will be significantly extended - how
much time this work will require and isn't that a duplication of work
done at libraries list?



i propose not to try to define reality, but accept existing one and
join committee's work on new library definition with a current
discussion of core libraries, which should define a set of libs
available on any Haskell compiler on any platform - aren't goals the
same?



instead of providing rather small and meaningless standard Haskell
library, now we can just include in Report docs existing and widely
used libs, such as Network, mtl and so on. This will mean that
language, defined in Haskell standard, can be used to write real
programs, which will be guaranteed to run in any Haskell environment.



of course, this mind game can't change anything in one moment. but it
will change \*accents\* 



first, Haskell with its libraries will become language for a real
work. such extended language isn't small nor easy to master in full,
but it is normal for any mature programming environment. people
learning Haskell should select in which area they need to specialize -
be it gaming or web service development, and study appropriate subset
of libs. people teaching Haskell now can show how \*standard\* Haskell may
be used to solve real world problems, and this should change treatment
of Haskell as academic language. also, we may expect that books
teaching Haskell will start to teach on using standard libs, while
their authors now don't consider teaching for non-standard libs



second, by declaring these libs as standard ones we create sort of
lingua franca, common language spoken by all Haskell users. for
example, now there are about 10 serialization libs. by declaring one of
them as standard, we will make choice simpler for most of users (who
don't need very specific features) and allow them to speak in common
language. in other words, number of Haskell libs is so large now that
we should define some core subset in order to escape syndrome of Babel tower.
defining core libraries set is just sharing knowledge that some
libraries are more portable, easier to use, faster and so on, so they
become more popular than alternatives in this area



third. now we have Cabal that automates installation of any lib. next
year we will got Hackage that automates downloading and checking
dependencies. but these tools still can't replace a rich set of
standard libs shipped with compiler. there are still many places and
social situations where Internet downloading isn't available. Compiler
can be sold on CD, transferred on USB stick. and separate Haskell libs
probably will be not included here. Standard libraries bundled with
compiler will ensure that at least this set of libs will be available
for any haskell installation. Internet access shouldn't be a
precondition for Haskell usage! :)



fourth. now there is tendency to write ghc-specific libs. by defining
requirements to the standard libs we may facilitate development of
more portable, well documented and quick-checked ones. or may be some
good enough libraries will be passed to society which will "polish"
them in order to include in the set. anyway, i hope that \*extensible\*
set of standard libraries with a published requirements to such libs
would facilitate "polishing" of all Haskell libs just because ;)



and this leads us to other question - whether this set and API of each
library should be fixed in language standard or it can evolve during
the time?...


