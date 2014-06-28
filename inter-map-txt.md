# [InterMapTxt](inter-map-txt)


## This is the place for defining [InterWiki](inter-wiki) prefixes



This page was modelled after the [
MeatBall:InterMapTxt](http://www.usemod.com/cgi-bin/mb.pl?InterMapTxt) page.
In addition, an optional comment is allowed after the mapping.



This page is interpreted in a special way by Trac, in order to support
InterWiki links in a flexible and dynamic way.



The code block after the first line separator in this page
will be interpreted as a list of InterWiki specifications:


```wiki
prefix <space> URL [<space> # comment]
```


By using `$1`, `$2`, etc. within the URL, it is possible to create 
[InterWiki](inter-wiki) links which support multiple arguments, e.g. Trac:ticket:40.
The URL itself can be optionally followed by a comment, 
which will subsequently be used for decorating the links 
using that prefix.



New InterWiki links can be created by adding to that list, in real time.
Note however that *deletions* are also taken into account immediately,
so it may be better to use comments for disabling prefixes.



Also note that InterWiki prefixes are case insensitive.


## List of Active Prefixes



<table><tr><th>*Prefix*</th>
<th>*Site*</th></tr>
<tr><th>[Acronym](http://www.acronymfinder.com/af-query.asp?String=exact&Acronym=RecentChanges)</th>
<th>[http://www.acronymfinder.com/af-query.asp?String=exact&Acronym=](http://www.acronymfinder.com/af-query.asp?String=exact&Acronym=)</th></tr>
<tr><th>[C2find](http://c2.com/cgi/wiki?FindPage&value=RecentChanges)</th>
<th>[http://c2.com/cgi/wiki?FindPage&value=](http://c2.com/cgi/wiki?FindPage&value=)</th></tr>
<tr><th>[c2Wiki](http://c2.com/cgi/wiki?RecentChanges)</th>
<th>[http://c2.com/cgi/wiki?](http://c2.com/cgi/wiki?)</th></tr>
<tr><th>[Cache](http://www.google.com/search?q=cache:RecentChanges)</th>
<th>[http://www.google.com/search?q=cache:](http://www.google.com/search?q=cache:)</th></tr>
<tr><th>[CheeseShop](http://cheeseshop.python.org/pypi/RecentChanges)</th>
<th>[Python Package $1 from the Cheese Shop](http://cheeseshop.python.org/pypi/)</th></tr>
<tr><th>[CPAN](http://search.cpan.org/perldoc?RecentChanges)</th>
<th>[http://search.cpan.org/perldoc?](http://search.cpan.org/perldoc?)</th></tr>
<tr><th>[DebianBug](http://bugs.debian.org/RecentChanges)</th>
<th>[http://bugs.debian.org/](http://bugs.debian.org/)</th></tr>
<tr><th>[DebianPackage](http://packages.debian.org/RecentChanges)</th>
<th>[http://packages.debian.org/](http://packages.debian.org/)</th></tr>
<tr><th>[Dictionary](http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query=RecentChanges)</th>
<th>[http://www.dict.org/bin/Dict?Database=\*&Form=Dict1&Strategy=\*&Query=](http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query=)</th></tr>
<tr><th>[Google](http://www.google.com/search?q=RecentChanges)</th>
<th>[http://www.google.com/search?q=](http://www.google.com/search?q=)</th></tr>
<tr><th>[GoogleGroups](http://groups.google.com/group/RecentChanges/msg/)</th>
<th>[Message $2 in $1 Google Group](http://groups.google.com/group/$1/msg/$2)</th></tr>
<tr><th>[hackage](http://hackage.haskell.org/package/RecentChanges)</th>
<th>[Haskell Package $1 from the HackageDB](http://hackage.haskell.org/package/$1)</th></tr>
<tr><th>[ISO](http://en.wikipedia.org/wiki/ISO_RecentChanges)</th>
<th>[ISO Standard $1 in Wikipedia](http://en.wikipedia.org/wiki/ISO_)</th></tr>
<tr><th>[JargonFile](http://downlode.org/perl/jargon-redirect.cgi?term=RecentChanges)</th>
<th>[http://downlode.org/perl/jargon-redirect.cgi?term=](http://downlode.org/perl/jargon-redirect.cgi?term=)</th></tr>
<tr><th>[MeatBall](http://www.usemod.com/cgi-bin/mb.pl?RecentChanges)</th>
<th>[http://www.usemod.com/cgi-bin/mb.pl?](http://www.usemod.com/cgi-bin/mb.pl?)</th></tr>
<tr><th>[Mercurial](http://www.selenic.com/mercurial/wiki/index.cgi/RecentChanges)</th>
<th>[the wiki for the Mercurial distributed SCM](http://www.selenic.com/mercurial/wiki/index.cgi/)</th></tr>
<tr><th>[MetaWiki](http://sunir.org/apps/meta.pl?RecentChanges)</th>
<th>[http://sunir.org/apps/meta.pl?](http://sunir.org/apps/meta.pl?)</th></tr>
<tr><th>[MetaWikiPedia](http://meta.wikipedia.org/wiki/RecentChanges)</th>
<th>[http://meta.wikipedia.org/wiki/](http://meta.wikipedia.org/wiki/)</th></tr>
<tr><th>[MODPYTHON](http://issues.apache.org/jira/browse/MODPYTHON-RecentChanges)</th>
<th>[Issue $1 in mod\_python's JIRA instance](http://issues.apache.org/jira/browse/MODPYTHON-)</th></tr>
<tr><th>[MoinMoin](http://moinmoin.wikiwikiweb.de/RecentChanges)</th>
<th>[http://moinmoin.wikiwikiweb.de/](http://moinmoin.wikiwikiweb.de/)</th></tr>
<tr><th>[mysql-bugs](http://bugs.mysql.com/bug.php?id=RecentChanges)</th>
<th>[Bug \#$1 in MySQL's bug database](http://bugs.mysql.com/bug.php?id=)</th></tr>
<tr><th>[peak](http://peak.telecommunity.com/DevCenter/RecentChanges)</th>
<th>[$1 in Python Enterprise Application Kit's Wiki](http://peak.telecommunity.com/DevCenter/)</th></tr>
<tr><th>[PEP](http://www.python.org/peps/pep-RecentChanges.html)</th>
<th>[Python Enhancement Proposal](http://www.python.org/peps/pep-$1.html)</th></tr>
<tr><th>[Phab](https://phabricator.haskell.org/RecentChanges)</th>
<th>[https://phabricator.haskell.org/$1](https://phabricator.haskell.org/$1)</th></tr>
<tr><th>[Python-issue](http://bugs.python.org/issueRecentChanges)</th>
<th>[Python Issue \#$1](http://bugs.python.org/issue$1)</th></tr>
<tr><th>[PythonBug](http://bugs.python.org/issueRecentChanges)</th>
<th>[Python Issue \#$1](http://bugs.python.org/issue$1)</th></tr>
<tr><th>[RFC](http://tools.ietf.org/html/rfcRecentChanges)</th>
<th>[IETF's RFC $1](http://tools.ietf.org/html/rfc$1)</th></tr>
<tr><th>[SQLite](http://www.sqlite.org/cvstrac/wiki?p=RecentChanges)</th>
<th>[http://www.sqlite.org/cvstrac/wiki?p=](http://www.sqlite.org/cvstrac/wiki?p=)</th></tr>
<tr><th>[SvnWiki](http://www.orcaware.com/svn/wiki/RecentChanges)</th>
<th>[Subversion Wiki](http://www.orcaware.com/svn/wiki/)</th></tr>
<tr><th>[trac-dev](http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/RecentChanges)</th>
<th>[Message $1 in Trac Development Mailing List](http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/)</th></tr>
<tr><th>[Trac-ML](http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/RecentChanges)</th>
<th>[Message $1 in Trac Mailing List](http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/)</th></tr>
<tr><th>[WhoIs](http://www.whois.sc/RecentChanges)</th>
<th>[http://www.whois.sc/](http://www.whois.sc/)</th></tr>
<tr><th>[Why](http://clublet.com/c/c/why?RecentChanges)</th>
<th>[http://clublet.com/c/c/why?](http://clublet.com/c/c/why?)</th></tr>
<tr><th>[WikiPedia](http://en.wikipedia.org/wiki/RecentChanges)</th>
<th>[http://en.wikipedia.org/wiki/](http://en.wikipedia.org/wiki/)</th></tr></table>



---


## Prefix Definitions


```wiki
PEP     http://www.python.org/peps/pep-$1.html    # Python Enhancement Proposal 
PythonBug    http://bugs.python.org/issue$1       # Python Issue #$1
Python-issue http://bugs.python.org/issue$1       # Python Issue #$1

Trac-ML  http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/ # Message $1 in Trac Mailing List
trac-dev http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/   # Message $1 in Trac Development Mailing List

Mercurial http://www.selenic.com/mercurial/wiki/index.cgi/ # the wiki for the Mercurial distributed SCM
RFC       http://tools.ietf.org/html/rfc$1 # IETF's RFC $1
ISO       http://en.wikipedia.org/wiki/ISO_ # ISO Standard $1 in Wikipedia

CheeseShop  http://cheeseshop.python.org/pypi/  # Python Package $1 from the Cheese Shop
SQLite      http://www.sqlite.org/cvstrac/wiki?p= 
mysql-bugs  http://bugs.mysql.com/bug.php?id=  # Bug #$1 in MySQL's bug database
peak        http://peak.telecommunity.com/DevCenter/ # $1 in Python Enterprise Application Kit's Wiki
MODPYTHON   http://issues.apache.org/jira/browse/MODPYTHON- # Issue $1 in mod_python's JIRA instance
SvnWiki     http://www.orcaware.com/svn/wiki/ # Subversion Wiki

# Haskell specific
hackage     http://hackage.haskell.org/package/$1 # Haskell Package $1 from the HackageDB
Phab        https://phabricator.haskell.org/$1

#
# A arbitrary pick of InterWiki prefixes...
#
Acronym          http://www.acronymfinder.com/af-query.asp?String=exact&Acronym=
C2find           http://c2.com/cgi/wiki?FindPage&value=
Cache            http://www.google.com/search?q=cache:
CPAN             http://search.cpan.org/perldoc?
DebianBug        http://bugs.debian.org/
DebianPackage    http://packages.debian.org/
Dictionary       http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query=
Google           http://www.google.com/search?q=
GoogleGroups     http://groups.google.com/group/$1/msg/$2        # Message $2 in $1 Google Group
JargonFile       http://downlode.org/perl/jargon-redirect.cgi?term=
MeatBall         http://www.usemod.com/cgi-bin/mb.pl?
MetaWiki         http://sunir.org/apps/meta.pl?
MetaWikiPedia    http://meta.wikipedia.org/wiki/
MoinMoin         http://moinmoin.wikiwikiweb.de/
WhoIs            http://www.whois.sc/
Why              http://clublet.com/c/c/why?
c2Wiki           http://c2.com/cgi/wiki?
WikiPedia        http://en.wikipedia.org/wiki/
```