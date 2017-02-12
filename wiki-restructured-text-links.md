# [TracLinks](trac-links) in reStructuredText



This document illustrates how to use the `:trac:` role in [
reStructuredText](http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html). The page is written like:


```wiki
{{{#!rst
Examples:

 * Tickets: :trac:`#1` or :trac:`ticket:1`
 * Ticket comments: :trac:`comment:ticket:1:2`
 * Reports: :trac:`{1}` or :trac:`report:1`
 * Changesets: :trac:`r1`, :trac:`[1]` or :trac:`changeset:1`
 * Revision log: :trac:`r1:3`, :trac:`[1:3]` or :trac:`log:@1:3`, :trac:`log:trunk@1:3`
 * Diffs: :trac:`diff:@20:30`, :trac:`diff:tags/trac-0.9.2/wiki-default//tags/trac-0.9.3/wiki-default` or :trac:`diff:trunk/trac@3538//sandbox/vc-refactoring/trac@3539`
 * Wiki pages: :trac:`CamelCase` or :trac:`wiki:CamelCase`
 * Milestones: :trac:`milestone:1.0`
 * Attachment: :trac:`attachment:ticket:944:attachment.1073.diff`
 * Files: :trac:`source:trunk/COPYING`
 * A specific file revision: :trac:`source:/trunk/COPYING@200`
 * A particular line of a specific file revision: :trac:`source:/trunk/COPYING@200#L25`

An explicit label can be specified, separated from the link by a space:

 * See :trac:`#1 ticket 1` and the :trac:`source:trunk/COPYING license`.
}}}
```


Provided you have [
docutils](http://docutils.sourceforge.net/) installed, the above block will render as:


---



Examples:


>
> - 
> - Tickets: [\#1](http://gitlabghc.nibbler/ghc/ghc/issues/1) or [ticket:1](http://gitlabghc.nibbler/ghc/ghc/issues/1)
> - 
> - Ticket comments: comment:ticket:1:2
> - 
> - Reports: [{1}](/trac/ghc/report/1) or [report:1](/trac/ghc/report/1)
> - 
> - Changesets: r1, \[1\] or changeset:1
> - 
> - Revision log: [r1:3](/trac/ghc/log/ghc/?revs=1%3A3), [\[1:3\]](/trac/ghc/log/ghc/?revs=1%3A3) or [log:\@1:3](/trac/ghc/log/ghc/?revs=1%3A3), [log:trunk\@1:3](/trac/ghc/log/ghc/trunk?revs=1%3A3)
> - 
> - Diffs: [diff:\@20:30](/trac/ghc/changeset?new=30&old=20), [diff:tags/trac-0.9.2/wiki-default//tags/trac-0.9.3/wiki-default](/trac/ghc/changeset?new_path=tags%2Ftrac-0.9.3%2Fwiki-default&old_path=tags%2Ftrac-0.9.2%2Fwiki-default) or [diff:trunk/trac\@3538//sandbox/vc-refactoring/trac\@3539](/trac/ghc/changeset?new=3539&new_path=sandbox%2Fvc-refactoring%2Ftrac&old=3538&old_path=trunk%2Ftrac)
> - 
> - Wiki pages: [CamelCase](/trac/ghc/wiki/CamelCase) or [wiki:CamelCase](/trac/ghc/wiki/CamelCase)
> - 
> - Milestones: milestone:1.0
> - 
> - Attachment: attachment:ticket:944:attachment.1073.diff
> - 
> - Files: source:trunk/COPYING
> - 
> - A specific file revision: source:/trunk/COPYING\@200
> - 
> - A particular line of a specific file revision: source:/trunk/COPYING\@200\#L25
> - 
>
>

An explicit label can be specified, separated from the link by a space:


>
> - 
> - See [ticket 1](http://gitlabghc.nibbler/ghc/ghc/issues/1) and the license.
> - 
>
>


---



Note that the above could have been written using substitution references and the `trac::` directive:


```wiki
{{{#!rst
See |ticket123|.

 .. |ticket123| trac:: ticket:123 this ticket
}}}
```


This renders as:


---



See [this ticket](http://gitlabghc.nibbler/ghc/ghc/issues/123).


>


---



See also: [WikiRestructuredText](wiki-restructured-text), [TracLinks](trac-links)


