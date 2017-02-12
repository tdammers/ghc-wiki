# Trac Macros






**Trac macros** extend the Trac engine with custom functionality. Macros are a special type of plugin and are written in Python. A macro inserts dynamic HTML data in any context supporting [WikiFormatting](wiki-formatting).



The macro syntax is `[[macro-name(optional-arguments)]]`.



**[WikiProcessors](wiki-processors)** are another kind of macros. They are typically used for source code highlighting, such as `!#python` or `!#apache` and when the source code spans multiple lines, such as:


```wiki
{{{#!wiki-processor-name
... 
}}}
```

## Using Macros



Macro calls are enclosed in double-square brackets `[[..]]`. Like Python functions, macros can have arguments, which is then a comma separated list within parentheses `[[..(,)]]`.


### Getting Detailed Help



The list of available macros and the full help can be obtained using the MacroList macro, as seen [below](wiki-macros#available-macros).



A brief list can be obtained via `[[MacroList(*)]]` or `[[?]]`.



Detailed help on a specific macro can be obtained by passing it as an argument to MacroList, e.g. `[[MacroList(MacroList)]]`, or, more conveniently, by appending a question mark (`?`) to the macro's name, like in `[[MacroList?]]`.


### Example



A list of the 3 most recently changed wiki pages starting with 'Trac':


<table><tr><th> Wiki Markup </th>
<th> Display 
</th>
<th></th></tr>
<tr><th>```wiki
[[RecentChanges(Trac,3)]]
```

</th>
<th>

### Jan 4, 2019

- [TracWikiMisc](/trac/ghc/wiki/TracWikiMisc)<small> ([diff](/trac/ghc/wiki/TracWikiMisc?action=diff&version=14))</small>

### Feb 12, 2017

- [TracSearch](/trac/ghc/wiki/TracSearch)<small> ([diff](/trac/ghc/wiki/TracSearch?action=diff&version=4))</small>
- [TracSyntaxColoring](/trac/ghc/wiki/TracSyntaxColoring)<small> ([diff](/trac/ghc/wiki/TracSyntaxColoring?action=diff&version=5))</small>



</th>
<th></th></tr>
<tr><td>
</td>
<th>```wiki
[[RecentChanges?(Trac,3)]]
```

</th>
<th>

### `[[RecentChanges]]`


List all pages that have recently been modified, ordered by the
time they were last modified.



This macro accepts two ordered arguments and a named argument. The named
argument can be placed in any position within the argument list.



The first parameter is a prefix string: if provided, only pages with names
that start with the prefix are included in the resulting list. If this
parameter is omitted, all pages are included in the list.



The second parameter is the maximum number of pages to include in the
list.



The `group` parameter determines how the list is presented:


<table><tr><th>`group=date`</th>
<td>The pages are presented in bulleted lists that are
grouped by date (default).
</td></tr>
<tr><th>`group=none`</th>
<td>The pages are presented in a single bulleted list.
</td></tr></table>



Tip: if you only want to specify a maximum number of entries and
don't want to filter by prefix, specify an empty first parameter,
e.g. `[[RecentChanges(,10,group=none)]]`.




</th></tr>
<tr><td>
</td>
<th>```wiki
[[?]]
```

</th>
<th>

### `[[Image]]`

Embed an image in wiki-formatted text.
The first argument is the file, as in `[[Image(filename.png)]]`

### `[[InterTrac]]`

Provide a list of known [InterTrac](/wiki/InterTrac) prefixes.

### `[[InterWiki]]`

Provide a description list for the known [InterWiki](/wiki/InterWiki) prefixes.

### `[[KnownMimeTypes]]`

List all known mime-types which can be used as [WikiProcessors](/wiki/WikiProcessors).



etc.


</th></tr></table>


## Available Macros



*Note that the following list will only contain the macro documentation if you've not enabled `-OO` optimizations, or not set the `PythonOptimize` option for [mod\_python](trac-mod-python).*



### `[[AboutWikiBoxes]]`


Display a wiki page on how to use boxes.


### `[[AboutWikiIcons]]`


Display a wiki page on how to use icons.


### `[[AboutWikiPhrases]]`


Display a wiki page on how to use attentional phrases.


### `[[BackLinks]]`

*Sorry, no documentation found*

### `[[BlogList]]`


A macro to display list of posts and extracts outside (or inside)
the Blog module - most commonly Wiki pages.



All arguments are optional:


```wiki
[[BlogList]]
```


Available named arguments:


- `recent=` - max. number of posts
- `category=` - a category
- `author=` - an author
- `period=` - time period of the format YYYY/MM
- `heading=` - a heading for the list
- `format=` - type of display (see below for details)
- `max_size=` - max. number of characters to render for each post
- `meta=` - use `=off` to hide date, author and categories (default 'on')


Example showing some available named arguments:


```wiki
[[BlogList(recent=5, max_size=250, period=2007/12, author=osimons, format=float, heading=Some Trac Posts)]]
```


The arguments for criteria are 'AND'-based, so the above example will render
at most 5 posts by 'osimons' in December 2007. 



There is no heading unless specified.



Without restriction on recent number of posts, it will use the number currently
active in the Blog module as default for 'float' and 'full' rendering, but for rendering
of 'inline' list it will render all found as default unless restricted. Additionally for
'float' and 'full' it will truncate content if it is larger than a max\_size (if set).



The `format=` keyword argument supports rendering these formats:


<table><tr><th>`format=inline`</th>
<th>Renders an unordered list in the normal text flow (default).
</th></tr>
<tr><th>`format=float`</th>
<th>A floating box out on the side of the page with slightly more detail.
</th></tr>
<tr><th>`format=full`</th>
<th>Full rendering like on period, category and author listings inside blog.
</th></tr></table>



The arguments can appear in any order.



Posts are rendered sorted by newest first for all modes.


### `[[CommitTicketReference]]`


Insert a changeset message into the output.



This macro must be called using wiki processor syntax as follows:


```wiki
{{{
#!CommitTicketReference repository="reponame" revision="rev"
}}}
```


where the arguments are the following:


- `repository`: the repository containing the changeset
- `revision`: the revision of the desired changeset

### `[[ExtractUrl]]`


Provides test macro for the `tracextracturl.extract_url` function.



This macro is intended for code testing by the developers of the above function 
and has no real usage for normal Trac users.



Macro usage: `[[ExtractUrl(traclink)]]` 

Result: The URL extracted by `extract_url`



$Id: macro.py 8545 2010-08-30 21:57:33Z martin\_s $
    


# Description for `extract_url()`



Extracts an URL from an Wiki link, e.g. to used in macro produced HTML code.



Website: [
http://trac-hacks.org/wiki/ExtractUrlPlugin](http://trac-hacks.org/wiki/ExtractUrlPlugin)



`$Id: extracturl.py 8545 2010-08-30 21:57:33Z martin_s $`


## Description



Returns an (possible relative) URL which can be used in HTML code.



If `raw` is true the returned link will point to a downloadable
version of the linked resource otherwise the same link is returned
which would be used in the resulting Wiki page.



The raw links are also usable as online resouces, e.g. if the link target
is to be used as input for a flash application etc.


## Usage



General:


```wiki
from tracextracturl import extract_url
# ...
  url = extract_url (env, context, wikilink, raw=False)
```


Inside [WikiMacros](wiki-macros):


```
from tracextracturl import extract_url

def MyMacro(WikiMacroBase):
  def expand_macro (self, formatter, name, content):
     # e.g. wikilink = 'wiki:WikiStart' or 'attachment:file.ext'
     url = extract_url(self.env, formatter.context, wikilink)
     rawurl = extract_url(self.env, formatter.context, wikilink, True)
```

## Example



Inside a Trac macro called from the wiki page 'ExamplePage' of project
'project1' on a multi-project trac server:


```wiki
    extract_url(self.env, formatter, 'attachment:file.js', True)
```


will return `/project1/raw-attachment/wiki/ExamplePage/file.js`,
which could be directly accessed by the browser inside some javascript 
or flash HTML object code produced by the macro.
    


### `[[GhcFile]]`


GhcFile macro.


### `[[GhcModule]]`


GhcModule macro.


### `[[Icon]]`


Shows a named icon that can be in line with text.



Syntax:


```wiki
[[Icon(name, size)]]
```


where


- `name` is the name of the icon.  When `name` contains a
  pattern character (`*` or `?`), a 2-column preview of
  matching icons is presented, which should mainly be used for
  finding and selecting an icon during wiki page editing in
  side-by-side mode (however, no more than 32 icons are
  presented to prevent exhaustive network traffic.)
- `size` is optionally one of `small`, `medium` or `large` or
  an abbreviation thereof (defaults `small`).


Example:


```wiki
[[Icon(smiley)]]
```


Use `ShowIcons` for static presentation of available icons.
Smileys like `:-)` are automatically rendered as icons. Use
`ShowSmileys` to se all available smileys.



Following wiki markup is equivalent to using this macro:


```wiki
(|name, size|)
```

### `[[Image]]`


Embed an image in wiki-formatted text.



The first argument is the file specification. The file specification may
reference attachments in three ways:


- `module:id:file`, where module can be either **wiki** or **ticket**,
  to refer to the attachment named *file* of the specified wiki page or
  ticket.
- `id:file`: same as above, but id is either a ticket shorthand or a Wiki
  page name.
- `file` to refer to a local attachment named 'file'. This only works from
  within that wiki page or a ticket.


The file specification may also refer to:


- repository files, using the `source:file` syntax
  (`source:file@rev` works also).
- files, using direct URLs: `/file` for a project-relative,
  `//file` for a server-relative, or `http://server/file` for
  absolute location. An [InterWiki](inter-wiki) prefix may be used.
- embedded data using the
  [ rfc2397](http://tools.ietf.org/html/rfc2397) `data` URL scheme,
  provided the URL is enclosed in quotes.


The remaining arguments are optional and allow configuring the attributes
and style of the rendered `<img>` element:


- digits and unit are interpreted as the size (ex. 120px, 25%)
  for the image
- `right`, `left`, `center`, `top`, `bottom` and `middle` are interpreted
  as the alignment for the image (alternatively, the first three can be
  specified using `align=...` and the last three using `valign=...`)
- `link=some TracLinks...` replaces the link to the image source by the
  one specified using a [TracLinks](trac-links). If no value is specified, the link is
  simply removed.
- `inline` specifies that the content generated be an inline XHTML
  element. By default, inline content is not generated, therefore images
  won't be rendered in section headings and other one-line content.
- `nolink` means without link to image source (deprecated, use `link=`)
- `key=value` style are interpreted as HTML attributes or CSS style
  indications for the image. Valid keys are:

  - align, valign, border, width, height, alt, title, longdesc, class,
    margin, margin-(left,right,top,bottom), id and usemap
  - `border`, `margin`, and `margin-`\* can only be a single number
    (units are pixels).
  - `margin` is superseded by `center` which uses auto margins


Examples:


```wiki
[[Image(photo.jpg)]]               # simplest
[[Image(photo.jpg, 120px)]]        # with image width size
[[Image(photo.jpg, right)]]        # aligned by keyword
[[Image(photo.jpg, nolink)]]       # without link to source
[[Image(photo.jpg, align=right)]]  # aligned by attribute
```


You can use an image from a wiki page, ticket or other module.


```wiki
[[Image(OtherPage:foo.bmp)]]    # from a wiki page
[[Image(base/sub:bar.bmp)]]     # from hierarchical wiki page
[[Image(#3:baz.bmp)]]           # from another ticket
[[Image(ticket:36:boo.jpg)]]    # from another ticket (long form)
[[Image(source:/img/bee.jpg)]]  # from the repository
[[Image(htdocs:foo/bar.png)]]   # from project htdocs dir
[[Image(shared:foo/bar.png)]]   # from shared htdocs dir (since 1.0.2)
```


*Adapted from the Image.py macro created by Shun-ichi Goto
\<gotoh@…\>*


### `[[InterTrac]]`


Provide a list of known [InterTrac](inter-trac) prefixes.


### `[[InterWiki]]`


Provide a description list for the known [InterWiki](inter-wiki) prefixes.


### `[[KnownMimeTypes]]`


List all known mime-types which can be used as [WikiProcessors](wiki-processors).



Can be given an optional argument which is interpreted as mime-type filter.


### `[[MacroList]]`


Display a list of all installed Wiki macros, including documentation if
available.



Optionally, the name of a specific macro can be provided as an argument. In
that case, only the documentation for that macro will be rendered.



Note that this macro will not be able to display the documentation of
macros if the `PythonOptimize` option is enabled for mod\_python!


### `[[PageOutline]]`


Display a structural outline of the current wiki page, each item in the
outline being a link to the corresponding heading.



This macro accepts four optional parameters:


- The first is a number or range that allows configuring the minimum and
  maximum level of headings that should be included in the outline. For
  example, specifying "1" here will result in only the top-level headings
  being included in the outline. Specifying "2-3" will make the outline
  include all headings of level 2 and 3, as a nested list. The default is
  to include all heading levels.
- The second parameter can be used to specify a custom title (the default
  is no title).
- The third parameter selects the style of the outline. This can be
  either `inline` or `pullout` (the latter being the default). The
  `inline` style renders the outline as normal part of the content, while
  `pullout` causes the outline to be rendered in a box that is by default
  floated to the right side of the other content.
- The fourth parameter specifies whether the outline is numbered or not.
  It can be either `numbered` or `unnumbered` (the former being the
  default). This parameter only has an effect in `inline` style.

### `[[ParseArgsTest]]`


Test macro for `tracadvparseargs.parse_args` function.



This macro is intended to be used by the developers of the above function to 
simplify the testing process and has no real value for normal Trac users.


## Macro usage


```wiki
[[ParseArgsTest(parser_options|||arguments_to_parse)]]
```


will call


```
parse_args(arguments_to_parse, **parser_options)
```


and will display its return value. See below for the list of parser options.


## Example


```wiki
[[ParseArgsTest(strict=True,delquotes=True|||key1=val1,key2="val2a,val2b")]]
```


will call


```wiki
parse_args('key1=val1,key2="val2a,val2b"', strict=True, delquotes=True)
```


    


# Description for `parse_args()`



Website: [
http://trac-hacks.org/wiki/AdvParseArgsPlugin](http://trac-hacks.org/wiki/AdvParseArgsPlugin)



This function is used in [WikiMacros](wiki-macros) to parse the macro arguments. This enhanced 
version is meant as a replacement of `trac.wiki.macros.parse_args` and supports 
several advanced options (see section [\#Parameters](wiki-macros#parameters)). The most important feature 
is the support for quoting the delimiter, e.g. 
'`key1=val1,key2="some,text",key3=val3`' will correctly return '`some,text`' as 
the value of `key2`. The original `parse_args` function would return '`"some`' 
and handle '`text"`' as separate argument.



`$Id: parseargs.py 13612 2014-01-24 00:45:47Z rjollos $`


## Documentation


### Definition


```
def parse_args (args, strict = True, multi = False, listonly = False, minlen = 0,
        quotechar = '"', escchar = '\\', delim = ',', delquotes = False)
```

### Usage Example


```
# Instead of: from trac.wiki.macros import parse_args
# Use:
from tracadvparseargs import parse_args

class SomeMacro(WikiMacroBase):
    def expand_macro(self, formatter, name, args):
        largs, kwargs = parse_args( args, <options> )
```

### Parameters


<table><tr><th>`args`</th>
<td>The argument string; 'content' in \`expand\_macro.
</td></tr>
<tr><th>`strict`</th>
<td>Enables strict checking of keys.
</td></tr>
<tr><th>`multi`</th>
<td>Enables folding of muliple given keys into list.

If set to `True`, values of multiple given keys will be returned
as list, but single given keys will return a scalar.

If set to a list, only the values of the listed keys will be
returned as list, but always as list even when there is only one
value.

If this list contains `'*'`, all values are always 
returned as list.
</td></tr>
<tr><th>`listonly`</th>
<td>If true only a list is returned, no directionary.
</td></tr>
<tr><th>`minlen`</th>
<td>Extend returned list to given minimum length. Only used when
`listonly=True`.
</td></tr></table>



**Parser parameters**


<table><tr><th>`quotechar`</th>
<td>The quote character to be used.
</td></tr>
<tr><th>`escchar`</th>
<td>The escape character to be used.
</td></tr>
<tr><th>`delim`</th>
<td>The delimiter character to be used.
</td></tr>
<tr><th>`delquotes`</th>
<td>Selects if quotes should be removed.
</td></tr></table>



    


### `[[ProjectStats]]`


Wiki macro listing some generic Trac statistics.



This macro accepts a comma-separated list of keyed parameters, in the form
"key=value". Valid keys:


- **wiki** -- statistics for [TracWiki](trac-wiki), values:

  - *count* -- show wiki page count
- **prefix** -- use with *wiki* key: only names that start with that
  prefix are included


'count' is also recognized without prepended key name.


### `[[RecentChanges]]`


List all pages that have recently been modified, ordered by the
time they were last modified.



This macro accepts two ordered arguments and a named argument. The named
argument can be placed in any position within the argument list.



The first parameter is a prefix string: if provided, only pages with names
that start with the prefix are included in the resulting list. If this
parameter is omitted, all pages are included in the list.



The second parameter is the maximum number of pages to include in the
list.



The `group` parameter determines how the list is presented:


<table><tr><th>`group=date`</th>
<td>The pages are presented in bulleted lists that are
grouped by date (default).
</td></tr>
<tr><th>`group=none`</th>
<td>The pages are presented in a single bulleted list.
</td></tr></table>



Tip: if you only want to specify a maximum number of entries and
don't want to filter by prefix, specify an empty first parameter,
e.g. `[[RecentChanges(,10,group=none)]]`.


### `[[Redirect]]`


This Trac plug-in implements a server sided redirect functionality.
The user interface is the wiki macro `Redirect` (alternativly `redirect`).


## Description



Website: [
http://trac-hacks.org/wiki/ServerSideRedirectPlugin](http://trac-hacks.org/wiki/ServerSideRedirectPlugin)



`$Id: plugin.py 11890 2012-08-04 01:37:28Z rjollos $`



This plug-in allow to place a redirect macro at the start of any wiki
page which will cause an server side redirect when the wiki page is
viewed.



This plug-in is compatible (i.e. can be used) with the client side
redirect macro TracRedirect but doesn't depend on it. Because the
redirect is caused by the server (using a HTTP redirect request to the
browser) it is much faster and less noticeable for the user. The
back-link feature of TracRedirect can also be used for server side
redirected pages because both generate the same URL attributes.



To edit a redirecting wiki page access its URL with `?action=edit`
appended. To view the page either use `?action=view`, which will print
the redirect target (if TracRedirect isn't active, which will redirect
the wiki using client side code), or `?redirect=no` which disables
redirection of both the ServerSideRedirectPlugin and TracRedirect
plug-in.



Direct after the redirect target is added (or modified) Trac will
automatically reload it, as it does with all wiki pages. This plug-in
will detect this and not redirect but display the wiki page with the
redirect target URL printed to provide feedback about the successful
change. However, further visits will trigger the redirect.


## Usage Examples



The following 'macro' at the begin of the wiki page will cause a
redirect to the *OtherWikiPage*.


```wiki
[[redirect(OtherWikiPage)]]
[[Redirect(OtherWikiPage)]]
```


Any other [TracLink](trac-links) can be used:


```wiki
[[redirect(wiki:OtherWikiPage)]]
[[Redirect(wiki:OtherWikiPage)]]
[[redirect(source:/trunk/file.py)]]
[[Redirect(source:/trunk/file.py)]]
[[redirect(http://www.example.com/)]]
[[Redirect(http://www.example.com/)]]
```


    


### `[[RepositoryIndex]]`


Display the list of available repositories.



Can be given the following named arguments:


<table><tr><th>*format*</th>
<td>
Select the rendering format:

- *compact* produces a comma-separated list of repository prefix
  names (default)
- *list* produces a description list of repository prefix names
- *table* produces a table view, similar to the one visible in
  the *Browse View* page

</td></tr>
<tr><th>*glob*</th>
<td>
Do a glob-style filtering on the repository names (defaults to '\*')
</td></tr>
<tr><th>*order*</th>
<td>
Order repositories by the given column (one of "name", "date" or
"author")
</td></tr>
<tr><th>*desc*</th>
<td>
When set to 1, order by descending order
</td></tr></table>



(*since 0.12*)


### `[[ShowIcons]]`


Renders in a table a list of available icons.
No more than 96 icons are displayed to prevent
exhaustive network traffic.



Syntax:


```wiki
[[ShowIcons(cols, name-pattern, size, header, limit)]]
```


where


- `cols` is optionally the number of columns in the table
  (defaults 3).
- `name-pattern` selects which icons to list (use `*` and
  `?`).
- `size` is optionally one of `small`, `medium` or `large` or
  an abbreviation thereof (defaults `small`).
- `header` is optionally one of `header` and `noheader` or
  an abbreviation thereof (header is displayed by default)
- `limit` specifies an optional upper limit of number of
  displayed icons (however, no more than 96
  will be displayed).


The last three optional parameters (`size`, `header` and
`limit`) can be stated in any order.



Example:


```wiki
[[ShowIcons(smile*)]]              # all small icons matching smile*
[[ShowIcons(4, smile*)]]           # four columns
[[ShowIcons(smile*, 10)]]          # limit to 10 icons
[[ShowIcons(smile*, 10, nohead)]]  # no header
[[ShowIcons(smile*, m)]]           # medium-size
```

### `[[ShowPhrases]]`


Renders in a table the list of known phrases that
are highlighted to catch attention.



Comment: Any delimiter `():<>` adjacent to a phrase will not be
presented. This makes it possible to naturally write `FIXME:`,
for example, but view the phrase highlighted without the colon
(`:`) which would not look natural. Prefixing a phrase with `!`
prevents it from being highlighted.


### `[[SubscriberList]]`


Display a list of all installed notification subscribers, including
documentation if available.



Optionally, the name of a specific subscriber can be provided as an
argument. In that case, only the documentation for that subscriber will
be rendered.



Note that this macro will not be able to display the documentation of
subscribers if the `PythonOptimize` option is enabled for mod\_python!


### `[[TicketQuery]]`


Wiki macro listing tickets that match certain criteria.



This macro accepts a comma-separated list of keyed parameters,
in the form "key=value".



If the key is the name of a field, the value must use the syntax
of a filter specifier as defined in [TracQuery\#QueryLanguage](trac-query#query-language).
Note that this is *not* the same as the simplified URL syntax
used for `query:` links starting with a `?` character. Commas (`,`)
can be included in field values by escaping them with a backslash (`\`).



Groups of field constraints to be OR-ed together can be separated by a
literal `or` argument.



In addition to filters, several other named parameters can be used
to control how the results are presented. All of them are optional.



The `format` parameter determines how the list of tickets is
presented:


- **list** -- the default presentation is to list the ticket ID next
  to the summary, with each ticket on a separate line.
- **compact** -- the tickets are presented as a comma-separated
  list of ticket IDs.
- **count** -- only the count of matching tickets is displayed
- **rawcount** -- only the count of matching tickets is displayed,
  not even with a link to the corresponding query (*since 1.1.1*)
- **table**  -- a view similar to the custom query view (but without
  the controls)
- **progress** -- a view similar to the milestone progress bars


The `max` parameter can be used to limit the number of tickets shown
(defaults to **0**, i.e. no maximum).



The `order` parameter sets the field used for ordering tickets
(defaults to **id**).



The `desc` parameter indicates whether the order of the tickets
should be reversed (defaults to **false**).



The `group` parameter sets the field used for grouping tickets
(defaults to not being set).



The `groupdesc` parameter indicates whether the natural display
order of the groups should be reversed (defaults to **false**).



The `verbose` parameter can be set to a true value in order to
get the description for the listed tickets. For **table** format only.
*deprecated in favor of the `rows` parameter*



The `rows` parameter can be used to specify which field(s) should
be viewed as a row, e.g. `rows=description|summary`



The `col` parameter can be used to specify which fields should
be viewed as columns. For **table** format only.



For compatibility with Trac 0.10, if there's a last positional parameter
given to the macro, it will be used to specify the `format`.
Also, using "&" as a field separator still works (except for `order`)
but is deprecated.


### `[[TitleIndex]]`


Insert an alphabetic list of all wiki pages into the output.



Accepts a prefix string as parameter: if provided, only pages with names
that start with the prefix are included in the resulting list. If this
parameter is omitted, all pages are listed. If the prefix is specified,
a second argument of value `hideprefix` can be given as well, in order
to remove that prefix from the output.



The prefix string supports the standard relative-path notation *when
using the macro in a wiki page*. A prefix string starting with `./`
will be relative to the current page, and parent pages can be
specified using `../`.



Several named parameters can be specified:


- `format=compact`: The pages are displayed as comma-separated links.
- `format=group`: The list of pages will be structured in groups
  according to common prefix. This format also supports a `min=n`
  argument, where `n` is the minimal number of pages for a group.
- `format=hierarchy`: The list of pages will be structured according
  to the page name path hierarchy. This format also supports a `min=n`
  argument, where higher `n` flatten the display hierarchy
- `depth=n`: limit the depth of the pages to list. If set to 0,
  only toplevel pages will be shown, if set to 1, only immediate
  children pages will be shown, etc. If not set, or set to -1,
  all pages in the hierarchy will be shown.
- `include=page1:page*2`: include only pages that match an item in the
  colon-separated list of pages. If the list is empty, or if no `include`
  argument is given, include all pages.
- `exclude=page1:page*2`: exclude pages that match an item in the colon-
  separated list of pages.


The `include` and `exclude` lists accept shell-style patterns.


### `[[TracAdminHelp]]`


Display help for trac-admin commands.



Examples:


```wiki
[[TracAdminHelp]]               # all commands
[[TracAdminHelp(wiki)]]         # all wiki commands
[[TracAdminHelp(wiki export)]]  # the "wiki export" command
[[TracAdminHelp(upgrade)]]      # the upgrade command
```

### `[[TracGuideToc]]`


Display a table of content for the Trac guide.



This macro shows a quick and dirty way to make a table-of-contents
for the Help/Guide. The table of contents will contain the Trac\* and
[WikiFormatting](wiki-formatting) pages, and can't be customized. See the
[ TocMacro](https://trac-hacks.org/wiki/TocMacro) for a more customizable
table of contents.


### `[[TracIni]]`


Produce documentation for the Trac configuration file.



Typically, this will be used in the [TracIni](trac-ini) page. The macro accepts
two ordered arguments and two named arguments.



The ordered arguments are a configuration section filter,
and a configuration option name filter: only the configuration
options whose section and name start with the filters are output.



The named arguments can be specified:


<table><tr><th>section</th>
<td>a glob-style filtering on the section names
</td></tr>
<tr><th>option</th>
<td>a glob-style filtering on the option names
</td></tr></table>


### `[[UserQuery]]`


Wiki macro listing users that match certain criteria.



This macro accepts a comma-separated list of keyed parameters, in the form
"key=value". Valid keys:


- **perm** -- show only that users, a permission action given by *value*
  has been granted to
- **locked** -- retrieve users, who's account has/has not been locked
  depending on boolean value
- **format** -- output style: 'count', 'list' or comma-separated values
  (default)
- **nomatch** -- replacement wiki markup that is displayed, if there's
  no match and output style isn't 'count' either


'count' is also recognized without prepended key name. Other non-keyed
parameters are:


- **locked** -- alias for 'locked=True'
- **visit** -- show a list of accounts with last-login information, only
  available in table format
- **name** -- forces replacement of maching username with their
  corresponding full names, if available; adds a full names column if combined
  with 'visit'
- **email** -- append email address to usernames, if available


Requires `USER_VIEW` permission for output in any format other then 'count'.
A misc placeholder with this statement is presented to unprivileged users.


### `[[Workflow]]`


Render a workflow graph.



This macro accepts a [TracWorkflow](trac-workflow) configuration and renders the states
and transitions as a directed graph. If no parameters are given, the
current ticket workflow is rendered. In [WikiProcessors](wiki-processors) mode the `width`
and `height` arguments can be specified.



(Defaults: `width = 800` and `height = 600`)



Examples:


```wiki
    [[Workflow()]]

    [[Workflow(go = here -> there; return = there -> here)]]

    {{{
    #!Workflow width=700 height=700
    leave = * -> *
    leave.operations = leave_status
    leave.default = 1

    create = <none> -> new
    create.default = 1

    create_and_assign = <none> -> assigned
    create_and_assign.label = assign
    create_and_assign.permissions = TICKET_MODIFY
    create_and_assign.operations = may_set_owner

    accept = new,assigned,accepted,reopened -> accepted
    accept.permissions = TICKET_MODIFY
    accept.operations = set_owner_to_self

    resolve = new,assigned,accepted,reopened -> closed
    resolve.permissions = TICKET_MODIFY
    resolve.operations = set_resolution

    reassign = new,assigned,accepted,reopened -> assigned
    reassign.permissions = TICKET_MODIFY
    reassign.operations = set_owner

    reopen = closed -> reopened
    reopen.permissions = TICKET_CREATE
    reopen.operations = del_resolution
    }}}
```

### `[[box]]`


View wiki text in a box.



Syntax:


```wiki
{{{#!box type align=... width=...
wiki text
}}}
```


or preferably when content is short:


```wiki
[[box(wiki text, type=..., align=..., width=...)]]
```


where


- `type` is an optional flag, or parameter, to call for
  attention depending on type of matter. When `type` is set,
  the box is decorated with an icon (except for `news`) and
  colored, depending on what *urgency* the type represents:

>
> >
> > <table><tr><th> Urgency *(box color)* </th>
> > <th> type 
> > </th></tr>
> > <tr><th> warn *(red)* </th>
> > <th> `bug`, `critical`, `error`, `important`, `stop`, `warning` 
> > </th></tr>
> > <tr><th> highlight *(yellow)* </th>
> > <th> `help`, `information`, `note`, `question`, `tips` 
> > </th></tr>
> > <tr><th> elaborate *(blue)* </th>
> > <th> `bad`, `chat`, `comment`, `discussion`, `good`, `no`, `nok`, `ok`, `talk`, `yes` 
> > </th></tr>
> > <tr><th> news *(green)* </th>
> > <th> `news` 
> > </th></tr>
> > <tr><th> normal *(white)* </th>
> > <th> `configuration`, `configure`, `details`, `look`, `magnifier`, `tool` 
> > </th></tr></table>
> >
> >
> >
> > `type` may be abbreviated as long as the abbreviation is
> > unique for one of the keywords above.
> >
> >
>

- `align` is optionally one of `right`, `left` or `center`.
  The `rbox` macro is an alias for `align=right`.
- `width` is optional and sets the width of the box (defaults
  `auto` except for right aligned boxes which defaults a fixed
  width). `width` should be set when `align=center` for
  proper results.


Examples:


```wiki
{{{#!box warn
= Warning
Beware of the bugs
}}}

[[box(Beware of the bugs, type=warn)]]
```


A `style` parameter is also accepted, to allow for custom
styling of the box. See also the `rbox`, `newsbox` and
`imagebox` macros (processors).


### `[[imagebox]]`


Present a centered box suitable for one image.



Syntax:


```wiki
{{{#!imagebox
wiki text
}}}
```


This box is typically used together with the `Image` macro:


```wiki
{{{#!imagebox
[[Image(file)]]

Caption
}}}
```


Note that the `size` parameter of the `Image` macro may not
behave as expected when using relative sizes (`%`).



The following parameters are also accepted:


- `align` -- One of `right`, `left` or `center` (defaults
  `center`).
- `width` -- Set the width of the box (defaults `auto` except
  for right aligned boxes which defaults a fixed width).
- `style` -- Custom styling of the box.


See also the `box`, `rbox` and `newsbox` macros (processors).


### `[[newsbox]]`


Present a news box to the right. (This is a shorthand for
`rbox news`)



Syntax:


```wiki
{{{#!newsbox
wiki text
}}}
```


The following parameters are also accepted:


- `width` -- Set the width of the box (defaults a fixed
  width).
- `style` -- Custom styling of the box.


See also the `box`, `rbox` and `imagebox` macros (processors).
*(Comment: This box corresponds to the well-known
*`NewsFlash`* macro.)*


### `[[rbox]]`


View a right-aligned box. (This is a shorthand for
`box align=right`)



Syntax:


```wiki
{{{#!rbox type width=...
wiki text
}}}
```


or preferably when content is short:


```wiki
[[rbox(wiki text, type=..., width=...)]]
```


where


- `type` is an optional flag, or parameter, to call for
  attention depending on type of matter. When `type` is set,
  the box is decorated with an icon (except for `news`) and
  colored, depending on what *urgency* the type represents:

>
> >
> > <table><tr><th> Urgency *(box color)* </th>
> > <th> type 
> > </th></tr>
> > <tr><th> warn *(red)* </th>
> > <th> `bug`, `critical`, `error`, `important`, `stop`, `warning` 
> > </th></tr>
> > <tr><th> highlight *(yellow)* </th>
> > <th> `help`, `information`, `note`, `question`, `tips` 
> > </th></tr>
> > <tr><th> elaborate *(blue)* </th>
> > <th> `bad`, `chat`, `comment`, `discussion`, `good`, `no`, `nok`, `ok`, `talk`, `yes` 
> > </th></tr>
> > <tr><th> news *(green)* </th>
> > <th> `news` 
> > </th></tr>
> > <tr><th> normal *(white)* </th>
> > <th> `configuration`, `configure`, `details`, `look`, `magnifier`, `tool` 
> > </th></tr></table>
> >
> >
> >
> > `type` may be abbreviated as long as the abbreviation is
> > unique for one of the keywords above.
> >
> >
>

- `width` is optional and sets the width of the box (defaults
  a fixed width). Use `width=auto` for an automatically sized
  box.


Examples:


```wiki
{{{#!rbox warn
= Warning
Beware of the bugs
}}}

[[rbox(Beware of the bugs, type=warn)]]
```


A `style` parameter is also accepted, to allow for custom
styling of the box. See also the `box`, `newsbox` and
`imagebox` macros (processors).


### `[[ref]]`


Reference section headers in the current page. To refer to the section
[Using Macros](#UsingMacros), use


```wiki
[[ref(Using Macros)]]
```



## Macros from around the world



The [
Trac Hacks](http://trac-hacks.org/) site provides a wide collection of macros and other Trac [plugins](trac-plugins) contributed by the Trac community. If you are looking for new macros, or have written one that you would like to share, please visit that site.


## Developing Custom Macros



Macros, like Trac itself, are written in the [
Python programming language](http://python.org/) and are developed as part of [TracPlugins](trac-plugins).



For more information about developing macros, see the [
development resources](http://trac.edgewall.org/intertrac/TracDev) on the main project site.



Here are 2 simple examples showing how to create a Macro. Also, have a look at [
Timestamp.py](http://trac.edgewall.org/intertrac/source%3Atags/trac-1.0.2/sample-plugins/Timestamp.py) for an example that shows the difference between old style and new style macros and at the [
macros/README](http://trac.edgewall.org/intertrac/source%3Atags/trac-0.11/wiki-macros/README) which provides more insight about the transition.


### Macro without arguments



To test the following code, save it in a `timestamp_sample.py` file located in the [TracEnvironment](trac-environment)'s `plugins/` directory.


```
from datetime import datetime
# Note: since Trac 0.11, datetime objects are used internally

from genshi.builder import tag

from trac.util.datefmt import format_datetime, utc
from trac.wiki.macros import WikiMacroBase

class TimeStampMacro(WikiMacroBase):
    """Inserts the current time (in seconds) into the wiki page."""

    revision = "$Rev$"
    url = "$URL$"

    def expand_macro(self, formatter, name, text):
        t = datetime.now(utc)
        return tag.strong(format_datetime(t, '%c'))
```

### Macro with arguments



To test the following code, save it in a `helloworld_sample.py` file located in the [TracEnvironment](trac-environment)'s `plugins/` directory.


```
from genshi.core import Markup

from trac.wiki.macros import WikiMacroBase

class HelloWorldMacro(WikiMacroBase):
    """Simple HelloWorld macro.

    Note that the name of the class is meaningful:
     - it must end with "Macro"
     - what comes before "Macro" ends up being the macro name

    The documentation of the class (i.e. what you're reading)
    will become the documentation of the macro, as shown by
    the !MacroList macro (usually used in the WikiMacros page).
    """

    revision = "$Rev$"
    url = "$URL$"

    def expand_macro(self, formatter, name, text, args):
        """Return some output that will be displayed in the Wiki content.

        `name` is the actual name of the macro (no surprise, here it'll be
        `'HelloWorld'`),
        `text` is the text enclosed in parenthesis at the call of the macro.
          Note that if there are ''no'' parenthesis (like in, e.g.
          [[HelloWorld]]), then `text` is `None`.
        `args` are the arguments passed when HelloWorld is called using a
        `#!HelloWorld` code block.
        """
        return 'Hello World, text = %s, args = %s' % \
            (Markup.escape(text), Markup.escape(repr(args)))

```


Note that `expand_macro` optionally takes a 4<sup>th</sup> parameter *`args`*. When the macro is called as a [WikiProcessor](wiki-processors), it is also possible to pass `key=value` [processor parameters](wiki-processors#). If given, those are stored in a dictionary and passed in this extra `args` parameter. In the other case, when called as a macro, `args` is `None`. (*since 0.12*).



For example, when writing:


```wiki
{{{#!HelloWorld style="polite" -silent verbose
<Hello World!>
}}}

{{{#!HelloWorld
<Hello World!>
}}}

[[HelloWorld(<Hello World!>)]]
```


One should get:


```wiki
Hello World, text = <Hello World!>, args = {'style': u'polite', 'silent': False, 'verbose': True}
Hello World, text = <Hello World!>, args = {}
Hello World, text = <Hello World!>, args = None
```


Note that the return value of `expand_macro` is **not** HTML escaped. Depending on the expected result, you should escape it yourself (using `return Markup.escape(result)`) or, if this is indeed HTML, wrap it in a Markup object (`return Markup(result)`) with `Markup` coming from Genshi (`from genshi.core import Markup`).



You can also recursively use a wiki Formatter (`from trac.wiki import Formatter`) to process the `text` as wiki markup:


```
from genshi.core import Markup
from trac.wiki.macros import WikiMacroBase
from trac.wiki import Formatter
import StringIO

class HelloWorldMacro(WikiMacroBase):
    def expand_macro(self, formatter, name, text, args):
        text = "whatever '''wiki''' markup you want, even containing other macros"
        # Convert Wiki markup to HTML, new style
        out = StringIO.StringIO()
        Formatter(self.env, formatter.context).format(text, out)
        return Markup(out.getvalue())
```