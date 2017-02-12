# Wiki Page Names






Wiki page names commonly use the [CamelCase](camel-case) convention. Within wiki text, any word in [CamelCase](camel-case) automatically becomes a hyperlink to the wiki page with that name.



[CamelCase](camel-case) page names follow these rules:


1. The name must consist of **alphabetic characters only**; no digits, spaces, punctuation or underscores are allowed.
1. A name must have at least two capital letters.
1. The first character must be capitalized.
1. Every capital letter must be followed by one or more lower-case letters. 
1. The use of slash ( / ) is permitted in page names, where it typically represents a hierarchy.


If you want to create a wiki page that does not follow [CamelCase](camel-case) rules. you can use the following syntax:


```wiki
 * [wiki:Wiki_page], [wiki:ISO9000],
   and with a label: [wiki:ISO9000 ISO 9000 standard]
 * [wiki:"Space Matters"]
   and with a label: [wiki:"Space Matters" all about white space]
 * or simply: ["WikiPageName"]s
 * even better, the [[WikiCreole link style]]
   and with a label: [[WikiCreole link style|WikiCreole style links]]
```


This will be rendered as:


- Wiki\_page?, ISO9000?,
  and with a label: ISO 9000 standard?
- Space Matters? *(that page name embeds space characters)*
  and with a label: all about white space?
- or simply: WikiPageName?s
- even better, the WikiCreole link style?
  and with a label: WikiCreole style links?


It is possible to link to a specific *version* of a Wiki page as you would do for a specific version of a file, for example: [WikiStart\@1](wiki-start?version=1).



You can prevent a CamelCase name from being interpreted as a [link](trac-links) by quoting it with an exclamation mark: `!CamelCase`. See [TracLinks\#EscapingLinks](trac-links#escaping-links).



As in the example above, you can append an anchor to a Wiki page name to link to a specific section within that page. The anchor can be seen by hovering the mouse over a section heading, then clicking on the ¶ sign that appears at its end. The anchor is usually generated automatically, but it is also possible to specify it explicitly: see [WikiFormatting\#using-explicit-id-in-heading](wiki-formatting#).



There are a few options that govern the rendering of [WikiPageNames](wiki-page-names):


- [CamelCase](camel-case) linking to missing pages can be disabled with the `ignore_missing_pages` [
  option](https://trac.edgewall.org/wiki/TracIni#wiki-section). Linking to missing pages is enabled by default.
- The `split_page_names` option, when enabled, will split [CamelCase](camel-case) words when rendering a link. For example, [WikiStart](wiki-start) will be rendered as [Wiki Start](wiki-start).

---



See also: [WikiNewPage](wiki-new-page), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracLinks](trac-links)


