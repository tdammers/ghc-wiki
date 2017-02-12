# CamelCase



New wiki links are automatically created when concatenating capitalized words, such that for example the word 'Camel' and the word 'Case' concatenated form a link to this [CamelCase](camel-case) page. 



CamelCase is the original wiki convention for creating hyperlinks, with the additional requirement that the capitals are followed by a lower-case letter; hence "AlabamA" and "ABc" will not be links.


## Customizing the Wiki behavior



While Trac remains faithful to the original Wiki style, it provides a number of ways to accommodate users with different preferences:


- To prevent the creation of a new link of a camel-cased word, prefix with an exclamation mark (`!`): `!CamelCase`.
- To ignore links to missing pages when the link is written using the [CamelCase](camel-case) style, that word can instead be replaced by a gray link followed by a question mark. This is useful in cases when [CamelCase](camel-case) style is used to name code artefacts like class names and there is no corresponding page for them. This option can be set in `ignore_missing_pages` in the [\[wiki\]](trac-ini#) section of [TracIni](trac-ini).
- To automatically insert space characters between the words of a [CamelCase](camel-case) link when rendering the link, use `split_page_names` in the [\[wiki\]](trac-ini#) section of [TracIni](trac-ini).
- Creation of explicit Wiki links is also easy, see [WikiPageNames](wiki-page-names) for details.
- Wiki formatting can be disabled completely in some places, for example when rendering commit log messages. See `wiki_format_messages` in the [\[changeset\]](trac-ini#) section of [TracIni](trac-ini).


See [TracIni](trac-ini) for more information on the available options.


## More information on CamelCase


- [ http://c2.com/cgi/wiki?WikiCase](http://c2.com/cgi/wiki?WikiCase)
- [
  http://en.wikipedia.org/wiki/CamelCase](http://en.wikipedia.org/wiki/CamelCase)

---



See also: [WikiPageNames](wiki-page-names), [WikiNewPage](wiki-new-page), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki)


