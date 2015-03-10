# Notes on Trac Wiki formatting



The GHC Wiki is open to everyone to edit, but you need to be logged in:


- [Register](/trac/ghc/trac/ghc/register) an account, so that you can edit pages
- If you don't want an account, for some reason, you [log in](/trac/ghc/trac/ghc/login) as user **guest** with password **guest**.


When writing Wiki pages you need to know the markup conventions:


- The [Help/Guide link](trac-guide) at the top of  every page gives a good description of the markup language etc.  
- From there you can get to the [main guide to the Trac Wiki formatting](wiki-formatting).  
- The "Wiki notes" link in the green sidebar on every page gets you to this page.


The notes on the rest of this page are the GHC team's observations and/or clarifications.


## Guidelines


- Don't duplicate information. Don't link everything from everywhere.
- If for example a page accessible from the left side bar (i.e. [Building Guide](building)) already has all the links for a certain topic (i.e. Testing), then other pages (i.e. [WorkingConventions](working-conventions) and [Debugging](debugging)) shouldn't duplicate all those links.


 


## Pointing to source files



Use the syntax `[[GhcFile(<file>)]]` or `source:<file>`, e.g. [README.md](/trac/ghc/browser/ghc/README.md) or [source:README.md](/trac/ghc/browser/README.md)[](/trac/ghc/export/HEAD/ghc/README.md) to point to source files in the GHC repository.  We can't currently point directly to source files in other repositories this way, although when we upgrade Trac to 0.12 the multi-repository support should enable us to do this.


## Pointing to commits



Example (see also [WorkingConventions/Git](working-conventions/git#)):


```wiki
In [changeset:"970816ac0028f2f42ac4140d29e2f0dfe0e9af3e/ghc"]:
{{{
#!CommitTicketReference repository="ghc" revision="970816ac0028f2f42ac4140d29e2f0dfe0e9af3e"
}}}
```


Result:


>
>
> In [970816ac0028f2f42ac4140d29e2f0dfe0e9af3e/ghc](/trac/ghc/changeset/970816ac0028f2f42ac4140d29e2f0dfe0e9af3e/ghc):
>
>
> ```message
> Use let !y = x in .. x .. instead of seq in $! and evaluate (#2273)
> ```


## Formatting list items with multiple paragraphs



[TracWiki](trac-wiki) recognizes list items by their indentation and the asterisk preceding the first line. However if a second paragraph is started after an empty line, [TracWiki](trac-wiki) will not consider it part of the list item despite the use of indentation. The "proper" way to do it is to separate the second and subsequent paragraphs by line break macros. For example:


```wiki
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
   Using line breaks we can start another paragraph.
   [[br]][[br]]
   And another
```

---


- A list item paragraph consists of a leading asterisk
  and indented text lines.

  Using line breaks we can start another paragraph.

  And another

---


## Formatting list items with multiple paragraphs and nested lists



Unfortunately the break line trick does not work for list items that contain nestes lists. Consider for example:


```wiki
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
   [[br]][[br]]
   See how this line gets added to the wrong list?
```

---


- A list item paragraph consists of a leading asterisk
  and indented text lines.

  - A nested list starts by adding additional space
    for the next list item.
  - Hey this one has another list element.


  See how this line gets added to the wrong list?

---



For the moment (until [TracWiki](trac-wiki) is fixed) the paragraphs following the nested list maybe formated as quotes.
Quotes happen to have the same indentation is list. Ugly, yes, but good enough for now.


```wiki
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
  See how this line starts at column two instead of column three?
```

---


- A list item paragraph consists of a leading asterisk
  and indented text lines.

  - A nested list starts by adding additional space
    for the next list item.
  - Hey this one has another list element.

  See how this line starts at column two instead of column three?

---


## Links to page sections



The GHC [TracWiki](trac-wiki) provides the `ref` macro to link to sections of a page.
To link to the previous section just invoke the ref macro as follows:


```wiki
[[ref(Formatting list items with multiple paragraphs and nested lists)]]
```

---



[Formatting list items with multiple paragraphs and nested lists](#Formattinglistitemswithmultipleparagraphsandnestedlists)


---


## Links to sections of different pages



For the moment there is no such macro to link to sections of other pages.
What you should do is to take note of the anchor that appears in the HTML source,
and add the anchor to a wiki link right after a hash:


```wiki
[wiki:Building/Using#StandardTargets standard targets]
```

---



[standard targets](building/using#standard-targets)


---


