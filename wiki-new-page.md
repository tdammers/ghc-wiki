# Steps to Add a New Wiki Page






You can create a new wiki page by typing the [CamelCase](camel-case) name of the page in the quick-search field at the top of the page, or by trying to view a wiki page of that name. Note that a page is "orphaned" by default until it is linked to from another page. 



You must be granted permission to create wiki pages. If you don't see the **Create this page** button when visiting a non-existent wiki page, you lack appropriate permission. Contact your local Trac administrator for more information.



A new wiki page can also be created as follows:


1. Choose a name for your new page. See [WikiPageNames](wiki-page-names) for naming conventions.
1. Edit an existing page or any other resource that support [WikiFormatting](wiki-formatting) and add a [link](trac-links) to your new page. If your page name is [CamelCase](camel-case), you can simply type the page name, provided the [ignore\_missing\_pages option](trac-ini#) is `disabled` (the default).
1. Save your changes.
1. Follow the link you created to take you to the new page.
1. Click the **Create this page** button to enter edit mode and add content to your new page.
1. Save your changes to publish your page.

## Rename a page



While choosing a good [page name](wiki-page-names) is important for usability purposes, you can always rename the page later. You will need the WIKI\_RENAME permission to rename pages.



When renaming a page, you'll be offered the possibility to create a page at the old location that contains a link to the new page, so that links pointing to the old location are not "dangling" (i.e. pointing to a non-existent page).


---



See also: [TracWiki](trac-wiki), [PageTemplates](page-templates), [WikiFormatting](wiki-formatting), [TracLinks](trac-links), [WikiDeletePage](wiki-delete-page)


