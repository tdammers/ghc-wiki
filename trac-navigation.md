# Trac Navigation



The main and meta navigation entries can be customized in some basic ways. The `[mainnav]` and `[metanav]` configuration sections can be used to customize the navigation item text and link, change the ordering of the navigation items, or even disable them.


### `[mainnav]`



`[mainnav]` corresponds to the **main navigation bar**, the one containing entries such as *Wiki*, *Timeline*, *Roadmap*, *Browse Source* and so on. This navigation bar is meant to access the default page of the main modules enabled in Trac that are accessible for the current user.



** Example ** 



In the following example we rename the link to [WikiStart](wiki-start) *Home*, and make the *View Tickets* entry link to a specific report.


```
[mainnav]
wiki.label = Home
tickets.href = /report/24
```

### `[metanav]`



`[metanav]` corresponds to the **meta navigation bar**, by default positioned above the main navigation bar and below the *Search* box. It contains the *Login*, *Logout*, *Help/Guide* etc. entries. This navigation bar is meant to access some global information about the Trac project and the current user.



There is one special entry in the  `[metanav]` section: `logout.redirect` is the page the user sees after hitting the logout button.  The *Help/Guide* link is also hidden in the following example.



** Example ** 


```
[metanav]
help = disabled
logout.redirect = wiki/Logout
```

### URL Formats



Possible URL formats for `.href` or `.redirect`:


<table><tr><th> **config** </th>
<th> **redirect to** 
</th></tr>
<tr><th> `wiki/Logout` </th>
<th> `/projects/env/wiki/Logout` 
</th></tr>
<tr><th> `http://hostname/` </th>
<th> `http://hostname/` 
</th></tr>
<tr><th> `/projects` </th>
<th> `/projects` 
</th></tr></table>


### Ordering



The `order` attribute specifies the order in which the navigation items are displayed. This can be particularly useful for plugins that add navigation items.



Non-negative floating point values may be used for the `order` attribute. The navigation items will be arranged from left to right in increasing order. Navigation items without an `order` attribute are sorted alphabetically by name.



The default values are:


```
[mainnav]
browser.order = 4
newticket.order = 6
roadmap.order = 3
search.order = 7
tickets.order = 5
timeline.order = 2
wiki.order = 1

[metanav]
about.order = 5
help.order = 4
login.order = 1
logout.order = 2
prefs.order = 3
```

### Context Navigation



Note that it is still not possible to customize the **contextual navigation bar**, ie the one usually placed below the main navigation bar.


---



See also: [TracInterfaceCustomization](trac-interface-customization), and the [
TracHacks:NavAddPlugin](http://trac-hacks.org/wiki/NavAddPlugin) or [
TracHacks:MenusPlugin](http://trac-hacks.org/wiki/MenusPlugin) (still needed for adding entries)


