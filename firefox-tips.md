
**Note: [Browser Tips](browser-tips) is now the updated wiki page on browser tips.**


---


# Firefox Tips


## Finding tickets by number



Add a new bookmark/entry, with


```wiki
Name: GHC ticket
Location: http://ghc.haskell.org/trac/ghc/ticket/%s
Keyword: ghc
```


Then typing `ghc 5129` in the title bar goes to that ticket (note the
space).


## Searching for tickets



Add a new bookmark, with


```wiki
Name: [t] GHC ticket search
Location: http://www.google.com/search?q=site:ghc.haskell.org/trac/ghc/ticket%20%s
Keyword: t
```


Now typing `t <query>` into the title bar searches the GHC ticket
database using Google, which is much faster than Trac's search.


