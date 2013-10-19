
# Wiki Icons



More than 3.300 icons are available using the following wiki markup:


```wiki
(|name, size|)
```


This shows a named icon that can be in line with text, where


- `name` is the name of the icon.
- `size` is optionally one of `small`, `medium` or `large` or an abbreviation
  thereof (defaults `small`).


Examples:


>
> <table><tr><th> Wiki markup </th>
> <th> Display 
> </th></tr>
> <tr><th>```wiki
> (|trac|) and (|subversion|) is like (|yin-yang|)
> ```
>
> </th>
> <th>  not handled: Image and not handled: Image is like not handled: Image  
> </th></tr>
> <tr><th>```wiki
> (|trac|) (|trac, medium|) (|trac, large|)
> ```
>
> </th>
> <th>  not handled: Image not handled: Image not handled: Image  
> </th></tr>
> <tr><th>```wiki
> (|clock|) (|calendar|) (|chart|) (|eye|) (|tick|)
> ```
>
> </th>
> <th>  not handled: Image not handled: Image not handled: Image not handled: Image not handled: Image  
> </th></tr></table>
>
>


When `name` contains a pattern character (`*` or `?`), a 2-column preview of
matching icons is presented, which should mainly be used for finding and
selecting an icon during wiki page editing in side-by-side mode. (The number of
displayed icons is limited to prevent exhaustive network traffic. Use
`ShowIcons` for static presentation of available icons.)



Example of the 2-column search preview:


```wiki
(|arrow*|)
```

>
>
> <small>Showing 32 of 122 small icons matching arrow\*</small>
>
> <table><tr><th>not handled: Image </th>
> <th>arrow</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-in</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-000-medium</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-in-out</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-045</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-join</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-090</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-join-090</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-135</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-merge</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-135-medium</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-move</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-180</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-out</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-225</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-repeat</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-225-medium</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-resize</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-270</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-return</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-315</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-skip</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-315-medium</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-split</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-branch</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-switch</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-branch-000-left</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-transition</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-circle</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-transition-090</th></tr>
> <tr><th>not handled: Image </th>
> <th>arrow-curve</th>
> <th></th>
> <th>not handled: Image </th>
> <th>arrow-turn</th></tr></table>
>
>
>


Narrow down your search until you find the wanted icon.



The [Icon macro](wiki-macros#) is equivalent to this wiki markup.


## Smileys



Following character sequences are defined as *smileys* and are automatically
displayed as icons:


>
>
> ShowSmileys?
>
>


Example:


>
> <table><tr><th> Wiki markup </th>
> <th> Display 
> </th></tr>
> <tr><th>```wiki
> This is a great feature :-)
> ```
>
> </th>
> <th>  This is a great feature :-)  
> </th></tr></table>
>
>


Prefixing a smiley character sequence with an exclamation mark (`!`) prevents
it from being interpreted as a smiley.



The list above was generated by the
[ShowSmileys macro](wiki-macros#). See also
[TracIni](trac-ini#), for configuration of smileys.


## ShowIcons macro



Use the [ShowIcons macro](wiki-macros#) to display a list of
available icons. (The number of displayed icons is limited to prevent
exhaustive network traffic.)



Syntax:


```wiki
[[ShowIcons(cols, name-pattern, size, header, limit)]]
```


where


- `cols` is optionally the number of columns in the table (defaults 3).
- `name-pattern` selects which icons to list (use `*` and `?`).
- `size` is optionally one of `small`, `medium` or `large` or an abbreviation
  thereof (defaults `small`).
- `header` is optionally one of `header` and `noheader` or an abbreviation
  thereof (header is displayed by default)
- `limit` specifies an optional upper limit of number of displayed icons.


The last three optional parameters (`size`, `header` and `limit`) can be stated
in any order.



Example:


```wiki
[[ShowIcons(smile*)]]              # all small icons matching smile*
[[ShowIcons(4, smile*)]]           # four columns
[[ShowIcons(smile*, 10)]]          # limit to 10 icons
[[ShowIcons(smile*, 10, nohead)]]  # no header
[[ShowIcons(smile*, m)]]           # medium-size
```


Another example:


```wiki
[[ShowIcons(*, 26)]]
```

>
>
> <small>Showing 26 of 3474 small icons matching \*</small>
>
> <table><tr><th>not handled: Image </th>
> <th>abacus</th>
> <th></th>
> <th>not handled: Image </th>
> <th>jar</th>
> <th></th>
> <th>not handled: Image </th>
> <th>safe</th></tr>
> <tr><th>not handled: Image </th>
> <th>baggage-cart</th>
> <th></th>
> <th>not handled: Image </th>
> <th>key</th>
> <th></th>
> <th>not handled: Image </th>
> <th>table</th></tr>
> <tr><th>not handled: Image </th>
> <th>cactus</th>
> <th></th>
> <th>not handled: Image </th>
> <th>language</th>
> <th></th>
> <th>not handled: Image </th>
> <th>ui-accordion</th></tr>
> <tr><th>not handled: Image </th>
> <th>dashboard</th>
> <th></th>
> <th>not handled: Image </th>
> <th>mac-os</th>
> <th></th>
> <th>not handled: Image </th>
> <th>validation</th></tr>
> <tr><th>not handled: Image </th>
> <th>ear</th>
> <th></th>
> <th>not handled: Image </th>
> <th>na</th>
> <th></th>
> <th>not handled: Image </th>
> <th>wall</th></tr>
> <tr><th>not handled: Image </th>
> <th>feed</th>
> <th></th>
> <th>not handled: Image </th>
> <th>occluder</th>
> <th></th>
> <th>not handled: Image </th>
> <th>xfn</th></tr>
> <tr><th>not handled: Image </th>
> <th>game</th>
> <th></th>
> <th>not handled: Image </th>
> <th>paint-brush</th>
> <th></th>
> <th>not handled: Image </th>
> <th>yin-yang</th></tr>
> <tr><th>not handled: Image </th>
> <th>haiku</th>
> <th></th>
> <th>not handled: Image </th>
> <th>question</th>
> <th></th>
> <th>not handled: Image </th>
> <th>zodiac</th></tr>
> <tr><th>not handled: Image </th>
> <th>ice</th>
> <th></th>
> <th>not handled: Image </th>
> <th>radar</th>
> <th></th>
> <th> </th>
> <th></th></tr></table>
>
>
>

---



**Icon Library License Terms**



The icon library contained herein is composed of the
[ Fugue icon library](http://p.yusukekamiyamane.com) with additional icons,

and can be used for any commercial or personal projects, but you may not lease,
license or sublicense the icons.



The [ Fugue icon library](http://p.yusukekamiyamane.com) is released under
[
Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/).

Some icons are Copyright (C) [
Yusuke Kamiyamane](http://p.yusukekamiyamane.com/).
All rights reserved.



Additional icons are released under same
[ license terms](http://trac.edgewall.org/wiki/TracLicense) as Trac.

Some icons are Copyright (C) [ Edgewall Software](http://www.edgewall.org).
All rights reserved.



