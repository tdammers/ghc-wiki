# Symbol Names



Since Haskell allows many symbols in constructor and variable names that C compilers or assembly might not allow (e.g. :, %, \#) these have to be encoded using z-encoding.  The encoding is as follows.  See [compiler/utils/Encoding.hs](/trac/ghc/browser/ghc/compiler/utils/Encoding.hs).


## Tuples


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> `()`  </th>
<th> Z0T     </th>
<th> Unit / 0-tuple 
</th></tr>
<tr><th>         </th>
<th>         </th>
<th> There is no Z1T 
</th></tr>
<tr><th> `(,)` </th>
<th> Z2T </th>
<th> 2-tuple 
</th></tr>
<tr><th> `(,,)` </th>
<th> Z3T </th>
<th> 3-tuple 
</th></tr>
<tr><th> ... </th>
<th> </th>
<th> And so on 
</th></tr></table>


## Unboxed Tuples


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th>         </th>
<th>         </th>
<th> There is no Z0H 
</th></tr>
<tr><th> `(# #)` </th>
<th> Z1H  </th>
<th> unboxed 1-tuple (note the space) 
</th></tr>
<tr><th> `(#,#)` </th>
<th> Z2H  </th>
<th> unboxed 2-tuple 
</th></tr>
<tr><th> `(#,,#)` </th>
<th> Z3H  </th>
<th> unboxed 3-tuple 
</th></tr>
<tr><th> ... </th>
<th> </th>
<th> And so on 
</th></tr></table>


## Alphanumeric Characters


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> a-y, A-Y, 0-9 </th>
<th> a-y, A-Y, 0-9 </th>
<th> Regular letters don't need escape sequences 
</th></tr>
<tr><th> z, Z </th>
<th> zz, ZZ </th>
<th> 'Z' and 'z' must be escaped 
</th></tr></table>


## Constructor Characters


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Comment 
</th></tr>
<tr><th> `(` </th>
<th> ZL </th>
<th> Left 
</th></tr>
<tr><th> `)` </th>
<th> ZR </th>
<th> Right 
</th></tr>
<tr><th> `[` </th>
<th> ZM </th>
<th> 'M' before 'N' in \[\] 
</th></tr>
<tr><th> `]` </th>
<th> ZN </th>
<th> 
</th></tr>
<tr><th> `:` </th>
<th> ZC </th>
<th> Colon 
</th></tr></table>


## Variable Characters


<table><tr><th> Decoded </th>
<th> Encoded </th>
<th> Mnemonic 
</th></tr>
<tr><th> `&` </th>
<th> za </th>
<th> Ampersand 
</th></tr>
<tr><th> `|` </th>
<th> zb </th>
<th> Bar 
</th></tr>
<tr><th> `^` </th>
<th> zc </th>
<th> Caret 
</th></tr>
<tr><th> `$` </th>
<th> zd </th>
<th> Dollar 
</th></tr>
<tr><th> `=` </th>
<th> ze </th>
<th> Equals 
</th></tr>
<tr><th> `>` </th>
<th> zg </th>
<th> Greater than 
</th></tr>
<tr><th> `#` </th>
<th> zh </th>
<th> Hash 
</th></tr>
<tr><th> `.` </th>
<th> zi </th>
<th> The dot of the 'i' 
</th></tr>
<tr><th> `<` </th>
<th> zl </th>
<th> Less than 
</th></tr>
<tr><th> `-` </th>
<th> zm </th>
<th> Minus 
</th></tr>
<tr><th> `!` </th>
<th> zn </th>
<th> Not 
</th></tr>
<tr><th> `+` </th>
<th> zp </th>
<th> Plus 
</th></tr>
<tr><th> `'` </th>
<th> zq </th>
<th> Quote 
</th></tr>
<tr><th> `\` </th>
<th> zr </th>
<th> Reverse slash 
</th></tr>
<tr><th> `/` </th>
<th> zs </th>
<th> Slash 
</th></tr>
<tr><th> `*` </th>
<th> zt </th>
<th> Times sign 
</th></tr>
<tr><th> `_` </th>
<th> zu </th>
<th> Underscore 
</th></tr>
<tr><th> `%` </th>
<th> zv </th>
<th> (TODO I don't know what the mnemonic for this one is. Perhaps relatiVe or diVide?) 
</th></tr></table>


## Other



Any other character is encoded as a 'z' followed by its hex code (lower case, variable length) followed by 'U'.  If the hex code starts with 'a', 'b, 'c', 'd', 'e' or 'f', then an extra '0' is placed before the hex code to avoid conflicts with the other escape characters.


## Examples


<table><tr><th> Before       </th>
<th> After 
</th></tr>
<tr><th> `Trak`      </th>
<th> `Trak` 
</th></tr>
<tr><th> `foo_wib` </th>
<th> `foozuwib` 
</th></tr>
<tr><th> `>`          </th>
<th> `zg` 
</th></tr>
<tr><th> `>1`        </th>
<th> `zg1` 
</th></tr>
<tr><th> `foo#`     </th>
<th> `foozh` 
</th></tr>
<tr><th> `foo##`   </th>
<th> `foozhzh` 
</th></tr>
<tr><th> `foo##1` </th>
<th> `foozhzh1` 
</th></tr>
<tr><th> `fooZ`     </th>
<th> `fooZZ` 
</th></tr>
<tr><th> `:+`        </th>
<th> `ZCzp` 
</th></tr>
<tr><th> `()`          </th>
<th> `Z0T` 
</th></tr>
<tr><th> `(,,,,)`      </th>
<th> `Z5T` 
</th></tr>
<tr><th> `(# #)`     </th>
<th> `Z1H` 
</th></tr>
<tr><th> `(#,,,,#)`  </th>
<th> `Z5H` 
</th></tr></table>


