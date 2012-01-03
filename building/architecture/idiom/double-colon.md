# Idiom: no double-colon rules



**Make** has a special type of rule of the form `target :: prerequisites`,
with the behaviour that all double-colon rules for a given target are
executed if the target needs to be rebuilt.  This style was popular
for things like "all" and "clean" targets in the past, but it's not
really necessary - see the "all" idiom above - and this means there's one fewer `make`ism you need to know about.


