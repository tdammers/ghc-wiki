# Idiom: Directories



In order to create directories, we use "order-only" make dependencies, e.g.


```wiki
build/foo.o : foo.c foo.h | $$(dir $$@)/.
    gcc $< -o $@
```


Dependencies after the `|` are order-only, i.e. we only require that the directory exists, not that `build/foo.o` is newer than it. The doubled-dollars are because this needs to happen during the second-expansion phase, as $@ is not available during the first-expansion. The `/.` on the end is because make will normalise `foo/bar/` to `foo/bar`, so we couldn't use a `%/` rule to create the directories. We therefore use a `%/.` rule.


