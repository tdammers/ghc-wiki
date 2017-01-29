# Building the documentation


## Haddock documentation



The GHC build includes Haddock, and the Haddock documentation for libraries is built and installed by default.



You can disable Haddock documentation for your build by adding


```wiki
HADDOCK_DOCS = NO
```


to your `mk/build.mk`.



It is also possible to process the libraries sources using [
HsColour](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hscolour), and for the Haddock documentation to include links to the HsColoured source code; in order to do this, just install `HsColour` and re-run `./configure`.  The configure script will tell you whether it found `HsColour` at the end.



To build the Haddock docs for a given library, do this:


```wiki
cd libraries/base
make html
```

## User's Guide



In order to process the documentation into HTML or printable formats, you need appropriate tools installed.  The `configure` script searches for the appropriate tools, and will tell you whether it found any.



To install the tools necessary for building the documentation, see [Building/Preparation](building/preparation).



The following make variables control the building of each type of documentation:


```wiki
BUILD_SPHINX_HTML = YES/NO
BUILD_SPHINX_PS   = YES/NO
BUILD_SPHINX_PDF  = YES/NO
```


Note: for GHC \<= 7.10, use:


```wiki
BUILD_DOCBOOK_HTML = YES/NO
BUILD_DOCBOOK_PS   = YES/NO
BUILD_DOCBOOK_PDF  = YES/NO
```


They are set to `YES` or `NO` in `mk/config.mk` by configure, depending on whether the necessary tools were detected on your system.  You can override the automatic settings in your `mk/build.mk` file.



To build a document on its own, for example the Users Guide, do this:


```wiki
cd docs/users_guide
make html
```


substitute 'html' for 'pdf' or 'ps' to build other types of documentation. Note that this will still build the whole compiler if you have not already built it as some pages
of the documentation are automatically generated. 



After building you can find the html users' guide in `docs/users_guide/build-html`.



See also [Commentary/UserManual](commentary/user-manual).


## Installing documentation



Documentation is installed by default by 'make install'.


## Diagnostics



If you see an error like this `make[1]: *** No rule to make target 'html'.  Stop.`:


- Documentation targets are most likely disabled in one of the build config files
- Use `make show VALUE=BUILD_SPHINX_HTML` to check if the target is enabled.

### Target disabled by configuration



Check for the values of documentation build variables (e.g. BUILD\_SPHINX\_HTML) in:


- `mk/flavours/<flavour>.mk`. Most custom build flavors disable documentation targets.
- `mk/config.mk` (`configure` may have disabled the doc build in `config.mk` because required tools were not detected)

### Target disabled because tools not available


- If `sphinx-build` tool is not installed on your machine the HTML docs will be disabled in `config.mk`
- If `xelatex` is not installed on your machine PDF docs will be disabled in `config.mk`
- If you install these tools later do not forget to run `configure` again
