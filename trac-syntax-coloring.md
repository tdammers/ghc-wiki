# Syntax Coloring of Source Code



Trac supports language-specific syntax highlighting of source code within wiki formatted text in [wiki processors](wiki-processors#) blocks and in the [repository browser](trac-browser). Syntax coloring is provided using [
Pygments](http://pygments.org/), which covers a wide range of programming languages and other structured texts, and is actively supported. If Pygments is not available, Trac will display the content as plain text. 


### About Pygments



[
Pygments](http://pygments.org/) is a highlighting library implemented in pure python, very fast, easy to extend and [
well documented](http://pygments.org/docs/).



The Pygments default style can specified in the [mime-viewer](trac-ini#) section of trac.ini. The default style can be overridden by setting a *Style* preference on the [preferences page](/trac/ghc/prefs/pygments). 



[
Pygments lexer](http://pygments.org/docs/lexers/) options can be specified as [WikiProcessor](wiki-processors) arguments and defaults can be set in the [environment configuration](trac-ini#).


## Syntax Coloring Support


### Supported languages



The list of currently supported languages can be found on the [
supported languages](http://pygments.org/languages/) page. The list represents the languages supported in the most recent version of Pygments, so the languages actually supported in your installation could differ if you have an older version installed. The listing of [
supported lexers](http://pygments.org/docs/lexers/) provides additional information about the default mime type to keyword mappings.



Explicit control of the mime type associated with a [WikiProcessor](wiki-processors) and file extension is available through the `mime_map` setting. For example, by default `.m` files are considered Objective-C files. In order to treat `.m` files as MATLAB files, add `text/matlab:m` to the `mime_map` setting in the [\[mimeviewer\] section of trac.ini](trac-ini#).



If a mimetype property such as `svn:mime-type` is set to `text/plain`, there is no coloring even if file is known type like `java`.


### Direct Rendering



Rich content may be directly *rendered* instead of syntax highlighted. This usually depends on which auxiliary packages are installed and on which components are activated in your setup. For example a `text/x-rst` document will be rendered via `docutils` if it is installed and the `trac.mimeview.rst.ReStructuredTextRenderer` is not disabled, and will be syntax highlighted otherwise.



In a similar way, a document with the mimetype `text/x-trac-wiki` is rendered using the Trac wiki formatter, unless the `trac.mimeview.api.WikiTextRenderer` component is disabled.



HTML documents are directly rendered only if the `render_unsafe_html` settings are enabled in the [TracIni](trac-ini) (those settings are present in multiple sections, as there are different security concerns depending where the document comes from). If you want to ensure that an HTML document gets syntax highlighted and not rendered, use the `text/xml` mimetype.


### Known MIME types



<table><tr><th>MIME Types</th>
<th>[WikiProcessors](/trac/ghc/wiki/WikiProcessors)</th></tr>
<tr><th>`application/atom+xml`</th>
<th>`xml`</th></tr>
<tr><th>`application/json`</th>
<th>`json`</th></tr>
<tr><th>`application/json-object`</th>
<th>`json-object`</th></tr>
<tr><th>`application/kal`</th>
<th>`kal`</th></tr>
<tr><th>`application/ld+json`</th>
<th>`json-ld jsonld`</th></tr>
<tr><th>`application/msword`</th>
<th>`doc dot`</th></tr>
<tr><th>`application/pdf`</th>
<th>`pdf`</th></tr>
<tr><th>`application/postscript`</th>
<th>`postscr postscript ps`</th></tr>
<tr><th>`application/rss+xml`</th>
<th>`rss`</th></tr>
<tr><th>`application/rtf`</th>
<th>`rtf`</th></tr>
<tr><th>`application/sparql-query`</th>
<th>`sparql`</th></tr>
<tr><th>`application/vnd.wolfram.cdf`</th>
<th>`mathematica mma nb`</th></tr>
<tr><th>`application/x-awk`</th>
<th>`awk gawk mawk nawk`</th></tr>
<tr><th>`application/x-befunge`</th>
<th>`befunge`</th></tr>
<tr><th>`application/x-brainfuck`</th>
<th>`bf brainfuck`</th></tr>
<tr><th>`application/x-chaiscript`</th>
<th>`chai chaiscript`</th></tr>
<tr><th>`application/x-clojure`</th>
<th>`clj clojure`</th></tr>
<tr><th>`application/x-clojurescript`</th>
<th>`cljs clojurescript`</th></tr>
<tr><th>`application/x-coldfusion`</th>
<th>`cfm`</th></tr>
<tr><th>`application/x-csh`</th>
<th>`csh tcsh`</th></tr>
<tr><th>`application/x-cython`</th>
<th>`cython pyrex pyx`</th></tr>
<tr><th>`application/x-dos-batch`</th>
<th>`bat batch cmd dos dosbatch winbatch`</th></tr>
<tr><th>`application/x-ecl`</th>
<th>`ecl`</th></tr>
<tr><th>`application/x-elisp`</th>
<th>`elisp emacs emacs-lisp`</th></tr>
<tr><th>`application/x-evoque`</th>
<th>`evoque`</th></tr>
<tr><th>`application/x-fantom`</th>
<th>`fan`</th></tr>
<tr><th>`application/x-fish`</th>
<th>`fish fishshell`</th></tr>
<tr><th>`application/x-forth`</th>
<th>`forth`</th></tr>
<tr><th>`application/x-gooddata-maql`</th>
<th>`maql`</th></tr>
<tr><th>`application/x-httpd-lasso[89]`</th>
<th>`html+lasso`</th></tr>
<tr><th>`application/x-httpd-php5`</th>
<th>`html+php`</th></tr>
<tr><th>`application/x-hy`</th>
<th>`hylang`</th></tr>
<tr><th>`application/x-hybris`</th>
<th>`hy hybris`</th></tr>
<tr><th>`application/x-jinja`</th>
<th>`django jinja`</th></tr>
<tr><th>`application/x-jsp`</th>
<th>`jsp`</th></tr>
<tr><th>`application/x-julia`</th>
<th>`jl julia`</th></tr>
<tr><th>`application/x-kid`</th>
<th>`genshi kid xml+genshi xml+kid`</th></tr>
<tr><th>`application/x-lua`</th>
<th>`lua`</th></tr>
<tr><th>`application/x-mako`</th>
<th>`mako`</th></tr>
<tr><th>`application/x-mason`</th>
<th>`mason`</th></tr>
<tr><th>`application/x-moonscript`</th>
<th>`moon moonscript`</th></tr>
<tr><th>`application/x-myghty`</th>
<th>`myghty`</th></tr>
<tr><th>`application/x-newlisp`</th>
<th>`newlisp`</th></tr>
<tr><th>`application/x-openedge`</th>
<th>`abl openedge progress`</th></tr>
<tr><th>`application/x-perl`</th>
<th>`perl pl`</th></tr>
<tr><th>`application/x-perl6`</th>
<th>`perl6 pl6`</th></tr>
<tr><th>`application/x-pygments-tokens`</th>
<th>`raw`</th></tr>
<tr><th>`application/x-pypylog`</th>
<th>`pypy pypylog`</th></tr>
<tr><th>`application/x-python`</th>
<th>`py python sage`</th></tr>
<tr><th>`application/x-python3`</th>
<th>`py3 python3`</th></tr>
<tr><th>`application/x-qt.qbs+qml`</th>
<th>`qbs qml`</th></tr>
<tr><th>`application/x-racket`</th>
<th>`racket rkt`</th></tr>
<tr><th>`application/x-ruby`</th>
<th>`duby rb ruby`</th></tr>
<tr><th>`application/x-ruby-templating`</th>
<th>`erb`</th></tr>
<tr><th>`application/x-sas`</th>
<th>`sas`</th></tr>
<tr><th>`application/x-scheme`</th>
<th>`scheme scm`</th></tr>
<tr><th>`application/x-sh-session`</th>
<th>`console shell-session`</th></tr>
<tr><th>`application/x-shellscript`</th>
<th>`bash ksh sh shell zsh`</th></tr>
<tr><th>`application/x-shen`</th>
<th>`shen`</th></tr>
<tr><th>`application/x-smarty`</th>
<th>`smarty`</th></tr>
<tr><th>`application/x-spitfire`</th>
<th>`cheetah spitfire`</th></tr>
<tr><th>`application/x-ssp`</th>
<th>`ssp`</th></tr>
<tr><th>`application/x-standardml`</th>
<th>`sml`</th></tr>
<tr><th>`application/x-stata`</th>
<th>`do stata`</th></tr>
<tr><th>`application/x-tcl`</th>
<th>`tcl`</th></tr>
<tr><th>`application/x-terraform`</th>
<th>`terraform tf`</th></tr>
<tr><th>`application/x-thrift`</th>
<th>`thrift`</th></tr>
<tr><th>`application/x-troff`</th>
<th>`roff troff`</th></tr>
<tr><th>`application/x-turtle`</th>
<th>`turtle`</th></tr>
<tr><th>`application/x-twig`</th>
<th>`twig`</th></tr>
<tr><th>`application/x-urbiscript`</th>
<th>`urbiscript`</th></tr>
<tr><th>`application/x-yaml`</th>
<th>`yml`</th></tr>
<tr><th>`application/xhtml+xml`</th>
<th>`html`</th></tr>
<tr><th>`application/xml+evoque`</th>
<th>`xml+evoque`</th></tr>
<tr><th>`application/xml+jinja`</th>
<th>`xml+django xml+jinja`</th></tr>
<tr><th>`application/xml+lasso`</th>
<th>`xml+lasso`</th></tr>
<tr><th>`application/xml+mako`</th>
<th>`xml+mako`</th></tr>
<tr><th>`application/xml+myghty`</th>
<th>`xml+myghty`</th></tr>
<tr><th>`application/xml+php`</th>
<th>`xml+php`</th></tr>
<tr><th>`application/xml+ruby`</th>
<th>`xml+erb xml+ruby`</th></tr>
<tr><th>`application/xml+smarty`</th>
<th>`xml+smarty`</th></tr>
<tr><th>`application/xml+spitfire`</th>
<th>`xml+cheetah xml+spitfire`</th></tr>
<tr><th>`application/xml+velocity`</th>
<th>`xml+velocity`</th></tr>
<tr><th>`application/xml-dtd`</th>
<th>`dtd`</th></tr>
<tr><th>`application/xquery`</th>
<th>`xq xql xqm xquery xqy`</th></tr>
<tr><th>`application/xsl+xml`</th>
<th>`xsl`</th></tr>
<tr><th>`application/xslt+xml`</th>
<th>`xslt`</th></tr>
<tr><th>`image/svg+xml`</th>
<th>`svg`</th></tr>
<tr><th>`image/x-icon`</th>
<th>`ico`</th></tr>
<tr><th>`model/vrml`</th>
<th>`vrml wrl`</th></tr>
<tr><th>`text/actionscript`</th>
<th>`actionscript as`</th></tr>
<tr><th>`text/actionscript3`</th>
<th>`actionscript3 as3`</th></tr>
<tr><th>`text/basic`</th>
<th>`basic qbasic`</th></tr>
<tr><th>`text/coffeescript`</th>
<th>`coffee coffee-script coffeescript`</th></tr>
<tr><th>`text/css`</th>
<th>`css`</th></tr>
<tr><th>`text/css+genshi`</th>
<th>`css+genshi css+genshitext`</th></tr>
<tr><th>`text/css+jinja`</th>
<th>`css+django css+jinja`</th></tr>
<tr><th>`text/css+lasso`</th>
<th>`css+lasso`</th></tr>
<tr><th>`text/css+mako`</th>
<th>`css+mako`</th></tr>
<tr><th>`text/css+myghty`</th>
<th>`css+myghty`</th></tr>
<tr><th>`text/css+php`</th>
<th>`css+php`</th></tr>
<tr><th>`text/css+ruby`</th>
<th>`css+erb css+ruby`</th></tr>
<tr><th>`text/css+smarty`</th>
<th>`css+smarty`</th></tr>
<tr><th>`text/gettext`</th>
<th>`po pot`</th></tr>
<tr><th>`text/html`</th>
<th>`htm`</th></tr>
<tr><th>`text/html+evoque`</th>
<th>`html+evoque`</th></tr>
<tr><th>`text/html+genshi`</th>
<th>`html+genshi html+kid`</th></tr>
<tr><th>`text/html+jinja`</th>
<th>`html+django html+jinja htmldjango`</th></tr>
<tr><th>`text/html+mako`</th>
<th>`html+mako`</th></tr>
<tr><th>`text/html+myghty`</th>
<th>`html+myghty`</th></tr>
<tr><th>`text/html+ruby`</th>
<th>`html+erb html+ruby rhtml`</th></tr>
<tr><th>`text/html+smarty`</th>
<th>`html+smarty`</th></tr>
<tr><th>`text/html+spitfire`</th>
<th>`html+cheetah html+spitfire htmlcheetah`</th></tr>
<tr><th>`text/html+twig`</th>
<th>`html+twig`</th></tr>
<tr><th>`text/html+velocity`</th>
<th>`html+velocity`</th></tr>
<tr><th>`text/idl`</th>
<th>`idl`</th></tr>
<tr><th>`text/inf`</th>
<th>`cfg dosini ini`</th></tr>
<tr><th>`text/ipf`</th>
<th>`igor igorpro`</th></tr>
<tr><th>`text/javascript`</th>
<th>`javascript js`</th></tr>
<tr><th>`text/javascript+genshi`</th>
<th>`javascript+genshi javascript+genshitext js+genshi js+genshitext`</th></tr>
<tr><th>`text/javascript+jinja`</th>
<th>`javascript+django javascript+jinja js+django js+jinja`</th></tr>
<tr><th>`text/javascript+lasso`</th>
<th>`javascript+lasso js+lasso`</th></tr>
<tr><th>`text/javascript+mako`</th>
<th>`javascript+mako js+mako`</th></tr>
<tr><th>`text/javascript+mygthy`</th>
<th>`javascript+myghty js+myghty`</th></tr>
<tr><th>`text/javascript+php`</th>
<th>`javascript+php js+php`</th></tr>
<tr><th>`text/javascript+ruby`</th>
<th>`javascript+erb javascript+ruby js+erb js+ruby`</th></tr>
<tr><th>`text/javascript+smarty`</th>
<th>`javascript+smarty js+smarty`</th></tr>
<tr><th>`text/javascript+spitfire`</th>
<th>`javascript+cheetah javascript+spitfire js+cheetah js+spitfire`</th></tr>
<tr><th>`text/jsgf`</th>
<th>`jsgf`</th></tr>
<tr><th>`text/juttle`</th>
<th>`juttle`</th></tr>
<tr><th>`text/limbo`</th>
<th>`limbo`</th></tr>
<tr><th>`text/livescript`</th>
<th>`live-script livescript`</th></tr>
<tr><th>`text/matlab`</th>
<th>`matlab`</th></tr>
<tr><th>`text/ncl`</th>
<th>`ncl`</th></tr>
<tr><th>`text/octave`</th>
<th>`octave`</th></tr>
<tr><th>`text/odin`</th>
<th>`odin`</th></tr>
<tr><th>`text/plain`</th>
<th>`AUTHORS COPYING ChangeLog INSTALL README RELEASE TXT text txt`</th></tr>
<tr><th>`text/prs.fallenstein.rst`</th>
<th>`rest restructuredtext rst`</th></tr>
<tr><th>`text/rsl`</th>
<th>`rsl`</th></tr>
<tr><th>`text/rust`</th>
<th>`rust`</th></tr>
<tr><th>`text/scilab`</th>
<th>`scilab`</th></tr>
<tr><th>`text/smali`</th>
<th>`smali`</th></tr>
<tr><th>`text/supercollider`</th>
<th>`sc supercollider`</th></tr>
<tr><th>`text/swig`</th>
<th>`swig`</th></tr>
<tr><th>`text/troff`</th>
<th>`groff man nroff`</th></tr>
<tr><th>`text/x-abap`</th>
<th>`abap`</th></tr>
<tr><th>`text/x-abnf`</th>
<th>`abnf`</th></tr>
<tr><th>`text/x-ada`</th>
<th>`ada ada2005 ada95 adb ads`</th></tr>
<tr><th>`text/x-agda`</th>
<th>`agda`</th></tr>
<tr><th>`text/x-alloy`</th>
<th>`alloy`</th></tr>
<tr><th>`text/x-ambienttalk`</th>
<th>`ambienttalk ambienttalk/2 at`</th></tr>
<tr><th>`text/x-apacheconf`</th>
<th>`aconf apache apacheconf`</th></tr>
<tr><th>`text/x-arduino`</th>
<th>`arduino`</th></tr>
<tr><th>`text/x-asp`</th>
<th>`asp`</th></tr>
<tr><th>`text/x-aspectj`</th>
<th>`aspectj`</th></tr>
<tr><th>`text/x-asymptote`</th>
<th>`asy asymptote`</th></tr>
<tr><th>`text/x-autohotkey`</th>
<th>`ahk autohotkey`</th></tr>
<tr><th>`text/x-autoit`</th>
<th>`autoit`</th></tr>
<tr><th>`text/x-bb`</th>
<th>`b3d blitzbasic bplus`</th></tr>
<tr><th>`text/x-bbcode`</th>
<th>`bbcode`</th></tr>
<tr><th>`text/x-bibtex`</th>
<th>`bib bibtex`</th></tr>
<tr><th>`text/x-bmx`</th>
<th>`blitzmax bmax`</th></tr>
<tr><th>`text/x-bnf`</th>
<th>`bnf`</th></tr>
<tr><th>`text/x-boo`</th>
<th>`boo`</th></tr>
<tr><th>`text/x-c++hdr`</th>
<th>`H HH c++hdr hh hpp`</th></tr>
<tr><th>`text/x-c++src`</th>
<th>`C C++ CC c++ c++src cc cpp`</th></tr>
<tr><th>`text/x-c-objdump`</th>
<th>`c-objdump`</th></tr>
<tr><th>`text/x-ceylon`</th>
<th>`ceylon`</th></tr>
<tr><th>`text/x-chdr`</th>
<th>`chdr h`</th></tr>
<tr><th>`text/x-cirru`</th>
<th>`cirru`</th></tr>
<tr><th>`text/x-clay`</th>
<th>`clay`</th></tr>
<tr><th>`text/x-cmake`</th>
<th>`cmake`</th></tr>
<tr><th>`text/x-cobol`</th>
<th>`cobol`</th></tr>
<tr><th>`text/x-common-lisp`</th>
<th>`cl common-lisp lisp`</th></tr>
<tr><th>`text/x-component-pascal`</th>
<th>`componentpascal cp`</th></tr>
<tr><th>`text/x-coq`</th>
<th>`coq`</th></tr>
<tr><th>`text/x-cpp-objdump`</th>
<th>`c++-objdumb cpp-objdump cxx-objdump`</th></tr>
<tr><th>`text/x-crocsrc`</th>
<th>`croc`</th></tr>
<tr><th>`text/x-cryptol`</th>
<th>`cry cryptol`</th></tr>
<tr><th>`text/x-crystal`</th>
<th>`cr crystal`</th></tr>
<tr><th>`text/x-csharp`</th>
<th>`C# c# cs csharp`</th></tr>
<tr><th>`text/x-csrc`</th>
<th>`c csrc xs`</th></tr>
<tr><th>`text/x-cuda`</th>
<th>`cu cuda`</th></tr>
<tr><th>`text/x-d-objdump`</th>
<th>`d-objdump`</th></tr>
<tr><th>`text/x-dart`</th>
<th>`dart`</th></tr>
<tr><th>`text/x-dg`</th>
<th>`dg`</th></tr>
<tr><th>`text/x-diff`</th>
<th>`patch`</th></tr>
<tr><th>`text/x-dockerfile-config`</th>
<th>`docker dockerfile`</th></tr>
<tr><th>`text/x-dsrc`</th>
<th>`d`</th></tr>
<tr><th>`text/x-dylan`</th>
<th>`dylan`</th></tr>
<tr><th>`text/x-dylan-console`</th>
<th>`dylan-console dylan-repl`</th></tr>
<tr><th>`text/x-dylan-lid`</th>
<th>`dylan-lid lid`</th></tr>
<tr><th>`text/x-earl-grey`</th>
<th>`earl-grey earlgrey eg`</th></tr>
<tr><th>`text/x-easytrieve`</th>
<th>`easytrieve`</th></tr>
<tr><th>`text/x-ebnf`</th>
<th>`ebnf`</th></tr>
<tr><th>`text/x-ecsrc`</th>
<th>`ec`</th></tr>
<tr><th>`text/x-eiffel`</th>
<th>`e eiffel`</th></tr>
<tr><th>`text/x-elisp`</th>
<th>`el`</th></tr>
<tr><th>`text/x-elixir`</th>
<th>`elixir ex exs`</th></tr>
<tr><th>`text/x-elixir-shellsession`</th>
<th>`iex`</th></tr>
<tr><th>`text/x-elm`</th>
<th>`elm`</th></tr>
<tr><th>`text/x-erl-shellsession`</th>
<th>`erl`</th></tr>
<tr><th>`text/x-erlang`</th>
<th>`erlang`</th></tr>
<tr><th>`text/x-ezhil`</th>
<th>`ezhil`</th></tr>
<tr><th>`text/x-factor`</th>
<th>`factor`</th></tr>
<tr><th>`text/x-fancysrc`</th>
<th>`fancy fy`</th></tr>
<tr><th>`text/x-felix`</th>
<th>`felix flx`</th></tr>
<tr><th>`text/x-flatline`</th>
<th>`flatline`</th></tr>
<tr><th>`text/x-fortran`</th>
<th>`f fortran`</th></tr>
<tr><th>`text/x-fsharp`</th>
<th>`fsharp`</th></tr>
<tr><th>`text/x-gas`</th>
<th>`asm gas`</th></tr>
<tr><th>`text/x-genshi`</th>
<th>`genshitext`</th></tr>
<tr><th>`text/x-gherkin`</th>
<th>`cucumber gherkin`</th></tr>
<tr><th>`text/x-glslsrc`</th>
<th>`glsl`</th></tr>
<tr><th>`text/x-gnuplot`</th>
<th>`gnuplot`</th></tr>
<tr><th>`text/x-gooddata-cl`</th>
<th>`gooddata-cl`</th></tr>
<tr><th>`text/x-gosrc`</th>
<th>`go`</th></tr>
<tr><th>`text/x-gosu`</th>
<th>`gosu`</th></tr>
<tr><th>`text/x-gosu-template`</th>
<th>`gst`</th></tr>
<tr><th>`text/x-groovy`</th>
<th>`groovy`</th></tr>
<tr><th>`text/x-haml`</th>
<th>`haml`</th></tr>
<tr><th>`text/x-handlebars-template`</th>
<th>`html+handlebars`</th></tr>
<tr><th>`text/x-haskell`</th>
<th>`haskell hs`</th></tr>
<tr><th>`text/x-hsail`</th>
<th>`hsa hsail`</th></tr>
<tr><th>`text/x-hx`</th>
<th>`haxe hx hxsl`</th></tr>
<tr><th>`text/x-idl`</th>
<th>`ice`</th></tr>
<tr><th>`text/x-idris`</th>
<th>`idr idris`</th></tr>
<tr><th>`text/x-inf`</th>
<th>`inf`</th></tr>
<tr><th>`text/x-iokesrc`</th>
<th>`ik ioke`</th></tr>
<tr><th>`text/x-iosrc`</th>
<th>`io`</th></tr>
<tr><th>`text/x-irclog`</th>
<th>`irc`</th></tr>
<tr><th>`text/x-isabelle`</th>
<th>`isabelle`</th></tr>
<tr><th>`text/x-j`</th>
<th>`j`</th></tr>
<tr><th>`text/x-jade`</th>
<th>`jade pug`</th></tr>
<tr><th>`text/x-java`</th>
<th>`java`</th></tr>
<tr><th>`text/x-java-properties`</th>
<th>`jproperties properties`</th></tr>
<tr><th>`text/x-jbst`</th>
<th>`duel jbst jsonml+bst`</th></tr>
<tr><th>`text/x-jcl`</th>
<th>`jcl`</th></tr>
<tr><th>`text/x-kconfig`</th>
<th>`kconfig kernel-config linux-config menuconfig`</th></tr>
<tr><th>`text/x-koka`</th>
<th>`koka`</th></tr>
<tr><th>`text/x-kotlin`</th>
<th>`kotlin`</th></tr>
<tr><th>`text/x-lasso`</th>
<th>`lasso lassoscript`</th></tr>
<tr><th>`text/x-latex`</th>
<th>`latex tex`</th></tr>
<tr><th>`text/x-lean`</th>
<th>`lean`</th></tr>
<tr><th>`text/x-less-css`</th>
<th>`less`</th></tr>
<tr><th>`text/x-lighttpd-conf`</th>
<th>`lighttpd lighty`</th></tr>
<tr><th>`text/x-literate-agda`</th>
<th>`lagda literate-agda`</th></tr>
<tr><th>`text/x-literate-cryptol`</th>
<th>`lcry lcryptol literate-cryptol`</th></tr>
<tr><th>`text/x-literate-haskell`</th>
<th>`lhaskell lhs literate-haskell`</th></tr>
<tr><th>`text/x-literate-idris`</th>
<th>`lidr lidris literate-idris`</th></tr>
<tr><th>`text/x-llvm`</th>
<th>`llvm`</th></tr>
<tr><th>`text/x-logos`</th>
<th>`logos`</th></tr>
<tr><th>`text/x-logtalk`</th>
<th>`logtalk`</th></tr>
<tr><th>`text/x-lsl`</th>
<th>`lsl`</th></tr>
<tr><th>`text/x-m4`</th>
<th>`m4`</th></tr>
<tr><th>`text/x-mail`</th>
<th>`mail`</th></tr>
<tr><th>`text/x-makefile`</th>
<th>`GNUMakefile Makefile bsdmake make makefile mf mk`</th></tr>
<tr><th>`text/x-markdown`</th>
<th>`md`</th></tr>
<tr><th>`text/x-mask`</th>
<th>`mask`</th></tr>
<tr><th>`text/x-minidsrc`</th>
<th>`minid`</th></tr>
<tr><th>`text/x-modelica`</th>
<th>`modelica`</th></tr>
<tr><th>`text/x-modula2`</th>
<th>`m2 modula2`</th></tr>
<tr><th>`text/x-monkey`</th>
<th>`monkey`</th></tr>
<tr><th>`text/x-moocode`</th>
<th>`moo moocode`</th></tr>
<tr><th>`text/x-mql`</th>
<th>`mq4 mq5 mql mql4 mql5`</th></tr>
<tr><th>`text/x-mysql`</th>
<th>`mysql`</th></tr>
<tr><th>`text/x-nasm`</th>
<th>`nasm`</th></tr>
<tr><th>`text/x-nasm-objdump`</th>
<th>`objdump-nasm`</th></tr>
<tr><th>`text/x-nemerle`</th>
<th>`nemerle`</th></tr>
<tr><th>`text/x-nescsrc`</th>
<th>`nesc`</th></tr>
<tr><th>`text/x-newspeak`</th>
<th>`newspeak`</th></tr>
<tr><th>`text/x-nginx-conf`</th>
<th>`nginx nginx-conf`</th></tr>
<tr><th>`text/x-nim`</th>
<th>`nim nimrod`</th></tr>
<tr><th>`text/x-nix`</th>
<th>`nix nixos`</th></tr>
<tr><th>`text/x-nsis`</th>
<th>`nsh nsi nsis`</th></tr>
<tr><th>`text/x-objc`</th>
<th>`m mm`</th></tr>
<tr><th>`text/x-objdump`</th>
<th>`objdump`</th></tr>
<tr><th>`text/x-objective-c`</th>
<th>`obj-c objc objective-c objectivec`</th></tr>
<tr><th>`text/x-objective-c++`</th>
<th>`obj-c++ objc++ objective-c++ objectivec++`</th></tr>
<tr><th>`text/x-objective-j`</th>
<th>`obj-j objective-j objectivej objj`</th></tr>
<tr><th>`text/x-ocaml`</th>
<th>`ml mli ocaml`</th></tr>
<tr><th>`text/x-ooc`</th>
<th>`ooc`</th></tr>
<tr><th>`text/x-opa`</th>
<th>`opa`</th></tr>
<tr><th>`text/x-parasail`</th>
<th>`parasail`</th></tr>
<tr><th>`text/x-pascal`</th>
<th>`delphi objectpascal pas pascal`</th></tr>
<tr><th>`text/x-patch`</th>
<th>`diff udiff`</th></tr>
<tr><th>`text/x-pawn`</th>
<th>`pawn`</th></tr>
<tr><th>`text/x-perl`</th>
<th>`PL pm`</th></tr>
<tr><th>`text/x-php`</th>
<th>`php php3 php4 php5`</th></tr>
<tr><th>`text/x-pig`</th>
<th>`pig`</th></tr>
<tr><th>`text/x-pike`</th>
<th>`pike`</th></tr>
<tr><th>`text/x-plpgsql`</th>
<th>`plpgsql`</th></tr>
<tr><th>`text/x-postgresql`</th>
<th>`postgres postgresql`</th></tr>
<tr><th>`text/x-postgresql-psql`</th>
<th>`postgres-console postgresql-console psql`</th></tr>
<tr><th>`text/x-povray`</th>
<th>`pov`</th></tr>
<tr><th>`text/x-powershell`</th>
<th>`posh powershell ps1 psm1`</th></tr>
<tr><th>`text/x-prolog`</th>
<th>`prolog`</th></tr>
<tr><th>`text/x-psp`</th>
<th>`psp`</th></tr>
<tr><th>`text/x-python-doctest`</th>
<th>`pycon python-doctest`</th></tr>
<tr><th>`text/x-python-traceback`</th>
<th>`pytb`</th></tr>
<tr><th>`text/x-python3-traceback`</th>
<th>`py3tb`</th></tr>
<tr><th>`text/x-r-doc`</th>
<th>`rd`</th></tr>
<tr><th>`text/x-r-profile`</th>
<th>`r s splus`</th></tr>
<tr><th>`text/x-rebol`</th>
<th>`rebol`</th></tr>
<tr><th>`text/x-red-system`</th>
<th>`red red/system`</th></tr>
<tr><th>`text/x-rexx`</th>
<th>`arexx rexx`</th></tr>
<tr><th>`text/x-rfc`</th>
<th>`rfc`</th></tr>
<tr><th>`text/x-robotframework`</th>
<th>`robotframework`</th></tr>
<tr><th>`text/x-rpm-spec`</th>
<th>`spec`</th></tr>
<tr><th>`text/x-rql`</th>
<th>`rql`</th></tr>
<tr><th>`text/x-ruby-shellsession`</th>
<th>`irb rbcon`</th></tr>
<tr><th>`text/x-sass`</th>
<th>`sass`</th></tr>
<tr><th>`text/x-scala`</th>
<th>`scala`</th></tr>
<tr><th>`text/x-scaml`</th>
<th>`scaml`</th></tr>
<tr><th>`text/x-scss`</th>
<th>`scss`</th></tr>
<tr><th>`text/x-slim`</th>
<th>`slim`</th></tr>
<tr><th>`text/x-sls`</th>
<th>`salt sls yaml+jinja`</th></tr>
<tr><th>`text/x-smalltalk`</th>
<th>`smalltalk squeak st`</th></tr>
<tr><th>`text/x-snobol`</th>
<th>`snobol`</th></tr>
<tr><th>`text/x-sourcepawn`</th>
<th>`sp`</th></tr>
<tr><th>`text/x-sql`</th>
<th>`sql`</th></tr>
<tr><th>`text/x-sqlite3-console`</th>
<th>`sqlite3`</th></tr>
<tr><th>`text/x-squidconf`</th>
<th>`squid squid.conf squidconf`</th></tr>
<tr><th>`text/x-swift`</th>
<th>`swift`</th></tr>
<tr><th>`text/x-systemverilog`</th>
<th>`sv systemverilog`</th></tr>
<tr><th>`text/x-tasm`</th>
<th>`tasm`</th></tr>
<tr><th>`text/x-tea`</th>
<th>`tea`</th></tr>
<tr><th>`text/x-textile`</th>
<th>`textile txtl`</th></tr>
<tr><th>`text/x-todo`</th>
<th>`todotxt`</th></tr>
<tr><th>`text/x-trac-wiki`</th>
<th>`moin trac-wiki`</th></tr>
<tr><th>`text/x-tsql`</th>
<th>`t-sql tsql`</th></tr>
<tr><th>`text/x-typescript`</th>
<th>`ts typescript`</th></tr>
<tr><th>`text/x-typoscript`</th>
<th>`typoscript`</th></tr>
<tr><th>`text/x-vala`</th>
<th>`vala vapi`</th></tr>
<tr><th>`text/x-vba`</th>
<th>`bas vb vb.net vba vbnet`</th></tr>
<tr><th>`text/x-vclsnippet`</th>
<th>`vclsnippet vclsnippets`</th></tr>
<tr><th>`text/x-vclsrc`</th>
<th>`vcl`</th></tr>
<tr><th>`text/x-verilog`</th>
<th>`v verilog`</th></tr>
<tr><th>`text/x-vhdl`</th>
<th>`vhd vhdl`</th></tr>
<tr><th>`text/x-vim`</th>
<th>`vim`</th></tr>
<tr><th>`text/x-whiley`</th>
<th>`whiley`</th></tr>
<tr><th>`text/x-windows-registry`</th>
<th>`registry`</th></tr>
<tr><th>`text/x-x10`</th>
<th>`x10 xten`</th></tr>
<tr><th>`text/x-xtend`</th>
<th>`xtend`</th></tr>
<tr><th>`text/x-yaml`</th>
<th>`yaml`</th></tr></table>



---



See also: [WikiProcessors](wiki-processors), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracBrowser](trac-browser)


