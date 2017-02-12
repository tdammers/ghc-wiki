# Installing Trac as CGI






>
>
> *Please note that using Trac via CGI is the slowest deployment method available. It is slower than [mod\_python](trac-mod-python), [FastCGI](trac-fast-cgi) and even [
> IIS/AJP](http://trac.edgewall.org/intertrac/TracOnWindowsIisAjp) on Windows.*
>
>


CGI script is the entrypoint that web-server calls when a web-request to an application is made. The `trac.cgi` script can be created using the `trac-admin <env> deploy <dir>` command which automatically substitutes the required paths, see [TracInstall\#cgi-bin](trac-install#). Make sure the script is executable by your web server.


## Apache web-server configuration



In [ Apache](http://httpd.apache.org/) there are two ways to run Trac as CGI:


1. Use a `ScriptAlias` directive that maps an URL to the `trac.cgi` script (recommended)
1. Copy the `trac.cgi` file into the directory for CGI executables used by your web server (commonly named `cgi-bin`). You can also create a symbolic link, but in that case make sure that the `FollowSymLinks` option is enabled for the `cgi-bin` directory.


To make Trac available at `http://yourhost.example.org/trac` add `ScriptAlias` directive to Apache configuration file, changing `trac.cgi` path to match your installation:


```
ScriptAlias /trac /path/to/www/trac/cgi-bin/trac.cgi
```

>
>
> *Note that this directive requires enabled `mod_alias` module.*
>
>


If you're using Trac with a single project you need to set its location using the `TRAC_ENV` environment variable:


```
<Location "/trac">
  SetEnv TRAC_ENV "/path/to/projectenv"
</Location>
```


Or to use multiple projects you can specify their common parent directory using the `TRAC_ENV_PARENT_DIR` variable:


```
<Location "/trac">
  SetEnv TRAC_ENV_PARENT_DIR "/path/to/project/parent/dir"
</Location>
```

>
>
> *Note that the `SetEnv` directive requires enabled `mod_env` module. It is also possible to set TRAC\_ENV in trac.cgi. Just add the following code between "try:" and "from trac.web ...":*
>
>

```
    import os
    os.environ['TRAC_ENV'] = "/path/to/projectenv"
```

>
>
> * Or for TRAC\_ENV\_PARENT\_DIR: *
>
>

```
    import os
    os.environ['TRAC_ENV_PARENT_DIR'] = "/path/to/project/parent/dir"
```


If you are using the [
Apache suEXEC](http://httpd.apache.org/docs/suexec.html) feature please see [
ApacheSuexec](http://trac.edgewall.org/intertrac/ApacheSuexec).



On some systems, you *may* need to edit the shebang line in the `trac.cgi` file to point to your real Python installation path. On a Windows system you may need to configure Windows to know how to execute a .cgi file (Explorer -\> Tools -\> Folder Options -\> File Types -\> CGI).


### Using WSGI



You can run a [
WSGI handler](http://henry.precheur.org/python/how_to_serve_cgi) [
under CGI](http://pythonweb.org/projects/webmodules/doc/0.5.3/html_multipage/lib/example-webserver-web-wsgi-simple-cgi.html).  You can [write your own application function](trac-mod-wsgi#), or use the deployed trac.wsgi's application.


## Mapping Static Resources



See [TracInstall\#MappingStaticResources](trac-install#mapping-static-resources).


## Adding Authentication



See [TracInstall\#ConfiguringAuthentication](trac-install#configuring-authentication).


---



See also:  [TracGuide](trac-guide), [TracInstall](trac-install), [TracModWSGI](trac-mod-wsgi), [TracFastCgi](trac-fast-cgi), [TracModPython](trac-mod-python)


