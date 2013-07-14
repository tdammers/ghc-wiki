
**Error: Macro BackLinks(BuildBot) failed**

```
'Environment' object has no attribute 'get_db_cnx'
```



# Current status



Buildbot is currently [down](status/apr10#), and we are working on a replacement.  See [Builder](builder) for more details.


# Setting up a nightly build



The GHC buildbot builds GHC on various platforms in various different ways each night, runs the test suite and performance benchmarks, and mails the results to the `ghc-builds@haskell.org` mailing list.  We're always keen to add more build slaves to the setup, especially if you have a platform that doesn't already have a build slave, so if you'd like to join the fun, please let us know at ghc-devs@….  If a platform is represented in the nightly builds, it's more likely we'll be able to identify and fix problems specific to that platform quickly.
        
To see the current status of the builds:


>
>
> [ http://darcs.haskell.org/buildbot](http://darcs.haskell.org/buildbot)
>
>

## To create a new build slave



First you, as a buildbot client, need to agree a buildbot username (`myUser`) and password (`myPass`) with the buildbot admins (just pick a username and password and send it to `ghc@well-typed.com`).  You'll also need to decide:


- when the build(s) should happen (time and timezone!)
- build HEAD, STABLE, or (the default) alternate between the two?
- full build (up to stage 3, with extra-libs, full testsuite, and 5 nofib runs) or a fast build (stage 2, no extra-libs, fast testsuite, no nofib runs), or (the default) something in-between


Finally, if there is anything special that needs to be done for the client (e.g. if gcc is in an unusual place) then you'll need to let the admins know.



Then you'll need to install buildbot and its dependencies on the machine that will be doing the nightly build; see the [
BuildBot website](http://buildbot.net/) for details.  NB. if you're on Windows, you'll need to install BuildBot under Cygwin using the Cygwin Python; there are various problems getting the GHC build to work via BuildBot using the native Win32 Python, so we've given up on that route for now.



In order to actually do the build, you'll also need the prerequisite tools for a GHC build, see [Building/Preparation](building/preparation), and you should have darcs installed too (we recommend at least version 0.9).



Now create and enter the directory you want the buildbot client to work in


```wiki
$ mkdir /buildbot/ghc
$ cd /buildbot/ghc
```


and tell buildbot to set up a slave there


```wiki
$ buildbot create-slave . darcs.haskell.org:9989 myUser myPass
```


This will print a few lines asking you to fill in `info/admin` and `info/host`. In the latter file, please include information on what operating system and architecture the machine is running.



By default it seems that buildbot uses a umask of 077, this probably isn't what you want.  For example, if you intend to upload distributions, they'll have restricted permissions.  You can change it in the `buildbot.tac` file, we set it to 002 for example:


```wiki
umask = 002
```


It also created `Makefile.sample`; we recommend renaming this to `Makefile`. You can now start the buildbot client with `make start` and stop it with `make stop`.



You can watch what your slave is doing by looking at the `twistd.log` file in the directory in which you're running your slave.


## Automating startup: Unix



The easiest way to make the client start up automatically is to use `cron`.  Type `crontab -e`, and add this line to your crontab file:


```wiki
@reboot cd <buildbotdir> && make start
```


Remember to change `<buildbotdir>` to your buildbot directory.



Cron will run the command in a minimal environment: it won't execute your normal shell startup files, so you won't have your usual `PATH` settings, for example.  To get the right `PATH` and other environment variables, we suggest adding them to the `make start` rule in `<buildbotdir>/Makefile`.  For example, my start rule looks something like this:


```wiki
start:
	PATH=/usr/bin:/bin:/home/simonmar/bin \
	http_proxy=http://157.58.63.38:80 \
	twistd --no_save -y buildbot.tac
```


It might be a good idea to have the buildbot restarted once a day before your build is due to start, just in case it has died for any reason.  I have another line in my crontab that looks like this:


```wiki
0 17 * * * cd <buildbotdir> && (make stop; make start)
```


To restart the client at 17.00, before the builds start at 18.00.



It's a good idea to test that running the client via a cron job actually works, so test it: setup a temporary cron job to start the client in a couple of minutes time, check that the client is up and running, and maybe force a build via the status page to check that the build environment is working.


## Automating startup: Windows



I did it the following way.  Create a script in `<buildbotdir>/restart.sh`:


```wiki
PATH=/bin:/usr/bin
cd <buildbotdir>
make stop
make start
```


(don't forget to create the script as a Unix text file, not a DOS text file, otherwise strange things will probably happen, they did to me anyway).



Create a new "Scheduled Task" via Control Panel-\>Scheduled Tasks.  The command you want to run is


```wiki
c:\cygwin\bin\sh <buildbotdir>/restart.sh
```


Schedule the task to run (a) at startup and possibly also (b) once a day, before your build is due to start.  You can add multiple schedulers for a task by checking the box at the bottom of the "Schedule" page of the scheduled task settings.


## Admin steps



(for the admins only...)



Pull the buildbot master configuration:


```wiki
$ darcs get buildbot@darcs.haskell.org:/home/buildbot/master
$ cd master
```


Edit `master.cfg`.  Add new entries to `slaves`, `schedulers`, and `builders` as necessary.  Record and push the changes.  Then restart the build master:


```wiki
$ ssh buildbot@darcs.haskell.org "cd master; make reconfig"
```


If there is anything unusual about the machine the build is being run on, e.g. the path to `gcc` is different, then you will need to add a field for the unusual thing to GhcDefaultConfig and alter the build steps to make use of it. Then make a special factory for the build client you are adding with this field changed as appropriate.


## Did it work?



Once the master is reconfiged and the client is started, the client should become visible on
[ http://darcs.haskell.org:8010/](http://darcs.haskell.org:8010/)



At present there is no way to force an immediate test build.


## Buildbot gotchas



On MacOS X, when the user who runs the buildbot process logs out, the process keeps  
running, but it loses the ability to do DNS queries!  This shows up as failures like this:


```wiki
darcs failed:  Not a repository:  
http://darcs.haskell.org/ghc
(Failed to download URL http://darcs.haskell.org/ghc/_darcs/inventory: couldn't resolve host name)
```


To resolve this, there is some extra work to do when you configure your buildbot client: [
http://buildbot.net/trac/wiki/UsingLaunchd](http://buildbot.net/trac/wiki/UsingLaunchd)


