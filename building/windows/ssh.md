# Configuring SSH On Windows



`ssh` comes with both Cygwin and MSYS. 
(Cygwin note: you need to ask for package `openssh` (not ssh)
in the Cygwin list of packages; or use the `ghc-depends`
package -- see [Installing and configuring Cygwin](#InstallingandconfiguringCygwin).)



There are several strange things about `ssh` on Windows that you need to know.


- The programs `ssh-keygen1`, `ssh1`, and `cvs`,
  seem to lock up `bash` entirely if they try to get user input (e.g. if
  they ask for a password).  To solve this, start up `cmd.exe` 
  and run it as follows:

  ```wiki
  c:\tmp> set CYGWIN32=tty
  c:\tmp> c:/user/local/bin/ssh-keygen1
  ```
- (Cygwin-only problem, I think.)
  `ssh` needs to access your directory `.ssh`, in your home directory.  
  To determine your home directory `ssh` first looks in 
  `c:/cygwin/etc/passwd` (or wherever you have Cygwin installed).  If there's an entry
  there with your userid, it'll use that entry to determine your home directory, *ignoring
  the setting of the environment variable $HOME*.  If the home directory is
  bogus, `ssh` fails horribly.   The best way to see what is going on is to say

  ```wiki
  ssh -v cvs.haskell.org
  ```

  which makes `ssh` print out information about its activity.

  You can fix this problem, either by correcting the home-directory field in 
  `c:/cygwin/etc/passwd`, or by simply deleting the entire entry for your userid. If
  you do that, `ssh` uses the $HOME environment variable instead.
- To protect your
  `.ssh` from access by anyone else,
  right-click your `.ssh` directory, and
  select `Properties`.  If you are not on
  the access control list, add yourself, and give yourself
  full permissions (the second panel).  Remove everyone else
  from the access control list.  Don't leave them there but
  deny them access, because 'they' may be a list that
  includes you!
- In fact `ssh` 3.6.1 now seems to *require*
  you to have Unix permissions 600 (read/write for owner only) 
  on the `.ssh/identity` file, else it 
  bombs out.  For your local C drive, it seems that `chmod 600 identity` works,
  but on Windows NT/XP, it doesn't work on a network drive (exact dteails obscure).  
  The solution seems to be to set the `$CYGWIN` environment
  variable to "`ntsec neta`".  The `$CYGWIN` environment variable is discussed
  in [
  the Cygwin User's Guide](http://cygwin.com/cygwin-ug-net/using-cygwinenv.html),
  and there are more details in [
  the Cygwin FAQ](http://cygwin.com/faq/faq_4.html#SEC44).
