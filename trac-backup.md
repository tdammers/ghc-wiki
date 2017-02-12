# Trac Backup






Trac backups are simply a copied snapshot of the entire [project environment](trac-environment) directory, including the database. Backups can be created using the `hotcopy` command in [trac-admin](trac-admin).



**Note**: Trac uses the `hotcopy` nomenclature to match that of [
Subversion](http://subversion.tigris.org/), to make it easier to remember when managing both Trac and Subversion servers.


## Creating a Backup



To create a backup of a live [TracEnvironment](trac-environment) simply run:


```
$ trac-admin /path/to/projenv hotcopy /path/to/backupdir
```


[trac-admin](trac-admin) will lock the database while copying.



The resulting backup directory is safe to handle using standard file-based backup tools like `tar` or `dump`/`restore`.



Please note, the `hotcopy` command will not overwrite a target directory and when such exists, the operation ends with an error: `Command failed: [Errno 17] File exists:` This is discussed in [
\#3198](http://trac.edgewall.org/intertrac/ticket%3A3198).


## Restoring a Backup



To restore an environment from a backup, stop the process running Trac, ie the Web server or [tracd](trac-standalone), restore the contents of your backup (path/to/backupdir) to your [project environment](trac-environment) directory and restart the service.



To restore a PostgreSQL database backup, use the command:


```
psql -U <user> -d <database> -f postgresql.dump
```


The `<database>` option is the same as the [database connection string](trac-environment#database-connection-strings) in the `[trac]` `database` option of *trac.ini*.


---



See also: [TracAdmin](trac-admin), [TracEnvironment](trac-environment), [TracGuide](trac-guide), [
TracMigrate](http://trac.edgewall.org/intertrac/TracMigrate)


