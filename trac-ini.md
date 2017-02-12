# The Trac Configuration File







Trac is configured through the **`trac.ini`** file, located in the `<projectenv>/conf` directory. The `trac.ini` configuration file and its parent directory should be writable by the web server.



Trac monitors the timestamp of the file to trigger a complete environment reload and flush its caches when the timestamp changes. Most changes to the configuration will be reflected immediately, though changes to the `[components]` or `[logging]` sections will require restarting the web server. You may also need to restart the web server after creating a [global configuration](trac-ini#lobal-configuration) file when none was previously present.


## Global Configuration



Configuration can be shared among environments using one or more global configuration files. Options in the global configuration will be merged with the environment-specific options, with local options overriding global options. The global configuration file is specified as follows:


```
[inherit]
file = /path/to/global/trac.ini
```


Multiple files can be specified using a comma-separated list.



Note that you can also specify a global option file when creating a new project, by adding the option `--inherit=/path/to/global/trac.ini` to [trac-admin](trac-admin#)'s `initenv` command. If you do not do this but nevertheless intend to use a global option file with your new environment, you will have to go through the newly generated `conf/trac.ini` file and delete the entries that will otherwise override those set in the global file.



There are two more entries in the [ \[inherit\] ](trac-ini#) section, `templates_dir` for sharing global templates and `plugins_dir`, for sharing plugins. Those entries can themselves be specified in the shared configuration file, and in fact, configuration files can even be chained if you specify another `[inherit] file` there.



Note that the templates found in the `templates/` directory of the [TracEnvironment](trac-environment) have precedence over those found in `[inherit] templates_dir`. In turn, the latter have precedence over the installed templates, so be careful about what you put there. Notably, if you override a default template, refresh your modifications when you upgrade to a new version of Trac. The preferred way to perform [TracInterfaceCustomization](trac-interface-customization) is still to write a custom plugin doing an appropriate `ITemplateStreamFilter` transformation.


## Reference for settings



This is a brief reference of available configuration options, and their default settings.



Documentation improvements should be discussed on the [
trac-dev mailing list](http://trac.edgewall.org/intertrac/MailingList%23Trac-dev) or described in a [
ticket](http://trac.edgewall.org/intertrac/NewTicket). Even better, [
submit a patch](http://trac.edgewall.org/intertrac/TracDev/SubmittingPatches) against the docstrings in the code.



### `[account-manager]`

<table><tr><th>[account\_changes\_notify\_addresses](#account-manager-account_changes_notify_addresses-option)</th>
<th>
List of email addresses that get notified of user changes, ie,
new user, password change and delete user.


</th>
<th>(no default)</th></tr>
<tr><th>[allow\_delete\_account](#account-manager-allow_delete_account-option)</th>
<th>
Allow users to delete their own account.


</th>
<th>`enabled`</th></tr>
<tr><th>[auth\_init](#account-manager-auth_init-option)</th>
<th>
Launch an initial Trac authentication setup.


</th>
<th>`enabled`</th></tr>
<tr><th>[cookie\_refresh\_pct](#account-manager-cookie_refresh_pct-option)</th>
<th>
Persistent sessions randomly get a new session cookie ID with
likelihood in percent per work hour given here (zero equals to never)
to decrease vulnerability of long-lasting sessions.


</th>
<th>`10`</th></tr>
<tr><th>[db\_htdigest\_realm](#account-manager-db_htdigest_realm-option)</th>
<th>
Realm to select relevant htdigest db entries


</th>
<th>(no default)</th></tr>
<tr><th>[db\_htpasswd\_hash\_type](#account-manager-db_htpasswd_hash_type-option)</th>
<th>
Default hash type of new/updated passwords


</th>
<th>`crypt`</th></tr>
<tr><th>[email\_regexp](#account-manager-email_regexp-option)</th>
<th>
A validation regular expression describing new account emails.
Define constraints for a valid email address. A custom pattern can
narrow or widen scope i.e. to accept UTF-8 characters.


</th>
<th>`(?i)^[A-Z0-9._%+-]+@(?:[A-Z0-9-]+\.)+[A-Z0-9-]{2,63}$`</th></tr>
<tr><th>[environ\_auth\_overwrite](#account-manager-environ_auth_overwrite-option)</th>
<th>
Whether environment variable REMOTE\_USER should get overwritten
after processing login form input. Otherwise it will only be set,
if unset at the time of authentication.


</th>
<th>`enabled`</th></tr>
<tr><th>[force\_passwd\_change](#account-manager-force_passwd_change-option)</th>
<th>
Force the user to change password when it's reset.


</th>
<th>`enabled`</th></tr>
<tr><th>[generated\_password\_length](#account-manager-generated_password_length-option)</th>
<th>
Length of the randomly-generated passwords created when resetting
the password for an account.


</th>
<th>`8`</th></tr>
<tr><th>[hash\_method](#account-manager-hash_method-option)</th>
<th>
IPasswordHashMethod used to create new/updated passwords


</th>
<th>`HtDigestHashMethod`</th></tr>
<tr><th>[login\_attempt\_max\_count](#account-manager-login_attempt_max_count-option)</th>
<th>
Lock user account after specified number of login attempts.
Value zero means no limit.


</th>
<th>`0`</th></tr>
<tr><th>[login\_opt\_list](#account-manager-login_opt_list-option)</th>
<th>
Set to True, to switch login page style showing alternative actions
in a single listing together.


</th>
<th>`disabled`</th></tr>
<tr><th>[notify\_actions](#account-manager-notify_actions-option)</th>
<th>
Comma separated list of actions to notify of.
Available actions 'new', 'change', 'delete'.


</th>
<th>(no default)</th></tr>
<tr><th>[password\_store](#account-manager-password_store-option)</th>
<th>
Ordered list of password stores, queried in turn.


</th>
<th>(no default)</th></tr>
<tr><th>[persistent\_sessions](#account-manager-persistent_sessions-option)</th>
<th>
Allow the user to be remembered across sessions without
needing to re-authenticate. This is, user checks a
"Remember Me" checkbox and, next time he visits the site,
he'll be remembered.


</th>
<th>`disabled`</th></tr>
<tr><th>[refresh\_passwd](#account-manager-refresh_passwd-option)</th>
<th>
Re-set passwords on successful authentication.
This is most useful to move users to a new password store or
enforce new store configuration (i.e. changed hash type),
but should be disabled/unset otherwise.


</th>
<th>`disabled`</th></tr>
<tr><th>[register\_basic\_question](#account-manager-register_basic_question-option)</th>
<th>
A question to ask instead of the standard prompt, to which the value of register\_basic\_token is the answer. Setting to empty string (default value) keeps the standard prompt.


</th>
<th>(no default)</th></tr>
<tr><th>[register\_basic\_token](#account-manager-register_basic_token-option)</th>
<th>
A string required as input to pass verification.


</th>
<th>(no default)</th></tr>
<tr><th>[register\_check](#account-manager-register_check-option)</th>
<th>
Ordered list of IAccountRegistrationInspector's to use for
registration checks.


</th>
<th>`BasicCheck,EmailCheck,BotTrapCheck,RegExpCheck,UsernamePermCheck`</th></tr>
<tr><th>[require\_approval](#account-manager-require_approval-option)</th>
<th>
Whether account registration requires administrative approval to
enable the account or not.


</th>
<th>`disabled`</th></tr>
<tr><th>[reset\_password](#account-manager-reset_password-option)</th>
<th>
Set to False, if there is no email system setup.


</th>
<th>`enabled`</th></tr>
<tr><th>[user\_lock\_max\_time](#account-manager-user_lock_max_time-option)</th>
<th>
Limit user account lock time to specified time (seconds).
This is relevant only with user\_lock\_time\_progression \> 1.


</th>
<th>`86400`</th></tr>
<tr><th>[user\_lock\_time](#account-manager-user_lock_time-option)</th>
<th>
Drop user account lock after specified time (seconds).
Value zero means unlimited lock time.


</th>
<th>`0`</th></tr>
<tr><th>[user\_lock\_time\_progression](#account-manager-user_lock_time_progression-option)</th>
<th>
Extend user account lock time incrementally. This is
based on logarithmic calculation and decimal numbers accepted:
Value '1' means constant lock time per failed login attempt.
Value '2' means double locktime after 2nd lock activation,
four times the initial locktime after 3rd, and so on.


</th>
<th>`1`</th></tr>
<tr><th>[username\_char\_blacklist](#account-manager-username_char_blacklist-option)</th>
<th>
Always exclude some special characters from usernames.
This is enforced upon new user registration.


</th>
<th>`:[]`</th></tr>
<tr><th>[username\_regexp](#account-manager-username_regexp-option)</th>
<th>
A validation regular expression describing new usernames. Define
constraints for allowed user names corresponding to local naming
policy.


</th>
<th>`(?i)^[A-Z0-9.\-_]{5,}$`</th></tr>
<tr><th>[verify\_email](#account-manager-verify_email-option)</th>
<th>
Verify the email address of Trac users.


</th>
<th>`enabled`</th></tr></table>

### `[attachment]`

<table><tr><th>[max\_size](#attachment-max_size-option)</th>
<th>
Maximum allowed file size (in bytes) for attachments.


</th>
<th>`262144`</th></tr>
<tr><th>[max\_zip\_size](#attachment-max_zip_size-option)</th>
<th>
Maximum allowed total size (in bytes) for an attachment list to be
downloadable as a `.zip`. Set this to -1 to disable download as `.zip`.
(*since 1.0*)


</th>
<th>`2097152`</th></tr>
<tr><th>[render\_unsafe\_content](#attachment-render_unsafe_content-option)</th>
<th>
Whether attachments should be rendered in the browser, or
only made downloadable.



Pretty much any file may be interpreted as HTML by the browser,
which allows a malicious user to attach a file containing cross-site
scripting attacks.



For public sites where anonymous users can create attachments it is
recommended to leave this option disabled.


</th>
<th>`disabled`</th></tr></table>

### `[browser]`

<table><tr><th>[color\_scale](#browser-color_scale-option)</th>
<th>
Enable colorization of the *age* column.



This uses the same color scale as the source code annotation:
blue is older, red is newer.


</th>
<th>`enabled`</th></tr>
<tr><th>[downloadable\_paths](#browser-downloadable_paths-option)</th>
<th>
List of repository paths that can be downloaded.



Leave this option empty if you want to disable all downloads, otherwise
set it to a comma-separated list of authorized paths (those paths are
glob patterns, i.e. "\*" can be used as a wild card). In a
multi-repository environment, the path must be qualified with the
repository name if the path does not point to the default repository
(e.g. /reponame/trunk). Note that a simple prefix matching is
performed on the paths, so aliases won't get automatically resolved.


</th>
<th>`/trunk,/branches/*,/tags/*`</th></tr>
<tr><th>[hide\_properties](#browser-hide_properties-option)</th>
<th>
Comma-separated list of version control properties to hide from
the repository browser.


</th>
<th>`svk:merge`</th></tr>
<tr><th>[intermediate\_color](#browser-intermediate_color-option)</th>
<th>
(r,g,b) color triple to use for the color corresponding
to the intermediate color, if two linear interpolations are used
for the color scale (see `intermediate_point`).
If not set, the intermediate color between `oldest_color` and
`newest_color` will be used.


</th>
<th>(no default)</th></tr>
<tr><th>[intermediate\_point](#browser-intermediate_point-option)</th>
<th>
If set to a value between 0 and 1 (exclusive), this will be the
point chosen to set the `intermediate_color` for interpolating
the color value.


</th>
<th>(no default)</th></tr>
<tr><th>[newest\_color](#browser-newest_color-option)</th>
<th>
(r,g,b) color triple to use for the color corresponding
to the newest color, for the color scale used in *blame* or
the browser *age* column if `color_scale` is enabled.


</th>
<th>`(255, 136, 136)`</th></tr>
<tr><th>[oldest\_color](#browser-oldest_color-option)</th>
<th>
(r,g,b) color triple to use for the color corresponding
to the oldest color, for the color scale used in *blame* or
the browser *age* column if `color_scale` is enabled.


</th>
<th>`(136, 136, 255)`</th></tr>
<tr><th>[oneliner\_properties](#browser-oneliner_properties-option)</th>
<th>
Comma-separated list of version control properties to render
as oneliner wiki content in the repository browser.


</th>
<th>`trac:summary`</th></tr>
<tr><th>[render\_unsafe\_content](#browser-render_unsafe_content-option)</th>
<th>
Whether raw files should be rendered in the browser, or only made
downloadable.



Pretty much any file may be interpreted as HTML by the browser,
which allows a malicious user to create a file containing cross-site
scripting attacks.



For open repositories where anyone can check-in a file, it is
recommended to leave this option disabled.


</th>
<th>`disabled`</th></tr>
<tr><th>[wiki\_properties](#browser-wiki_properties-option)</th>
<th>
Comma-separated list of version control properties to render
as wiki content in the repository browser.


</th>
<th>`trac:description`</th></tr></table>

### `[changeset]`

<table><tr><th>[max\_diff\_bytes](#changeset-max_diff_bytes-option)</th>
<th>
Maximum total size in bytes of the modified files (their old size
plus their new size) for which the changeset view will attempt to show
the diffs inlined.


</th>
<th>`10000000`</th></tr>
<tr><th>[max\_diff\_files](#changeset-max_diff_files-option)</th>
<th>
Maximum number of modified files for which the changeset view will
attempt to show the diffs inlined.


</th>
<th>`0`</th></tr>
<tr><th>[wiki\_format\_messages](#changeset-wiki_format_messages-option)</th>
<th>
Whether wiki formatting should be applied to changeset messages.



If this option is disabled, changeset messages will be rendered as
pre-formatted text.


</th>
<th>`enabled`</th></tr></table>

### `[components]`


This section is used to enable or disable components
provided by plugins, as well as by Trac itself. The component
to enable/disable is specified via the name of the
option. Whether its enabled is determined by the option value;
setting the value to `enabled` or `on` will enable the
component, any other value (typically `disabled` or `off`)
will disable the component.



The option name is either the fully qualified name of the
components or the module/package prefix of the component. The
former enables/disables a specific component, while the latter
enables/disables any component in the specified
package/module.



Consider the following configuration snippet:


```wiki
[components]
trac.ticket.report.ReportModule = disabled
acct_mgr.* = enabled
```


The first option tells Trac to disable the
[report module](trac-reports).
The second option instructs Trac to enable all components in
the `acct_mgr` package. Note that the trailing wildcard is
required for module/package matching.



To view the list of active components, go to the *Plugins*
page on *About Trac* (requires `CONFIG_VIEW`
[permissions](trac-permissions)).



See also: [TracPlugins](trac-plugins)


### `[fullblog]`

<table><tr><th>[all\_rss\_icons](#fullblog-all_rss_icons-option)</th>
<th>
Controls whether or not to display rss icons more than once


</th>
<th>`disabled`</th></tr>
<tr><th>[archive\_rss\_icon](#fullblog-archive_rss_icon-option)</th>
<th>
Controls whether or not to display the rss icon


</th>
<th>`enabled`</th></tr>
<tr><th>[default\_postname](#fullblog-default_postname-option)</th>
<th>
Option for a default naming scheme for new posts. The string
can include substitution markers for time (UTC) and user: %Y=year,
%m=month, %d=day, %H=hour, %M=minute, %S=second, $USER.
Example template string: `%Y/%m/%d/my_topic`


</th>
<th>(no default)</th></tr>
<tr><th>[month\_names](#fullblog-month_names-option)</th>
<th>
Ability to specify a list of month names for display in groupings.
If empty it will make a list from default locale setting.
Enter list of 12 months like:
`month_names = January, February, ..., December`


</th>
<th>(no default)</th></tr>
<tr><th>[num\_items\_front](#fullblog-num_items_front-option)</th>
<th>
Option to specify how many recent posts to display on the
front page of the Blog (and RSS feeds).


</th>
<th>`20`</th></tr>
<tr><th>[personal\_blog](#fullblog-personal_blog-option)</th>
<th>
When using the Blog as a personal blog (only one author), setting to 'True'
will disable the display of 'Browse by author:' in sidebar, and also removes
various author links and references.


</th>
<th>`disabled`</th></tr></table>

### `[git]`

<table><tr><th>[cached\_repository](#git-cached_repository-option)</th>
<th>
Wrap `GitRepository` in `CachedRepository`.


</th>
<th>`disabled`</th></tr>
<tr><th>[git\_bin](#git-git_bin-option)</th>
<th>
Path to the git executable.


</th>
<th>`git`</th></tr>
<tr><th>[git\_fs\_encoding](#git-git_fs_encoding-option)</th>
<th>
Define charset encoding of paths within git repositories.


</th>
<th>`utf-8`</th></tr>
<tr><th>[persistent\_cache](#git-persistent_cache-option)</th>
<th>
Enable persistent caching of commit tree.


</th>
<th>`disabled`</th></tr>
<tr><th>[shortrev\_len](#git-shortrev_len-option)</th>
<th>
The length at which a sha1 should be abbreviated to (must
be \>= 4 and \<= 40).


</th>
<th>`7`</th></tr>
<tr><th>[trac\_user\_rlookup](#git-trac_user_rlookup-option)</th>
<th>
Enable reverse mapping of git email addresses to trac user ids.
Performance will be reduced if there are many users and the
`cached_repository` option is `disabled`.



A repository resync is required after changing the value of this
option.


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_committer\_id](#git-use_committer_id-option)</th>
<th>
Use git-committer id instead of git-author id for the
changeset *Author* field.


</th>
<th>`enabled`</th></tr>
<tr><th>[use\_committer\_time](#git-use_committer_time-option)</th>
<th>
Use git-committer timestamp instead of git-author timestamp
for the changeset *Timestamp* field.


</th>
<th>`enabled`</th></tr>
<tr><th>[wikishortrev\_len](#git-wikishortrev_len-option)</th>
<th>
The minimum length of an hex-string for which
auto-detection as sha1 is performed (must be \>= 4 and \<= 40).


</th>
<th>`40`</th></tr></table>

### `[gitweb-repositories]`

<table><tr><th>[projects\_base](#gitweb-repositories-projects_base-option)</th>
<th>
Path to the base of your git projects


</th>
<th>(no default)</th></tr>
<tr><th>[projects\_list](#gitweb-repositories-projects_list-option)</th>
<th>
Path to a gitweb-formatted projects.list


</th>
<th>(no default)</th></tr>
<tr><th>[projects\_url](#gitweb-repositories-projects_url-option)</th>
<th>
Template for project URLs. `%s` will be replaced with the repo
name


</th>
<th>(no default)</th></tr>
<tr><th>[sync\_per\_request](#gitweb-repositories-sync_per_request-option)</th>
<th>
Repositories to sync on every request
(not recommended).


</th>
<th>(no default)</th></tr></table>

### `[header_logo]`

<table><tr><th>[alt](#header_logo-alt-option)</th>
<th>
Alternative text for the header logo.


</th>
<th>`(please configure the [header_logo] section in trac.ini)`</th></tr>
<tr><th>[height](#header_logo-height-option)</th>
<th>
Height of the header logo image in pixels.


</th>
<th>`-1`</th></tr>
<tr><th>[link](#header_logo-link-option)</th>
<th>
URL to link to, from the header logo.


</th>
<th>(no default)</th></tr>
<tr><th>[src](#header_logo-src-option)</th>
<th>
URL of the image to use as header logo.
It can be absolute, server relative or relative.



If relative, it is relative to one of the `/chrome` locations:
`site/your-logo.png` if `your-logo.png` is located in the `htdocs`
folder within your [TracEnvironment](trac-environment);
`common/your-logo.png` if `your-logo.png` is located in the
folder mapped to the [htdocs\_location](trac-ini#) URL.
Only specifying `your-logo.png` is equivalent to the latter.


</th>
<th>`site/your_project_logo.png`</th></tr>
<tr><th>[width](#header_logo-width-option)</th>
<th>
Width of the header logo image in pixels.


</th>
<th>`-1`</th></tr></table>

### `[httpauth]`

<table><tr><th>[formats](#httpauth-formats-option)</th>
<th>
Request formats to force HTTP authentication on


</th>
<th>(no default)</th></tr>
<tr><th>[paths](#httpauth-paths-option)</th>
<th>
Paths to force HTTP authentication on.


</th>
<th>`/login/xmlrpc`</th></tr></table>

### `[inherit]`

<table><tr><th>[htdocs\_dir](#inherit-htdocs_dir-option)</th>
<th>
Path to the *shared htdocs directory*.



Static resources in that directory are mapped to /chrome/shared
under the environment URL, in addition to common and site locations.



This can be useful in site.html for common interface customization
of multiple Trac environments.



(*since 1.0*)


</th>
<th>(no default)</th></tr>
<tr><th>[plugins\_dir](#inherit-plugins_dir-option)</th>
<th>
Path to the *shared plugins directory*.



Plugins in that directory are loaded in addition to those in
the directory of the environment `plugins`, with this one
taking precedence.


</th>
<th>(no default)</th></tr>
<tr><th>[templates\_dir](#inherit-templates_dir-option)</th>
<th>
Path to the *shared templates directory*.



Templates in that directory are loaded in addition to those in the
environments `templates` directory, but the latter take precedence.


</th>
<th>(no default)</th></tr></table>

### `[intertrac]`


This section configures [InterTrac](inter-trac) prefixes. Options in this section
whose name contain a `.` define aspects of the [InterTrac](inter-trac) prefix
corresponding to the option name up to the `.`. Options whose name
don't contain a `.` define an alias.



The `.url` is mandatory and is used for locating the other Trac.
This can be a relative URL in case that Trac environment is located
on the same server.



The `.title` information is used for providing a useful tooltip when
moving the cursor over an [InterTrac](inter-trac) link.



Example configuration:


```wiki
[intertrac]
# -- Example of setting up an alias:
t = trac

# -- Link to an external Trac:
trac.title = Edgewall's Trac for Trac
trac.url = http://trac.edgewall.org
```

### `[interwiki]`


Every option in the `[interwiki]` section defines one [InterWiki](inter-wiki)
prefix. The option name defines the prefix. The option value defines
the URL, optionally followed by a description separated from the URL
by whitespace. Parametric URLs are supported as well.



**Example:**


```wiki
[interwiki]
MeatBall = http://www.usemod.com/cgi-bin/mb.pl?
PEP = http://www.python.org/peps/pep-$1.html Python Enhancement Proposal $1
tsvn = tsvn: Interact with TortoiseSvn
```

### `[logging]`

<table><tr><th>[log\_file](#logging-log_file-option)</th>
<th>
If `log_type` is `file`, this should be a path to the
log-file.  Relative paths are resolved relative to the `log`
directory of the environment.


</th>
<th>`trac.log`</th></tr>
<tr><th>[log\_format](#logging-log_format-option)</th>
<th>
Custom logging format.



If nothing is set, the following will be used:



`Trac[$(module)s] $(levelname)s: $(message)s`



In addition to regular key names supported by the
[ Python logger library](http://docs.python.org/library/logging.html)
one could use:


- `$(path)s`     the path for the current environment
- `$(basename)s` the last path component of the current environment
- `$(project)s`  the project name


Note the usage of `$(...)s` instead of `%(...)s` as the latter form
would be interpreted by the ConfigParser itself.



Example:
`($(thread)d) Trac[$(basename)s:$(module)s] $(levelname)s: $(message)s`


</th>
<th>(no default)</th></tr>
<tr><th>[log\_level](#logging-log_level-option)</th>
<th>
Level of verbosity in log.



Should be one of (`CRITICAL`, `ERROR`, `WARNING`, `INFO`, `DEBUG`).


</th>
<th>`DEBUG`</th></tr>
<tr><th>[log\_type](#logging-log_type-option)</th>
<th>
Logging facility to use.



Should be one of (`none`, `file`, `stderr`, `syslog`, `winlog`).


</th>
<th>`none`</th></tr></table>

### `[mainnav]`


Configures the main navigation bar,
which by default contains *Wiki*, *Timeline*, *Roadmap*,
*Browse Source*, *View Tickets*, *New Ticket*, *Search*  and
*Admin*.



The `label`, `href`, and `order`  attributes can be specified. Entries
can be disabled by setting the value of the navigation item to
`disabled`.



The following example renames the link to [WikiStart](wiki-start) to *Home*,
links the *View Tickets* entry to a specific report and disables
the *Search* entry.


```
[mainnav]
wiki.label = Home
tickets.href = /report/24
search = disabled
```


See [TracNavigation](trac-navigation) for more details.


### `[mastertickets]`

<table><tr><th>[acceptable\_formats](#mastertickets-acceptable_formats-option)</th>
<th>
The formats that may be chosen; execute dot -T? for a
list of options.


</th>
<th>`png,cmapx`</th></tr>
<tr><th>[closed\_color](#mastertickets-closed_color-option)</th>
<th>
Color of closed tickets


</th>
<th>`green`</th></tr>
<tr><th>[closed\_text](#mastertickets-closed_text-option)</th>
<th>
Text for key showing closed tickets


</th>
<th>`Done`</th></tr>
<tr><th>[dot\_path](#mastertickets-dot_path-option)</th>
<th>
Path to the dot executable.


</th>
<th>`dot`</th></tr>
<tr><th>[full\_graph](#mastertickets-full_graph-option)</th>
<th>
Show full dep. graph, not just direct blocking links


</th>
<th>`disabled`</th></tr>
<tr><th>[graph\_direction](#mastertickets-graph_direction-option)</th>
<th>
Direction of the dependency graph (TD = Top Down,
DT = Down Top, LR = Left Right, RL = Right Left).


</th>
<th>`TD`</th></tr>
<tr><th>[gs\_path](#mastertickets-gs_path-option)</th>
<th>
Path to the ghostscript executable.


</th>
<th>`gs`</th></tr>
<tr><th>[highlight\_target](#mastertickets-highlight_target-option)</th>
<th>
Highlight target tickets in graph


</th>
<th>`disabled`</th></tr>
<tr><th>[opened\_color](#mastertickets-opened_color-option)</th>
<th>
Color of opened tickets


</th>
<th>`red`</th></tr>
<tr><th>[opened\_text](#mastertickets-opened_text-option)</th>
<th>
Text for key showing opened tickets


</th>
<th>`ToDo`</th></tr>
<tr><th>[show\_key](#mastertickets-show_key-option)</th>
<th>
Show a key for open/closed nodes


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_gs](#mastertickets-use_gs-option)</th>
<th>
If enabled, use ghostscript to produce nicer output.


</th>
<th>`disabled`</th></tr></table>

### `[metanav]`


Configures the meta navigation
entries, which by default are *Login*, *Logout*, *Preferences*,
*Help/Guide* and *About Trac*. The allowed attributes are the
same as for `[mainnav]`. Additionally, a special entry is supported -
`logout.redirect` is the page the user sees after hitting the logout
button. For example:


```
[metanav]
logout.redirect = wiki/Logout
```


See [TracNavigation](trac-navigation) for more details.


### `[milestone]`

<table><tr><th>[default\_group\_by](#milestone-default_group_by-option)</th>
<th>
Default field to use for grouping tickets in the grouped
progress bar. (*since 1.2*)


</th>
<th>`component`</th></tr>
<tr><th>[default\_retarget\_to](#milestone-default_retarget_to-option)</th>
<th>
Default milestone to which tickets are retargeted when
closing or deleting a milestone. (*since 1.1.2*)


</th>
<th>(no default)</th></tr>
<tr><th>[stats\_provider](#milestone-stats_provider-option)</th>
<th>
Name of the component implementing `ITicketGroupStatsProvider`,
which is used to collect statistics on groups of tickets for display
in the milestone views.


</th>
<th>`DefaultTicketGroupStatsProvider`</th></tr></table>

### `[milestone-groups]`


As the workflow for tickets is now configurable, there can
be many ticket states, and simply displaying closed tickets
vs. all the others is maybe not appropriate in all cases. This
section enables one to easily create *groups* of states that
will be shown in different colors in the milestone progress
bar.



Note that the groups can only be based on the ticket
*status*, nothing else. In particular, it's not possible to
distinguish between different closed tickets based on the
*resolution*.



Example configuration with three groups, *closed*, *new*
and *active* (the default only has closed and active):


```wiki
# the 'closed' group correspond to the 'closed' tickets
closed = closed

# .order: sequence number in the progress bar
closed.order = 0

# .query_args: optional parameters for the corresponding
#              query.  In this example, the changes from the
#              default are two additional columns ('created' and
#              'modified'), and sorting is done on 'created'.
closed.query_args = group=resolution,order=time,col=id,col=summary,col=owner,col=type,col=priority,col=component,col=severity,col=time,col=changetime

# .overall_completion: indicates groups that count for overall
#                      completion percentage
closed.overall_completion = true

new = new
new.order = 1
new.css_class = new
new.label = new

# Note: one catch-all group for other statuses is allowed
active = *
active.order = 2

# .css_class: CSS class for this interval
active.css_class = open

# .label: displayed label for this group
active.label = in progress
```


The definition consists in a comma-separated list of accepted
status.  Also, '\*' means any status and could be used to
associate all remaining states to one catch-all group.



The CSS class can be one of: new (yellow), open (no color) or
closed (green). Other styles can easily be added using custom
CSS rule: `table.progress td.<class> { background: <color> }`
to a [site/style.css](trac-interface-customization#) file
for example.


### `[mimeviewer]`

<table><tr><th>[max\_preview\_size](#mimeviewer-max_preview_size-option)</th>
<th>
Maximum file size for HTML preview.


</th>
<th>`262144`</th></tr>
<tr><th>[mime\_map](#mimeviewer-mime_map-option)</th>
<th>
List of additional MIME types and keyword mappings.
Mappings are comma-separated, and for each MIME type,
there's a colon (":") separated list of associated keywords
or file extensions.


</th>
<th>`text/x-dylan:dylan,text/x-idl:ice,text/x-ada:ads:adb`</th></tr>
<tr><th>[mime\_map\_patterns](#mimeviewer-mime_map_patterns-option)</th>
<th>
List of additional MIME types associated to filename patterns.
Mappings are comma-separated, and each mapping consists of a MIME type
and a Python regexp used for matching filenames, separated by a colon
(":"). (*since 1.0*)


</th>
<th>`text/plain:README(?!\.rst)|INSTALL(?!\.rst)|COPYING.*`</th></tr>
<tr><th>[pygments\_default\_style](#mimeviewer-pygments_default_style-option)</th>
<th>
The default style to use for Pygments syntax highlighting.


</th>
<th>`trac`</th></tr>
<tr><th>[pygments\_modes](#mimeviewer-pygments_modes-option)</th>
<th>
List of additional MIME types known by Pygments.



For each, a tuple `mimetype:mode:quality` has to be
specified, where `mimetype` is the MIME type,
`mode` is the corresponding Pygments mode to be used
for the conversion and `quality` is the quality ratio
associated to this conversion. That can also be used
to override the default quality ratio used by the
Pygments render.


</th>
<th>(no default)</th></tr>
<tr><th>[tab\_width](#mimeviewer-tab_width-option)</th>
<th>
Displayed tab width in file preview.


</th>
<th>`8`</th></tr>
<tr><th>[treat\_as\_binary](#mimeviewer-treat_as_binary-option)</th>
<th>
Comma-separated list of MIME types that should be treated as
binary data.


</th>
<th>`application/octet-stream,application/pdf,application/postscript,application/msword,application/rtf`</th></tr></table>

### `[notification]`

<table><tr><th>[admit\_domains](#notification-admit_domains-option)</th>
<th>
Comma-separated list of domains that should be considered as
valid for email addresses (such as localdomain).


</th>
<th>(no default)</th></tr>
<tr><th>[ambiguous\_char\_width](#notification-ambiguous_char_width-option)</th>
<th>
Width of ambiguous characters that should be used in the table
of the notification mail.



If `single`, the same width as characters in US-ASCII. This is
expected by most users. If `double`, twice the width of
US-ASCII characters.  This is expected by CJK users. (*since
0.12.2*)


</th>
<th>`single`</th></tr>
<tr><th>[batch\_subject\_template](#notification-batch_subject_template-option)</th>
<th>
Like `ticket_subject_template` but for batch modifications.
(*since 1.0*)


</th>
<th>`${prefix} Batch modify: ${tickets_descr}`</th></tr>
<tr><th>[default\_format.email](#notification-default_format.email-option)</th>
<th>
Default format to distribute email notifications.


</th>
<th>`text/plain`</th></tr>
<tr><th>[email\_address\_resolvers](#notification-email_address_resolvers-option)</th>
<th>
Comma separated list of email resolver components in the order
they will be called.  If an email address is resolved, the remaining
resolvers will not be called.


</th>
<th>`SessionEmailResolver`</th></tr>
<tr><th>[email\_sender](#notification-email_sender-option)</th>
<th>
Name of the component implementing `IEmailSender`.



This component is used by the notification system to send emails.
Trac currently provides `SmtpEmailSender` for connecting to an SMTP
server, and `SendmailEmailSender` for running a `sendmail`-compatible
executable. (*since 0.12*)


</th>
<th>`SmtpEmailSender`</th></tr>
<tr><th>[ignore\_domains](#notification-ignore_domains-option)</th>
<th>
Comma-separated list of domains that should not be considered
part of email addresses (for usernames with Kerberos domains).


</th>
<th>(no default)</th></tr>
<tr><th>[message\_id\_hash](#notification-message_id_hash-option)</th>
<th>
Hash algorithm to create unique Message-ID header.
*(since 1.0.13)*


</th>
<th>`md5`</th></tr>
<tr><th>[mime\_encoding](#notification-mime_encoding-option)</th>
<th>
Specifies the MIME encoding scheme for emails.



Supported values are: `none`, the default value which uses 7-bit
encoding if the text is plain ASCII or 8-bit otherwise. `base64`,
which works with any kind of content but may cause some issues with
touchy anti-spam/anti-virus engine. `qp` or `quoted-printable`,
which works best for european languages (more compact than base64) if
8-bit encoding cannot be used.


</th>
<th>`none`</th></tr>
<tr><th>[sendmail\_path](#notification-sendmail_path-option)</th>
<th>
Path to the sendmail executable.



The sendmail program must accept the `-i` and `-f` options.


>
>
> (*since 0.12*)
>
>

</th>
<th>`sendmail`</th></tr>
<tr><th>[smtp\_always\_bcc](#notification-smtp_always_bcc-option)</th>
<th>
Comma-separated list of email addresses to always send
notifications to. Addresses are not public (Bcc:).


</th>
<th>(no default)</th></tr>
<tr><th>[smtp\_always\_cc](#notification-smtp_always_cc-option)</th>
<th>
Comma-separated list of email addresses to always send
notifications to. Addresses can be seen by all recipients
(Cc:).


</th>
<th>(no default)</th></tr>
<tr><th>[smtp\_default\_domain](#notification-smtp_default_domain-option)</th>
<th>
Default host/domain to append to addresses that do not specify
one. Fully qualified addresses are not modified. The default
domain is appended to all username/login for which an email
address cannot be found in the user settings.


</th>
<th>(no default)</th></tr>
<tr><th>[smtp\_enabled](#notification-smtp_enabled-option)</th>
<th>
Enable email notification.


</th>
<th>`disabled`</th></tr>
<tr><th>[smtp\_from](#notification-smtp_from-option)</th>
<th>
Sender address to use in notification emails.



At least one of `smtp_from` and `smtp_replyto` must be set, otherwise
Trac refuses to send notification mails.


</th>
<th>`trac@localhost`</th></tr>
<tr><th>[smtp\_from\_author](#notification-smtp_from_author-option)</th>
<th>
Use the author of the change as the sender in notification emails
(e.g. reporter of a new ticket, author of a comment). If the
author hasn't set an email address, `smtp_from` and
`smtp_from_name` are used instead.
(*since 1.0*)


</th>
<th>`disabled`</th></tr>
<tr><th>[smtp\_from\_name](#notification-smtp_from_name-option)</th>
<th>
Sender name to use in notification emails.


</th>
<th>(no default)</th></tr>
<tr><th>[smtp\_password](#notification-smtp_password-option)</th>
<th>
Password for authenticating with SMTP server.


</th>
<th>(no default)</th></tr>
<tr><th>[smtp\_port](#notification-smtp_port-option)</th>
<th>
SMTP server port to use for email notification.


</th>
<th>`25`</th></tr>
<tr><th>[smtp\_replyto](#notification-smtp_replyto-option)</th>
<th>
Reply-To address to use in notification emails.



At least one of `smtp_from` and `smtp_replyto` must be set, otherwise
Trac refuses to send notification mails.


</th>
<th>`trac@localhost`</th></tr>
<tr><th>[smtp\_server](#notification-smtp_server-option)</th>
<th>
SMTP server hostname to use for email notifications.


</th>
<th>`localhost`</th></tr>
<tr><th>[smtp\_subject\_prefix](#notification-smtp_subject_prefix-option)</th>
<th>
Text to prepend to subject line of notification emails.



If the setting is not defined, then `[$project_name]` is used as the
prefix. If no prefix is desired, then specifying an empty option
will disable it.


</th>
<th>`__default__`</th></tr>
<tr><th>[smtp\_user](#notification-smtp_user-option)</th>
<th>
Username for authenticating with SMTP server.


</th>
<th>(no default)</th></tr>
<tr><th>[ticket\_subject\_template](#notification-ticket_subject_template-option)</th>
<th>
A Genshi text template snippet used to get the notification
subject.



The template variables are documented on the
[TracNotification](trac-notification#) page.


</th>
<th>`${prefix} #${ticket.id}: ${summary}`</th></tr>
<tr><th>[use\_public\_cc](#notification-use_public_cc-option)</th>
<th>
Addresses in the To and Cc fields are visible to all recipients.



If this option is disabled, recipients are put in the Bcc list.


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_short\_addr](#notification-use_short_addr-option)</th>
<th>
Permit email address without a host/domain (i.e. username only).



The SMTP server should accept those addresses, and either append
a FQDN or use local delivery. See also `smtp_default_domain`. Do not
use this option with a public SMTP server.


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_tls](#notification-use_tls-option)</th>
<th>
Use SSL/TLS to send notifications over SMTP.


</th>
<th>`disabled`</th></tr></table>

### `[notification-subscriber]`


The notifications subscriptions are controlled by plugins. All
`INotificationSubscriber` components are in charge. These components
may allow to be configured via this section in the `trac.ini` file.



See [TracNotification](trac-notification) for more details.



Available subscribers:

<table><tr><th>Subscriber</th>
<th>Description</th></tr>
<tr><th>`AlwaysEmailSubscriber`</th>
<th></th></tr>
<tr><th>`CarbonCopySubscriber`</th>
<th>Ticket that I'm listed in the CC field is modified</th></tr>
<tr><th>`TicketAlwaysEmailSubscriber`</th>
<th></th></tr>
<tr><th>`TicketOwnerSubscriber`</th>
<th>Ticket that I own is created or modified</th></tr>
<tr><th>`TicketPreviousUpdatersSubscriber`</th>
<th>Ticket that I previously updated is modified</th></tr>
<tr><th>`TicketReporterSubscriber`</th>
<th>Ticket that I reported is modified</th></tr>
<tr><th>`TicketUpdaterSubscriber`</th>
<th>I update a ticket</th></tr></table>



### `[project]`

<table><tr><th>[admin](#project-admin-option)</th>
<th>
E-Mail address of the project's administrator.


</th>
<th>(no default)</th></tr>
<tr><th>[admin\_trac\_url](#project-admin_trac_url-option)</th>
<th>
Base URL of a Trac instance where errors in this Trac
should be reported.



This can be an absolute or relative URL, or '.' to reference
this Trac instance. An empty value will disable the reporting
buttons.


</th>
<th>`.`</th></tr>
<tr><th>[descr](#project-descr-option)</th>
<th>
Short description of the project.


</th>
<th>`My example project`</th></tr>
<tr><th>[footer](#project-footer-option)</th>
<th>
Page footer text (right-aligned).


</th>
<th>`Visit the Trac open source project at<br /><a href="http://trac.edgewall.org/">http://trac.edgewall.org/</a>`</th></tr>
<tr><th>[icon](#project-icon-option)</th>
<th>
URL of the icon of the project.


</th>
<th>`common/trac.ico`</th></tr>
<tr><th>[name](#project-name-option)</th>
<th>
Name of the project.


</th>
<th>`My Project`</th></tr>
<tr><th>[url](#project-url-option)</th>
<th>
URL of the main project web site, usually the website in
which the `base_url` resides. This is used in notification
e-mails.


</th>
<th>(no default)</th></tr></table>

### `[pygments-lexer]`


Configure Pygments [ lexer](http://pygments.org/docs/lexers/) options.



For example, to set the
[
PhpLexer](http://pygments.org/docs/lexers/#lexers-for-php-and-related-languages) options
`startinline` and `funcnamehighlighting`:


```
[pygments-lexer]
php.startinline = True
php.funcnamehighlighting = True
```


The lexer name is derived from the class name, with `Lexer` stripped
from the end. The lexer *short names* can also be used in place
of the lexer name.


### `[query]`

<table><tr><th>[default\_anonymous\_query](#query-default_anonymous_query-option)</th>
<th>
The default query for anonymous users. The query is either
in [query language](trac-query#) syntax, or a URL query
string starting with `?` as used in `query:`
[Trac links](trac-query#).


</th>
<th>`status!=closed&cc~=$USER`</th></tr>
<tr><th>[default\_query](#query-default_query-option)</th>
<th>
The default query for authenticated users. The query is either
in [query language](trac-query#) syntax, or a URL query
string starting with `?` as used in `query:`
[Trac links](trac-query#).


</th>
<th>`status!=closed&owner=$USER`</th></tr>
<tr><th>[items\_per\_page](#query-items_per_page-option)</th>
<th>
Number of tickets displayed per page in ticket queries,
by default.


</th>
<th>`100`</th></tr>
<tr><th>[ticketlink\_query](#query-ticketlink_query-option)</th>
<th>
The base query to be used when linkifying values of ticket
fields. The query is a URL query
string starting with `?` as used in `query:`
[Trac links](trac-query#).
(*since 0.12*)


</th>
<th>`?status=!closed`</th></tr></table>

### `[report]`

<table><tr><th>[items\_per\_page](#report-items_per_page-option)</th>
<th>
Number of tickets displayed per page in ticket reports,
by default.


</th>
<th>`100`</th></tr>
<tr><th>[items\_per\_page\_rss](#report-items_per_page_rss-option)</th>
<th>
Number of tickets displayed in the rss feeds for reports.


</th>
<th>`0`</th></tr></table>

### `[repositories]`


One of the alternatives for registering new repositories is to
populate the `[repositories]` section of the `trac.ini`.



This is especially suited for setting up convenience aliases,
short-lived repositories, or during the initial phases of an
installation.



See [TracRepositoryAdmin](trac-repository-admin#) for details
about the format adopted for this section and the rest of that page for
the other alternatives.



(*since 0.12*)


### `[revisionlog]`

<table><tr><th>[default\_log\_limit](#revisionlog-default_log_limit-option)</th>
<th>
Default value for the limit argument in the [TracRevisionLog](trac-revision-log).


</th>
<th>`100`</th></tr>
<tr><th>[graph\_colors](#revisionlog-graph_colors-option)</th>
<th>
Comma-separated list of colors to use for the [TracRevisionLog](trac-revision-log)
graph display. (*since 1.0*)


</th>
<th>`#cc0,#0c0,#0cc,#00c,#c0c,#c00`</th></tr></table>

### `[roadmap]`

<table><tr><th>[stats\_provider](#roadmap-stats_provider-option)</th>
<th>
Name of the component implementing `ITicketGroupStatsProvider`,
which is used to collect statistics on groups of tickets for display
in the roadmap views.


</th>
<th>`DefaultTicketGroupStatsProvider`</th></tr></table>

### `[search]`

<table><tr><th>[default\_disabled\_filters](#search-default_disabled_filters-option)</th>
<th>
Specifies which search filters should be disabled by
default on the search page. This will also restrict the
filters for the quick search function. The filter names
defined by default components are: `wiki`, `ticket`,
`milestone` and `changeset`.  For plugins, look for
their implementation of the ISearchSource interface, in
the `get_search_filters()` method, the first member of
returned tuple. Once disabled, search filters can still
be manually enabled by the user on the search page.
(*since 0.12*)


</th>
<th>(no default)</th></tr>
<tr><th>[min\_query\_length](#search-min_query_length-option)</th>
<th>
Minimum length of query string allowed when performing a search.


</th>
<th>`3`</th></tr></table>

### `[spam-filter]`


This section is used to handle all configurations used by
spam filter plugin.


<table><tr><th>[account\_karma](#spam-filter-account_karma-option)</th>
<th>
By how many points a failed registration check impacts
the overall score.


</th>
<th>`0`</th></tr>
<tr><th>[account\_replace\_checks](#spam-filter-account_replace_checks-option)</th>
<th>
Replace checks in account manager totally.


</th>
<th>`disabled`</th></tr>
<tr><th>[akismet\_api\_key](#spam-filter-akismet_api_key-option)</th>
<th>
Wordpress key required to use the Akismet API.


</th>
<th>(no default)</th></tr>
<tr><th>[akismet\_api\_url](#spam-filter-akismet_api_url-option)</th>
<th>
URL of the Akismet service.


</th>
<th>`rest.akismet.com/1.1/`</th></tr>
<tr><th>[akismet\_karma](#spam-filter-akismet_karma-option)</th>
<th>
By how many points an Akismet reject impacts the overall karma of
a submission.


</th>
<th>`10`</th></tr>
<tr><th>[attachment\_karma](#spam-filter-attachment_karma-option)</th>
<th>
The karma given to attachments.


</th>
<th>`0`</th></tr>
<tr><th>[attachment\_sample\_size](#spam-filter-attachment_sample_size-option)</th>
<th>
The maximum number of bytes from an attachment to pass through
the spam filters.


</th>
<th>`16384`</th></tr>
<tr><th>[authenticated\_karma](#spam-filter-authenticated_karma-option)</th>
<th>
The karma given to authenticated users, in case
`trust_authenticated` is false.


</th>
<th>`20`</th></tr>
<tr><th>[badcontent\_file](#spam-filter-badcontent_file-option)</th>
<th>
Local file to be loaded to get [BadContent](bad-content). Can be used in
addition to [BadContent](bad-content) wiki page.


</th>
<th>(no default)</th></tr>
<tr><th>[bayes\_karma](#spam-filter-bayes_karma-option)</th>
<th>
By what factor Bayesian spam probability score affects the overall
karma of a submission.


</th>
<th>`15`</th></tr>
<tr><th>[bayes\_min\_training](#spam-filter-bayes_min_training-option)</th>
<th>
The minimum number of submissions in the training database required
for the filter to start impacting the karma of submissions.


</th>
<th>`25`</th></tr>
<tr><th>[blogspam\_json\_api\_url](#spam-filter-blogspam_json_api_url-option)</th>
<th>
URL of the BlogSpam service.


</th>
<th>`test.blogspam.net:9999`</th></tr>
<tr><th>[blogspam\_json\_skip\_tests](#spam-filter-blogspam_json_skip_tests-option)</th>
<th>
Comma separated list of tests to skip.


</th>
<th>`45-wordcount.js,60-drone.js,80-sfs.js`</th></tr>
<tr><th>[blogspam\_karma](#spam-filter-blogspam_karma-option)</th>
<th>
By how many points an BlogSpam reject impacts the overall karma of
a submission.


</th>
<th>`5`</th></tr>
<tr><th>[botscout\_api\_key](#spam-filter-botscout_api_key-option)</th>
<th>
API key required to use BotScout.


</th>
<th>(no default)</th></tr>
<tr><th>[botscout\_karma](#spam-filter-botscout_karma-option)</th>
<th>
By how many points a BotScout reject impacts the overall karma of
a submission.


</th>
<th>`3`</th></tr>
<tr><th>[captcha](#spam-filter-captcha-option)</th>
<th>
CAPTCHA method to use for verifying humans.


</th>
<th>`ExpressionCaptcha`</th></tr>
<tr><th>[captcha\_areyouahuman\_host](#spam-filter-captcha_areyouahuman_host-option)</th>
<th>
Host name for AreYouAHuman usage.


</th>
<th>`ws.areyouahuman.com`</th></tr>
<tr><th>[captcha\_areyouahuman\_publisher\_key](#spam-filter-captcha_areyouahuman_publisher_key-option)</th>
<th>
Publisher key for AreYouAHuman usage.


</th>
<th>(no default)</th></tr>
<tr><th>[captcha\_areyouahuman\_scoring\_key](#spam-filter-captcha_areyouahuman_scoring_key-option)</th>
<th>
Scoring key for AreYouAHuman usage.


</th>
<th>(no default)</th></tr>
<tr><th>[captcha\_expression\_ceiling](#spam-filter-captcha_expression_ceiling-option)</th>
<th>
Maximum value of individual terms in numeric CAPTCHA
expression.


</th>
<th>`10`</th></tr>
<tr><th>[captcha\_expression\_terms](#spam-filter-captcha_expression_terms-option)</th>
<th>
Number of terms in numeric CAPTCHA expression.


</th>
<th>`3`</th></tr>
<tr><th>[captcha\_failed\_karma](#spam-filter-captcha_failed_karma-option)</th>
<th>
By how many points a failed CAPTCHA impacts the overall score.


</th>
<th>`1`</th></tr>
<tr><th>[captcha\_image\_alphabet](#spam-filter-captcha_image_alphabet-option)</th>
<th>
Alphabet to choose image CAPTCHA challenge from.


</th>
<th>`abcdefghkmnopqrstuvwxyz`</th></tr>
<tr><th>[captcha\_image\_font\_size](#spam-filter-captcha_image_font_size-option)</th>
<th>
Font size to use in image CAPTCHA.


</th>
<th>`25`</th></tr>
<tr><th>[captcha\_image\_fonts](#spam-filter-captcha_image_fonts-option)</th>
<th>
Set of fonts to choose from when generating image CAPTCHA.


</th>
<th>`vera.ttf`</th></tr>
<tr><th>[captcha\_image\_letters](#spam-filter-captcha_image_letters-option)</th>
<th>
Number of letters to use in image CAPTCHA challenge.


</th>
<th>`6`</th></tr>
<tr><th>[captcha\_karma](#spam-filter-captcha_karma-option)</th>
<th>
By how many points a successful CAPTCHA response increases the
overall score.


</th>
<th>`20`</th></tr>
<tr><th>[captcha\_karma\_lifetime](#spam-filter-captcha_karma_lifetime-option)</th>
<th>
Time in seconds that a successful CAPTCHA response increases
karma.


</th>
<th>`86400`</th></tr>
<tr><th>[captcha\_keycaptcha\_private\_key](#spam-filter-captcha_keycaptcha_private_key-option)</th>
<th>
Private key for KeyCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th>[captcha\_keycaptcha\_user\_id](#spam-filter-captcha_keycaptcha_user_id-option)</th>
<th>
User id for KeyCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th>[captcha\_lifetime](#spam-filter-captcha_lifetime-option)</th>
<th>
Time in seconds before database cleanup is called.


</th>
<th>`3600`</th></tr>
<tr><th>[captcha\_recaptcha\_private\_key](#spam-filter-captcha_recaptcha_private_key-option)</th>
<th>
Private key for reCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th>[captcha\_recaptcha\_public\_key](#spam-filter-captcha_recaptcha_public_key-option)</th>
<th>
Public key for reCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th>[extlinks\_allowed\_domains](#spam-filter-extlinks_allowed_domains-option)</th>
<th>
List of domains that should be allowed in external links


</th>
<th>`example.com,example.org`</th></tr>
<tr><th>[extlinks\_karma](#spam-filter-extlinks_karma-option)</th>
<th>
By how many points too many external links in a submission impact
the overall score.


</th>
<th>`2`</th></tr>
<tr><th>[fspamlist\_api\_key](#spam-filter-fspamlist_api_key-option)</th>
<th>
API key required to use FSpamList.


</th>
<th>(no default)</th></tr>
<tr><th>[fspamlist\_karma](#spam-filter-fspamlist_karma-option)</th>
<th>
By how many points a FSpamList reject impacts the overall karma of
a submission.


</th>
<th>`3`</th></tr>
<tr><th>[httpbl\_api\_key](#spam-filter-httpbl_api_key-option)</th>
<th>
Http:BL API key required for use.


</th>
<th>(no default)</th></tr>
<tr><th>[httpbl\_spammer\_karma](#spam-filter-httpbl_spammer_karma-option)</th>
<th>
By how many points listing as "comment spammer" impacts the
overall karma of a submission.


</th>
<th>`6`</th></tr>
<tr><th>[ip6\_blacklist\_servers](#spam-filter-ip6_blacklist_servers-option)</th>
<th>
Servers used for IPv6 blacklisting.


</th>
<th>`all.s5h.net,dnsbl.dronebl.org,bl.ipv6.spameatingmonkey.net`</th></tr>
<tr><th>[ip\_blacklist\_karma](#spam-filter-ip_blacklist_karma-option)</th>
<th>
By how many points blacklisting by a single server impacts the
overall karma of a submission.


</th>
<th>`5`</th></tr>
<tr><th>[ip\_blacklist\_servers](#spam-filter-ip_blacklist_servers-option)</th>
<th>
Servers used for IPv4 blacklisting.


</th>
<th>`list.blogspambl.com,all.s5h.net,dnsbl.tornevall.org,dnsbl.dronebl.org`</th></tr>
<tr><th>[ip\_throttle\_karma](#spam-filter-ip_throttle_karma-option)</th>
<th>
By how many points exceeding the configured maximum number of posts
per hour impacts the overall score.


</th>
<th>`3`</th></tr>
<tr><th>[ipbadcontent\_file](#spam-filter-ipbadcontent_file-option)</th>
<th>
Local file to be loaded to get BadIP. Can be used in
addition to BadIP wiki page.


</th>
<th>(no default)</th></tr>
<tr><th>[ipregex\_karma](#spam-filter-ipregex_karma-option)</th>
<th>
By how many points a match with a pattern on the BadIP page
impacts the overall karma of a submission.


</th>
<th>`20`</th></tr>
<tr><th>[is\_forwarded](#spam-filter-is_forwarded-option)</th>
<th>
Interpret X-Forwarded-For header for IP checks.


</th>
<th>`disabled`</th></tr>
<tr><th>[logging\_enabled](#spam-filter-logging_enabled-option)</th>
<th>
Whether all content submissions and spam filtering activity should
be logged to the database.


</th>
<th>`enabled`</th></tr>
<tr><th>[max\_external\_links](#spam-filter-max_external_links-option)</th>
<th>
The maximum number of external links allowed in a submission until
that submission gets negative karma.


</th>
<th>`4`</th></tr>
<tr><th>[max\_posts\_by\_ip](#spam-filter-max_posts_by_ip-option)</th>
<th>
The maximum allowed number of submissions per hour from a single IP
address. If this limit is exceeded, subsequent submissions get negative
karma.


</th>
<th>`10`</th></tr>
<tr><th>[min\_karma](#spam-filter-min_karma-option)</th>
<th>
The minimum score required for a submission to be allowed.


</th>
<th>`0`</th></tr>
<tr><th>[purge\_age](#spam-filter-purge_age-option)</th>
<th>
The number of days after which log entries should be purged.


</th>
<th>`7`</th></tr>
<tr><th>[regex\_karma](#spam-filter-regex_karma-option)</th>
<th>
By how many points a match with a pattern on the [BadContent](bad-content) page
impacts the overall karma of a submission.


</th>
<th>`5`</th></tr>
<tr><th>[register\_karma](#spam-filter-register_karma-option)</th>
<th>
The karma given to registrations.


</th>
<th>`0`</th></tr>
<tr><th>[reject\_handler](#spam-filter-reject_handler-option)</th>
<th>
The handler used to reject content.


</th>
<th>`FilterSystem`</th></tr>
<tr><th>[report\_pages](#spam-filter-report_pages-option)</th>
<th>
List of page types to add spam report link


</th>
<th>`wiki,attachment,ticket`</th></tr>
<tr><th>[session\_karma](#spam-filter-session_karma-option)</th>
<th>
By how many points an existing and configured session improves the
overall karma of the submission. A third of the points is granted for
having an existing session at all, the other two thirds are granted
when the user has his name and/or email address set in the session,
respectively.


</th>
<th>`9`</th></tr>
<tr><th>[show\_blacklisted](#spam-filter-show_blacklisted-option)</th>
<th>
Show the matched bad content patterns in rejection message.


</th>
<th>`enabled`</th></tr>
<tr><th>[show\_blacklisted\_ip](#spam-filter-show_blacklisted_ip-option)</th>
<th>
Show the matched bad IP patterns in rejection message.


</th>
<th>`enabled`</th></tr>
<tr><th>[show\_train\_only](#spam-filter-show_train_only-option)</th>
<th>
Show the buttons for training without deleting entry.


</th>
<th>`disabled`</th></tr>
<tr><th>[skip\_external](#spam-filter-skip_external-option)</th>
<th>
Skip external calls when this negative karma is already reached
by internal tests.


</th>
<th>`20`</th></tr>
<tr><th>[skip\_externalham](#spam-filter-skip_externalham-option)</th>
<th>
Skip external calls when this positive karma is already reached
by internal tests.


</th>
<th>`30`</th></tr>
<tr><th>[spam\_monitor\_entries](#spam-filter-spam_monitor_entries-option)</th>
<th>
How many monitor entries are displayed by default (between 5 and 10000).


</th>
<th>`100`</th></tr>
<tr><th>[spam\_report\_entries](#spam-filter-spam_report_entries-option)</th>
<th>
How many report entries are displayed by default (between 5 and 10000).


</th>
<th>`100`</th></tr>
<tr><th>[spam\_user\_defaultmode](#spam-filter-spam_user_defaultmode-option)</th>
<th>
Default mode for spam user admin panel.


</th>
<th>`overview`</th></tr>
<tr><th>[spam\_user\_maxage](#spam-filter-spam_user_maxage-option)</th>
<th>
How many days no login are considered for dead accounts.


</th>
<th>`200`</th></tr>
<tr><th>[spam\_user\_minwiki](#spam-filter-spam_user_minwiki-option)</th>
<th>
How many wiki edits are still an unused account.


</th>
<th>`0`</th></tr>
<tr><th>[stop\_external](#spam-filter-stop_external-option)</th>
<th>
Stop external calls when this negative karma is reached.


</th>
<th>`50`</th></tr>
<tr><th>[stop\_externalham](#spam-filter-stop_externalham-option)</th>
<th>
Stop external calls when this positive karma is reached.


</th>
<th>`50`</th></tr>
<tr><th>[stopforumspam\_api\_key](#spam-filter-stopforumspam_api_key-option)</th>
<th>
API key used to report SPAM.


</th>
<th>(no default)</th></tr>
<tr><th>[stopforumspam\_karma](#spam-filter-stopforumspam_karma-option)</th>
<th>
By how many points a StopForumSpam reject impacts the overall karma of
a submission.


</th>
<th>`4`</th></tr>
<tr><th>[train\_external](#spam-filter-train_external-option)</th>
<th>
Allow training of external services.


</th>
<th>`enabled`</th></tr>
<tr><th>[trap\_karma](#spam-filter-trap_karma-option)</th>
<th>
By how many points a trap reject impacts the overall karma of
a submission.


</th>
<th>`10`</th></tr>
<tr><th>[trap\_name](#spam-filter-trap_name-option)</th>
<th>
Name of the invisible trap field, should contain some reference
to e-mail for better results.


</th>
<th>`sfp_email`</th></tr>
<tr><th>[trap\_name\_hidden](#spam-filter-trap_name_hidden-option)</th>
<th>
Name of the hidden trap field, should contain some reference
to e-mail for better results.


</th>
<th>`sfph_mail`</th></tr>
<tr><th>[trap\_name\_register](#spam-filter-trap_name_register-option)</th>
<th>
Name of the register trap field, should contain some reference
to web/homepage for better results.


</th>
<th>`spf_homepage`</th></tr>
<tr><th>[trust\_authenticated](#spam-filter-trust_authenticated-option)</th>
<th>
Whether content submissions by authenticated users should be trusted
without checking for potential spam or other abuse.


</th>
<th>`disabled`</th></tr>
<tr><th>[url\_blacklist\_karma](#spam-filter-url_blacklist_karma-option)</th>
<th>
By how many points blacklisting by a single bad URL impacts the
overall karma of a submission.


</th>
<th>`3`</th></tr>
<tr><th>[url\_blacklist\_servers](#spam-filter-url_blacklist_servers-option)</th>
<th>
Servers used for URL blacklisting.


</th>
<th>`urired.spameatingmonkey.net,multi.surbl.org,dbl.spamhaus.org`</th></tr>
<tr><th>[use\_external](#spam-filter-use_external-option)</th>
<th>
Allow usage of external services.


</th>
<th>`enabled`</th></tr></table>

### `[sqlite]`

<table><tr><th>[extensions](#sqlite-extensions-option)</th>
<th>
Paths to [ sqlite extensions](https://sqlite.org/loadext.html).
The paths may be absolute or relative to the Trac environment.
(*since 0.12*)


</th>
<th>(no default)</th></tr></table>

### `[svn]`

<table><tr><th>[authz\_file](#svn-authz_file-option)</th>
<th>
The path to the Subversion
[
authorization (authz) file](http://svnbook.red-bean.com/en/1.7/svn.serverconfig.pathbasedauthz.html).
To enable authz permission checking, the `AuthzSourcePolicy` permission
policy must be added to `[trac] permission_policies`. Non-absolute
paths are relative to the Environment `conf` directory.


</th>
<th>(no default)</th></tr>
<tr><th>[authz\_module\_name](#svn-authz_module_name-option)</th>
<th>
The module prefix used in the `authz_file` for the default
repository. If left empty, the global section is used.


</th>
<th>(no default)</th></tr></table>

### `[tags]`

<table><tr><th>[listtagged\_items\_per\_page](#tags-listtagged_items_per_page-option)</th>
<th>
Number of tagged resources displayed per page of tag query results requested by `ListTagged` macros and from `/tags`.


</th>
<th>`100`</th></tr></table>

### `[ticket]`

<table><tr><th>[allowed\_empty\_fields](#ticket-allowed_empty_fields-option)</th>
<th>
Comma-separated list of `select` fields that can have
an empty value. (*since 1.1.2*)


</th>
<th>`milestone,version`</th></tr>
<tr><th>[commit\_ticket\_update\_check\_perms](#ticket-commit_ticket_update_check_perms-option)</th>
<th>
Check that the committer has permission to perform the requested
operations on the referenced tickets.



This requires that the user names be the same for Trac and repository
operations.


</th>
<th>`enabled`</th></tr>
<tr><th>[commit\_ticket\_update\_commands.close](#ticket-commit_ticket_update_commands.close-option)</th>
<th>
Commands that close tickets, as a space-separated list.


</th>
<th>`close closed closes fix fixed fixes`</th></tr>
<tr><th>[commit\_ticket\_update\_commands.refs](#ticket-commit_ticket_update_commands.refs-option)</th>
<th>
Commands that add a reference, as a space-separated list.



If set to the special value `<ALL>`, all tickets referenced by the
message will get a reference to the changeset.


</th>
<th>`addresses re references refs see`</th></tr>
<tr><th>[commit\_ticket\_update\_envelope](#ticket-commit_ticket_update_envelope-option)</th>
<th>
Require commands to be enclosed in an envelope.



Must be empty or contain two characters. For example, if set to `[]`,
then commands must be in the form of `[closes #4]`.


</th>
<th>(no default)</th></tr>
<tr><th>[commit\_ticket\_update\_notify](#ticket-commit_ticket_update_notify-option)</th>
<th>
Send ticket change notification when updating a ticket.


</th>
<th>`enabled`</th></tr>
<tr><th>[default\_cc](#ticket-default_cc-option)</th>
<th>
Default cc: list for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_component](#ticket-default_component-option)</th>
<th>
Default component for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_description](#ticket-default_description-option)</th>
<th>
Default description for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_keywords](#ticket-default_keywords-option)</th>
<th>
Default keywords for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_milestone](#ticket-default_milestone-option)</th>
<th>
Default milestone for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_owner](#ticket-default_owner-option)</th>
<th>
Default owner for newly created tickets.


</th>
<th>`< default >`</th></tr>
<tr><th>[default\_priority](#ticket-default_priority-option)</th>
<th>
Default priority for newly created tickets.


</th>
<th>`major`</th></tr>
<tr><th>[default\_resolution](#ticket-default_resolution-option)</th>
<th>
Default resolution for resolving (closing) tickets.


</th>
<th>`fixed`</th></tr>
<tr><th>[default\_severity](#ticket-default_severity-option)</th>
<th>
Default severity for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_summary](#ticket-default_summary-option)</th>
<th>
Default summary (title) for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[default\_type](#ticket-default_type-option)</th>
<th>
Default type for newly created tickets.


</th>
<th>`defect`</th></tr>
<tr><th>[default\_version](#ticket-default_version-option)</th>
<th>
Default version for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th>[max\_comment\_size](#ticket-max_comment_size-option)</th>
<th>
Maximum allowed comment size in characters.


</th>
<th>`262144`</th></tr>
<tr><th>[max\_description\_size](#ticket-max_description_size-option)</th>
<th>
Maximum allowed description size in characters.


</th>
<th>`262144`</th></tr>
<tr><th>[max\_summary\_size](#ticket-max_summary_size-option)</th>
<th>
Maximum allowed summary size in characters. (*since 1.0.2*)


</th>
<th>`262144`</th></tr>
<tr><th>[preserve\_newlines](#ticket-preserve_newlines-option)</th>
<th>
Whether Wiki formatter should respect the new lines present
in the Wiki text.
If set to 'default', this is equivalent to 'yes' for new environments
but keeps the old behavior for upgraded environments (i.e. 'no').


</th>
<th>`default`</th></tr>
<tr><th>[restrict\_owner](#ticket-restrict_owner-option)</th>
<th>
Make the owner field of tickets use a drop-down menu.
Be sure to understand the performance implications before activating
this option. See
[Assign-to as Drop-Down List](trac-tickets#).



Please note that e-mail addresses are **not** obfuscated in the
resulting drop-down menu, so this option should not be used if
e-mail addresses must remain protected.


</th>
<th>`disabled`</th></tr>
<tr><th>[workflow](#ticket-workflow-option)</th>
<th>
Ordered list of workflow controllers to use for ticket actions.


</th>
<th>`ConfigurableTicketWorkflow`</th></tr></table>

### `[ticket-custom]`


In this section, you can define additional fields for tickets. See
[TracTicketsCustomFields](trac-tickets-custom-fields) for more details.


### `[ticket-workflow]`


The workflow for tickets is controlled by plugins. By default,
there's only a `ConfigurableTicketWorkflow` component in charge.
That component allows the workflow to be configured via this section
in the `trac.ini` file. See [TracWorkflow](trac-workflow) for more details.


### `[timeline]`

<table><tr><th>[abbreviated\_messages](#timeline-abbreviated_messages-option)</th>
<th>
Whether wiki-formatted event messages should be truncated or not.



This only affects the default rendering, and can be overriden by
specific event providers, see their own documentation.


</th>
<th>`enabled`</th></tr>
<tr><th>[changeset\_collapse\_events](#timeline-changeset_collapse_events-option)</th>
<th>
Whether consecutive changesets from the same author having
exactly the same message should be presented as one event.
That event will link to the range of changesets in the log view.


</th>
<th>`disabled`</th></tr>
<tr><th>[changeset\_long\_messages](#timeline-changeset_long_messages-option)</th>
<th>
Whether wiki-formatted changeset messages should be multiline or
not.



If this option is not specified or is false and `wiki_format_messages`
is set to true, changeset messages will be single line only, losing
some formatting (bullet points, etc).


</th>
<th>`disabled`</th></tr>
<tr><th>[changeset\_show\_files](#timeline-changeset_show_files-option)</th>
<th>
Number of files to show (`-1` for unlimited, `0` to disable).



This can also be `location`, for showing the common prefix for the
changed files.


</th>
<th>`0`</th></tr>
<tr><th>[default\_daysback](#timeline-default_daysback-option)</th>
<th>
Default number of days displayed in the Timeline, in days.


</th>
<th>`30`</th></tr>
<tr><th>[max\_daysback](#timeline-max_daysback-option)</th>
<th>
Maximum number of days (-1 for unlimited) displayable in the
Timeline.


</th>
<th>`90`</th></tr>
<tr><th>[newticket\_formatter](#timeline-newticket_formatter-option)</th>
<th>
Which formatter flavor (e.g. 'html' or 'oneliner') should be
used when presenting the description for new tickets.
If 'oneliner', the \[timeline\] abbreviated\_messages option applies.


</th>
<th>`oneliner`</th></tr>
<tr><th>[ticket\_show\_component](#timeline-ticket_show_component-option)</th>
<th>
Enable the display of component of tickets in the timeline.
(*since 1.1.1*)


</th>
<th>`disabled`</th></tr>
<tr><th>[ticket\_show\_details](#timeline-ticket_show_details-option)</th>
<th>
Enable the display of all ticket changes in the timeline, not only
open / close operations.


</th>
<th>`disabled`</th></tr></table>

### `[trac]`

<table><tr><th>[auth\_cookie\_lifetime](#trac-auth_cookie_lifetime-option)</th>
<th>
Lifetime of the authentication cookie, in seconds.



This value determines how long the browser will cache
authentication information, and therefore, after how much
inactivity a user will have to log in again. The default value
of 0 makes the cookie expire at the end of the browsing
session. (*since 0.12*)


</th>
<th>`0`</th></tr>
<tr><th>[auth\_cookie\_path](#trac-auth_cookie_path-option)</th>
<th>
Path for the authentication cookie. Set this to the common
base path of several Trac instances if you want them to share
the cookie.  (*since 0.12*)


</th>
<th>(no default)</th></tr>
<tr><th>[auto\_preview\_timeout](#trac-auto_preview_timeout-option)</th>
<th>
Inactivity timeout in seconds after which the automatic wiki preview
triggers an update. This option can contain floating-point values. The
lower the setting, the more requests will be made to the server. Set
this to 0 to disable automatic preview. (*since 0.12*)


</th>
<th>`2.0`</th></tr>
<tr><th>[auto\_reload](#trac-auto_reload-option)</th>
<th>
Automatically reload template files after modification.


</th>
<th>`disabled`</th></tr>
<tr><th>[backup\_dir](#trac-backup_dir-option)</th>
<th>
Database backup location


</th>
<th>`db`</th></tr>
<tr><th>[base\_url](#trac-base_url-option)</th>
<th>
Reference URL for the Trac deployment.



This is the base URL that will be used when producing
documents that will be used outside of the web browsing
context, like for example when inserting URLs pointing to Trac
resources in notification e-mails.


</th>
<th>(no default)</th></tr>
<tr><th>[check\_auth\_ip](#trac-check_auth_ip-option)</th>
<th>
Whether the IP address of the user should be checked for
authentication (*since 0.9*).


</th>
<th>`disabled`</th></tr>
<tr><th>[database](#trac-database-option)</th>
<th>
Database connection
[string](trac-environment#) for this
project


</th>
<th>`sqlite:db/trac.db`</th></tr>
<tr><th>[debug\_sql](#trac-debug_sql-option)</th>
<th>
Show the SQL queries in the Trac log, at DEBUG level.


</th>
<th>`disabled`</th></tr>
<tr><th>[default\_charset](#trac-default_charset-option)</th>
<th>
Charset to be used when in doubt.


</th>
<th>`utf-8`</th></tr>
<tr><th>[default\_date\_format](#trac-default_date_format-option)</th>
<th>
The date format. Valid options are 'iso8601' for selecting
ISO 8601 format, or leave it empty which means the default
date format will be inferred from the browser's default
language. (*since 1.0*)


</th>
<th>(no default)</th></tr>
<tr><th>[default\_dateinfo\_format](#trac-default_dateinfo_format-option)</th>
<th>
The date information format. Valid options are 'relative' for
displaying relative format and 'absolute' for displaying absolute
format. (*since 1.0*)


</th>
<th>`relative`</th></tr>
<tr><th>[default\_handler](#trac-default_handler-option)</th>
<th>
Name of the component that handles requests to the base
URL.



Options include `TimelineModule`, `RoadmapModule`,
`BrowserModule`, `QueryModule`, `ReportModule`, `TicketModule`
and `WikiModule`.


</th>
<th>`WikiModule`</th></tr>
<tr><th>[default\_language](#trac-default_language-option)</th>
<th>
The preferred language to use if no user preference has
been set. (*since 0.12.1*)


</th>
<th>(no default)</th></tr>
<tr><th>[default\_timezone](#trac-default_timezone-option)</th>
<th>
The default timezone to use


</th>
<th>(no default)</th></tr>
<tr><th>[genshi\_cache\_size](#trac-genshi_cache_size-option)</th>
<th>
The maximum number of templates that the template loader will cache
in memory. You may want to choose a higher value if your site uses a
larger number of templates, and you have enough memory to spare, or
you can reduce it if you are short on memory.


</th>
<th>`128`</th></tr>
<tr><th>[htdocs\_location](#trac-htdocs_location-option)</th>
<th>
Base URL for serving the core static resources below
`/chrome/common/`.



It can be left empty, and Trac will simply serve those resources
itself.



Advanced users can use this together with
[trac-admin ... deploy \<deploydir\>](trac-admin) to allow serving the
static resources for Trac directly from the web server.
Note however that this only applies to the `<deploydir>/htdocs/common`
directory, the other deployed resources (i.e. those from plugins)
will not be made available this way and additional rewrite
rules will be needed in the web server.


</th>
<th>(no default)</th></tr>
<tr><th>[ignore\_auth\_case](#trac-ignore_auth_case-option)</th>
<th>
Whether login names should be converted to lower case
(*since 0.9*).


</th>
<th>`disabled`</th></tr>
<tr><th>[jquery\_location](#trac-jquery_location-option)</th>
<th>
Location of the jQuery JavaScript library (version 1.11.3).



An empty value loads jQuery from the copy bundled with Trac.



Alternatively, jQuery could be loaded from a CDN, for example:
[
http://code.jquery.com/jquery-1.11.3.min.js](http://code.jquery.com/jquery-1.11.3.min.js),
[
http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js](http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js) or
[
https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js](https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js).



(*since 1.0*)


</th>
<th>(no default)</th></tr>
<tr><th>[jquery\_ui\_location](#trac-jquery_ui_location-option)</th>
<th>
Location of the jQuery UI JavaScript library (version 1.11.4).



An empty value loads jQuery UI from the copy bundled with Trac.



Alternatively, jQuery UI could be loaded from a CDN, for example:
[
https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js](https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js)
or
[
http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/jquery-ui.min.js](http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/jquery-ui.min.js).



(*since 1.0*)


</th>
<th>(no default)</th></tr>
<tr><th>[jquery\_ui\_theme\_location](#trac-jquery_ui_theme_location-option)</th>
<th>
Location of the theme to be used with the jQuery UI JavaScript
library (version 1.11.4).



An empty value loads the custom Trac jQuery UI theme from the copy
bundled with Trac.



Alternatively, a jQuery UI theme could be loaded from a CDN, for
example:
[
https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/start/jquery-ui.css](https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/start/jquery-ui.css)
or
[
http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/themes/start/jquery-ui.css](http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/themes/start/jquery-ui.css).



(*since 1.0*)


</th>
<th>(no default)</th></tr>
<tr><th>[never\_obfuscate\_mailto](#trac-never_obfuscate_mailto-option)</th>
<th>
Never obfuscate `mailto:` links explicitly written in the wiki,
even if `show_email_addresses` is false or the user doesn't have
EMAIL\_VIEW permission.


</th>
<th>`disabled`</th></tr>
<tr><th>[permission\_policies](#trac-permission_policies-option)</th>
<th>
List of components implementing `IPermissionPolicy`, in the order
in which they will be applied. These components manage fine-grained
access control to Trac resources.


</th>
<th>`ReadonlyWikiPolicy,DefaultPermissionPolicy,LegacyAttachmentPolicy`</th></tr>
<tr><th>[permission\_store](#trac-permission_store-option)</th>
<th>
Name of the component implementing `IPermissionStore`, which is used
for managing user and group permissions.


</th>
<th>`DefaultPermissionStore`</th></tr>
<tr><th>[pg\_dump\_path](#trac-pg_dump_path-option)</th>
<th>
Location of pg\_dump for Postgres database backups


</th>
<th>`pg_dump`</th></tr>
<tr><th>[request\_filters](#trac-request_filters-option)</th>
<th>
Ordered list of filters to apply to all requests.


</th>
<th>(no default)</th></tr>
<tr><th>[resizable\_textareas](#trac-resizable_textareas-option)</th>
<th>
Make `<textarea>` fields resizable. Requires JavaScript.
(*since 0.12*)


</th>
<th>`enabled`</th></tr>
<tr><th>[secure\_cookies](#trac-secure_cookies-option)</th>
<th>
Restrict cookies to HTTPS connections.



When true, set the `secure` flag on all cookies so that they
are only sent to the server on HTTPS connections. Use this if
your Trac instance is only accessible through HTTPS.


</th>
<th>`disabled`</th></tr>
<tr><th>[show\_email\_addresses](#trac-show_email_addresses-option)</th>
<th>
Show email addresses instead of usernames. If false, email
addresses are obfuscated for users that don't have EMAIL\_VIEW
permission.


</th>
<th>`disabled`</th></tr>
<tr><th>[show\_full\_names](#trac-show_full_names-option)</th>
<th>
Show full names instead of usernames. (*since 1.2*)


</th>
<th>`enabled`</th></tr>
<tr><th>[show\_ip\_addresses](#trac-show_ip_addresses-option)</th>
<th>
Show IP addresses for resource edits (e.g. wiki). Since 1.0.5 this
option is deprecated and will be removed in 1.3.1.


</th>
<th>`disabled`</th></tr>
<tr><th>[timeout](#trac-timeout-option)</th>
<th>
Timeout value for database connection, in seconds.
Use '0' to specify *no timeout*.


</th>
<th>`20`</th></tr>
<tr><th>[use\_base\_url\_for\_redirect](#trac-use_base_url_for_redirect-option)</th>
<th>
Optionally use `[trac] base_url` for redirects.



In some configurations, usually involving running Trac behind
a HTTP proxy, Trac can't automatically reconstruct the URL
that is used to access it. You may need to use this option to
force Trac to use the `base_url` setting also for
redirects. This introduces the obvious limitation that this
environment will only be usable when accessible from that URL,
as redirects are frequently used.


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_chunked\_encoding](#trac-use_chunked_encoding-option)</th>
<th>
If enabled, send contents as chunked encoding in HTTP/1.1.
Otherwise, send contents with `Content-Length` header after entire of
the contents are rendered. (*since 1.0.6*)


</th>
<th>`disabled`</th></tr>
<tr><th>[use\_xsendfile](#trac-use_xsendfile-option)</th>
<th>
When true, send a `X-Sendfile` header and no content when sending
files from the filesystem, so that the web server handles the content.
This requires a web server that knows how to handle such a header,
like Apache with `mod_xsendfile` or lighttpd. (*since 1.0*)


</th>
<th>`disabled`</th></tr>
<tr><th>[wiki\_toolbars](#trac-wiki_toolbars-option)</th>
<th>
Add a simple toolbar on top of Wiki \<textarea\>s.
(*since 1.0.2*)


</th>
<th>`enabled`</th></tr>
<tr><th>[xsendfile\_header](#trac-xsendfile_header-option)</th>
<th>
The header to use if `use_xsendfile` is enabled. If Nginx is used,
set `X-Accel-Redirect`. (*since 1.0.6*)


</th>
<th>`X-Sendfile`</th></tr></table>

### `[versioncontrol]`

<table><tr><th>[allowed\_repository\_dir\_prefixes](#versioncontrol-allowed_repository_dir_prefixes-option)</th>
<th>
Comma-separated list of allowed prefixes for repository
directories when adding and editing repositories in the repository
admin panel. If the list is empty, all repository directories are
allowed. (*since 0.12.1*)


</th>
<th>(no default)</th></tr>
<tr><th>[default\_repository\_type](#versioncontrol-default_repository_type-option)</th>
<th>
Default repository connector type.



This is used as the default repository type for repositories defined
in the [TracIni\#repositories-section repositories](trac-ini#) section or using
the "Repositories" admin panel. (*since 0.12*)


</th>
<th>`svn`</th></tr></table>

### `[wiki]`

<table><tr><th>[default\_edit\_area\_height](#wiki-default_edit_area_height-option)</th>
<th>
Default height of the textarea on the wiki edit page.
(*Since 1.1.5*)


</th>
<th>`20`</th></tr>
<tr><th>[ignore\_missing\_pages](#wiki-ignore_missing_pages-option)</th>
<th>
Enable/disable highlighting [CamelCase](camel-case) links to missing pages.


</th>
<th>`disabled`</th></tr>
<tr><th>[max\_size](#wiki-max_size-option)</th>
<th>
Maximum allowed wiki page size in characters.


</th>
<th>`262144`</th></tr>
<tr><th>[render\_unsafe\_content](#wiki-render_unsafe_content-option)</th>
<th>
Enable/disable the use of unsafe HTML tags such as `<script>` or
`<embed>` with the HTML [WikiProcessor](wiki-processors).



For public sites where anonymous users can edit the wiki it is
recommended to leave this option disabled.


</th>
<th>`disabled`</th></tr>
<tr><th>[safe\_origins](#wiki-safe_origins-option)</th>
<th>
List of URIs considered "safe cross-origin", that will be
rendered as `img` element without `crossorigin="anonymous"` attribute
or used in `url()` of inline style attribute even if
`[wiki] render_unsafe_content` is `false` (*since 1.0.15*).



To make any origins safe, specify "\*" in the list.


</th>
<th>`data:`</th></tr>
<tr><th>[safe\_schemes](#wiki-safe_schemes-option)</th>
<th>
List of URI schemes considered "safe", that will be rendered as
external links even if `[wiki] render_unsafe_content` is `false`.


</th>
<th>`cvs,file,ftp,git,irc,http,https,news,sftp,smb,ssh,svn,svn+ssh`</th></tr>
<tr><th>[split\_page\_names](#wiki-split_page_names-option)</th>
<th>
Enable/disable splitting the [WikiPageNames](wiki-page-names) with space characters.


</th>
<th>`disabled`</th></tr></table>

### `[wikiextras]`

<table><tr><th>[done\_phrases](#wikiextras-done_phrases-option)</th>
<th>
Analogous to `FIXME`-phrases, but presentation is less eye-catching.


</th>
<th>`DONE,DEBUGGED,FIXED,REVIEWED`</th></tr>
<tr><th>[fixme\_phrases](#wikiextras-fixme_phrases-option)</th>
<th>
A list of attentional phrases or single words, separated by comma
(`,`) that will be highlighted to catch attention. Any delimiter
`():<>` adjacent to a phrase will not be presented. (i.e. do not
include any of these delimiters in this list). This makes it possible
to naturally write, for example, `FIXME:` in a wiki text, but view the
phrase highlighted without the colon (`:`) which would not look
natural. Use the `ShowPhrases` macro to show a list of currently
defined phrases.


</th>
<th>`BUG,FIXME`</th></tr>
<tr><th>[icon\_limit](#wikiextras-icon_limit-option)</th>
<th>
To prevent exhaustive network traffic, limit the maximum number of
icons generated by the macro `Icon`. Set to 0 for unlimited number of
icons (this will produce exhaustive network traffic--you have been
warned!)


</th>
<th>`32`</th></tr>
<tr><th>[rbox\_width](#wikiextras-rbox_width-option)</th>
<th>
Width of right aligned boxes.


</th>
<th>`300`</th></tr>
<tr><th>[shadowless\_boxes](#wikiextras-shadowless_boxes-option)</th>
<th>
Use shadowless boxes.


</th>
<th>`disabled`</th></tr>
<tr><th>[shadowless\_icons](#wikiextras-shadowless_icons-option)</th>
<th>
Use shadowless icons.


</th>
<th>`disabled`</th></tr>
<tr><th>[showicons\_limit](#wikiextras-showicons_limit-option)</th>
<th>
To prevent exhaustive network traffic, limit the maximum number of
icons generated by the macro `ShowIcons`. Set to 0 for
unlimited number of icons (this will produce exhaustive network
traffic--you have been warned!)


</th>
<th>`96`</th></tr>
<tr><th>[todo\_phrases](#wikiextras-todo_phrases-option)</th>
<th>
Analogous to `FIXME`-phrases, but presentation is less eye-catching.


</th>
<th>`REVIEW,TODO`</th></tr>
<tr><th>[wide\_toc](#wikiextras-wide_toc-option)</th>
<th>
Right aligned boxes with table of contents,
produced by the `PageOutline` macro, are either
as wide as ordinary right aligned boxes (`true`) or
narrow (`false`).


</th>
<th>`disabled`</th></tr></table>



---



See also: [TracGuide](trac-guide), [TracAdmin](trac-admin), [TracEnvironment](trac-environment)


