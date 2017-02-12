


# Change Log



This is a rough list of changes between released versions.



To see where Trac is going in future releases, see the [
Roadmap](http://trac.edgewall.org/intertrac/roadmap).


## 1.2.x Releases


### 1.2 'Hermes'



*(November 5, 2016)*



Trac 1.2 is the first major release of Trac in more than 4 years.



The following are some highlights from the release:


- Extensible notification system ([
  \#3517](http://trac.edgewall.org/intertrac/%233517))
- Notification preference panel ([
  \#4056](http://trac.edgewall.org/intertrac/%234056))
- Usernames replaced with full names ([
  \#7339](http://trac.edgewall.org/intertrac/%237339))
- Restyled ticket changelog ([
  \#11835](http://trac.edgewall.org/intertrac/%2311835))
- Workflow controls on the *New Ticket* page ([
  \#2045](http://trac.edgewall.org/intertrac/%232045))
- Editable wiki page version comments ([
  \#6573](http://trac.edgewall.org/intertrac/%236573))
- Datetime custom fields ([ \#1942](http://trac.edgewall.org/intertrac/%231942))


For more information see the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.2) and the detailed
release notes for [
 1.2](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.2%23DevelopmentReleases%20) and [
 1.0.8 through 1.0.13](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20)
(as 1.2 contains all the fixes done for 1.0.8 through 1.0.13).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.2) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.2)


## 1.1.x Releases



* 1.1.x releases are development releases leading eventually to Trac 1.2. See them as kind of snapshots of [
source:trunk](http://trac.edgewall.org/intertrac/source%3Atrunk).
*



** No guarantees of feature and API compatibility is made from one 1.1.x release to the next. *
***


### 1.2rc1



*(September 14, 2016)*



The first Trac 1.2 release candidate is the culmination of nearly 4 years of development.



Highlights of the changes since 1.1.6:


- Pygments lexer options can be specified as [WikiProcessor](wiki-processors) arguments and defaults can be set in the environment configuration ([
  \#5654](http://trac.edgewall.org/intertrac/%235654)).
- Usernames are replaced with full names when `[trac]` `show_full_names` is true ([
  \#7339](http://trac.edgewall.org/intertrac/%237339)).
- Enum tables on the Ticket Admin pages can be reordered by drag and drop. ([
  \#11682](http://trac.edgewall.org/intertrac/%2311682)).
- Ticket changelog is restyled and has a new *Show comments* preference ([
  \#11835](http://trac.edgewall.org/intertrac/%2311835)).
- Authentication cookies can be shared across subdomains when `[trac]` `auth_cookie_domain` is configured ([
  \#12251](http://trac.edgewall.org/intertrac/%2312251)).


For more information see the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed
release notes for [
 1.2rc1](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20) and [
 1.0.8 through 1.0.13](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20)
(as 1.2rc1 contains all the fixes done for 1.0.8 through 1.0.13).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.2rc1) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.2)


### 1.1.6



*(July 17, 2015)*



Trac 1.1.6 contains more than a half dozen minor fixes and enhancements.



For more information see the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed
release notes for [
 1.1.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20) and [
 1.0.7](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20)
(as 1.1.6 contains all the fixes done for 1.0.7).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.6) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.6)


### 1.1.5



*(May 18, 2015)*



Highlights of the changes:


- Corrected highlighting of unmodified values in *Config* section of the *About Trac* page ([
  \#6551](http://trac.edgewall.org/intertrac/%236551)).
- New helper methods on `DatabaseManager` class for plugins to upgrade the database ([
  \#8172](http://trac.edgewall.org/intertrac/%238172)).
- New `[notification-subscriber]` config section for general configuration of notification subscription defaults and `SubscriberList` macro ([
  \#11875](http://trac.edgewall.org/intertrac/%2311875)).
- Removed dependency on `ConfigObj` for [TracFineGrainedPermissions](trac-fine-grained-permissions) ([
  \#11982](http://trac.edgewall.org/intertrac/%2311982)).
- `Image` macro supports [InterWiki](inter-wiki) prefixes ([
  \#12025](http://trac.edgewall.org/intertrac/%2312025)).


See also the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed
release notes for [
 1.1.5](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20), [
 1.0.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.7](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20)
(as 1.1.5 contains all the fixes done for 1.0.6 and 0.12.7).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.5) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.5)


### 1.1.4



*(March 24, 2015)*



Highlights of the changes:


- Performance improvements with MySQL/MariaDB ([
  \#3676](http://trac.edgewall.org/intertrac/%233676)).
- Click on *Permissions* Admin page table row toggles all
  checkboxes in the row ([
  \#11417](http://trac.edgewall.org/intertrac/%2311417)).
- Configuration sections are written to trac.ini when enabling a
  component through [TracAdmin](trac-admin) or the web administration module
  ([ \#11437](http://trac.edgewall.org/intertrac/%2311437)).
- Subscription rules can be reordered by drag and drop ([
  \#11941](http://trac.edgewall.org/intertrac/%2311941)).


See also the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed
release notes for [
 1.1.4](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20)
and [
 1.0.4/1.0.5](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20)
(as 1.1.4 contains all the fixes done for 1.0.4 and 1.0.5).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.4) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.4)


### 1.1.3



*(January 13, 2015)*



The following list contains highlights of the changes:


- The ticket creation step can be configured in the [TracWorkflow](trac-workflow) and the
  workflow controls are present on the NewTicket page ([
  \#2045](http://trac.edgewall.org/intertrac/%232045)).
- New notification system that can be extended by plugins ([
  \#3517](http://trac.edgewall.org/intertrac/%233517)).
- New preferences panel for notification subscriptions ([
  \#4056](http://trac.edgewall.org/intertrac/%234056)).
- Wiki page version comments can be edited by users with `WIKI_ADMIN` ([
  \#6573](http://trac.edgewall.org/intertrac/%236573)).
- Improved positioning of *Add Comment* section and *author* field
  on the ticket form ([ \#10207](http://trac.edgewall.org/intertrac/%2310207)).
- The delete confirmation pages warn if attachments will also be deleted
  ([ \#11542](http://trac.edgewall.org/intertrac/%2311542)).
- Removed support for [
  SilverCity](http://trac.edgewall.org/intertrac/SilverCity), Enscript and PhpRenderer syntax
  highlighters ([ \#11795](http://trac.edgewall.org/intertrac/%2311795)).
- Combined *Date & Time* and *Language* preference panels as
  *Localization* ([ \#11813](http://trac.edgewall.org/intertrac/%2311813)).
- Groups and permissions can be used in the workflow `set_owner` attribute
  ([ \#11839](http://trac.edgewall.org/intertrac/%2311839)).


See also the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed release notes for [
 1.1.3](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20) and [
 1.0.3](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) (as 1.1.3 contains all the fixes done 
for 1.0.3).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.3) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.3)


### 1.1.2



*(October 23, 2014)*



The following list contains highlights of the changes:


- Dropped support for Python 2.5. Trac can no longer be run on Python 2.5 as incompatible changes have been made in the source code ([
  \#11600](http://trac.edgewall.org/intertrac/%2311600)).
- The new ticket workflow action `may_set_owner` is similar to `set_owner` but the owner defaults to the existing ticket owner rather than the current user ([
  \#10018](http://trac.edgewall.org/intertrac/%2310018)).
- The new option `[ticket]` `optional_fields` specifies ticket select fields that are treated as optional (i.e. an empty value is allowed) ([
  \#10772](http://trac.edgewall.org/intertrac/%2310772)).
- Line number and row highlighting annotations can be specified for WikiProcessor code blocks ([
  \#10834](http://trac.edgewall.org/intertrac/%2310834)).
- The *default handler* can be set as a session preference ([
  \#11597](http://trac.edgewall.org/intertrac/%2311597)), and the default value for all users can be set from the *Basic Settings* admin page ([
  \#11519](http://trac.edgewall.org/intertrac/%2311519)).
- Attachments can't be added to read-only wiki pages ([
  \#11244](http://trac.edgewall.org/intertrac/%2311244)).
- Tables on the admin pages have a *Select all* checkbox in the header ([
  \#10994](http://trac.edgewall.org/intertrac/%2310994)).
- Submit buttons are disabled if the required items are not selected ([
  \#11056](http://trac.edgewall.org/intertrac/%2311056)).
- The Admin *Permissions* page has a *Copy Permissions* form for copying permissions between users and groups ([
  \#11099](http://trac.edgewall.org/intertrac/%2311099)).
- The new option `[milestone]` `default_retarget_to` determines the default milestone for retargeting tickets when a milestone is deleted or closed, and can be specified from the *Milestone* admin page ([
  \#10010](http://trac.edgewall.org/intertrac/%2310010)).
- The *retarget* select is not shown when closing or deleting a milestone which has no tickets associated with it ([
  \#11366](http://trac.edgewall.org/intertrac/%2311366)).
- *Clear default* buttons allow the ticket system default values (e.g. `default_milestone`, `default_version`) to be cleared through the corresponding admin pages ([
  \#10772](http://trac.edgewall.org/intertrac/%2310772), [
  \#11300](http://trac.edgewall.org/intertrac/%2311300)).
- The `TitleIndex` macro supports relative path prefixes when used on wiki pages ([
  \#11455](http://trac.edgewall.org/intertrac/%2311455)).
- [
  CommitTicketUpdater](http://trac.edgewall.org/intertrac/CommitTicketUpdater) will recognize a ticket reference that includes a trailing `#comment:N` or `#comment:description` ([
  \#11622](http://trac.edgewall.org/intertrac/%2311622)).
- The *Tickets* column of the milestone table on the *Milestone* admin page contains links to the query page showing all tickets associated with the milestone, grouped by status ([
  \#11661](http://trac.edgewall.org/intertrac/%2311661)).
- Authz policy can be used to restrict access to the *Report List* page using the resource id `-1` ([
  \#11697](http://trac.edgewall.org/intertrac/%2311697)).


See also the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed release notes for [
 1.1.2](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20), [
 1.0.2](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20) (as 1.1.2 contains all the fixes done for 1.0.2 and 0.12.6).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.2) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.2)


### 1.1.1



*(February 3, 2013)*



Trac 1.1.1 starts the 1.1.x development line leading to 1.2 with some new features and a few not-so-disruptive changes.



The following list contains only a few highlights:


- Added support for custom ticket fields of type time ([
  \#1942](http://trac.edgewall.org/intertrac/%231942))
- In new tickets, custom time ticket fields may default to an absolute or relative date / time ([
  \#10853](http://trac.edgewall.org/intertrac/%2310853))
- In [TracBatchModify](trac-batch-modify), custom time ticket fields can be changed with a date(time)picker popup control ([
  \#10854](http://trac.edgewall.org/intertrac/%2310854))
- Optionally display the component of tickets in their timeline entries (`[timeline]` `ticket_show_component` setting) ([
  \#10885](http://trac.edgewall.org/intertrac/%2310885))
- Fixed batch modification when no fields are changed ([
  \#10924](http://trac.edgewall.org/intertrac/%2310924))
- Dynamic variables can be used in the report title and description ([
  \#10979](http://trac.edgewall.org/intertrac/%2310979))
- jQuery upgraded to 1.8.3, jQuery UI upgraded to 1.9.2 and jQuery UI Timepicker upgraded to 1.1.1 ([
  \#10976](http://trac.edgewall.org/intertrac/%2310976))
- Dropped support for Python 2.5, either Python 2.6 or Python 2.7 is required *(well, as it happens, 2.5 *still* works, that's a bug ;-) )*


See also the [
API changes](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ApiChanges/1.1) and the detailed release notes for [
 1.1.1](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.1%23DevelopmentReleases%20), [
 1.0.1](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.5](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20) (as 1.1.1 contains all the fixes done for 1.0.1 and 0.12.5).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.1.1) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.1.1)


## 1.0.x Releases


### 1.0.13



*(September 11, 2016)*



Trac 1.0.13 provides around a dozen bug fixes and minor
enhancements. The following are some highlights:


- Use locale environment variables to negotiate locale 
  on console ([ \#12418](http://trac.edgewall.org/intertrac/%2312418)).
- Fixed using incorrect revisions when downloading a zip
  file via browser page from Git repository ([
  \#12557](http://trac.edgewall.org/intertrac/%2312557)).


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.13) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.13)


### 1.0.12



*(July 4, 2016)*



Trac 1.0.12 provides around 20 bug fixes and minor enhancements. The following are some highlights:


- Reconnect to PostgreSQL server after restarting it
  ([ \#4984](http://trac.edgewall.org/intertrac/%234984)).
- Workflow actions on the batch modify form are sorted
  by the default attribute ([
  \#12447](http://trac.edgewall.org/intertrac/%2312447)).
- Fixed Pygments stylesheet not found when style name
  contained a dash ([ \#12505](http://trac.edgewall.org/intertrac/%2312505)).
- Fixed incorrect parsing of projects list file by
  `GitwebProjectsRepositoryProvider` ([
  \#12518](http://trac.edgewall.org/intertrac/%2312518)).
- `TracIni` macro displays option documentation as 
  multi-line rather than one-liner ([
  \#12522](http://trac.edgewall.org/intertrac/%2312522)).
- Fixed regression with `GitConnector` leading to
  `IOError: Too many open files` ([
  \#12524](http://trac.edgewall.org/intertrac/%2312524)).


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.12) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.12)


### 1.0.11



*(May 7, 2016)*



Trac 1.0.11 provides more than 30 bug fixes and minor
enhancements. As in 1.0.10, an area of focus has been to
eliminate tracebacks in the logs due to invalid requests.
The following are some additional highlights:


- Fixed resetting *Oldest first* after auto-preview of
  ticket change log ([ \#12381](http://trac.edgewall.org/intertrac/%2312381)).
- Trac is now distributed as wheel package ([
  \#12391](http://trac.edgewall.org/intertrac/%2312391)).
- Fixed database exceptions in query system when 
  *milestones/versions/enums* are not defined and a custom
  field of the same name is added ([
  \#12399](http://trac.edgewall.org/intertrac/%2312399)).
- Custom field *milestone* was not shown when
  standard *milestone* field was hidden ([
  \#12400](http://trac.edgewall.org/intertrac/%2312400)).
- Query system now sorts by `enum.value` rather than
  `ticket.type` for `order=type` ([
  \#12402](http://trac.edgewall.org/intertrac/%2312402)). 
- Added support for Babel 2.3.2 (2.3.0 and 2.3.1 should
  not be used)  ([ \#12445](http://trac.edgewall.org/intertrac/%2312445)).


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.11) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.11)


### 1.0.10



*(February 20, 2016)*



Trac 1.0.10 provides more than 30 bug fixes and minor enhancements. Two areas of focus
have been fixing test failures on Windows and eliminating tracebacks in the logs due to
invalid requests.



See the detailed release notes for [
 1.0.10](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.10) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.10)


### 1.0.9



*(September 10, 2015)*



Trac 1.0.9 provides more than a dozen minor fixes and enhancements, including significantly reduced memory usage by the Git repository connector.



See the detailed release notes for [
 1.0.9](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.9) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.9)


### 1.0.8



*(July 24, 2015)*



Trac 1.0.8 fixes a regression introduced in Trac 1.0.7: the session
for an authenticated username containing non-alphanumeric characters
could not be retrieved, resulting in the user being denied access to
every realm and resource ([
\#12129](http://trac.edgewall.org/intertrac/%2312129)).



See the detailed release notes for [
 1.0.8](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.8) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.8)


### 1.0.7



*(July 17, 2015)*



Trac 1.0.7 contains more than a dozen minor fixes and enhancements, including the following highlights:


- Custom `svn:keywords` definitions are expanded in Subversion 1.8 and later ([
  \#11364](http://trac.edgewall.org/intertrac/%2311364)).
- Fixed MySQL performance regression in query with custom fields ([
  \#12113](http://trac.edgewall.org/intertrac/%2312113)).


See the detailed release notes for [
 1.0.7](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.7) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.7)


### 1.0.6



*(May 20, 2015)*



Trac 1.0.6 provides more than 20 fixes and enhancements. The following are some highlights:


- Hash changeset ids and branch names can be used in revision ranges ([
  \#11050](http://trac.edgewall.org/intertrac/%2311050))
- Improved rendering performance using chunked response when `[trac]` `use_chunked_encoding` is `True` ([
  \#11802](http://trac.edgewall.org/intertrac/%2311802))
- Improved performance of Git repositories ([
  \#11971](http://trac.edgewall.org/intertrac/%2311971)).
- Header to send when `[trac]` `use_xsendfile` is `True` can be specified through the option `[trac]` `xsendfile_header`. X-Sendfile is supported in Nginx by specifying `X-Accel-Redirect` for the header ([
  \#11981](http://trac.edgewall.org/intertrac/%2311981)).
- Symbolic link can be used for `conf/trac.ini` in environment directory ([
  \#12000](http://trac.edgewall.org/intertrac/%2312000)).
- Hyphen character can be used in WikiProcessor parameter name ([
  \#12023](http://trac.edgewall.org/intertrac/%2312023)).


See the detailed release notes for [
 1.0.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.7](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20) (as 1.0.6 also contains the changes in 0.12.7).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.6) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.6)


### 1.0.5



*(March 24, 2015)*



Trac 1.0.5 provides several fixes. The following are some highlights:


- Images are not rendered in the timeline ([
  \#10751](http://trac.edgewall.org/intertrac/%2310751)).
- Git tags are shown in the browser view ([
  \#11964](http://trac.edgewall.org/intertrac/%2311964)).
- Added support for `journal_mode` and `synchronous` pragmas
  in `sqlite:` database connection string ([
  \#11967](http://trac.edgewall.org/intertrac/%2311967)).


See the detailed release notes for [
 1.0.5](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.5) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.5)


### 1.0.4



*(February 8, 2015)*



Trac 1.0.4 contains a few fixes, including a fix for a regression in 1.0.3.


- Workflow action labels were not displayed unless name attribute
  was explicitly defined ([
  \#11930](http://trac.edgewall.org/intertrac/%2311930)).


See the detailed release notes for [
 1.0.4](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.4) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.4)


### 1.0.3



*(January 13, 2015)*



Trac 1.0.3 is a maintenance release containing numerous fixes and minor
enhancements. The following are a few of the highlights:



The following list contains only a few highlights:


- Notification is sent when adding an attachment to a ticket ([
  \#2259](http://trac.edgewall.org/intertrac/%232259)).
- Stylesheets and scripts are loaded during autopreview, resulting in proper
  syntax highlighting when code [WikiProcessors](wiki-processors) are added ([
  \#10470](http://trac.edgewall.org/intertrac/%2310470)) and display
  of Workflow graphs without explicit autopreview ([
  \#10674](http://trac.edgewall.org/intertrac/%2310674)).
- Merge changesets are shown as differences against first parent, resulting
  in less noisy changesets ([
  \#10740](http://trac.edgewall.org/intertrac/%2310740)).
- Pygments 2.0 is supported ([
  \#11796](http://trac.edgewall.org/intertrac/%2311796)).
- Fixed error when completing the `initenv` [TracAdmin](trac-admin) command ([
  \#11797](http://trac.edgewall.org/intertrac/%2311797)).
- Performance improvement on systems with many thousands of authenticated
  users due to caching of Environment.get\_known\_users ([
  \#11868](http://trac.edgewall.org/intertrac/%2311868)).
- Distribution metadata of wheel package is supported and displayed on the
  About page ([ \#11877](http://trac.edgewall.org/intertrac/%2311877)).
- … and more than 3 dozen total fixes!


See the detailed release notes for [
 1.0.3](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.3) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.3)


### 1.0.2



*(October 23, 2014)*



Trac 1.0.2 contains a number of bug fixes and minor enhancements, as well as a major update for many translations.



The following list contains only a few highlights:


- Subversion keywords are expanded and EOL substitutions made when viewing a file in the repository browser and when downloading a file ([
  \#717](http://trac.edgewall.org/intertrac/%23717)). 
- Notification email is sent to the old owner when a ticket is reassigned ([
  \#2311](http://trac.edgewall.org/intertrac/%232311)).
- Ticket change history is updated when renaming and deleting a milestone, and when retargeting tickets to another milestone ([
  \#4582](http://trac.edgewall.org/intertrac/%234582), [
  \#5658](http://trac.edgewall.org/intertrac/%235658)).
- Numerous fixes for the Authz permissions policy in the browser/repository ([
  \#10961](http://trac.edgewall.org/intertrac/%2310961), [
  \#11646](http://trac.edgewall.org/intertrac/%2311646)), wiki ([
  \#8976](http://trac.edgewall.org/intertrac/%238976), [
  \#11067](http://trac.edgewall.org/intertrac/%2311067)), admin ([
  \#11069](http://trac.edgewall.org/intertrac/%2311069)) and report ([
  \#11176](http://trac.edgewall.org/intertrac/%2311176)) realms.
- Multiple forms submits are disallowed ([
  \#10138](http://trac.edgewall.org/intertrac/%2310138)).
- `ConfigurationError` is raised if any of the `permission_policies` can't be loaded, preventing possible information leakage due to internal and installation errors ([
  \#10285](http://trac.edgewall.org/intertrac/%2310285)).
- Wiki toolbars can be disabled through a configuration setting ([
  \#10837](http://trac.edgewall.org/intertrac/%2310837))
- The number of entries in a table is shown next to heading on applicable admin pages ([
  \#11027](http://trac.edgewall.org/intertrac/%2311027)).
- *Cancel* buttons are consistently located on all pages ([
  \#11076](http://trac.edgewall.org/intertrac/%2311076)).
- Focus is placed on a text element when an edit page is loaded ([
  \#11084](http://trac.edgewall.org/intertrac/%2311084)).
- The *Edit conflict* and *Merge* warning messages are always visible in side-by-side edit mode ([
  \#11102](http://trac.edgewall.org/intertrac/%2311102)).
- Improvements to the layout of the Report ([
  \#11106](http://trac.edgewall.org/intertrac/%2311106), [
  \#11664](http://trac.edgewall.org/intertrac/%2311664)) and Ticket pages ([
  \#11471](http://trac.edgewall.org/intertrac/%2311471)).
- Genshi 0.7 compatibility ([
  \#11218](http://trac.edgewall.org/intertrac/%2311218)).
- Numerous minor fixes for Git repository support.
- … and more than a hundred more fixes!


See the detailed release notes for [
 1.0.2](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20) (as 1.0.2 contains all the fixes done for 0.12.6).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.2) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.2)


### 1.0.1



*(February 1, 2013)*



Trac 1.0.1 contains a number of bug fixes and minor enhancements, as well as a major update for many translations.



The following list contains only a few highlights:


- Fix zip source download for large directories in Subversion repositories ([
  \#10840](http://trac.edgewall.org/intertrac/%2310840))
- Performance improvement for the Roadmap, by caching milestone properties ([
  \#10879](http://trac.edgewall.org/intertrac/%2310879))
- Added a *select all* checkbox to table of components for each plugin on the Plugins admin panel ([
  \#9609](http://trac.edgewall.org/intertrac/%239609))
- Restore the *Modify* link at the top of the ticket page, as it was in Trac 0.12 ([
  \#10856](http://trac.edgewall.org/intertrac/%2310856))
- `ListOption` keeps values other than empty string and None in raw list as default ([
  \#10541](http://trac.edgewall.org/intertrac/%2310541))
- Prevent possibility of multiple identical info or warning messages being presented to the user ([
  \#10987](http://trac.edgewall.org/intertrac/%2310987))
- The BatchModify select-all checkboxes are toggled with tri-state behavior when the ticket checkboxes are toggled ([
  \#10992](http://trac.edgewall.org/intertrac/%2310992))
- Update the ticket changetime to the current time when deleting a ticket comment ([
  \#10486](http://trac.edgewall.org/intertrac/%2310486))


See the detailed release notes for [
 1.0.1](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0%23MaintenanceReleases%20) and [
 0.12.5](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20) (as 1.0.1 contains all the fixes done for 0.12.5).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0.1) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0.1)


### **1.0 'Cell' **



*(September 7, 2012)*



Trac 1.0 is a major release adding refreshed user interface and improved DVCS repository support as the most visible changes.



The following list contains only a few highlights:


- The default theme looks more modern, especially on recent browsers (no effort has been made to make it look better on older browsers like IE6 or 7) 
- The \[TH:GitPlugin\] has been donated by Herbert Valerio Riedel to the Trac project (many thanks!) and is now maintained here as an optional component
- As a consequence, the Subversion support has been moved below `tracopt.versioncontrol` as well
- The Git and Mercurial log view feature a visualization of the branching structure 
- Usability improvements for the tickets, with a better support for conflict detection and resolution
- Integration of the \[TH:BatchModifyPlugin\], contributed by Brian Meeker (many thanks!) and is now maintained there as a default component
- jQuery/UI integration, featuring a date picker for date fields
- Improved integration with Pygments syntax highlighting
- ... and numerous smaller features added and bugs fixed since 0.12!


See the full list in [
1.0](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/1.0).



[
source:/tags/trac-1.0 View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-1.0%20View%20Tag) \| [
milestone:1.0 View Milestone](http://trac.edgewall.org/intertrac/milestone%3A1.0%20View%20Milestone)


## 0.12.x Releases


### 0.12.7



*(July 12, 2015)*



Trac 0.12.7 fixes a minor security issue, as well as a half dozen other minor issues:


- [InterWiki](inter-wiki) filters links through `[wiki] safe_schemes` option if `[wiki] render_unsafe_content` is disabled ([
  \#12053](http://trac.edgewall.org/intertrac/%2312053)).


See the detailed release notes for [
 0.12.7](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.7) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.7)


### 0.12.6



*(October 23, 2014)*



Trac 0.12.6 contains fixes for a few issues:


- Subversion blame would fail for a path with URL-encoded characters ([
  \#10386](http://trac.edgewall.org/intertrac/%2310386)), a lower-case drive letter on Windows ([
  \#10514](http://trac.edgewall.org/intertrac/%2310514)), or a non-ascii filename with Subversion 1.7 ([
  \#11167](http://trac.edgewall.org/intertrac/%2311167)).
- Improved performance rendering `svn:mergeinfo` properties in browser view ([
  \#8459](http://trac.edgewall.org/intertrac/%238459)) and changeset view ([
  \#11219](http://trac.edgewall.org/intertrac/%2311219)).
- Query with many custom fields would fail ([
  \#11140](http://trac.edgewall.org/intertrac/%2311140)).
- Zip archive had a timestamp with no timezone information ([
  \#11162](http://trac.edgewall.org/intertrac/%2311162)).
- Failure or incorrect ranges rendering log [TracLinks](trac-links) ([
  \#11308](http://trac.edgewall.org/intertrac/%2311308), [
  \#11346](http://trac.edgewall.org/intertrac/%2311346)).
- Textareas in ticket view did not wrap correctly in IE 11 ([
  \#11376](http://trac.edgewall.org/intertrac/%2311376)).
- Emails were not being obfuscated in owner field on CSV export from ticket and query pages ([
  \#11594](http://trac.edgewall.org/intertrac/%2311594)).
- Locale data was not being included in egg in Distribute 0.6.29 and later ([
  \#11640](http://trac.edgewall.org/intertrac/%2311640)).
- Deleting a milestone would not delete its attachments ([
  \#11672](http://trac.edgewall.org/intertrac/%2311672)).
- Added support for Babel 1.0 and later ([
  \#11258](http://trac.edgewall.org/intertrac/%2311258), [
  \#11345](http://trac.edgewall.org/intertrac/%2311345)).
- Added support for `ConfigObj` 5.0 and later ([
  \#11498](http://trac.edgewall.org/intertrac/%2311498)).
- … and dozens more fixes!


See the detailed release notes for [
 0.12.6](http://trac.edgewall.org/intertrac/wiki%3ATracDev/ReleaseNotes/0.12%23MaintenanceReleases%20).



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.6) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.6)


### 0.12.5



*(January 15, 2013)*



Trac 0.12.5 contains fixes for a few issues:


- upload of .mht files ([
  MHTML](http://en.wikipedia.org/wiki/MHTML) web page archive files) now works ([
  \#9880](http://trac.edgewall.org/intertrac/%239880))
- more robust parsing of attachment URLs ([
  \#10280](http://trac.edgewall.org/intertrac/%2310280)) and uploaded file names ([
  \#10850](http://trac.edgewall.org/intertrac/%2310850))
- lots of improvement to the date formatting code, which is now much more robust when timezone and daylight saving time computations are involved ([
  \#10768](http://trac.edgewall.org/intertrac/%2310768), [
  \#10863](http://trac.edgewall.org/intertrac/%2310863), [
  \#10864](http://trac.edgewall.org/intertrac/%2310864), [
  \#10912](http://trac.edgewall.org/intertrac/%2310912), [
  \#10920](http://trac.edgewall.org/intertrac/%2310920))
- no longer generate invalid JSON encoded data with Python 2.4 and 2.5 ([
  \#10877](http://trac.edgewall.org/intertrac/%2310877))
- ... and fix a couple more minor defects ([
  \#10967](http://trac.edgewall.org/intertrac/%2310967), [
  \#10892](http://trac.edgewall.org/intertrac/%2310892), [
  \#10923](http://trac.edgewall.org/intertrac/%2310923), [
  \#10858](http://trac.edgewall.org/intertrac/%2310858), [
  \#10835](http://trac.edgewall.org/intertrac/%2310835))


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.5) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.5)


### 0.12.4



*(September 7, 2012)*



Trac 0.12.4 contains only a handful of minor fixes.



[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.4) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.4)


### 0.12.3



*(February 6, 2012)*



Trac 0.12.3 contains a few minor fixes and a few minor features.


- compatibility with Subversion 1.7 ([
  \#10414](http://trac.edgewall.org/intertrac/%2310414))
- easier troubleshooting of common startup errors ([
  \#10024](http://trac.edgewall.org/intertrac/%2310024))
- jQuery upgraded to 1.4.4 ([
  \#10001](http://trac.edgewall.org/intertrac/%2310001))
- improve fine-grained permission handling in the source browser ([
  \#9976](http://trac.edgewall.org/intertrac/%239976), [
  \#10208](http://trac.edgewall.org/intertrac/%2310208), [
  \#10110](http://trac.edgewall.org/intertrac/%2310110))
- added compatibility with MySQL 5.5.3 utf8mb4 databases ([
  \#9766](http://trac.edgewall.org/intertrac/%239766))
- ... and dozens more fixes!


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.3) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.3)


### 0.12.2



*(January 31, 2011)*



Trac 0.12.2 contains a few minor fixes and a few minor features.



This list contains only a few highlights:


- install: improved robustness of Trac installation if Babel is
  installed after the fact ([
  \#9439](http://trac.edgewall.org/intertrac/%239439), [
  \#9595](http://trac.edgewall.org/intertrac/%239595), [
  \#9961](http://trac.edgewall.org/intertrac/%239961))
- notifications: support for Asian character width ([
  \#4717](http://trac.edgewall.org/intertrac/%234717))
- roadmap: fix display of progress bar in some corner cases ([
  \#9718](http://trac.edgewall.org/intertrac/%239718))
  and respect the overall\_completion milestone group setting ([
  \#9721](http://trac.edgewall.org/intertrac/%239721))
- reports: reports and queries look much better, as the columns now
  keep the same width across groups; the absence of word wrapping in
  reports has been fixed ([ \#9825](http://trac.edgewall.org/intertrac/%239825))
- web admin: improved layout ([
  \#8866](http://trac.edgewall.org/intertrac/%238866), [
  \#9963](http://trac.edgewall.org/intertrac/%239963))
- web: it's now possible to log in different Trac instances sharing
  the same URL prefix (e.g. /project and /project-test) ([
  \#9951](http://trac.edgewall.org/intertrac/%239951))


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.2) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.2)


### 0.12.1



*(October 9, 2010)*



Trac 0.12.1 contains a few important performance improvements, some minor fixes and a few minor features.



This list contains only a few highlights:


- db: improve concurrency behavior ([
  \#9111](http://trac.edgewall.org/intertrac/%239111))
- fcgi: add an environment variable `TRAC_USE_FLUP` to control the usage of flup vs. bundled \_fcgi.py (defaults to 0, i.e. use bundled as before)
- svn authz: improve compatibility with svn 1.5 format ([
  \#8289](http://trac.edgewall.org/intertrac/%238289))
- milestone: allow to set the time for the due date ([
  \#6369](http://trac.edgewall.org/intertrac/%236369), [
  \#9582](http://trac.edgewall.org/intertrac/%239582))
- ticket: fixes for the CC: property ([
  \#8597](http://trac.edgewall.org/intertrac/%238597), [
  \#9522](http://trac.edgewall.org/intertrac/%239522))
- notification: improved the formatting of ticket fields in notification e-mails ([
  \#9484](http://trac.edgewall.org/intertrac/%239484), [
  \#9494](http://trac.edgewall.org/intertrac/%239494))
- i18n: added a configuration option to set the default language ([
  \#8117](http://trac.edgewall.org/intertrac/%238117))
- several fixes for upgrade ([
  \#9400](http://trac.edgewall.org/intertrac/%239400), [
  \#9416](http://trac.edgewall.org/intertrac/%239416), [
  \#9483](http://trac.edgewall.org/intertrac/%239483), [
  \#9556](http://trac.edgewall.org/intertrac/%239556))


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12.1) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12.1)


### ** 0.12 'Babel' **



*(June 13, 2010)*



Trac 0.12 is a major release introducing i18n and multiple repository support as the most visible changes.



The following list contains only a few highlights:


- The user interface is translated in a dozen of languages, provided the \[Babel:\] package is installed
- Multiple repositories can be associated to a single Trac environment; the repositories can be of heterogeneous types (svn, hg, git, darcs...)
- Usability improvements for the Wiki, with a nice side-by-side edit mode with automatic preview
- Richer Wiki syntax, with much improved support for tables, partial [
  WikiCreole](http://trac.edgewall.org/intertrac/WikiCreole) compatibility and numerous smaller improvements
- Usability improvements for the Ticket module, with automatic preview of comments while you type and possibility to edit or remove them later
- Improved Custom Queries (time fields, multiple disjoint conditions, a.k.a. OR queries)
- Timeline filtering by user
- ... and numerous smaller features added and bugs fixed since 0.11!


[ View Tag](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.12) \| [
View Milestone](http://trac.edgewall.org/intertrac/milestone%3A0.12)


## Older Releases



For releases prior to 0.12, see [
TracChangeLog\@95](http://trac.edgewall.org/intertrac/TracChangeLog%4095).


