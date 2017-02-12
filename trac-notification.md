# Email Notification of Ticket Changes






Trac supports notification of ticket changes via email. 



Email notification is useful to keep users up-to-date on tickets/issues of interest, and also provides a convenient way to post all ticket changes to a dedicated mailing list. For example, this is how the [
Trac-tickets](http://lists.edgewall.com/archive/trac-tickets/) mailing list is set up.



Disabled by default, notification can be activated and configured in [trac.ini](trac-ini).


## Receiving Notification Mails



When reporting a new ticket or adding a comment, enter a valid email address or your Trac username in the *reporter*, *assigned to/owner* or *cc* field. Trac will automatically send you an email when changes are made to the ticket, depending on how notification is configured.


### How to use your username to receive notification mails



To receive notification mails, you can either enter a full email address or your Trac username. To get notified with a simple username or login, you need to specify a valid email address in the *Preferences* page. 



Alternatively, a default domain name (**`smtp_default_domain`**) can be set in the [TracIni](trac-ini) file, see [Configuration Options](trac-notification#configuration-options) below. In this case, the default domain will be appended to the username, which can be useful for an "Intranet" kind of installation.



When using apache and mod\_kerb for authentication against Kerberos / Active Directory, usernames take the form (**`username@EXAMPLE.LOCAL`**). To avoid this being interpreted as an email address, add the Kerberos domain to  (**`ignore_domains`**).


### Ticket attachment notifications



Since 1.0.3 Trac will send notifications when a ticket attachment is added or deleted. Usually attachment notifications will be enabled in an environment by default. To disable the attachment notifications for an environment the `TicketAttachmentNotifier` component must be disabled:


```
[components]
trac.ticket.notification.TicketAttachmentNotifier = disabled
```

## Configuring SMTP Notification



**Important:** For [TracNotification](trac-notification) to work correctly, the `[trac] base_url` option must be set in [trac.ini](trac-ini). 


### Configuration Options



These are the available options for the `[notification]` section in trac.ini:



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





### Example Configuration (SMTP)


```
[notification]
smtp_enabled = true
smtp_server = mail.example.com
smtp_from = notifier@example.com
smtp_replyto = myproj@projects.example.com
smtp_always_cc = ticketmaster@example.com, theboss+myproj@example.com
```

### Example Configuration (`sendmail`)


```
[notification]
smtp_enabled = true
email_sender = SendmailEmailSender
sendmail_path = /usr/sbin/sendmail
smtp_from = notifier@example.com
smtp_replyto = myproj@projects.example.com
smtp_always_cc = ticketmaster@example.com, theboss+myproj@example.com
```

### Subscriber Configuration



The default subscriptions are configured in the `[notification-subscriber]` section in trac.ini:



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






Each user can override these defaults in his *Notifications* preferences.



For example to unsubscribe from notifications for one's own changes and comments, the rule "Never notify: I update a ticket" should be added above other subscription rules.


### Customizing the e-mail subject



The e-mail subject can be customized with the `ticket_subject_template` option, which contains a [
Genshi text template](http://genshi.edgewall.org/wiki/Documentation/text-templates.html) snippet. The default value is:


```
$prefix #$ticket.id: $summary
```


The following variables are available in the template:


- `env`: The project environment (see [
  env.py](http://trac.edgewall.org/intertrac/source%3A/trunk/trac/env.py)).
- `prefix`: The prefix defined in `smtp_subject_prefix`.
- `summary`: The ticket summary, with the old value if the summary was edited.
- `ticket`: The ticket model object (see [
  model.py](http://trac.edgewall.org/intertrac/source%3A/trunk/trac/ticket/model.py)). Individual ticket fields can be addressed by appending the field name separated by a dot, eg `$ticket.milestone`.

### Customizing the e-mail content



The notification e-mail content is generated based on `ticket_notify_email.txt` in `trac/ticket/templates`. You can add your own version of this template by adding a `ticket_notify_email.txt` to the templates directory of your environment. The default looks like this:


```
$ticket_body_hdr
$ticket_props
{% choose ticket.new %}\
{%   when True %}\
$ticket.description
{%   end %}\
{%   otherwise %}\
{%     if changes_body %}\
${_('Changes (by %(author)s):', author=change.author)}

$changes_body
{%     end %}\
{%     if changes_descr %}\
{%       if not changes_body and not change.comment and change.author %}\
${_('Description changed by %(author)s:', author=change.author)}
{%       end %}\
$changes_descr
--
{%     end %}\
{%     if change.comment %}\

${changes_body and _('Comment:') or _('Comment (by %(author)s):', author=change.author)}

$change.comment
{%     end %}\
{%   end %}\
{% end %}\

-- 
${_('Ticket URL: <%(link)s>', link=ticket.link)}
$project.name <${project.url or abs_href()}>
$project.descr
```

## Sample Email


```wiki
#42: testing
---------------------------+------------------------------------------------
       Id:  42             |      Status:  assigned                
Component:  report system  |    Modified:  Fri Apr  9 00:04:31 2004
 Severity:  major          |   Milestone:  0.9                     
 Priority:  lowest         |     Version:  0.6                     
    Owner:  anonymous      |    Reporter:  jonas@example.com               
---------------------------+------------------------------------------------
Changes:
  * component:  changeset view => search system
  * priority:  low => highest
  * owner:  jonas => anonymous
  * cc:  daniel@example.com =>
         daniel@example.com, jonas@example.com
  * status:  new => assigned

Comment:
I'm interested too!

--
Ticket URL: <http://example.com/trac/ticket/42>
My Project <http://myproj.example.com/>
```

## Customizing e-mail content for MS Outlook



MS Outlook normally presents plain text e-mails with a variable-width font, and as a result the ticket properties table will most certainly look like a mess in MS Outlook. This can be fixed with some customization of the [e-mail template](trac-notification#).



Replace the following second row in the template:


```wiki
$ticket_props
```


with this (requires Python 2.6 or later):


```wiki
--------------------------------------------------------------------------
{% with
   pv = [(a[0].strip(), a[1].strip()) for a in [b.split(':') for b in
         [c.strip() for c in 
          ticket_props.replace('|', '\n').splitlines()[1:-1]] if ':' in b]];
   sel = ['Reporter', 'Owner', 'Type', 'Status', 'Priority', 'Milestone', 
          'Component', 'Severity', 'Resolution', 'Keywords'] %}\
${'\n'.join('%s\t%s' % (format(p[0]+':', ' <12'), p[1]) for p in pv if p[0] in sel)}
{% end %}\
--------------------------------------------------------------------------
```


The table of ticket properties is replaced with a list of a selection of the properties. A tab character separates the name and value in such a way that most people should find this more pleasing than the default table when using MS Outlook.


\#42: testing

--------------------------------------------------------------------------

<table><tr><th>Reporter:</th>
<th>jonas\@example.com</th></tr>
<tr><th>Owner:</th>
<th>anonymous</th></tr>
<tr><th>Type:</th>
<th>defect</th></tr>
<tr><th>Status:</th>
<th>assigned</th></tr>
<tr><th>Priority:</th>
<th>lowest</th></tr>
<tr><th>Milestone:</th>
<th>0.9</th></tr>
<tr><th>Component:</th>
<th>report system</th></tr>
<tr><th>Severity:</th>
<th>major</th></tr>
<tr><th>Resolution:</th>
<th> </th></tr>
<tr><th>Keywords:</th>
<th> </th></tr></table>


--------------------------------------------------------------------------

Changes:


  \* component:  changeset view =\> search system

  \* priority:  low =\> highest

  \* owner:  jonas =\> anonymous

  \* cc:  daniel\@example.com =\>

          daniel\@example.com, jonas\@example.com

  \* status:  new =\> assigned


Comment:

I'm interested too!


--

Ticket URL: \<http://example.com/trac/ticket/42\>

My Project \<http://myproj.example.com/\>


**Important**: Only those ticket fields that are listed in `sel` are part of the HTML mail. If you have defined custom ticket fields which are to be part of the mail, then they have to be added to `sel`. Example:


```wiki
   sel = ['Reporter', ..., 'Keywords', 'Custom1', 'Custom2']
```


However, the solution is still a workaround to an automatically HTML-formatted e-mail.


## Using GMail as the SMTP relay host



Use the following configuration snippet:


```
[notification]
smtp_enabled = true
use_tls = true
mime_encoding = base64
smtp_server = smtp.gmail.com
smtp_port = 587
smtp_user = user
smtp_password = password
```


where *user* and *password* match an existing GMail account, ie the ones you use to log in on [
http://gmail.com](http://gmail.com).



Alternatively, you can use `smtp_port = 25`.

You should not use `smtp_port = 465`. Doing so may deadlock your ticket submission. Port 465 is reserved for the SMTPS protocol, which is not supported by Trac. See [
\#7107](http://trac.edgewall.org/intertrac/comment%3A2%3Aticket%3A7107) for details.


## Troubleshooting



If you cannot get the notification working, first make sure the log is activated and have a look at the log to find if an error message has been logged. See [TracLogging](trac-logging) for help about the log feature.



Notification errors are not reported through the web interface, so the user who submits a change or a new ticket never gets notified about a notification failure. The Trac administrator needs to look at the log to find the error trace.


### *Permission denied* error



Typical error message:


```
  ...
  File ".../smtplib.py", line 303, in connect
    raise socket.error, msg
  error: (13, 'Permission denied')
```


This error usually comes from a security settings on the server: many Linux distributions do not allow the web server (Apache, ...) to post email messages to the local SMTP server.



Many users get confused when their manual attempts to contact the SMTP server succeed:


```
telnet localhost 25
```


This is because a regular user may connect to the SMTP server, but the web server cannot:


```
sudo -u www-data telnet localhost 25
```


In such a case, you need to configure your server so that the web server is authorized to post to the SMTP server. The actual settings depend on your Linux distribution and current security policy. You may find help in the Trac [
MailingList](http://trac.edgewall.org/intertrac/MailingList) archive.



Relevant ML threads:


- SELinux: [
  http://article.gmane.org/gmane.comp.version-control.subversion.trac.general/7518](http://article.gmane.org/gmane.comp.version-control.subversion.trac.general/7518)


For SELinux in Fedora 10:


```
$ setsebool -P httpd_can_sendmail 1
```

### *Suspected spam* error



Some SMTP servers may reject the notification email sent by Trac.



The default Trac configuration uses Base64 encoding to send emails to the recipients. The whole body of the email is encoded, which sometimes trigger *false positive* spam detection on sensitive email servers. In such an event, change the default encoding to "quoted-printable" using the `mime_encoding` option.



Quoted printable encoding works better with languages that use one of the Latin charsets. For Asian charsets, stick with the Base64 encoding.


---



See also: [TracTickets](trac-tickets), [TracIni](trac-ini), [TracGuide](trac-guide), [
TracDev/NotificationApi](http://trac.edgewall.org/intertrac/TracDev/NotificationApi)


