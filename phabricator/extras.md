


# Extra Phabricator features



While you can mostly just think of Phabricator as a powerful review too, it's really a suite of applications. There are several that are very important which are described below, as well as some philosophical guidelines.


### First step: configuring your applications



The very first thing you want to do before continuing is *configure the applications you can see*. First off, click on the 'Customize Applications' link, which you can find on the bottom of the left side pane:



[](https://i.imgur.com/JURmxre.png)



Next, configure the applications you want, and drag them around to configure the order. Do this by clicking **Pin Applications** on the top right.



**Make sure you have the following applications**:


- Diffusion
- Differential
- Audit
- Herald
- Harbormaster
- Owners


Remember: you can change the order by dragging things around.


### Differential: pre-commit code review



**[
Differential](https://phabricator.haskell.org/differential)** is **the most important tool in Phabricator**. It's what does code review, and lets you review other peoples code to tell them "Great job" or "I guess this is OK" or "Please fix this!"



When you post a review with `arc diff`, the review will show up in Differential and notify the specified reviewers quickly. They can then review your change, or they can


#### Creating diffs



To make a diff, just run `arc diff`. You should keep separate changes separated on different branches. This ensures your changes are isolated and you don't accidentally submit a review of things you shouldn't:


```wiki
$ cd ~/code/ghc-head
$ git checkout -b fix-trac-1234
$ emacs ...
$ git commit -asm "compiler: fix trac issue #1234"
$ arc diff
...
$ git checkout master
... do things ...
$ git checkout -b fix-trac-5678
$ git commit -asm "rts: fix trac issue #5678"
$ arc diff
```


Alternatively, you can upload diffs **from the web UI**. Export your patch as you normally would, go to the Differential application, and click "Create diff" in the very top right. Phabricator will guide you through the rest.


#### Notes on `arc diff`



Note that when you run `arc diff`, it will drop you into an editor to summarize your commit message, add a **test plan**, and **reviewers**, **GHC Trac Issues**, and optionally any **subscribers**. Subscribers will be notified of changes to a review. Reviewers are a set of (comma separated) people who must look at the review. These people have the same usernames as they do on Trac, with one exception: `austin` is Phab's username for Trac's `thoughtpolice`. A **test plan** just specifies how you can verify the change - feel free to make it simple and just say 'validate' or otherwise something silly. **GHC Trac Issues** is a special field that adds metadata about the Trac ticket you're working on.



**A change cannot be merged until at least one reviewer has signed off**. All revisions must have at least one reviewer. Ideally, multiple people will review a change.



Some reviewers are automatically added by **herald**. These are currently bgamari and austin.



If you want wider attention, you can specify the reviewer as `#ghc` - this specifies a group of GHC developers who may come to review it as well.



(**Question**: What does the `#ghc` mean? **Answer**: Essentially, in Phabricator, a Project is composed of a group of people. A project can be referred to by a hashtag, which basically incorporates everyone involved in the project.)



Finally, **GHC Trac Tickets** should be a comma separated list of issue numbers, for example, `GHC Trac Issues: #123, #456`. This will add metadata in Differential allowing you to see the ticket.


#### Differential UI



Don't forget the keyboard shortcuts, in particular `z` lets you post a comment without scrolling past a diff.


#### Landing reviews



If you're submitting a patch to GHC and not a committer, Austin or Ben will land your changes - and we really appreciate it!



If you *are* a committer, you can leave it to Austin or Herbert as well if you'd like, but ideally you'll do it yourself.



If you have commit access, go to your repository with the branch and your changes. Then you can do:


```wiki
$ git checkout master
$ arc patch --nobranch <differential revision>
$ git push origin master
```


This will:


- Pull the patch down as a single change, and apply it to your branch.
- Push it upstream - Phabricator will then close the branch for you as it will see the differential revision notifier in the commit.


**Note: don't use `arc land`! It assigns the commit to yourself, even if you didn't write it.**



If you dislike the rather verbose commit messages created by Phabricator, you can run `git rebase -i origin/master` before pushing and use the `reword` command on each commit.



You're done!


### Audit: post-commit code review



**[
Audit](https://phabricator.haskell.org/audit)** is an application that allows you to keep track of people who touch your code, and then do things when that happens - 
like review their code, and tell them how they messed everything up.



Auditing works mostly the same way as reviewing, only after the fact - instead of accepting or rejecting, you can **Raise Concerns** or **Accept Revision**. If you raise a concern, the author will be notified and generally required to rectify the change. You may use Diffusion to [
browse the GHC repository](https://phabricator.haskell.org/diffusion/GHC), find a commit, and then audit it. For example, by viewing commit [
b6352c9912536929537dcebac9d02d4f995c1657](https://phabricator.haskell.org/rGHCb6352c9912536929537dcebac9d02d4f995c1657), we can look at the diff. Then, go to the bottom, and you can take actions like **Accept Review** or **Raise Concerns** - this is an equivalent to a regular audit for arbitrary commits.



**NB**: commits you should audit, or commits of yours that have had concerns raised will appear on the homepage. They look like this, found at the bottom of the homepage:



[](https://i.imgur.com/r5mpGkb.png)



The section **Need Attention** are commits you need to examine. The **Problem Commits** section are the commits of yours people have raised issues with.



You can also **request an audit** by putting a field in your commit. If you make your commit message say:


```wiki
rts: Fix Trac #XYZ

Auditors: simonmar
```


Then anyone in the `Auditors` field will automatically need to audit your commit. This is very useful for having someone review things after the fact without losing track of it.


### Owners: keeping track of your code



**[
Owners](https://phabricator.haskell.org/owners)** is an application that lets you easily categorize source code, see who owns it, and make it easy to track changes to those files. First, open up the application, and click **All** on the left-side pane. You'll see a screen like this:



[](https://i.imgur.com/cQe8zed.png)



A **Package** is a set of paths in a source tree, which is *owned* by one-or-more people. Every package has a *Primary* owner (with a bolded name), as well as fellow owners. For example, the **GHC - Runtime System** package consists of the files in `./includes` and `./rts` in the GHC repository. `simonmar` is the primary owner, while `ezyang` is a fellow owner. Similarly, `austin` owns the **GHC - Testsuite** package.



Packages and owners are mostly useful for keeping track of things using **Herald**.


### Herald: notifications and tracking



**[
Herald](https://phabricator.haskell.org/herald/)** is an application that allows you to trigger actions on events in Phabricator. For example, when someone posts a diff, comments, or random things happen, it can respond by sending emails, adding subscribers, or triggering audits.



First, you have to select the event the rule will trigger on. Normally this will be *Commit* if you want to analyze commits, or *Differential Revision* if you want it to trigger on new patches for review.



Next, you have to select the type of rule. You will always want the type to be *Personal* - so it only affects you and nobody else.



Finally, you have the rules screen. It should be mostly self explanatory: set combinations of conditions, and sets of actions to take when the conditions are satisfied.



For example, Austin has a **Personal Rule** for **Differential Revisions**. The rule is only triggered when the repository **is any of** `rGHC` (**NB**: the `r` is important!), and **Every time** it **Adds me as a blocking reviewer**.



[](https://i.imgur.com/lUPSMmZ.png)



With this rule, all GHC patches must be signed off on.



Here's a more complex rule that Austin uses, combining Owners, Herald, and Audit:



[](https://i.imgur.com/9eZYKJo.png)



**NOTE**: The last field is truncated, but says "Any package whos owners...", i.e. the last clause means it only fires if I am the owner of a package, set up in the Owners application.


### Harbormaster: building patches



See [wiki:Phabricator/Harbormaster](phabricator/harbormaster) for more.


### Diffusion: browsing the GHC repository



**[
Diffusion](https://phabricator.haskell.org/diffusion)** is a simple, fast repository browser for GHC which you can use to browse the repo, audit commits, explore branches or just read code.



Note that in Phabricator, every repository has what we call a **callsign**. A callsign is a short, unique identifier for a repository. The GHC repository has the **GHC** callsign. Sometimes in the UI when referring to a repository, you must use the unambiguous name `rGHC`, signifying the repository callsign.


## Linking trac tickets, and wiki syntax



Phabricator has some special support for GHC.


### Remarkup syntax



In any place in Phabricator, if you say the word `#1234` to refer to a Trac ticket, it will auto-hyperlink that phrase to the right place:



[](https://i.imgur.com/Fv2Pi1v.png)



This occurs in *every* application, so refer to Trac tickets at will!


### Linking reviews to Trac tickets and vice versa



Let's say you have a revision and it fixes a bug on Trac - like [\#8634](http://gitlabghc.nibbler/ghc/ghc/issues/8634). And let's say you have a differential revision for it - like [
Phab:D69](https://phabricator.haskell.org/D69).



First, **when you submit a review, add the ticket number to the `GHC Trac Issues` field**. This field will be shown when you run `arc diff`. This field should be a comma-separated list of ticket numbers. By doing this, these ticket numbers will automatically appear in Phabricator:



[](https://i.imgur.com/QIGhD0T.png)



Next, add a link to the revision in the Trac ticket. Fill out the field called "Differential Revisions" when you modify the ticket. You can hyperlink to any Phabricator revision using the syntax `Phab:Dxx` with a specific number. For example, to link to Differential Revision D69, say [
Phab:D69](https://phabricator.haskell.org/D69). As an example, Ticket [\#8634](http://gitlabghc.nibbler/ghc/ghc/issues/8634) has this set:



[](https://i.imgur.com/gYHkAhe.png)


## Contributing to our Phabricator infrastructure



Our Phabricator instance is essentially stock upstream with no patches. But, it does have some extensions we wrote, for things like custom fields in Differential, and custom remarkup syntax.



The auxiliary library we use is known as **[
libphutil-haskell](https://github.com/haskell-infra/libphutil-haskell)**, which provides these features. It's written in PHP, so if you want to contribute, you'll have to leave the Haskell world behind!


## Tips



There are some good tips for using Phabricator, including...


### Dashboards



When you login, by default you'll be greeted by a default **Dashboard**, which are Phabricator's way of having custom pages.



If you go to the [
Dashboards](https://phabricator.haskell.org/dashboard/) application, you can create a new dashboard, and then create panels to go on it. You can then move panels around on the editor to customize your home page with audits, commits, etc. Once you've created a Dashboard, you can install it as your default home one as well.



The default dashboard should be relatively well tuned for what GHC developers need, but if you need a custom one, feel free to share!


### Commandeering revisions



Occasionally, you may need to take over a patch from someone else on Phab. That can be done by **Commandeering the Revision**. To do that, go to a differential revision, go to the bottom, and use the **Commandeer** action. You will now own the review, be allowed to update it, and more.


### Keyboard shortcuts



On any page, hit the `?` key on your keyboard to bring up all the **keyboard shortcuts**. These are especially useful when reviewing code, since you can quickly navigate between changes in a review to easily navigate.


### Remarkup reference



Be sure to read about Phabricator's markup language, [
Remarkup](https://secure.phabricator.com/book/phabricator/article/remarkup/). Most importantly, make sure you know how to use those image [
macros](https://phabricator.haskell.org/macro/).


### Applications configuration



You can configure the applications that appear on the left side panel by visiting your [
panel settings](https://phabricator.haskell.org/settings/panel/home/). You can probably safely get rid of most things except **Differential**, **Diffusion**, **Audit**, and **Herald**.


### Arcanist



Arcanist has a few useful commands - in particular, you'll probably like `arc paste`, and `arc list`.



`arc paste` will allow you to instantly upload things to Phab's [
PasteBin application](https://phabricator.haskell.org/paste/). 



`arc list` will show you all your revisions currently open, and what state they're in. For example, Austin's `arc list` may look like:


```wiki
$ arc list
* Needs Revision D4: Add support for faster copies via Intel Extended REP-MOVSB
* Accepted       D13: Make Applicative a superclass of Monad 
```

### Pastebin



To upload things to the Phabricator pastebin, cat the file into `arc paste`:


```wiki
$ cat foo.txt | arc paste
```


Grab pastes using `arc paste` as well:


```wiki
$ arc paste P23
foo and bar!
```

### Email tips



First off, note that **you can reply to an email from Phabricator** to comment on a review; inbound mail handling works fine. In the email you have handler actions, which you can use to control what action to post to a review - for example, replying to an email with `!reject` and some content will comment on the review, and reject it and ask for revisions.



Second, Phab has a very 'Getting Things Done' interface, which means it tries to only alert you as to what is relevant in a particular project or codebase. You can carefully control what emails you get using Herald particularly, but you still may want to crop things. Be sure to [
configure your mail rules](https://secure.phabricator.com/book/phabricator/article/mail_rules/)!


### External editor support



If you want, you can also configure Phab to use an external editor so you can launch things [
right from your browser](https://secure.phabricator.com/book/phabricator/article/external_editor/)!


### Review IRC logs



There are active IRC logs kept on Phabricator using the [
ChatLog](https://phabricator.haskell.org/chatlog/) application


### Multi-factor authentication



If you're paranoid, enable [
multi-factor authentication](https://secure.phabricator.com/book/phabricator/article/multi_factor_auth/) for your account.


### Play Arkanoid



If you're waiting to validate or compile, run `arc anoid` to play a game of Arkanoid. (requires `python2` in `PATH`)


