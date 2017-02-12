# Using RSS with Trac






Several of the Trac modules support content syndication using the RSS ([
Really Simple Syndication](http://en.wikipedia.org/wiki/RSS)) XML format. RSS pushes out updates to Trac whenever they occur and to whoever has subscribed to it. Using the RSS subscription feature in Trac, you can easily monitor progress of the project, a set of issues or even changes to a single file.



Trac supports RSS feeds in:


- [TracTimeline](trac-timeline) — Use the RSS feed to **subscribe to project events**. Monitor overall project progress in your favorite RSS reader.
- [TracTickets](trac-tickets), [TracReports](trac-reports), and [TracQuery](trac-query) — Allows syndication of report and ticket query results. Be notified about important and relevant issue tickets.
- [TracBrowser](trac-browser) and [TracRevisionLog](trac-revision-log) — Syndication of file changes. Stay up to date with changes to a specific file or directory.

## How to access RSS data



Anywhere in Trac where RSS is available, you should find a small orange **RSS** icon, typically at the bottom of the page:


RSS feed


Clicking the icon will access the RSS feed for that specific resource.



**Note:** Different modules provide different data in their RSS feeds. Usually the syndicated information corresponds to the current view. For example, if you click the RSS link on a report page, the feed will be based on that report. It might be explained by thinking of the RSS feeds as an *alternate view of the data currently displayed*.



Since Trac 1.0 an RSS feed can be retrieved from a Trac site that requires authentication. Hover over the RSS icon, right click and *copy link address*.


## Links


- *Specifications:*

  - [
    http://blogs.law.harvard.edu/tech/rss](http://blogs.law.harvard.edu/tech/rss) — RSS 2.0 Specification.

- *Multi-platform RSS readers:*

  - [
    http://www.rssowl.org/](http://www.rssowl.org/) — Open source, Eclipse-based, RSS reader for Linux, Mac and Windows systems that supports https and authenticated feeds.

- *Linux/BSD/\*n\*x systems:*

  - [
    http://liferea.sourceforge.net/](http://liferea.sourceforge.net/) — Open source GTK2 RSS Reader for Linux.
  - [
    Akregator](http://akregator.sourceforge.net/) — Open source KDE RSS Reader, part of KDE-PIM.

- *Mac OS X systems:*

  - [
    http://ranchero.com/netnewswire/](http://ranchero.com/netnewswire/) — An excellent RSS reader for Mac OS X, has both free and paid versions.
  - [
    http://www.utsire.com/shrook/](http://www.utsire.com/shrook/) — An RSS reader for Max OS X that supports https, even with self signed certificates, and authenticated feeds.
  - [
    http://vienna-rss.sourceforge.net/](http://vienna-rss.sourceforge.net/) — Open source Feed Reader for Mac OS X with smart folders support.

- *Windows systems:*

  - [
    http://www.rssreader.com/](http://www.rssreader.com/) — Free and powerful RSS Reader for MS Windows.
  - [
    http://www.sharpreader.net/](http://www.sharpreader.net/) — A free RSS Reader written in .NET for MS Windows.

- *Firefox:*

  - [
    http://www.mozilla.org/products/firefox/](http://www.mozilla.org/products/firefox/) — Mozilla Firefox features plenty [
    add-ons](https://addons.mozilla.org/en-US/firefox/search/?q=rss&appver=&platform=) for supporting RSS.

---



See also: [TracGuide](trac-guide), [TracTimeline](trac-timeline), [TracReports](trac-reports), [TracBrowser](trac-browser)


