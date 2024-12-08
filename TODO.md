# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https` requests, `dns` lookups and `icmp` pings before and after the user login phase. Work has already been been started to solve the `http/https` requests and `dns` lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

Caveat: It seems that Android does a http request to https://www.google.com and verifies that the SSL cerificate is ok. I will accept that Bespoke stays in the captive portal mini-browser. If this is too annoying, we can write a native app that listens on AP/SSID connections and raises a notification + contains a Webview. We need such a app later on anyway.

## Web app

* When a user sifts through *posts* and their *comments* they are
  automatically marked as read (as the web page scrolls by). The next
  time a user logins, *posts* that he/she has been accessing will be
  marked as unread (if there are new *comments* added to them). If a
  user clicks on such a *post* he/she is automatically taken to the
  *comments*, but is scrolled down to the unread *comments*. Rinse and
  repeat.

* Add a HTTP GET back channel for asynchronous updates done by other
  users, i.e. a websocket is overkill for now (maybe forever),
  i.e. one subscribe(postId) allowed per session-id.

* Make sure refresh of post.html correctly once a minute.

* Mark top posts if there are unread comments.

* Add aging of *posts*. This is done by removing *posts* when the they
  reach a certain age (e.g. 30 days). Optional.

* Attachments [later]

* File sharing [later]

* Add public key encryption for *posts*, *messages* and *files* [later]

* Add direct *messages* between users. Messages are encrypted blobs in
  dets (public key encrypted). Support for multiple recipients
  (uniquely encrypted for each recipient). [later]

* Native WebView wrapper app (for syncing etc) [later]

### Remember

* Enable SSL in Web server

* Sanitize HTML

* Maybe ask Kim to improve the layout. Just a little bit for now: Margins, paddings, fonts and coloring. Just very low hanging fruits. More can be done at a later point in time.
