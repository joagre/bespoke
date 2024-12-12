# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https` requests, `dns` lookups and `icmp` pings before and after the user login phase. Work has already been been started to solve the `http/https` requests and `dns` lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

Caveat: It seems that Android does a http request to https://www.google.com and verifies that the SSL cerificate is ok. I will accept that Bespoke stays in the captive portal mini-browser. If this is too annoying, we can write a native app that listens on AP/SSID connections and raises a notification + contains a Webview. We need such a app later on anyway.

## Web app

* Add aging of *posts*. This is done by removing *posts* when the they
  reach a certain age (e.g. 30 days). Optional. [later]

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
