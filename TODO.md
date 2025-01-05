# TODO

* add an attachents field to post record
* move files on server side when creating post
* Add attachments to add_reply + do something with parent
* show a slidenav for top pasts and replies oif their are attachments

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study more
  extensively how real-life captive portals answer to `http/https`
  requests, `dns` lookups and `icmp` pings before and after the user
  login phase. Work has already been been started to solve the
  `http/https` requests and `dns` lookups as seen in
  [webapp_rest.erl](lib/webapp/src/webapp_rest.erl).
  
  `webapp_rest.erl` previously used the experimental
  [dnsmasq-tool](lib/main/bin/dnsmasq-tool) to switch between different
  dns lookup behaviours before and after user login. This is no longer
  done though, because of a show-stopping caveat:

  Caveat: It seems that Android does a request to
  https://www.google.com and strictly verifies the SSL
  certificate. **Duh!** For that reason we will accept that Bespoke
  stays in the captive portal mini-browser for now. If this is too
  annoying, we can write a native app that listens on AP/SSID
  connections and raises a notification + contains a WebView. We will
  build such an app later on anyway (see below).

## Web app

* Add attachment support for forum posts

* Add end-to-end encrypted direct messaging

* Add file sharing

* Add destructive aging of forum posts, direct messages and files

* Add native WebView wrapper apps (for syncing and offline browsing)

### Remember

* Enable SSL-only in Web server

* Sanitize HTML
