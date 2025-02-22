# TODO

## Captive portal mechanics

REWRITE THIS BULLET POINT:

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

* Ensa filename:name path file_path etc mm in all code
* Ensa use of naming when it comes to file upploading
* Move marshalling into webapp_rest.erl for post adn everything else too
* Move post body from #post{}   to /var/tmp/bespoke/post/<post_id>/body
* Move post attachments #post{} to /var/tmp/bespoke/post/<post_id>/attachment/<filename>, ...
* Make all erlanf code 100 fill column
* Create db_user_db.erl
* Use ram_db access functions for insert and lookup etc


* Add direct messaging

* Add Signal protocol under the hood

====

* Maybe: Add Contacts - A new top level entry point in indexhtml

  - Make it possible to add/remove all username instances to Contacts with a
    single click
  - Mark all username instances in Contacts
  - Only users in Contacts are suggested when writing a new direct message

* Maybe: Add destructive aging of forum posts, direct messages and files

## Syncing

- Smart database syncing between B3S devices

## Misc

- /README.md: Add a small video instead of a screenshot
