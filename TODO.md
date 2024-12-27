# TODO

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https` requests, `dns` lookups and `icmp` pings before and after the user login phase. Work has already been been started to solve the `http/https` requests and `dns` lookups in [webapp_rest.erl](webapp/src/webapp_rest.erl). `webapp_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

Caveat: It seems that Android does a http request to https://www.google.com and verifies that the SSL cerificate is ok. I will accept that Bespoke stays in the captive portal mini-browser. If this is too annoying, we can write a native app that listens on AP/SSID connections and raises a notification + contains a Webview. We need such a app later on anyway.

## Web app

* Add attachment support for Forum posts

* Add end-to-end encrypted direct messaging

* Add file sharing

* Add destructive aging of forum posts, direct messages and files

* Add native WebView wrapper apps (for syncing and offline browsing)

### Remember

* Enable only SSL in Web server

* Sanitize HTML
