# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https` requests, `dns` lookups and `icmp` pings before and after the user login phase. Work has already been been started to solve the `http/https` requests and `dns` lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

Caveat: It seems that Android does a http request to httpd://www.google.com and verifies that the cerificate is ok. I will accept that Bespoke stays n the captive portal mini-browser. If this is too annoying, users we can write a natove app tat listsens on ssid connections and raises a notification + a Webview. We need such a app later on anuway.

## Web app

* Check that captive portal worls for android and Ubunti/Chrome

* Add /switch_user.html

* Add /change_password.html

* Update and verify that /login.html works

* Clean out db_serv.erl and db_rest.erl (from stale auth code)

* Make sure the session-id is used for all operations performed in the app, i.e. creating and deleting posts and comments etc. Remember to add a trash action to top_posts.html.

* Add a HTTP GET back channel for asynchronous updates done by other users, i.e. a websocket is overkill for now (maybe forever).

* When a user sifts through *posts* and their *comments* they are automatically marked as read (as the web page scrolls by). The next time a user logins, *posts* that he/she has been accessing will be marked as unread (if there are new *comments* added to them). If a user clicks on such a *post* he/she is automatically taken to the *comments*, but is scrolled down to the unread *comments*. Rinse and repeat.

* Add closed *posts*. Members of a closed *posts* just share a password for now. To add a new user to a closed *post*, members propagates the password offline (or whatever).

* Add direct *messages* between users. They are just special cases of closed *posts*. We keep them in a separate tab and call them "Messages".

* Maybe ask Kim to improve the layout. Just a little bit for now: Margins, paddings, fonts and coloring. Just very low hanging fruits. More can be done at a later point in time.

* Native WebView wrapper app (for syncing etc)
