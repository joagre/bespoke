# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https` requests, `dns` lookups and `icmp` pings before and after the user login phase. Work has already been been started to solve the `http/https` requests and `dns` lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

## Web app

* When a user login the first time (and enters the login web page) the MAC address is stored and associated with an auto-generated letter based username of the MAC address. The next time he/she logins he/she is automatically associated with the previously auto-generated username.

* Make it possible for a user to create a password protected custom username instead of the auto-generated ditto.

* Add a HTTP GET back channel for asynchronous updates done by other users, i.e. a websocket is overkill for now (maybe forever).

* When a user sifts through *posts* and their *comments* they are automatically marked as read (as the web page scrolls by). The next time a user logins, *posts* that he/she has been accessing will be marked as unread (if there are new *comments* added to them). If a user clicks on such a *post* he/she is automatically taken to the *comments*, but is scrolled down to the unread *comments*. Rinse and repeat.

* Add closed *posts*. Members of a closed *posts* just share a password for now. To add a new user to a closed *post*, members propagates the password offline (or whatever).

* Add direct *messages* between users. They are just special cases of closed *posts*. We keep them in a separate tab and call them "Messages".

* Maybe ask Kim to improve the layout. Just a little bit for now: Margins, paddings, fonts and coloring. Just very low hanging fruits. More can be done at a later point in time.
