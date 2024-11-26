# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https`, `dns` lookups and `icmp/pings` before and after the user login phase. Work has already been been started to solve the `http/https` and `dns` lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup behaviours before and after user login.

## Web app

* When a user accesses a Bespoke BBS the first time (and enters the login web page) the MAC address is stored and associated with an auto-generated letter based username of the MAC address. The next time he/she is automatically associated with that auto-generated username on login.

* Make it possible for a user to create a password protected custom username instead of the auto-generated ditto.

* When a user sifts through *posts* their *comments* are automatically marked as read (as the web page scrolls by). The next time a user visits the Bespoke BBS, *posts* that he/she has been accessing will be marked as unread (if there are new *comments* added to them). If a user clicks on such a *post* he/she is automatically taken to the *comments*, but is scrolled down to the unread *comments*. Rinse and repeat.

* Direct *messages* between users are just special cases of *posts*. We keep them in a separate tab and call them "Messages". "Messages" are private to a user but has the same usability semantics as *posts*.

* Add closed *posts*. Members of a closed *posts* just share a password for now. To add a new user to a closed *post*, members propagates the password offline (or whatever).

* Add a HTTP GET back channel for asynchronous updates done by other users, i.e. a websocket is overkill for now (maybe forever).

* Maybe ask Kim to improve the layout. Just a little bit for now: Margins, paddings, fonts and coloring. Just very low hanging fruits. More can be done at a later point in time.
