# A suggested TODO list

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study how existing captive portal answers to `http/https`, `dns` lookups and `icmp/pings` before and after the user *login* phase. Work has already been been started to solve the http/https and dns lookups in [db_rest.erl](db/src/db_rest.erl). `db_rest.erl` uses the [dnsmasq-tool](main/bin/dnsmasq-tool) to switch between different dns lookup beahviours before and after user *login*.

## Web app

* When a user accesses a Bespoke BBS the first time (and enters the the *login* web page) the MAC address is stored and a associated with an auto-generated letter based username of the MAC address. The next time he/she is automatically associated with that auto-generated username.

* Make it possible for a user to create a password protected custom username instead of the auto-generated ditto.

* When a user sifts through posts (and comments to posts) they are automatically marked as read (as the web page scrolls by). The next time the user visits the Bespoke BBS posts that he/she has been accessing will be marked as unread (if there are new comments added to it). If a user clicks on such a post he/she is automatically taken to to comments and scrolled down to the unread comments. Rinse and repeat.

* Direct message is just a special case of posts. We can keep them in a separate tab, i.e. a user see two tabs: "Posts" and "Messages". "Messages" are private to the user.

* Add closed groups. Use a shared groups password for now. To add a new user the group members propagates that password offline (or whatever).

* Add a HTTP GET back channel for asynchronous updates done by others, i.e. a websocket is overkill for now (maybe forever).
