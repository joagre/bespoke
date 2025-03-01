# TODO

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study more
  extensively how real-life captive portals answer to `http/https`
  requests, `dns` lookups and `icmp` pings before and after the user
  login phase.

## Web app

* Add line numbers etc in main.erl

* Move marshalling into webapp_rest.erl for post adn everything else too
* Move post attachments #post{} to /var/tmp/bespoke/post/<post_id>/attachment/<filename>, ...
* Create db_user_db.erl etc for all
* Use ram_db access functions for insert and lookup etc
* bagify read cache och generalisera den föý mer användning




* Add direct messaging frontend
* Add Signal protocol under the hood
* Maybe: Add an admin user
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
