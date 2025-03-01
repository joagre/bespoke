# TODO

## Captive portal mechanics

* Make the captive portal mechanics work flawlessly. Study more
  extensively how real-life captive portals answer to `http/https`
  requests, `dns` lookups and `icmp` pings before and after the user
  login phase.

## Web app

* move all webapp marshalling into webapp_marshallingh.erl

* Create db_user_db.erl etc for all
* db_file_db.erl
* db_post_db.erl
* make sure to not marshall anything in db_serv.erl and db_user_serv.erl (you get it)
  * Move marshalling into webapp_rest.erl for post adn everything else too



* Move post attachments #post{} to /var/tmp/bespoke/post/<post_id>/attachment/<filename>, ...





* Add direct messaging frontend
* Add Signal protocol under the hood
* Later: Add an admin user
* Later: Add Contacts - A new top level entry point in indexhtml
  - Make it possible to add/remove all username instances to Contacts with a
    single click
  - Mark all username instances in Contacts
  - Only users in Contacts are suggested when writing a new direct message
* Later: Add destructive aging of forum posts, direct messages and files

## Syncing

- Later: Smart database syncing between B3S devices

## Misc

- /README.md: Add a small video instead of a screenshot
