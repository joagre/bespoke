# TODO

## Captive portal mechanics

## Misc

- /README.md: Add a small video instead of a screenshot

## Web app

* Refactoring of erl:
  * Rearrange commenting in erlang files (take inspiration from add_top_message.js etc)
* Check iphone
* Verify that the whole enchilda works on Pi
* Build release
* Extend reddit extraction to include media

* Add Signal protocol under the hood
* Add an admin user

* Maybe: Add Contacts - A new top level entry point in index.html
  - Make it possible to add/remove all username instances to Contacts with a
    single click
  - Mark all username instances in Contacts
  - Only users in Contacts are suggested when writing a new direct message
* Maybe: Add destructive aging of forum posts, direct messages and files
* Maybe: Rewrite the post record in db.hrl to not use lists for replies, likers
  and attachments, i.e. use idets.erl tables instead in db_post_db.erl.

## Syncing

- Smart database syncing between B3S devices
