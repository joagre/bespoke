# TODO

## Captive portal mechanics

* Verify that the captive portal mechanics works flawlessly. Study more
  extensively how real-life captive portals answer to `http/https` requests.
## Web app

* Add direct messaging frontend

  1 top_messages.html
  2 add_top_message.html
    * add_attachments.html DONE
  3 message.html (flat)
  4 add_reply_message.html
    * add_attachments.html DONE

* Add Signal protocol under the hood
* Later: Add an admin user
* Later: Add Contacts - A new top level entry point in index.html
  - Make it possible to add/remove all username instances to Contacts with a
    single click
  - Mark all username instances in Contacts
  - Only users in Contacts are suggested when writing a new direct message
* Later: Add destructive aging of forum posts, direct messages and files
* Later: Rewrite the post record in db.hrl to not use lists for replies, likers
  and attachments, i.e. use idets.erl tables instead in db_post_db.erl.

## Syncing

- Later: Smart database syncing between B3S devices

## Misc

- /README.md: Add a small video instead of a screenshot
