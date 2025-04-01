# TODO

## Captive portal mechanics

* Verify that the captive portal mechanics works flawlessly. Study more
  extensively how real-life captive portals answer to `http/https` requests.

## Web app

* Add direct messaging frontend
  * update add_top_post.html to use progress and upload from indexdb
  * update add_reply_post.html to use progress and upload from indexdb
  * clean out file uploding from add_attachments.js

message_lib.js
add_top_message.js
add_reply_message.js
message.js
top_messages.js
has_been_read_divider.js
progress.js
add_top_post.js
add_reply_post.js
post_lib.js


top_posts.js
post.js

add_attachments.js





make sure that no extra includes are done after cleanup

resten...

* Refactoring of js:
  * Rearrange commenting in javacript files (take inspiration from add_top_message.js etc)

* Refactoring of erl:
  * Rearrange commenting in erlang files (take inspiration from add_top_message.js etc)

* Check iphone

* Verify that the whole enchilda works on Pi

* Build release

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
