# TODO

## Captive portal mechanics

* Verify that the captive portal mechanics works flawlessly. Study more
  extensively how real-life captive portals answer to `http/https` requests.

## Web app

* Add direct messaging frontend

  * conclude top_messages.js
  * write message.js
  * write add_reply_message.js

  * update add_top_post.html to use progress and uploade from indexdb
  * update add_reply_post.html to use progress and uploade from indexdb

  * clean add_attachments.js to load locally
    * clean out file uploding from add_attachments.js

  * Make sure all innerHtml is sanitized + in uhtml use

* Refactoring of js:
  * Rearrange commenting in javacript files (take inspiration from add_top_message.js)
  * mark_messages -> mark_messages_as_read
  * rip apart bespoke.js

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
