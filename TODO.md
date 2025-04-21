# TODO

## Captive portal

* Try to remove visual cruft in the captivice portal browser:
  - On Iphone a fat header is always visible
  - On Android an initial scary warning is shown

## Web app

* Add Signal protocol under the hood
* Rewrite the post record in db.hrl to not use lists for replies, likers
  and attachments, i.e. use idets.erl tables instead in db_post_db.erl.

Later:

* Make it possible to use uploaded files as attachments

## Syncing

* Smart database syncing between B3S devices
