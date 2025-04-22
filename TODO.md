# TODO

## Captive portal

* Try to remove visual cruft in the captivice portal browser:
  - On Iphone a fat header is always visible
  - On Android an initial scary warning is shown

## Web app

* Investigate client SSL warnings in the Erlang node
* Add width and height to image attachmens to avoid re-rendering during lazy loading of images
* Add public-key infra structire under the hood ala Signal
* Rewrite the post record in db.hrl to not use lists for replies, likers
  and attachments, i.e. use idets.erl tables instead in db_post_db.erl.
* Development has only been performed on Chrome/Android, Chrome/Debian and
  Safari/Iphone, i.e. more testing is needed. :-)

Later:

* Make it possible to use uploaded files as attachments

## Syncing

* Smart database syncing between B3S devices
