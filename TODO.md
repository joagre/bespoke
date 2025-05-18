# TODO

J = Joakim
L = Later

## Captive portal

* On Iphone a fat header is always visible
  - I have not figured out how to break free from the captive portal browser
  - Make sure that upload of attachments work as it should
* On Android an initial scary warning is shown when we leave the captive portal browser
  - The built-in file uploader widget cannot be opened in the captive portal browser

## Web app

Client:

J Remove Truth Social vibes
J Verify that Firefox/Linux works
L Make it possible to use uploaded files as attachments
L Refactor bespoke.js into several distinct singletons

Server:

* Investigate client SSL warnings in the Erlang node
L Rewrite the post record in db.hrl to not use lists for replies, likers
  and attachments, i.e. use dets indexes instead as provided by idets.erl tables
  instead in db_post_db.erl.
L Create downsized thumbnails of image attachments in Forum
L Show image attachments in the Inbox as thumbnails (must be generated on the
  fly in the browser)

Both:

J Add a settings menu item when logged in as admin:
  - Configurable: Title text (Use a nice generic title)
  - Configurable: About text (Use a nice generic text)
J Add width and height to image attachments to avoid re-rendering of the DOM
  during lazy loading of images in the Forum
J Add public-key infra structire under the hood ala Signal

## Syncing

* Smart database syncing between B3S devices (needs extensions to the web app
  as well to make it toggle between offline b3s devices)
