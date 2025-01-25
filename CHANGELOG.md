# Changelog

# 0.2.2

### Fixed [0.2.2]

- A bug made it impossible to add attachments to posts (sigh!)

### Added [0.2.2]

- All HTML and Javascript files are now signed during the building of Bespoke
  release images. The browser never loads any HTML or Javascript that has a bad
  signature. This check is done by a pinned (semi-permanently cached) Service
  Worker in the browser which is made permanent in the browser cache at a single
  point in time (ideally before the server takeover :-). If the server is taken
  over after the pinning occured no malicious HTML and Javascript can be loaded
  into the browser. A malicious attacker can obviously alter the REST API on the
  server but all forum posts are signed and direct messages are stored as
  encrypted blobs (produced with the private key each user has in the browser's
  local storage).

- The Bespoke web server is a now a good citizen when it comes to Accept-Encoding
  and Transfer-Encoding headers, i.e. third-party (and big) Javascript files are
  now gzipped during transfer to the browser.

- The Service Worker mentioned above additionally cache all HTML and JavaScript
  files in the browser (until their signatures are changed).

- SSL has been enabled with a self-signed certificate, or else Service Workers
  refuse to start.

## [0.2.1]

### Fixed [0.2.1]

- Complete overhaul of the captive portal logic

- Complete overhaul of read/unread handling of posts in the forum

## [0.2.0]

### Fixed [0.2.0]

- Fixed a serious subscription bug which caused the browser to hang

### Added [0.2.0]

- If an USB WiFi adapter is connected during boot it will be used
  instead of the built-in WiFi card

- Forum posts can now have attachments

### Changed [0.2.0]

- Complete overhaul of the look and feel

- Moved Argon2 password hashing from the server to the browser,
  i.e. the server will **never** perform any encryption from now on.

## [0.1.0]

Initial release
