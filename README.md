# Bespoke BBS (B3S) - A location-based BBS

## What?

<b>Bespoke BBS (B3S)</b> is a <i>location-based Bulletin Board
System</i>. It’s not meant for the Internet—instead, it lives in your
local area as a WiFi Access Point. This is by design, encouraging
genuine local interactions.

The first version of B3S includes a straightforward <b>Forum</b>. In
the near future, the Forum will be fleshed out with features you’d
expect—such as attachments, images, and reactions like downvotes (and
whatnot). After that, <b>end-to-end encrypted messaging</b> and
<b>file sharing</b> come next.

Further down the line, the B3S client Web app will be wrapped into
native apps for <b>Android</b> and <b>iPhone</b>. These apps will make
it effortless to sync content in the background just by passing near a
B3S instance. You’ll also be able to interact with B3S offline, with
syncing handled automatically when reconnected.

## Why?

Because the Internet has become a cesspool of mass surveillance,
intrusive advertisements, manipulative algorithms, influencers,
relentless tracking, and endless spam. You know what I mean. Let’s
strip away the noise and keep things <b>local, simple, and
meaningful</b> instead.

## How?

B3S is a self-contained BBS web app running behind its own WiFi Access
Point—no Internet required, ever. The necessary hardware is incredibly
cheap, and installation is almost effortless.

You might wonder if the WiFi range is too short to make this
practical. In most cases, with properly positioned hardware and a
clear line of sight, you can achieve a range of several hundred
meters. Add a low-cost sector antenna, and that range extends up to
four times farther.

### Installation

* $15: Buy a Raspberry Pi Zero 2 W: https://www.adafruit.com/product/5291)
* $10: Buy a SD/MicroSD Memory Card (8 GB SDHC): https://www.adafruit.com/product/1294

Download the latest Bespoke image from
https://github.com/joagre/bespoke/releases and write it to the SD
card. On a Linux machine:

```
gunzip bespoke-0.1.0.img.gz
sudo dd if=bespoke-0.1.0.img of=/dev/mmcblk0 bs=4M status=progress
sync
```

Optionally buy an extra WiFi USB adapter and a Directional Panel
Antenna to extened the range:

* $28: WiFi USB adapter: https://alfa-network.eu/wi-fi/wi-fi-adapters/awus036nhv
* $18: Directional Panel Antenna: https://alfa-network.eu/antennas/wi-fi-antennas/apa-m25

That's it.
