# Bespoke BBS (B3S) – Offline by Design

![Feature Overview](doc/screenshot.png)

## What is Bespoke BBS (B3S)?

Bespoke BBS (B3S) is a local, offline communication hub inspired by traditional Bulletin Board Systems. It features a *Forum*, end-to-end encrypted *Direct Messaging*, and easy-to-use *File Sharing*.

B3S isn't built for the internet. Instead, it runs locally as a self-contained appliance providing its own dedicated WiFi network.

Each user can carry their personal B3S appliance, which automatically synchronizes with all (or selected) B3S appliances that come within range. This means interactions can happen entirely offline, with appliances syncing seamlessly in the background when they meet again.

So in essence, a B3S applicance can be setup as a stationary service or be carried around as a mobile ditto. You can even set up a B3S appliance in your car, allowing you to connect with other users while on the move. This opens up a world of possibilities for local communication, whether you're at home, at work, or on the go.

## Why Use B3S?

The modern internet has become overwhelmed by mass surveillance, intrusive algorithms, endless spam, and noise. B3S offers a simpler alternative by keeping interactions local, secure, and meaningful.

## How Does It Work?

B3S runs as a self-contained appliance behind its own WiFi Access Point—no internet connection required. The hardware needed is inexpensive, and setup is straightforward.

Concerned about WiFi range? Typically, with a clear line-of-sight, B3S achieves hundred meters of coverage. If needed, adding a low-cost sector antenna can extend the range.

The appliance frontend is a straightforward web app running directly in your browser, automatically launching as a captive portal when clients connect to the B3S SSID. Users can also install B3S as a Progressive Web App (PWA), making it behave similarly to a native app. PWAs launch from your home screen or app drawer and can support offline capabilities.

### Security

All direct messages stored on the B3S appliance backend are kept encrypted as opaque blobs, ensuring message confidentiality even if the appliance is physically accessed.

All encryption used in direct messaging is handled exclusively within your browser through public key cryptography. For added security:

- Private keys are securely stored in your browser's built-in key storage.
- HTML and JavaScript files served from the B3S appliance are cryptographically signed. If any tampering occurs, browsers will refuse to load compromised code.

While you initially trust the B3S appliance during setup, all subsequent software updates require explicit acknowledgment. If a PWA is installed offline, potential attack vectors become even smaller.

### Installation

You'll need:

- **Raspberry Pi Zero 2W** (\~\$15): [Adafruit Product Page](https://www.adafruit.com/product/5291) (or similar)
- **MicroSD Card (8 GB SDHC)** (\~\$5): [Adafruit Product Page](https://www.adafruit.com/product/1294) (or similar)

**Steps:**

1. Download the latest Bespoke image from the [GitHub releases page](https://github.com/joagre/bespoke/releases).

2. Write the downloaded image to your SD card. On Linux:

NOTE: Verify carefully that your SD card sits on /dev/sda!!!

```bash
gunzip bespoke-0.9.0.img.gz
sudo dd if=bespoke-0.9.0.img of=/dev/sda bs=4M status=progress
sync
```

3. Insert the SD card into your Raspberry Pi, power it up, connect to the `AcmeHub` WiFi network, and follow the setup instructions provided.

That's it.

### Optional Hardware for Extended Range

To boost your WiFi coverage, consider these affordable additions:

- **WiFi USB Adapter (\~\$28):** [Alfa Network AWUS036NHV](https://alfa-network.eu/wi-fi/wi-fi-adapters/awus036nhv) (or similar)
- **Directional Panel Antenna (\~\$18):** [Alfa Network APA-M25](https://alfa-network.eu/antennas/wi-fi-antennas/apa-m25) (or similar)

### Optional 3D-Printed Case

Enhance your setup by 3D printing a case for your Raspberry Pi Zero:

- [Raspberry Pi Zero Case on Thingiverse](https://www.thingiverse.com/thing:1167846) (or similar)

# Credits (Such as They Are)

Cobbling by **The Bespoke Team**.
No new inventions, no shiny breakthroughs – just time‑tested parts bolted together until they blinked.
If it runs, thank the pioneers who did the real work; if it crashes, enjoy the retro vibes.

![The Bespoke team](doc/the-team.gif)
