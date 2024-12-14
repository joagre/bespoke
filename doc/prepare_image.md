# Hardware

Raspberry Pi Zero 2W

# OS

Prepare a SD card with Raspberry Pi 64 OS Lite using
`rpi-imager`. Configure a wlan, assign the hostname to bespoke and
enable ssh.

# Enable USB gadget mode on the SD card

Edit `bootfs/config.txt`:

```
[all]
dtoverlay=dwc2
```

NOTE: A USB keyboard etc will no longer work

Edit `rootfs/etc/modules`:

```
dwc2
g_ether
```

# Configure ethernet over USB

* Insert SD card into Pi
* Start Pi
* Login to Pi over ssh/WiFi
* Attach a **data** USB cable from Pi to host
* Use `ip addr show` to verify that `usb0` is up on the Pi
* Do the same and host and verift `enXXX` is up, where `XXX` is a
  random number like `enxdecde80060f0`.

NOTE: `enxdecde80060f0` will be something else the next time a USB
cable is inserted.

* Prepare dhcpcd on the Pi:

```
sudo systemctl stop NetworkManager
sudo systemctl disable NetworkManager
sudo systemctl mask NetworkManager
sudo apt install dhcpcd5
```

* Edit `/etc/dhcpcd.conf` on the Pi:

```
interface usb0
static ip_address=192.168.7.2/24
static routers=192.168.7.1
static domain_name_servers=8.8.8.8 8.8.4.4
```

* Start dhcpcd on the Pi:

```
sudo systemctl enable dhcpcd
sudo systemctl start dhcpcd
```

* Assign a temporary ip address to `enxdecde80060f0`, e.g.

```
sudo ip addr add 192.168.7.1/24 dev enxdecde80060f0
sudo ip link set enxdecde80060f0 up
```

NOTE: The network manager on the host may now and then remove this
address. For your own good I suppose. Fucking retard. Renaming
ethernet-over-usb interfaces to `usbeth` solves this. More on that
later.

Test the connection over ISB

* Test `ping 192.168.7.1` on the host
* Test `ping 192.168.7.2` on the Pi

Make it possible for the Pi to use the host as a default gateway. Do
this on the host:

```
sudo sysctl -w net.ipv4.ip_forward=1
sudo iptables -t nat -A POSTROUTING -o wlp2s0 -j MASQUERADE
sudo iptables -A FORWARD -i wlp2s0 -o enxdecde80060f0 -m state --state RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i enxdecde80060f0 -o wlp2s0 -j ACCEPT
```

If you update the host with a udev rule that always renames
usb-over-ethernet (enXXX) devices to `usbeth`, i.e `SUBSYSTEM=="net",
ACTION=="add", DRIVERS=="usb", NAME="usbeth"`, you can use the script
`./bin/usbeth-setup` to assign an ip address and link to `usbeth`, as
well create the config neededed for the default gateway on the Pi to
work. Look in the script for details.

# Configure Wifi AP on Pi

Install hostapd and dnsmasq and stop them temporarily:

```
sudo apt update
sudo apt install -y hostapd dnsmasq
sudo systemctl stop hostapd
sudo systemctl stop dnsmasq
```

Edit `/etc/dhcpcd.conf`:

```
interface wlan0
static ip_address=192.168.4.1/24
nohook wpa_supplicant
nogateway
```

Restart dhcpcd:

```
sudo service dhcpcd restart
```

Backup dnsmasq.conf:

```
sudo cp /etc/dnsmasq.conf /etc/dnsmasq.conf.orig
```

Edit `/etc/dnsmasq.conf`:

```
interface=wlan0
dhcp-range=192.168.4.10,192.168.4.100,255.255.255.0,24h
```

Edit `/etc/hostapd/hostapd.conf`:

```
interface=wlan0
driver=nl80211
ssid=BespokeBBS
hw_mode=g
channel=7
wmm_enabled=0
macaddr_acl=0
auth_algs=1
ignore_broadcast_ssid=0
```

Edit `/etc/default/hostapd`:

```
DAEMON_CONF="/etc/hostapd/hostapd.conf"
```

Start and enable hostapd and dnsmasq:

```
sudo systemctl unmask hostapd
sudo systemctl enable hostapd
sudo systemctl enable dnsmasq
sudo systemctl start hostapd
sudo systemctl start dnsmasq
```

Remove wpa_supplicant:

```
sudo apt remove --purge -y wpa_supplicant
sudo rm -f /etc/wpa_supplicant/wpa_supplicant.conf
sudo systemctl disable wpa_supplicant.service
```

# Route all traffic to the portal

# Install required packages

# Build and install Bespoke BBS

Touch /var/tmp/bespoke.bootstrap

# Create image

# How to install image on Pi
