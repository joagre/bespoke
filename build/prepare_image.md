# Prepare a Bespoke image

## Hardware

Raspberry Pi Zero 2W

## OS

Prepare a SD card with Raspbian Pi 64 OS Lite using
`rpi-imager`. Configure a wlan, assign the hostname to bespoke and
enable ssh.

## Enable USB gadget mode on the SD card

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

## Configure ethernet over USB

* Insert SD card into Pi
* Start Pi
* Login to Pi over ssh/WiFi
* Attach a **data** USB cable from Pi to host
* Use `ip addr show` to verify that `usb0` is up on the Pi
* Do the same on the host and verify that `enXXX` is up, where `XXX` is a
  random number like `enxdecde80060f0`.

NOTE: `enxdecde80060f0` will be something else the next time a USB
cable is inserted.

* Prepare dhcpcd on the Pi:

```bash
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

```bash
sudo systemctl enable dhcpcd
sudo systemctl start dhcpcd
```

* Assign a temporary ip address to `enxdecde80060f0` on the host, e.g.

```bash
sudo ip addr add 192.168.7.1/24 dev enxdecde80060f0
sudo ip link set enxdecde80060f0 up
```

NOTE: The network manager on the host may now and then remove this
address. For your own good I suppose. Fucking retard. Renaming
ethernet-over-usb interfaces to `usbeth` solves this. More on that
later.

Test the connection over USB:

* Test `ping 192.168.7.1` on the host
* Test `ping 192.168.7.2` on the Pi

Make it possible for the Pi to use the host as a default gateway. Do
this on the host:

```bash
sudo sysctl -w net.ipv4.ip_forward=1
sudo iptables -t nat -A POSTROUTING -o wlp2s0 -j MASQUERADE
sudo iptables -A FORWARD -i wlp2s0 -o enxdecde80060f0 -m state --state RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i enxdecde80060f0 -o wlp2s0 -j ACCEPT
```

If you update the host with a udev rule that always renames
usb-over-ethernet (enXXX) devices to `usbeth`, i.e `SUBSYSTEM=="net", ACTION=="add", DRIVERS=="usb", NAME="usbeth", ENV{NM_UNMANAGED}="1"`, then you can use the
script `./bin/usbeth_setup.sh` to assign an ip address and link to
`usbeth`, as well as create the network config needed for the default
gateway on the Pi to work. Look in the script for details.

## Configure Wifi AP on Pi

Install hostapd and dnsmasq and stop them temporarily:

```bash
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

interface wlan1
static ip_address=192.168.5.1/24
nohook wpa_supplicant
nogateway
```

Restart dhcpcd:

```bash
sudo service dhcpcd restart
```

Edit `/etc/dnsmasq.conf`:

```
interface=wlan0
dhcp-range=192.168.4.10,192.168.4.100,255.255.255.0,24h
address=/#/192.168.4.1
no-resolv
no-poll

interface=wlan1
dhcp-range=192.168.5.10,192.168.5.100,255.255.255.0,24h
address=/#/192.168.5.1
no-resolv
no-poll
```

Unmask hostapd and start dnsmasq:

```bash
sudo systemctl unmask hostapd
sudo systemctl enable dnsmasq
sudo systemctl start dnsmasq
```

Edit `/etc/hostapd/hostapd.conf`:

```
interface=wlan0
driver=nl80211
ssid=AcmeHub
hw_mode=g
channel=1
wmm_enabled=0
macaddr_acl=0
auth_algs=1
ignore_broadcast_ssid=0
```

Edit `/etc/hostapd/hostapd-wlan1.conf`:

```
interface=wlan1
driver=nl80211
ssid=AcmeHub
hw_mode=g
channel=6
wmm_enabled=0
macaddr_acl=0
auth_algs=1
ignore_broadcast_ssid=0
```

Copy the default (wlan0) hostapd service to `hostapd-wlan1.service`:

```bash
sudo cp /lib/systemd/system/hostapd.service /etc/systemd/system/hostapd-wlan1.service
```

Update `/etc/systemd/system/hostapd-wlan1.service`:

```
[Unit]
Description=Access point and authentication server for Wi-Fi and Ethernet (wlan1)
Documentation=man:hostapd(8)
After=network.target
ConditionFileNotEmpty=/etc/hostapd/hostapd_wlan1.conf

[Service]
Type=forking
PIDFile=/run/hostapd_wlan1.pid
Restart=on-failure
RestartSec=2
Environment=DAEMON_CONF=/etc/hostapd/hostapd_wlan1.conf
EnvironmentFile=-/etc/default/hostapd
ExecStart=/usr/sbin/hostapd -B -P /run/hostapd_wlan1.pid $DAEMON_OPTS ${DAEMON_CONF}

[Install]
WantedBy=multi-user.target
```

Remove wpa_supplicant:

```bash
sudo apt remove --purge -y wpa_supplicant
sudo rm -f /etc/wpa_supplicant/wpa_supplicant.conf
sudo systemctl disable wpa_supplicant.service
```

## Install and prepare required packages

```bash
sudo apt install erlang-nox wamerican emacs-nox erlang-mode ntpdate rfkill
sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`
```

## Build, install and config Bespoke BBS

Do this on a build machine:

```bash
make release
scp build/releases/bespoke-0.9.0.tar.gz pi@bespoke.local:/home/pi/
```

Do this on the Pi:

```bash
sudo ntpdate pool.ntp.org
tar zxvf bespoke-0.9.0.tar.gz
cd bespoke-0.9.0
make install os-install
```

Done!

## Create image

NOTE: Check  that /dev/sda is the SD card on your machine!!!

Shrink partition:

```bash
sudo -s
dd if=/dev/sda of=/media/jocke/EXTERNSL/bespoke-0.9.0-full.img bs=4M status=progress
sync
apt update && sudo apt install -y wget parted gzip pigz xz-utils udev e2fsprogs
cd /media/jocke/EXTERNSL
wget https://raw.githubusercontent.com/Drewsif/PiShrink/master/pishrink.sh
chmod +x pishrink.sh
./pishrink.sh bespoke-0.9.0-full.img bespoke-0.9.0-pishrinked.img
sync
mv bespoke-0.9.0-pishrinked.img bespoke-0.9.0.img
gzip bespoke-0.9.0.img
```

## Create a release on github

```bash
git tag -a v0.9.0 -m "Release v0.9.0"
git push origin v0.9.0
```

Go to the github webui and draft a new release.

It is safer to upload a new release image using gh:

```bash
gh release upload v0.9.0 /media/jocke/EXTERNSL/bespoke-0.9.0.img.gz
```

Note1: Uploading a release from the github webui is hell.

Note2: There is a 2GB cap on public/free github accounts and that cannot be
changed (even paying github lots of money)

# How to install image on Pi

```bash
gunzip bespoke-0.9.0.img.gz
sudo dd if=bespoke-0.9.0.img of=/dev/sda bs=4M status=progress
sync
```
