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

* Assign a temporary ip address to `enxdecde80060f0` on the host, e.g.

```
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

```
sudo sysctl -w net.ipv4.ip_forward=1
sudo iptables -t nat -A POSTROUTING -o wlp2s0 -j MASQUERADE
sudo iptables -A FORWARD -i wlp2s0 -o enxdecde80060f0 -m state --state RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i enxdecde80060f0 -o wlp2s0 -j ACCEPT
```

If you update the host with a udev rule that always renames
usb-over-ethernet (enXXX) devices to `usbeth`, i.e `SUBSYSTEM=="net", ACTION=="add", DRIVERS=="usb", NAME="usbeth", ENV{NM_UNMANAGED}="1"`, then you can use the
script `./bin/usbeth-setup` to assign an ip address and link to
`usbeth`, as well as create the network config needed for the default
gateway on the Pi to work. Look in the script for details.

## Configure Wifi AP on Pi

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

interface wlan1
static ip_address=192.168.5.1/24
nohook wpa_supplicant
nogateway
```

Restart dhcpcd:

```
sudo service dhcpcd restart
```

Edit `/etc/dnsmasq.conf`:

```
interface=wlan0
dhcp-range=192.168.4.10,192.168.4.100,255.255.255.0,24h
address=/#/192.168.4.1

interface=wlan1
dhcp-range=192.168.5.10,192.168.5.100,255.255.255.0,24h
address=/#/192.168.5.1
```

Umask hostapd and start dnsmasq:

```
sudo systemctl unmask hostapd
sudo systemctl enable dnsmasq
sudo systemctl start dnsmasq
```

Edit `/etc/hostapd/hostapd.conf`:

```
interface=wlan0
driver=nl80211
ssid=BespokeBBS
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
ssid=BespokeBBS1111
hw_mode=g
channel=6
wmm_enabled=0
macaddr_acl=0
auth_algs=1
ignore_broadcast_ssid=0
```

Copy the default (wlan0) hostapd service to `hostapd-wlan1.service`:

```
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

```
sudo apt remove --purge -y wpa_supplicant
sudo rm -f /etc/wpa_supplicant/wpa_supplicant.conf
sudo systemctl disable wpa_supplicant.service
```

## Install and prepare required packages

```
sudo apt install erlang-nox wamerican emacs-nox erlang-mode ntpdate rfkill
sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`
```

## Build, install and config Bespoke BBS

Do this on a build machine:

```
make release
scp build/releases/bespoke-0.1.0.tar.gz pi@bespoke.local:/home/pi/
```

Copy necessary config files to the Pi:

```
scp build/config/bespoke.service pi@bespoke.local:/home/pi/
scp build/config/99-usb-wifi-host.rules pi@bespoke.local:/home/pi/
scp build/config/change-ssid pi@bespoke.local:/home/pi/
scp build/config/blacklist-rfkill.conf pi@bespoke.local:/home/pi/
scp build/config/switch-hostapd.service pi@bespoke.local:/home/pi/
```

Do this on the Pi:

```
sudo ntpdate pool.ntp.org

tar zxvf bespoke-0.1.0.tar.gz
cd bespoke-0.1.0
make install

sudo mv bespoke.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl start bespoke.service
sudo systemctl enable bespoke.service

sudo mv 99-usb-wifi-host.rules /etc/udev/rules.d/
sudo udevadm control --reload
sudo udevadm trigger

sudo mv change-ssid /etc/sudoers.d/change-ssid
sudo chown root:root /etc/sudoers.d/change-ssid
sudo chmod 440 /etc/sudoers.d/change-ssid

sudo mv blacklist-rfkill.conf /etc/modprobe.d/

sudo mv switch-hostapd.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl start switch-hostapd.service
sudo systemctl enable switch-hostapd.service
```

Done!

## Create image

Shrink partition:

```
sudo dd if=/dev/mmcblk0 of=/media/jocke/EXTERNSL/bespoke-0.1.0-32GB.img bs=4M status=progress
sync
sudo apt update && sudo apt install -y wget parted gzip pigz xz-utils udev e2fsprogs
wget https://raw.githubusercontent.com/Drewsif/PiShrink/master/pishrink.sh
./pishrink.sh bespoke-0.1.0-32GB.img bespoke-0.1.0-pishrinked.img
sync
mv bespoke-0.1.0-pishrinked.img bespoke-0.1.0.img
gzip bespoke-0.1.0.img
```

# How to install image on Pi

```
gunzip bespoke-0.1.0.img.gz
sudo dd if=bespoke-0.1.0.img of=/dev/mmcblk0 bs=4M status=progress
sync
```
