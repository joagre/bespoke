# Hardware

Raspberry Pi Zero 2W

# OS

Prepare a SD card with Raspberry Pi 64 OS Lite using
`rpi-imager`. Configure a wlan, assign the hostname to b3s and enable
ssh.

# Enable USB gadget mode on the SD card

* Add `dtoverlay=dwc2` to `bootfs/config.txt` under `[all]`
* Add `dwc2` and `g_ether` to `rootfs/etc/modules`

# Configure ethernet over USB

* Insert SD card into Pi
* Boot
* Login to Pi over ssh/WiFi
* Attach a **data** USB cable from Pi to host
* On Pi: Use `ip addr show` to verify that `usb0` is up
* On host: Use `ip addr show` to verify that `enXXX` is up, where
`XXX` is a random number like `enxdecde80060f0`
* On Pi: Prepare dhcpcd:

```
sudo systemctl stop NetworkManager
sudo systemctl disable NetworkManager
sudo systemctl mask NetworkManager
sudo apt install dhcpcd5
```

* On Pi: Add this to `/etc/dhcpcd.conf`:

```
interface usb0
static ip_address=192.168.7.2/24
static routers=192.168.7.1
static domain_name_servers=8.8.8.8 8.8.4.4
```

* On Pi: Start dhcpcd:

```
sudo systemctl enable dhcpcd
sudo systemctl start dhcpcd
```

* On host: Temporary assign an ip address to `enxdecde80060f0`, e.g.

```
sudo ip addr add 192.168.7.1/24 dev enxdecde80060f0
sudo ip link set enxdecde80060f0 up
```

* On host: `ping 192.168.7.1`
* On Pi: `ping 192.168.7.2`
* On host: Temporary let Pi use the host as a default gateway:

```
sudo sysctl -w net.ipv4.ip_forward=1
sudo iptables -t nat -A POSTROUTING -o wlp2s0 -j MASQUERADE
sudo iptables -A FORWARD -i wlp2s0 -o enxdecde80060f0 -m state --state RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i enxdecde80060f0 -o wlp2s0 -j ACCEPT
```

NOTE: The script `./bin/prepare-bespoke-usb` assigns an ip address and
link to `enXXX` and creates all config needed for the default gateway
on the Pi to work. Look in the script for details.

Furthermore, make sure to disable the default gatyeway over
wlan0. usb0 should be the only default gateway.

# Configure Wifi AP
