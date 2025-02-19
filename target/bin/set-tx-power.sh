# -*- mode: sh; -*-
#!/bin/sh

# exit if wlan1 is down
if [ -z "$(ifconfig | grep wlan1)" ]; then
    echo "wlan1 is down"
    exit 1
fi

sudo ifconfig wlan1 down
echo "==== Before ===="
iwconfig wlan1
sudo iw reg set BO
sudo iwconfig wlan1 txpower 30
#sudo iw dev wlan1 set txpower fixed 3000
sudo ifconfig wlan1 up
echo "==== After ===="
iwconfig wlan1
