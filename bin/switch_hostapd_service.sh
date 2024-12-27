#!/bin/bash

# Check if wlan1 is available
if ip link show wlan1 > /dev/null 2>&1; then
    echo "wlan1 detected. Using hostapd-wlan1.service."
    systemctl stop hostapd-wlan0.service
    systemctl disable hostapd-wlan0.service
    systemctl enable hostapd-wlan1.service
    systemctl start hostapd-wlan1.service
else
    echo "wlan1 not detected. Using hostapd-wlan0.service."
    systemctl stop hostapd-wlan1.service
    systemctl disable hostapd-wlan1.service
    systemctl enable hostapd-wlan0.service
    systemctl start hostapd-wlan0.service
fi
