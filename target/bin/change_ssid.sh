#!/bin/bash

grep -q "Raspberry Pi" /proc/cpuinfo || exit 255

usage () {
    >&2 echo "Usage: change_ssid.sh <SSID>"
    >&2 echo "       Update SSID in /etc/hostapd/hostapd.conf and reboot"
    >&2 echo "       Example: change-ssid MyWiFi123"
    exit 1
}

is_ssid () {
    [[ $1 =~ ^[a-zA-Z0-9 _.-]+$ ]]
}

if [ $# -ne 1 ]; then
    usage
fi

if ! is_ssid "$1"; then
    usage
fi

interface="wlan0"
if systemctl is-active --quiet hostapd-wlan1.service; then
    interface="wlan1"
fi

KEY_TO_UPDATE="ssid"
hostapd_conf="/etc/hostapd/hostapd-${interface}.conf"
new_ssid=$1

# Create temporary file with cleanup
tmpfile=$(mktemp) || { echo "Failed to create temp file"; exit 1; }
trap 'rm -f "$tmpfile"' EXIT

# Update SSID
sed "s/^${KEY_TO_UPDATE}=.*/${KEY_TO_UPDATE}=${new_ssid}/" "${hostapd_conf}" > "$tmpfile"
mv "$tmpfile" "$hostapd_conf" || { echo "Failed to update configuration"; exit 1; }

# Restart hostapd service
systemctl restart "hostapd-${interface}" || { echo "Failed to restart hostapd"; exit 1; }
