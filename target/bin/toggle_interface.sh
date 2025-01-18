#!/bin/bash

set -euo pipefail

if ! grep -q "Raspberry Pi" /proc/device-tree/model; then
    echo "Exiting: Must run on a Raspberry Pi"
    exit 1
fi

BASE_DIR=/home/pi/bespoke
TARGET_DIR=${BASE_DIR}/target
RUNTIME_DIR=/var/tmp/bespoke
LOG_FILE=${RUNTIME_DIR}/log/boot.log
DNSMASQ_CONF=/etc/dnsmasq.conf

run_cmd() {
  local output
  local timestamp
  timestamp=$(date "+%Y-%m-%d %H:%M:%S")

  echo -n "[$timestamp] $0: $*: " >> "$LOG_FILE"
  if ! output=$("${@}" 2>&1); then
    echo "FAILED - $output" >> "$LOG_FILE"
    exit 1
  fi
  echo "DONE" >> "$LOG_FILE"
}

mkdir -p "$(dirname "$LOG_FILE")"
touch "$LOG_FILE"

interface=wlan0
ip_address=192.168.4.1
if ip link show wlan1 > /dev/null 2>&1; then
    interface=wlan1
    ip_address=192.168.5.1
fi

echo "** $0: Using interface ${interface} with IP ${ip_address}" >> "$LOG_FILE"

# Restart hostapd
run_cmd systemctl restart hostapd-${interface}

# Configure iptables (clear first)
run_cmd iptables -t nat -F
run_cmd iptables -t nat -X
run_cmd iptables -t nat -A PREROUTING -i "${interface}" -p tcp --dport 80 -j DNAT --to-destination "${ip_address}:80"
run_cmd iptables -t nat -A PREROUTING -i "${interface}" -p tcp --dport 443 -j DNAT --to-destination "${ip_address}:443"
run_cmd iptables -t nat -A POSTROUTING -o "${interface}" -j MASQUERADE

# Restart dnsmasq
run_cmd install -m 644 "${TARGET_DIR}/etc/dnsmasq-${interface}.conf" "${DNSMASQ_CONF}"
run_cmd systemctl restart dnsmasq
