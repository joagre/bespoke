#!/bin/bash

if ! grep -q "Raspberry Pi" /proc/device-tree/model; then
    echo "Exiting: Must run on a Raspberry Pi"
    exit 1
fi

BASE_DIR=/home/pi/bespoke
TARGET_DIR=${BASE_DIR}/target
RUNTIME_DIR=/var/tmp/bespoke
LOG_FILE=${RUNTIME_DIR}/log/boot.log
DNSMASQ_CONF=/etc/dnsmasq.conf

if [ ! -f ${LOG_FILE} ]; then
    touch ${LOG_FILE}
fi

interface=wlan0
ip_address=192.168.4.1
if ip link show wlan1 > /dev/null 2>&1; then
    interface=wlan1
    ip_address=192.168.5.1
fi

echo -n "** $0: Starting hostapd-${interface}: " >> $LOG_FILE
systemctl start hostapd-${interface}
echo "DONE" >> $LOG_FILE

echo -n "** $0: Add packet filter rules: " >> $LOG_FILE
echo sudo iptables -t nat -A PREROUTING -i ${interface} -p tcp --dport 80 -j DNAT --to-destination ${ip_address}:80
echo sudo iptables -t nat -A PREROUTING -i ${interface} -p tcp --dport 443 -j DNAT --to-destination ${ip_address}:443
echo sudo iptables -t nat -A POSTROUTING -o ${interface} -j MASQUERADE
echo "DONE" >> $LOG_FILE

echo -n "** $0: Restarting dnsmasq: " >> $LOG_FILE
echo sudo cp ${TARGET_DIR}/etc/dnsmasq-${interface}.conf /etc/dnsmasq.conf
echo sudo systemctl restart dnsmasq
echo "DONE" >> $LOG_FILE
