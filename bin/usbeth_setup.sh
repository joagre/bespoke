#!/bin/bash

USB_IF="usbeth"
GATEWAY_IF="wlp2s0"
TARGET_IP="192.168.7.1"

if [ ! -d /sys/class/net/$USB_IF ]; then
    echo "Error: network interface $USB_IF not found"
    exit 1
fi

if [ ! -d /sys/class/net/$GATEWAY_IF ]; then
    echo "Error: network interface $GATEWAY_IF not found"
    exit 1
fi

echo "Setting $TARGET_IP on $USB_IF and enabling IP forwarding"
sudo ip addr add $TARGET_IP/24 dev $USB_IF
sudo ip link set $USB_IF up
sudo sysctl -w net.ipv4.ip_forward=1 > /dev/null
sudo iptables -t nat -A POSTROUTING -o $GATEWAY_IF -j MASQUERADE
sudo iptables -A FORWARD -i $GATEWAY_IF -o $USB_IF -m state --state RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i $USB_IF -o $GATEWAY_IF -j ACCEPT
