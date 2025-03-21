# -*- mode: sh; -*-
#!/bin/sh

grep -q "Raspberry Pi" /proc/cpuinfo || exit 255

# Example usage:
#
# $ dnsmasq-tool /etc/dnsmasq.conf --post 01:11:22:33:44:55 --clear 02:11:22:33:44:55 --stdout --restart-dnsmasq

# Note: This script must be run as root or with sudo. To allow a user
# to run this script as root without a password, add a line such as
# the following to /etc/sudoers:
#
# pi ALL=(ALL) NOPASSWD: /home/pi/src/bespoke/main/bin/dnsmasq-tool

usage () {
    >&2 echo "dnsmasq-tool --help | -h"
    >&2 echo "    Display available options"
    >&2 echo "dnsmasq-tool <DNSMASQ-CONF>"
    >&2 echo "             [--post <MAC-ADDRESS>]"
    >&2 echo "             [--clear <MAC-ADDRESS>]"
    >&2 echo "             [--stdout] [--restart-dnsmasq]"
    >&2 echo "    Add post-login dhcp-host entries or clear them from DNSMASQ-CONF and"
    >2 echo "     optionally restart dnsmasq"
    >&2 echo "dnsmasq-tool <DNSMASQ-CONF> -- clear-all"
    >&2 echo "    Clear *all* post-login dhcp-host entries"
    exit 1
}

is_param () {
    case $1 in
        --*)
            usage;
            ;;
        *)
            ;;
    esac
}

is_mac_address () {
    if ! echo $1 | grep -qE '^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$'; then
        echo "Error: $1 is not a valid MAC address"
        exit 1
    fi
}

log () {
    { echo "$(date) $1" >> "$log_file"; } 2>/dev/null
}

if [ $# -lt 1 ]; then
    usage
fi

log_file="/tmp/dnsmasq-tool.log"
if [ ! -f $log_file ]; then
    touch $log_file 2>/dev/null
fi

dnsmasq_conf=$1
if [ ! -f $dnsmasq_conf ]; then
    echo "Error: $1 is not a valid dnsmasq.conf file"
    exit 1
fi
shift

tmpfile=$(mktemp)
cp $dnsmasq_conf $tmpfile

stdout=false
restart_dnsmasq=false
full_command="$0 $@"

while :; do
    case $1 in
        --help | -h)
            usage
            ;;
	--post)
            if [ $# -lt 2 ]; then
                usage
            fi
            is_param $2
            is_mac_address $2
            # Remove any existing dhcp-host entry for this MAC address
            sed -i "/dhcp-host=$2/d" $tmpfile
            # Add post-login entry
            echo "dhcp-host=$2,set:postlogin" >> $tmpfile
            shift
            ;;
	--clear)
            if [ $# -lt 2 ]; then
                usage
            fi
            is_param $2
            is_mac_address $2
            # Remove any existing dhcp-host entry for this MAC address
            sed -i "/dhcp-host=$2/d" $tmpfile
            shift
            ;;
        --clear-all)
            # Remove *all* post-login dhcp-host entries
            sed -i '/set:postlogin/d' $tmpfile
            ;;
        --stdout)
            stdout=true
            ;;
        --restart-dnsmasq)
            restart_dnsmasq=true
            ;;
        --)
            shift
            break
            ;;
        "")
            break
            ;;
        *)
            usage
    esac
    command shift
done

if [ $stdout = true ]; then
    cat $tmpfile
else
    cp $tmpfile $dnsmasq_conf
fi

rm $tmpfile

log $full_command

if [ $restart_dnsmasq = true ]; then
    log "dnsmasq restarting..."
    sudo systemctl restart dnsmasq
fi

exit 0
