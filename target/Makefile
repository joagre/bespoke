BASE_DIR=/home/pi/bespoke
JS_DIR=$(BASE_DIR)/lib/webapp/priv/docroot/js
RUNTIME_DIR=/var/tmp/bespoke

.PHONY: install check-target create-dirs enable-bootstrap setup-change-ssid setup-dhcp install-toggle-interface install-bespoke-service install-wlan1-udev-rule blacklist-rfkill clean-runtime-dir

install: check-target create-dirs enable-bootstrap setup-change-ssid setup-dhcp install-toggle-interface install-bespoke-service install-wlan1-udev-rule blacklist-rfkill

check-target:
	if ! grep -q "Raspberry Pi" /proc/device-tree/model 2>/dev/null; then \
		echo "Error: This must be run on a Raspberry Pi"; \
		exit 1; \
	fi

create-dirs:
	mkdir -p $(RUNTIME_DIR)/attachment/tmp && \
	mkdir -p $(RUNTIME_DIR)/log && \
	mkdir -p $(RUNTIME_DIR)/db

enable-bootstrap:
	touch $(RUNTIME_DIR)/bootstrap

setup-change-ssid:
	sudo install -m 440 etc/sudoers.d/change-ssid /etc/sudoers.d/ && \
	sudo chown root:root /etc/sudoers.d/change-ssid && \
	sudo chown root:root bin/change_ssid.sh && \
	sudo chmod 700 bin/change_ssid.sh

setup-dhcp:
	sudo install -m 644 etc/dhcpcd.conf /etc/ && \
	sudo systemctl restart dhcpcd

install-toggle-interface:
	sudo install -m 644 etc/hostapd/hostapd-wlan0.conf /etc/hostapd/ && \
	sudo install -m 644 etc/hostapd/hostapd-wlan1.conf /etc/hostapd/ && \
	sudo install -m 644 etc/systemd/system/hostapd-wlan0.service /etc/systemd/system/ && \
	sudo install -m 644 etc/systemd/system/hostapd-wlan1.service /etc/systemd/system/ && \
	sudo install -m 644 etc/systemd/system/bespoke-toggle-interface.service /etc/systemd/system/ && \
	sudo systemctl daemon-reload && \
	sudo systemctl restart bespoke-toggle-interface && \
	sudo systemctl enable bespoke-toggle-interface

install-bespoke-service:
	sudo install -m 644 etc/systemd/system/bespoke.service /etc/systemd/system/ && \
	sudo systemctl daemon-reload && \
	sudo systemctl enable bespoke

install-wlan1-udev-rule:
	sudo install -m 644 etc/udev/rules.d/99-wlan1.rules /etc/udev/rules.d/ && \
	sudo udevadm control --reload-rules && \
	sudo udevadm trigger

blacklist-rfkill:
	sudo install -m 644 etc/modprobe.d/blacklist-rfkill.conf /etc/modprobe.d/

clean-runtime-dir:
	rm -rf $(RUNTIME_DIR)
