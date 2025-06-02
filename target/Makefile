BASE_DIR=/home/pi/bespoke
JS_DIR=$(BASE_DIR)/lib/webapp/priv/docroot/js
BESPOKE_RUNTIME_DIR=/var/tmp/bespoke

# Top-level rules

all: bespoke-install

bespoke-install: check-target create-env enable-bootstrap unpack-key restart

bespoke-clean:
	rm -rf $(BESPOKE_RUNTIME_DIR)

os-install: check-target create-env setup-change-ssid setup-dhcp install-toggle-interface install-bespoke-service install-wlan1-udev-rule blacklist-rfkill bespoke-install

# Bespoke rules

check-target:
	if ! grep -q "Raspberry Pi" /proc/device-tree/model 2>/dev/null; then \
		echo "Error: This must be run on a Raspberry Pi"; \
		exit 1; \
	fi

create-env:
	: # Create the runtime directory structure
	mkdir -p $(BESPOKE_RUNTIME_DIR)/db && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/tmp && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/message && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/post && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/file && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/log && \
	: # Create the runtime directory structure
	mkdir -p $(BESPOKE_RUNTIME_DIR)/db && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/tmp && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/message && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/post && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/file && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/log && \
	: # Create the runtime configuration file
	if [ ! -f $(BESPOKE_RUNTIME_DIR)/bespoke.conf ]; then \
		echo "BespokeVersion=\"`cat VERSION`\"" > $(BESPOKE_RUNTIME_DIR)/bespoke.conf; \
	else \
		sed -i "s/^BespokeVersion=.*/BespokeVersion=\"`cat VERSION`\"/" $(BESPOKE_RUNTIME_DIR)/bespoke.conf; \
	fi && \
	: # Create the runtime directory for the webapp
	if [ ! -d $(BESPOKE_RUNTIME_DIR)/local ]; then \
		cp -fr lib/webapp/priv/local $(BESPOKE_RUNTIME_DIR)/; \
	fi && \

enable-bootstrap:
	touch $(BESPOKE_RUNTIME_DIR)/bootstrap

unpack-key:
	$(MAKE) -C $(BASE_DIR)/lib/webapp/priv/b3s.zone all

restart:
	cd $(BASE_DIR) || exit 1 && \
	sudo systemctl stop bespoke && \
	sudo systemctl start bespoke && \
	sudo systemctl is-active bespoke

# OS rules

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
