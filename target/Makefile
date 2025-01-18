RUNTIME_DIR = /var/tmp/bespoke

.PHONY: install create-dirs ensure-bootstrap install-toggle-interface clean

install: create-dirs ensure-bootstrap install-toggle-interface

create-dirs:
	mkdir -p $(RUNTIME_DIR)/attachment/tmp && \
	mkdir -p $(RUNTIME_DIR)/log && \
	mkdir -p $(RUNTIME_DIR)/db

ensure-bootstrap:
	touch $(RUNTIME_DIR)/bootstrap

install-toggle-interface:
	sudo install -m 644 etc/hostapd/hostapd-wlan0.conf /etc/hostapd/ && \
	sudo install -m 644 etc/hostapd/hostapd-wlan1.conf /etc/hostapd/ && \
	sudo install -m 644 etc/systemd/system/hostapd-wlan0.service /etc/systemd/system/ && \
	sudo install -m 644 etc/systemd/system/hostapd-wlan1.service /etc/systemd/system/ && \
	sudo install -m 644 etc/systemd/system/bespoke-toggle-interface.service /etc/systemd/system/ && \
	sudo systemctl daemon-reload && \
	sudo systemctl restart bespoke-toggle-interface && \
	sudo systemctl enable bespoke-toggle-interface

clean:
	rm -rf $(RUNTIME_DIR)
