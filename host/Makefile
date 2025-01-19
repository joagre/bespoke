.PHONY: install-usb-ethernet-udev-rule

install: install-usb-ethernet-udev-rule

install-wlan1-udev-rule:
	sudo install -m 644 etc/udev/rules.d/70-usb-ethernet.rules /etc/udev/rules.d/ && \
	sudo udevadm control --reload-rules && \
	sudo udevadm trigger
