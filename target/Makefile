install: install-toggle-interface

install-toggle-interface:
	sudo cp etc/systemd/system/bespoke-toggle-interface.service /etc/systemd/system/
	sudo systemctl daemon-reload
	sudo systemctl start bespoke-toggle-interface.service
	sudo systemctl enable bespoke-toggle-interface.service
