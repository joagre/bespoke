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
	install -m 644 etc/systemd/system/bespoke-toggle-interface.service /etc/systemd/system/ && \
	systemctl daemon-reload && \
	systemctl restart bespoke-toggle-interface && \
	systemctl enable bespoke-toggle-interface && \
	systemctl is-active bespoke-toggle-interface

clean:
	rm -rf $(RUNTIME_DIR)
