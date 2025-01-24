JS_DIR=priv/docroot/js

all: priv/cert.pem $(JS_DIR)/uikit.js.gz $(JS_DIR)/uikit.min.js.gz $(JS_DIR)/uikit-icons.js.gz $(JS_DIR)/uikit-icons.min.js.gz $(JS_DIR)/uhtml.min.js.gz $(JS_DIR)/sodium.js.gz
	(cd src && $(MAKE) all)

$(JS_DIR)/%.js.gz: $(JS_DIR)/%.js
	gzip -k -f -c $< > $@

release: all

runtest:

priv/cert.pem:
	@openssl req -new -newkey rsa:2048 -days 365 -nodes -x509 -subj "/" -keyout /tmp/key.pem -out /tmp/cert.pem > /dev/null 2>&1
	@cat /tmp/key.pem /tmp/cert.pem > priv/cert.pem
	@rm -f /tmp/key.pem /tmp/cert.pem

setcap:
	sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`

clean:
	(cd src && $(MAKE) clean)
	rm -f priv/cert.pem
	rm -f $(JS_DIR)/*.gz
