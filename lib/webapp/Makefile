JS_DIR=priv/docroot/js

all: priv/cert.pem
	(cd src && $(MAKE) all)
	gzip -k -f -c $(JS_DIR)/uikit.js > $(JS_DIR)/uikit.js.gz
	gzip -k -f -c $(JS_DIR)/uikit.min.js > $(JS_DIR)/uikit.min.js.gz
	gzip -k -f -c $(JS_DIR)/uikit-icons.js > $(JS_DIR)/uikit-icons.js.gz
	gzip -k -f -c $(JS_DIR)/uikit-icons.min.js > $(JS_DIR)/uikit-icons.min.js.gz
	gzip -k -f -c $(JS_DIR)/uhtml.min.js > $(JS_DIR)/uhtml.min.js.gz
	gzip -k -f -c $(JS_DIR)/sodium.js > $(JS_DIR)/sodium.js.gz

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
