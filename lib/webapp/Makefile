all:
	$(MAKE) -C src all
	$(MAKE) -C priv/docroot gzip

sign:
	$(MAKE) -C priv/docroot sign

eslint:
	$(MAKE) -C priv/docroot/js eslint

release: all

runtest:

setcap:
	sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`

clean:
	$(MAKE) -C src clean
	$(MAKE) -C priv/docroot clean
	rm -f $(KEY_FILE) $(CERT_FILE) $(SAN_FILE)
