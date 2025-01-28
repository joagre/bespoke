all:
	(cd src && $(MAKE) all)
	(cd priv/docroot && $(MAKE) all)

eslint:
	(cd priv/docroot/js && $(MAKE) eslint)

release: all

runtest:

setcap:
	sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`

clean:
	(cd src && $(MAKE) clean)
	(cd priv/docroot && make clean)
	rm -f $(KEY_FILE) $(CERT_FILE) $(SAN_FILE)
