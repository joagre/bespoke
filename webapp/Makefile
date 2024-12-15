all: priv/cert.pem
	(cd src && $(MAKE) all)

release:

priv/cert.pem:
	@openssl req -new -newkey rsa:2048 -days 365 -nodes -x509 -subj "/" -keyout /tmp/key.pem -out /tmp/cert.pem > /dev/null 2>&1
	@cat /tmp/key.pem /tmp/cert.pem > priv/cert.pem
	@rm -f /tmp/key.pem /tmp/cert.pem

setcap:
	sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`

clean:
	(cd src && $(MAKE) clean)
