DOMAIN = b3s.zone
IP = 192.168.4.1
DAYS = 365
KEY_FILE = priv/server-key.pem
CERT_FILE = priv/server-cert.pem
SAN_FILE = priv/server-san.cnf

all: $(CERT_FILE)
	(cd src && $(MAKE) all)
	(cd priv/docroot/js && $(MAKE) all)

release: all

runtest:

$(CERT_FILE): $(KEY_FILE) $(SAN_FILE)
	@echo "Generating self-signed certificate..."
	openssl req -x509 -new -nodes -key $(KEY_FILE) -sha256 -days $(DAYS) \
		-out $(CERT_FILE) -config $(SAN_FILE) -subj "/C=SE/ST=Stockholm/O=BespokeBBS/CN=$(DOMAIN)"

$(KEY_FILE):
	@echo "Generating private key..."
	openssl genrsa -out $(KEY_FILE) 2048

$(SAN_FILE):
	@echo "Creating SAN configuration file..."
	@echo "[ req ]" > $(SAN_FILE)
	@echo "distinguished_name = req_distinguished_name" >> $(SAN_FILE)
	@echo "x509_extensions = v3_req" >> $(SAN_FILE)
	@echo "[ req_distinguished_name ]" >> $(SAN_FILE)
	@echo "" >> $(SAN_FILE)
	@echo "[ v3_req ]" >> $(SAN_FILE)
	@echo "keyUsage = keyEncipherment, dataEncipherment" >> $(SAN_FILE)
	@echo "extendedKeyUsage = serverAuth" >> $(SAN_FILE)
	@echo "subjectAltName = @alt_names" >> $(SAN_FILE)
	@echo "[ alt_names ]" >> $(SAN_FILE)
	@echo "DNS.1 = $(DOMAIN)" >> $(SAN_FILE)
	@echo "IP.1 = $(IP)" >> $(SAN_FILE)

setcap:
	sudo setcap cap_net_bind_service=+ep `find /usr/local/lib/erlang/ -name beam.smp`

clean:
	(cd src && $(MAKE) clean)
	(cd priv/docroot/js && make clean)
	rm -f $(KEY_FILE) $(CERT_FILE) $(SAN_FILE)
