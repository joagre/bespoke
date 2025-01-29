all: server.key

server.key: server.dat
	gunzip -k -f -c $< > $@
