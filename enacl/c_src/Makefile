#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

SOURCES=aead.c enacl.c enacl_ext.c enacl_nif.c generichash.c hash.c kdf.c kx.c public.c pwhash.c randombytes.c secret.c secretstream.c sign.c
CFLAGS+=-std=c18 -O -Werror -Wno-incompatible-pointer-types -Wno-unused-result
LDFLAGS+=-shared

all: ../priv/enacl_nif.so

../priv/enacl_nif.so: $(SOURCES)
	$(CC) $(CFLAGS) -o ../priv/enacl_nif.so -fpic $(SOURCES) $(LDFLAGS) -lsodium

clean:
	rm -rf *.o ../priv/enacl_nif.so
