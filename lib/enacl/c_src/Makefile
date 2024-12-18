CROSS_COMPILE?=
CC=$(CROSS_COMPILE)gcc
STRIP=$(CROSS_COMPILE)strip
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
CFLAGS+=-std=c18 -O -Wno-incompatible-pointer-types -Wno-unused-result
LDFLAGS+=-shared
LIBSODIUM_INCLUDE?=
LIBSODIUM_LIB?=

ifneq ($(LIBSODIUM_INCLUDE),)
    CFLAGS += -I$(LIBSODIUM_INCLUDE)
    LDFLAGS += -L$(LIBSODIUM_LIB)
endif

SOURCES=aead.c enacl.c enacl_ext.c enacl_nif.c generichash.c hash.c kdf.c kx.c public.c pwhash.c randombytes.c secret.c secretstream.c sign.c

all: ../priv/enacl_nif.so

release: clean
	$(MAKE) CROSS_COMPILE=aarch64-linux-gnu- LIBSODIUM_INCLUDE=../../../vendor/libsodium-arm/include LIBSODIUM_LIB=../../../vendor/libsodium-arm/lib all
	aarch64-linux-gnu-$(STRIP) ../priv/enacl_nif.so

../priv/enacl_nif.so: $(SOURCES)
	$(CC) $(CFLAGS) -o ../priv/enacl_nif.so -fpic $(SOURCES) $(LDFLAGS) -lsodium

clean:
	rm -rf *.o ../priv/enacl_nif.so
