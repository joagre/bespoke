# Cross-compilation settings
CROSS_COMPILE ?=
CC=$(CROSS_COMPILE)gcc
STRIP=$(CROSS_COMPILE)strip

# Paths and flags
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
CFLAGS+=-std=c18 -O -Wno-incompatible-pointer-types -Wno-unused-result
LDFLAGS+=-shared

# Add paths for cross-compiled libsodium headers and library if necessary

Hmm. This is not ok for some reason in my Makefile:

LIBSODIUM_INCLUDE ?=
LIBSODIUM_LIB ?=
ifneq ($(LIBSODIUM_INCLUDE),)
    CFLAGS += -I$(LIBSODIUM_INCLUDE)
    LDFLAGS += -L$(LIBSODIUM_LIB)
endif

# Source files
SOURCES=aead.c enacl.c enacl_ext.c enacl_nif.c generichash.c hash.c kdf.c kx.c public.c pwhash.c randombytes.c secret.c secretstream.c sign.c

# Build target
all: ../priv/enacl_nif.so

release: clean
	CROSS_COMPILE=arm-linux-gnueabihf-
	$(MAKE) CROSS_COMPILE=$(CROSS_COMPILE) LIBSODIUM_INCLUDE=../../vendor/libsodium-build/include LIBSODIUM_LIB=../../vendor/libsodium-build/lib all
	$(CROSS_COMPILE)$(STRIP) ../priv/enacl_nif.so

../priv/enacl_nif.so: $(SOURCES)
	$(CC) $(CFLAGS) -o ../priv/enacl_nif.so -fpic $(SOURCES) $(LDFLAGS) -lsodium

clean:
	rm -rf *.o ../priv/enacl_nif.so
