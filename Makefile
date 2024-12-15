ERL=$(shell which erl)
ERL_TOP=$(ERL:%/bin/erl=%)
LPATH=$(abspath $(dir $(realpath $(firstword $(MAKEFILE_LIST))))..)
LIBS=apptools db enacl main rester webapp
TESTS=db

all:
	for lib in $(LIBS) ; do \
		(cd $$lib && env ERL_LIBS=. ERLC_FLAGS=+debug_info $(MAKE) all) || exit 1; \
	done

release: mrproper
	@for lib in $(LIBS) ; do \
		(cd $$lib && env ERL_LIBS=. $(MAKE) release) || exit 1; \
	done
	@rm -fr releases/b3s
	@mkdir -p releases/b3s
	@for lib in $(LIBS); do \
		mkdir -p releases/b3s/$$lib/ebin; \
		cp -r $$lib/ebin/*.beam $$lib/ebin/*.app releases/b3s/$$lib/ebin; \
	done
	@for lib in $(LIBS); do \
		if [ -d $$lib/priv ]; then \
			mkdir -p releases/b3s/$$lib; \
			cp -r $$lib/priv releases/b3s/$$lib; \
		fi; \
	done
	@for lib in $(LIBS); do \
		if [ -d $$lib/bin ]; then \
			mkdir -p releases/b3s/$$lib; \
			cp -r $$lib/bin releases/b3s/$$lib; \
		fi; \
	done
	@cp LICENSE releases/b3s
	@echo "Building release"
	@echo "Version: $(shell cat VERSION)"
	@echo "Release: releases/b3s-$(shell cat VERSION).tar.gz"
	@tar -czf releases/b3s-$(shell cat VERSION).tar.gz -C releases b3s

setcap:
	(cd webapp; make setcap)

clean:
	for lib in $(LIBS) ; do \
		(cd $$lib && env ERL_LIBS=. $(MAKE) clean) || exit 1; \
	done
	rm -f .dialyzer.plt

runtest:
	for lib in $(TESTS) ; do \
		(cd $$lib/test && env ERL_LIBS=. $(MAKE) runtest) || exit 1; \
	done

mrproper: clean cleanfluff
	rm -f .dialyzer_init.plt

cleanfluff:
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;

#
# Type checking
#

.PHONY: dialyzer

DIALYZER_APPS=apptools db main rester webapp

dialyzer: .dialyzer.plt all
	dialyzer --verbose --no_check_plt --plt .dialyzer.plt -r $(DIALYZER_APPS:%=$(LPATH)/bespoke/%/ebin)

.dialyzer.plt: .dialyzer_init.plt
	rm -f $@ ; cp $< $@
	dialyzer --verbose --check_plt --plt $@ || true

DIALYZER_PLT_APPS=asn1 compiler crypto erts inets kernel mnesia parsetools public_key runtime_tools sasl ssl stdlib syntax_tools tools

.dialyzer_init.plt:
	rm -f $@
	@echo "BEWARE: This will take several minutes the first time..."
	dialyzer --verbose --build_plt --output_plt $@ -r $(DIALYZER_PLT_APPS:%=$(ERL_TOP)/lib/erlang/lib/%-*/ebin)
