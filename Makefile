BESPOKE_RUNTIME_DIR=/var/tmp/bespoke

all:
	mkdir -p $(BESPOKE_RUNTIME_DIR)/db && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/tmp && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/message && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/post && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/file && \
	mkdir -p $(BESPOKE_RUNTIME_DIR)/log && \
	touch $(BESPOKE_RUNTIME_DIR)/bespoke.conf && \
	$(MAKE) -C external all && \
	$(MAKE) -C lib all

sign:
	$(MAKE) -C lib/webapp/priv/docroot sign

release: mrproper
	$(MAKE) -C lib release && \
	$(MAKE) -C external release && \
	$(MAKE) -C build release

runtest:
	$(MAKE) -C lib runtest

setcap:
	$(MAKE) -C webapp setcap

clean:
	$(MAKE) -C lib clean && \
	$(MAKE) -C external clean && \
	rm -f .dialyzer.plt

reset:
	rm -rf $(BESPOKE_RUNTIME_DIR)/* && \
	$(MAKE) all

mrproper: clean
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;

distclean: mrproper
	rm -f .dialyzer_init.plt
	$(MAKE) -C build clean

#
# Type checking
#

eslint:
	$(MAKE) -C lib/webapp/priv/docroot/js eslint

ERL=$(shell which erl)
ERL_TOP=$(ERL:%/bin/erl=%)
LPATH=$(abspath $(dir $(realpath $(firstword $(MAKEFILE_LIST))))..)
DIALYZER_APPS=apptools db main webapp
DIALYZER_EXTERNAL_APPS=mixmesh/rester

dialyzer: .dialyzer.plt all
	dialyzer --verbose --no_check_plt --plt .dialyzer.plt -r $(DIALYZER_APPS:%=$(LPATH)/bespoke/lib/%/ebin) $(DIALYZER_EXTERNAL_APPS:%=$(LPATH)/bespoke/external/%/ebin)

.dialyzer.plt: .dialyzer_init.plt
	rm -f $@ ; cp $< $@
	dialyzer --verbose --check_plt --plt $@ || true

DIALYZER_PLT_APPS=asn1 compiler crypto erts inets kernel mnesia parsetools public_key runtime_tools sasl ssl stdlib syntax_tools tools

.dialyzer_init.plt:
	rm -f $@
	@echo "BEWARE: This will take several minutes the first time..."
	dialyzer --verbose --build_plt --output_plt $@ -r $(DIALYZER_PLT_APPS:%=$(ERL_TOP)/lib/erlang/lib/%-*/ebin)
