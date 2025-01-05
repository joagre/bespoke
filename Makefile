all:
	mkdir -p /var/tmp/bespoke/{attachment,log,db}
	mkdir -p /var/tmp/bespoke/attachment/tmp
	(cd lib && $(MAKE) all)
	(cd external && $(MAKE) all)

release: mrproper
	(cd lib && $(MAKE) release)
	(cd external && $(MAKE) release)
	(cd build && $(MAKE) release)

runtest:
	(cd lib && $(MAKE) runtest)

setcap:
	(cd webapp; make setcap)

clean:
	(cd lib && $(MAKE) clean)
	(cd external && $(MAKE) clean)
	rm -f .dialyzer.plt

mrproper: clean
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;

distclean: mrproper
	rm -f .dialyzer_init.plt
	(cd build && $(MAKE) clean)

#
# Type checking
#

ERL=$(shell which erl)
ERL_TOP=$(ERL:%/bin/erl=%)
LPATH=$(abspath $(dir $(realpath $(firstword $(MAKEFILE_LIST))))..)
DIALYZER_APPS=apptools db main webapp
DIALYZER_EXTERNAL_APPS=mixmesh/rester

dialyzer: .dialyzer.plt all
	dialyzer --verbose --no_check_plt --plt .dialyzer.plt -r $(DIALYZER_APPS:%=$(LPATH)/bespoke/lib/%/ebin) $(DIALYZER_EXTERNAL_APPS:%=$(LPATH)/external/%/ebin)

.dialyzer.plt: .dialyzer_init.plt
	rm -f $@ ; cp $< $@
	dialyzer --verbose --check_plt --plt $@ || true

DIALYZER_PLT_APPS=asn1 compiler crypto erts inets kernel mnesia parsetools public_key runtime_tools sasl ssl stdlib syntax_tools tools

.dialyzer_init.plt:
	rm -f $@
	@echo "BEWARE: This will take several minutes the first time..."
	dialyzer --verbose --build_plt --output_plt $@ -r $(DIALYZER_PLT_APPS:%=$(ERL_TOP)/lib/erlang/lib/%-*/ebin)
