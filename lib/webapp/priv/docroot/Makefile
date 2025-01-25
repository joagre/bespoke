HTML_FILES := $(wildcard *.html)
HTML_SIGNATURE_FILES := $(patsubst %.html,signatures/%.html.sig,$(HTML_FILES))

all: sign
	(cd js; $(MAKE) sign gzip)

sign: $(HTML_SIGNATURE_FILES) signatures/sw.js.sig

signatures/%.html.sig: %.html
	@echo "Signing $<..."
	@../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

signatures/sw.js.sig: sw.js
	@echo "Signing $<..."
	@../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

clean:
	rm -f $(HTML_SIGNATURE_FILES) signatures/sw.js.sig
	(cd js; $(MAKE) clean)
