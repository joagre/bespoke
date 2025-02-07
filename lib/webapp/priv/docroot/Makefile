HTML_FILES := $(wildcard *.html)
HTML_SIGNATURE_FILES := $(patsubst %.html,signatures/%.html.sig,$(HTML_FILES))

all: sign eslint gzip

sign: not_on_target $(HTML_SIGNATURE_FILES)
	$(MAKE) -C js sign

not_on_target:
	@if grep -q "Raspberry Pi" /proc/cpuinfo; then \
		echo "Error: This build should not be run on a Raspberry Pi"; \
		exit 255; \
	fi

signatures/%.html.sig: %.html
	@echo "Signing $<..."
	@../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

eslint:
	$(MAKE) -C js eslint

gzip:
	$(MAKE) -C js gzip

clean:
	$(MAKE) -C js clean
