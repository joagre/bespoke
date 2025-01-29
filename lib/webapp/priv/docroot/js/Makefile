JS_FILES := $(wildcard *.js)
JS_SIGNATURE_FILES := $(patsubst %.js,../signatures/%.js.sig,$(JS_FILES))
THIRD_PARTY_JS_FILES=purify.min.js marked.min.js sodium.js uikit.js uikit.min.js uikit-icons.js uikit-icons.min.js uhtml.min.js
THIRD_PARTY_GZ_FILES := $(patsubst %.js,%.js.gz,$(THIRD_PARTY_JS_FILES))
BESPOKE_JS_FILES := $(filter-out $(THIRD_PARTY_JS_FILES), $(JS_FILES))

gzip: $(THIRD_PARTY_GZ_FILES)

%.js.gz: %.js
	gzip -k -f -c $< > $@

sign: not_on_target $(JS_SIGNATURE_FILES)

not_on_target:
	if grep -q "Raspberry Pi" /proc/cpuinfo; then \
		echo "Error: This build should not be run on a Raspberry Pi"; \
		exit 255; \
	fi

../signatures/%.js.sig: %.js
	@echo "Signing $<..."
	@../../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

eslint:
	@for file in $(BESPOKE_JS_FILES); do \
		echo "Linting $$file..."; \
		npx eslint $$file || exit 1; \
	done

clean:
	rm -f $(THIRD_PARTY_GZ_FILES)
