JS_FILES := $(wildcard *.js)
JS_SIGNATURE_FILES := $(patsubst %.js,%.js.sig,$(JS_FILES))
THIRD_PARTY_JS_FILES=marked.min.js sodium.js uikit.js uikit.min.js uikit-icons.js uikit-icons.min.js uhtml.min.js
THIRD_PARTY_GZ_FILES := $(patsubst %.js,%.js.gz,$(THIRD_PARTY_JS_FILES))
BESPOKE_JS_FILES := $(filter-out $(THIRD_PARTY_JS_FILES), $(JS_FILES))

all: sign gzip

sign: $(JS_SIGNATURE_FILES) ../sw.js.sig

%.js.sig: %.js
	@echo "Signing $<..."
	@../../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

../sw.js.sig: ../sw.js
	@echo "Signing $<..."
	@../../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

gzip: $(THIRD_PARTY_GZ_FILES)

%.js.gz: %.js
	gzip -k -f -c $< > $@

lint:
	@for file in $(BESPOKE_JS_FILES); do \
		echo "Linting $$file..."; \
		npx eslint $$file || exit 1; \
	done

clean:
	rm -f $(JS_SIGNATURE_FILES) ../sw.js.sig $(THIRD_PARTY_GZ_FILES)
