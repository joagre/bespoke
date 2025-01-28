JS_FILES := $(wildcard *.js)
JS_SIGNATURE_FILES := $(patsubst %.js,../signatures/%.js.sig,$(JS_FILES))
THIRD_PARTY_JS_FILES=purify.min.js marked.min.js sodium.js uikit.js uikit.min.js uikit-icons.js uikit-icons.min.js uhtml.min.js
THIRD_PARTY_GZ_FILES := $(patsubst %.js,%.js.gz,$(THIRD_PARTY_JS_FILES))
BESPOKE_JS_FILES := $(filter-out $(THIRD_PARTY_JS_FILES), $(JS_FILES))

all: sign gzip

sign: $(JS_SIGNATURE_FILES)

../signatures/%.js.sig: %.js
	@echo "Signing $<..."
	@../../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

gzip: $(THIRD_PARTY_GZ_FILES)

%.js.gz: %.js
	gzip -k -f -c $< > $@

eslint:
	@for file in $(BESPOKE_JS_FILES); do \
		echo "Linting $$file..."; \
		npx eslint $$file || exit 1; \
	done

clean:
	rm -f $(JS_SIGNATURE_FILES) $(THIRD_PARTY_GZ_FILES)
