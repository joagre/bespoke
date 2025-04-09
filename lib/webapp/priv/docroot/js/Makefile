JS_FILES := $(wildcard *.js)
JS_SIGNATURE_FILES := $(patsubst %.js,../signatures/%.js.sig,$(JS_FILES))
THIRD_PARTY_JS_FILES=purify.min.js marked.min.js sodium.js uikit.js uikit.min.js uikit-icons.js uikit-icons.min.js uhtml.min.js StreamSaver.js sw.js
THIRD_PARTY_GZ_FILES := $(patsubst %.js,%.js.gz,$(THIRD_PARTY_JS_FILES))
BESPOKE_JS_FILES := $(filter-out $(THIRD_PARTY_JS_FILES), $(JS_FILES))
BESPOKE_JS_OK_FILES := $(patsubst %.js,checked/%.js.ok,$(BESPOKE_JS_FILES))

all: gzip

sign: not_on_target $(JS_SIGNATURE_FILES)

not_on_target:
	@if grep -q "Raspberry Pi" /proc/cpuinfo; then \
		echo "Error: This build should not be run on a Raspberry Pi"; \
		exit 255; \
	fi

../signatures/%.js.sig: %.js
	@echo "Signing $<" && \
	../../../../../bin/bespoke --clean -- -noinput -eval webapp_crypto:sign_file\("\"$<\""\) > $@

eslint: $(BESPOKE_JS_OK_FILES)

checked/%.js.ok: %.js
	@echo "Checking $<" && \
	mkdir -p checked && \
	npx eslint $< || exit 1 && \
	touch $@

gzip: $(THIRD_PARTY_GZ_FILES)

%.js.gz: %.js
	@echo "Compressing $<" && \
	gzip -k -f -c $< > $@

clean:
	rm -f $(THIRD_PARTY_GZ_FILES) $(BESPOKE_JS_OK_FILES)
