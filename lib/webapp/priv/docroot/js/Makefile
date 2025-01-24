JS_FILE := $(wildcard *.js)
JS_FILES := $(filter-out marked.min.js sodium.js uikit.js uikit.min.js uikit-icons.js uikit-icons.min.js uhtml.min.js, $(JS_FILE))

lint:
	@echo "Linting..."
	@for file in $(JS_FILES); do \
		echo "Linting $$file..."; \
		npx eslint $$file || exit 1; \
	done
