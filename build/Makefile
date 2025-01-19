LIBS=apptools db main webapp
EXTERNAL_DIRS := $(shell find ../external/ -mindepth 2 -maxdepth 2 -type d)
RELEASE_DIR="bespoke-$(shell cat ../VERSION)"
TARGET_DIR="releases/$(RELEASE_DIR)"

release:
	@echo "Cleaning up"
	@rm -fr $(TARGET_DIR)
	@mkdir -p $(TARGET_DIR)
	@echo "Copying ebin files"
	@for lib in $(LIBS); do \
		mkdir -p $(TARGET_DIR)/lib/$$lib/ebin; \
		cp -r ../lib/$$lib/ebin/*.beam ../lib/$$lib/ebin/*.app $(TARGET_DIR)/lib/$$lib/ebin; \
	done
	@echo "Copying external ebin files"
	@for lib in $(EXTERNAL_DIRS); do \
		app=$$(basename $$lib); \
		echo "lib: $$lib"; \
		echo "app: $$app"; \
		mkdir -p $(TARGET_DIR)/external/$$app/ebin; \
		cp -r $$lib/ebin/*.beam $$lib/ebin/*.app $(TARGET_DIR)/external/$$app/ebin; \
	done
	@echo "Copying priv files"
	@for lib in $(LIBS); do \
		if [ -d ../lib/$$lib/priv ]; then \
			mkdir -p $(TARGET_DIR)/lib/$$lib; \
			cp -r ../lib/$$lib/priv $(TARGET_DIR)/lib/$$lib; \
		fi; \
	done
	@echo "Copying bin files"
	@cp -r ../bin $(TARGET_DIR)
	@rm -f $(TARGET_DIR)/bin/run_test.sh
	@echo "Copying top Makefile"
	@cp Makefile.release $(TARGET_DIR)/Makefile
	@echo "Removing spurious files"
	@find $(TARGET_DIR) -name "*.dump" -exec rm {} \;
	@find $(TARGET_DIR) -name ".gitignore" -exec rm {} \;
	@echo "Building release"
	@echo "Version: $(shell cat ../VERSION)"
	@echo "Release: $(TARGET_DIR).tar.gz"
	@tar -czf $(TARGET_DIR).tar.gz -C releases $(RELEASE_DIR)

clean:
	rm -fr releases/*
