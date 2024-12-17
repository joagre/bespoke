LIBS=apptools db enacl main rester webapp

release:
	@echo "Cleaning up"
	@rm -fr releases/bespoke
	@mkdir -p releases/bespoke
	@echo "Copying ebin files"
	@for lib in $(LIBS); do \
		mkdir -p releases/bespoke/$$lib/ebin; \
		cp -r ../$$lib/ebin/*.beam ../$$lib/ebin/*.app releases/bespoke/$$lib/ebin; \
	done
	@echo "Copying priv files"
	@for lib in $(LIBS); do \
		if [ -d ../$$lib/priv ]; then \
			mkdir -p releases/bespoke/$$lib; \
			cp -r ../$$lib/priv releases/bespoke/$$lib; \
		fi; \
	done
	@echo "Copying bin files"
	@for lib in $(LIBS); do \
		if [ -d ../$$lib/bin ]; then \
			mkdir -p releases/bespoke/$$lib; \
			cp -r ../$$lib/bin releases/bespoke/$$lib; \
		fi; \
	done
	@echo "Copying bin files"
	@cp -r ../bin releases/bespoke
	@echo "Copying bespoke service"
	@mkdir -p releases/bespoke/config
	@cp bespoke.service releases/bespoke/config
	@echo "Copy top Makefile"
	@cp Makefile.release releases/bespoke/Makefile
	@echo "Removing spurious files"
	@find releases/bespoke -name "*.dump" -exec rm {} \;
	@find releases/bespoke -name "*.db" -exec rm {} \;
	@find releases/bespoke -name ".gitignore" -exec rm {} \;
	@echo "Building release"
	@echo "Version: $(shell cat ../VERSION)"
	@echo "Release: releases/bespoke-$(shell cat ../VERSION).tar.gz"
	@tar -czf releases/bespoke-$(shell cat ../VERSION).tar.gz -C releases bespoke
