LIBS=apptools db enacl main webapp

all:
	for lib in $(LIBS) ; do \
		(cd $$lib && $(MAKE) all) || exit $$?; \
	done

release:
	for lib in $(LIBS); do \
		(cd $$lib && $(MAKE) release) || exit $$?; \
	done

runtest:
	for lib in $(LIBS); do \
		(cd $$lib && $(MAKE) runtest) || exit $$?; \
	done

clean:
	for lib in $(LIBS); do \
		(cd $$lib && $(MAKE) clean) || exit $$?; \
	done
