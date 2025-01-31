LIBS=apptools db enacl main webapp

all:
	for lib in $(LIBS) ; do \
		$(MAKE) -C $$lib all || exit $$?; \
	done

release:
	for lib in $(LIBS); do \
		$(MAKE) -C $$lib release || exit $$?; \
	done

runtest:
	for lib in $(LIBS); do \
		$(MAKE) -C $$lib runtest || exit $$?; \
	done

clean:
	for lib in $(LIBS); do \
		$(MAKE) -C $$lib  clean || exit $$?; \
	done
