LIBS=apptools db main rester
TESTS=db

all:
	for lib in $(LIBS) ; do \
		(cd $$lib && env ERL_LIBS=. $(MAKE) all) || exit 1; \
	done

clean:
	for lib in $(LIBS) ; do \
		(cd $$lib && env ERL_LIBS=. $(MAKE) clean) || exit 1; \
	done

runtest:
	for lib in $(TESTS) ; do \
		(cd $$lib/test && env ERL_LIBS=. $(MAKE) runtest) || exit 1; \
	done


mrproper: clean cleanfluff

cleanfluff:
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;
