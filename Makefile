LIBS=apptools db jsone main rester
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
		(cd $$lib/test && env $(MAKE)) || exit 1; \
	done


mrproper: clean cleanfluff

cleanfluff:
	@for lib in $(ALL_LIST) ; do \
		find $(LPATH)/$$lib \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;; \
	done
