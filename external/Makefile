REPOS=mixmesh/rester.git
DIRS=$(REPOS:%.git=%)
GITHUB_BASE=git@github.com:

all: clone
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir all; \
	done

release: all

runtest:

clean:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir clean; \
	done

clone: $(DIRS)

$(DIRS):
	if [ ! -d "$@" ]; then \
		git clone $(GITHUB_BASE)$@.git $@; \
	fi
