#@BEGIN-APP-DEFAULT-RULES@
APP=$(shell basename `cd ..; pwd`)
ERLC=$(shell which erlc)
EXTERNAL_DIRS=$(shell find ../../../external/ -mindepth 1 -maxdepth 1 -type d)
ERLC_FLAGS+=-MMD -MF .$<.d -W -I ../.. $(EXTERNAL_DIRS:%=-I %)
YRL_SRC=$(wildcard *.yrl)
XRL_SRC=$(wildcard *.xrl)
ERL_SOURCES=$(wildcard *.erl) $(YRL_SRC:%.yrl=%.erl) $(XRL_SRC:%.xrl=%.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=../ebin/%.beam)
ALL_OBJECTS=$(ERL_OBJECTS)
ERL_MODULES=$(ERL_SOURCES:%.erl=%)
comma=,
empty=
space=$(empty) $(empty)
MODULES=$(subst $(space),$(comma),$(ERL_MODULES))
VERSION=$(shell git describe --always --tags)
APP_SRC=$(APP).app.src
APP_TARGET=../ebin/$(APP).app

.PRECIOUS: $(YRL_SRC:%.yrl=%.erl) $(XRL_SRC:%.xrl=%.erl)

all:
	env ERLC_FLAGS=+debug_info $(MAKE) build

release:
	$(MAKE) build

build: $(APP_TARGET) $(ALL_OBJECTS)

clean:
	rm -f $(APP_TARGET) $(ALL_OBJECTS) *.core .*.d

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

%.erl: %.yrl
	$(ERLC) $<

%.erl: %.xrl
	$(ERLC) $<

$(APP_TARGET): $(APP_SRC)
	sed -e 's;{vsn,.*git};{vsn,"$(VERSION)"};' -e 's;"@@MODULES@@";$(MODULES);' $(APP_SRC) > $(APP_TARGET)

-include .*.d
#@END-APP-DEFAULT-RULES@
