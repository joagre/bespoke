#@BEGIN-TEST-DEFAULT-RULES@
ERLC=$(shell which erlc)
ERLC_FLAGS=-I ../.. +debug_info -Ddebug
ERL_HEADERS=$(wildcard *.hrl) $(wildcard ../../*/include/*.hrl)
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=%.beam)
ALL_OBJECTS=$(ERL_OBJECTS)

all: $(ALL_OBJECTS)

$(ERL_OBJECTS): $(ERL_HEADERS)

clean:
	rm -f $(ALL_OBJECTS) *.core

%.beam: %.erl
	env $(ERLC) $(ERLC_FLAGS) $<
#@END-TEST-DEFAULT-RULES@

runtest: all
	../../../bin/run_test.sh --config ../../mixmesh/etc/mixmesh-do-nothing.conf .
