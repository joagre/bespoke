#@BEGIN-TEST-DEFAULT-RULES@
ERLC=$(shell which erlc)
ERLC_FLAGS=-Werror -I ../.. +debug_info -Ddebug
ERL_HEADERS=$(wildcard *.hrl) $(wildcard ../../*/include/*.hrl)
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=%.beam)
ALL_OBJECTS=$(ERL_OBJECTS)

all: $(ALL_OBJECTS)

$(ERL_OBJECTS): $(ERL_HEADERS)

clean:
	rm -f $(ALL_OBJECTS) *.core

create_dummy_db:
	rm -f ../priv/*.db; ../../main/bin/bespoke -- -noinput -run db_tools create_dummy_db -run init stop

%.beam: %.erl
	env $(ERLC) $(ERLC_FLAGS) $<
#@END-TEST-DEFAULT-RULES@

ERLC_FLAGS=-I ../.. +debug_info -Ddebug

runtest: all
	rm -f ../priv/*.db; ../../../bin/run_test.sh db
	rm -f ../priv/*.db; ../../../bin/run_test.sh rest
