REBAR = $(shell pwd)/rebar3
APP=sb_core

.PHONY: all ct test clean elvis compile rel

all: compile

clean:
	$(REBAR) cover -r
	$(REBAR) clean

eunit:
	$(REBAR) eunit -v
	$(REBAR) cover -v

rel:
	$(REBAR) release

run: rel
	./_build/default/rel/ct_router/bin/ct_router

ct:
	$(REBAR) ct -v
	$(REBAR) cover -v

tests: elvis eunit ct
	$(REBAR) dialyzer

elvis:
	$(REBAR) lint

compile:
	$(REBAR) compile


loc:
	cloc --by-file src/*.erl
