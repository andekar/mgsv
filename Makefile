ERL ?= erl
APP := mgsv

.PHONY: deps

all: deps
	@rebar compile

deps:
	@rebar get-deps

clean:
	@rebar clean

distclean: clean
	@rebar delete-deps

release:
	@rebar clean
	@rebar compile
	cd rel
	@rebar -v generate

rel:
	@rebar clean
	@rebar compile
	cd rel
	@rebar -v generate

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'