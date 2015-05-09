ERL ?= erl
APP := mgsv

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

release:
	@./rebar compile
	cd rel
	@./rebar -v generate

rels:
	@./rebar compile
	cd rel; .././rebar -v generate; .././rebar generate-upgrade previous_release=$(PREV); mv *.tar.gz mgsv_running/releases/ 

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

small:
	@./rebar compile skip_deps=true
