ERL ?= erl
APP := mgsv

.PHONY: deps

all:
	@./rebar3 compile

clean:
	@./rebar3 clean

distclean: clean
	@./rebar3 delete-deps

release:
	@./rebar3 compile
	cd rel
	@./rebar3 -v generate

rels:
	@./rebar compile
	cd rel; .././rebar -v generate; .././rebar generate-upgrade previous_release=$(PREV); mv *.tar.gz mgsv_running/releases/

rel: clean release

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

small:
	@./rebar3 compile skip_deps=true
