ERL ?= erl
APP := mgsv

.PHONY: deps

all:
	@cd cert; ./generate_certs
	@./rebar3 compile

clean:
	@./rebar3 clean

distclean: clean
	@./rebar3 delete-deps

release:
	@./rebar3 release
