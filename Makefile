ERL ?= erl
APP := mgsv

.PHONY: deps

all:
	@cd cert; ./generate_certs
	@./rebar3 release

release:
	@./rebar3 release

docker:
	@cd cert; ./generate_certs
	@./rebar3 as prod release -o /artifacts
