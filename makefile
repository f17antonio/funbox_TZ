REPO       ?= fbtz
TAG         = $(shell git describe --tags)
REVISION   ?= $(shell echo $(TAG) | sed -e 's/^$(REPO)-//')
VERSION    ?= $(shell echo $(REVISION) | tr - .)
EPMD        = $(shell pgrep epmd)tail
TAIL        = tail

.PHONY: install

REBAR=./rebar
RELX=./relx

export EUNIT_ERL_OPTS = -args_file etc/vm.args -config etc/sys.config

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

install:
	@$(REBAR) get-deps compile
	@$(RELX) release tar

unit-test:
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

distclean: clean relclean
	@$(REBAR) delete-deps

docs:
	@$(REBAR) skip_deps=true doc

start:
	@./_rel/$(REPO)/bin/$(REPO) start

console:
	@./_rel/$(REPO)/bin/$(REPO) console

attach:
	@./_rel/$(REPO)/bin/$(REPO) attach

stop:
	@./_rel/$(REPO)/bin/$(REPO) stop
