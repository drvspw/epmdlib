BASEDIR = $(shell pwd)
REBAR = rebar3

APPNAME = $(shell basename $(BASEDIR))
ifneq ($(APPNAME), epmdlib)
  APPNAME = epmdlib
endif

LINTERS = check-deps lint xref eunit

ERLCMD := erl -pa _build/default/lib/$(APPNAME)/ebin/

compile: ## compile
	$(REBAR) compile

eunit: ## run eunit tests
	$(REBAR) eunit

xref: ## xref analysis
	$(REBAR) xref

dialyzer: ## dialyzer
	$(REBAR) dialyzer

check-deps: ## check dependencies
	$(REBAR) check-deps

lint: ## lint
	$(REBAR) lint

test: $(LINTERS)

console: test ## launch a shell
	$(REBAR) shell

clean: ## clean
	$(REBAR) clean
	rm -rf _build

test-coverage-report: ## generate test coverage report
	$(REBAR) cover --verbose

foo1: ## start a node name foo1
	$(ERLCMD) -proto_dist epmdlib_common -start_epmd false -epmd_module epmdlib_epmd_client -epmdlib_dist_transport tcp6 -sname foo1@localhost

foo2: ## start a node name bar
	$(ERLCMD) -proto_dist epmdlib_common -start_epmd false -epmd_module epmdlib_epmd_client -epmdlib_dist_transport tcp6 -sname foo2@localhost

help: ## Display help information
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
