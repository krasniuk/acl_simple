all: get-deps compile

get-deps:
	@rebar3 get-deps

compile:
	@rebar3 compile

clean:
	@rm -rf _build/
	@rm rebar.lock

eunit:
	@rebar3 eunit

ct:
	@rebar3 ct

tests: eunit ct

default: 
	@rebar3 release

rel: 
	@rebar3 as prod release

relclean:
	@rm -rf _build/

dialyze:
	@rebar3 dialyzer

docs:
	@rebar3 edoc
