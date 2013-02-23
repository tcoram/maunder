REBAR=./rebar

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build_plt

analyze:
	@$(REBAR) analyze
