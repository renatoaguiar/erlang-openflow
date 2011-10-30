REBAR=./rebar

all:
	@$(REBAR) compile

test:
	@$(REBAR) eunit
	@$(REBAR) ct

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
