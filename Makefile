REBAR=./rebar

.PHONY: all test doc clean distclean

all:
	@$(REBAR) compile

test:
	@$(REBAR) eunit
	@$(REBAR) ct

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
