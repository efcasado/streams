REBAR  = ./rebar

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit --verbose

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc
