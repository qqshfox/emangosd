ERL=erl
REBAR=./rebar
APP=emangosd

all: clean compile

compile:
	$(REBAR) compile

generate:
	$(REBAR) generate

start: compile
	$(ERL) -pa ebin -boot start_sasl -s $(APP) start
clean:
	$(REBAR) clean
	rm -rf temp *.dump
