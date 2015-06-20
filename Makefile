all:
	rebar compile

clean:
	rebar clean

dialyzer:
	dialyzer ebin/ -r ../matchmaker/ebin -r ../gnugo/ebin
