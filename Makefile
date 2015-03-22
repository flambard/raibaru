all:
	erl -pa ebin/ -pa ../matchmaker/ebin -make

clean:
	rm ebin/*.beam

dialyzer:
	dialyzer ebin/ -r ../matchmaker/ebin -r ../gnugo/ebin
