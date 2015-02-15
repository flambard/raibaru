all:
	erl -make

clean:
	rm ebin/*.beam

dialyzer:
	dialyzer ebin/
