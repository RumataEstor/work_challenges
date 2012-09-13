
compile:
	./rebar compile

clean:
	./rebar clean

dialyzer: compile
	echo "-*- mode: compilation-minor -*-">dialyzer_result_a
	-dialyzer ebin --get_warnings -Wrace_conditions -Wunderspecs>>dialyzer_result_a
	grep -x -v -f .dialyzer_exceptions<dialyzer_result_a>dialyzer_result

test: compile always_make
	./rebar eunit

always_make:
