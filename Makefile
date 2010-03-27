# vim: noexpandtab

compile:
	# Compiling libs
	erlc -o ebin/ lib/*.erl
	# Compiling compiler
	erlc -o ebin/ src/rice_compiler.erl
	erlc -o ebin/ src/rice_transform.erl
	erl -pa neotoma/ebin/ -noshell -eval 'neotoma:file("src/rice_peg.peg", [{transform_module, rice_transform}]), halt().'
	erlc -o ebin/ -pa neotoma/ebin/ src/rice_peg.erl && rm src/rice_peg.erl

clean:
	# Cleaning compiled files
	rm ebin/*

test:
	erl -noshell -eval 'io:format("~s~n", [rice:compile("tests/test_export.ri")]), halt().'
