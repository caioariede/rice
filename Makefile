# vim: noexpandtab

compile:
	erlc -o ebin/ "src/rice_transform.erl"
	# Compiling PEG
	erl -pa neotoma/ebin/ -noshell -eval 'neotoma:file("src/rice_peg.peg", [{transform_module, rice_transform}]), halt().'
	# Compiling generated file
	erlc -o ebin/ -pa neotoma/ebin/ "src/rice_peg.erl" && rm src/rice_peg.erl
	# Compiling frontend
	erlc -o ebin/ -pa neotoma/ebin/ -pa lib/ "src/rice.erl"

test:
	erl -noshell -eval 'io:format("~s~n", [rice:compile("tests/test_export.ri")]), halt().'
