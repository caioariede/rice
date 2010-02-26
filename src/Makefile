# vim: noexpandtab

compile:
	erl -noshell -compile "rice_transform.erl"
	# Compiling PEG
	erl -pa ../neotoma/ebin/ -noshell -eval 'neotoma:file("rice_peg.peg", [{transform_module, rice_transform}]), halt().'
	# Compiling generated file
	erl -pa ../neotoma/ebin/ -noshell -compile "rice_peg.erl"
	# Compiling rice compiler frontend
	erl -pa ../neotoma/ebin/ -noshell -compile "rice.erl"

test:
	erl -noshell -eval 'io:format("~s~n", [rice:file("test.ri")]), halt().'
