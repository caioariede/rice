# vim: noexpandtab

compile:
	# Compiling libs
	erlc -o ebin/ lib/*.erl
	# Compiling compiler
	erlc -o ebin/ src/*.erl
	#erl -pa neotoma/ebin/ -noshell -eval 'neotoma:file("src/rice_peg.peg", [{transform_module, rice_transform}]), halt().'
	erlc -o ebin/ -pa neotoma/ebin/ src/rice_peg.erl #&& rm src/rice_peg.erl

clean:
	# Cleaning compiled files
	rm ebin/*

test:
	erl -noshell -eval 'io:format("~s~n", [rice:compile("tests/test_export.ri")]), halt().'

peg2:
	erlc -o ebin/ src/peg2.erl
	erlc -o ebin/ src/peg2rice.erl
