# vim: noexpandtab

compile:
	# Compiling libs
	erlc -o ebin/ lib/*.erl
	# Compiling compiler
	erlc -o ebin/ src/*.erl

clean:
	# Cleaning compiled files
	rm ebin/*
