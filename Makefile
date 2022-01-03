build:
	dune build stdlib
	RML_RECOMPILE_RZI=0 dune build 

test: build
	dune runtest

install: build
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
