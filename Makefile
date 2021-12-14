build:
	(cd configure-tools && dune build embedrzi.exe)
	./_build/default/configure-tools/embedrzi.exe . > compiler/global/rzi.ml
	echo "rzi.ml written"
	dune build --profile release

test: build
	dune runtest

install: build
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
