build:
	(cd configure-tools && dune build embedrzi.exe)
	./_build/default/configure-tools/embedrzi.exe . > compiler/global/rzi.ml
	echo "rzi.ml written"
	(cd compiler && dune build rmlc.a)

test : build
	(cd test && dune runtest)


clean:
	dune clean
