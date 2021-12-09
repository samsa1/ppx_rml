#!/bin/sh

ocamlfind ocamlc -o $2 -I `rmlc -where` -thread -linkpkg -package graphics rmllib.cma $1