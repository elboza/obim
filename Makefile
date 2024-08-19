TARGET=obim
.PHONY: .help .clean
all: help

build:
	ocamlyacc -v parser.mly
	ocamllex lexer.mll
	ocamlfind ocamlopt -thread -package ocamline -linkpkg ast.ml parser.mli lexer.ml parser.ml env.ml native_lib.ml eval.ml repl.ml obim.ml -o $(TARGET)

clean:
	rm -f *.cmo 
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.o
	rm -f lexer.ml parser.ml parser.mli

help:
	@echo "obim Makefile..."

