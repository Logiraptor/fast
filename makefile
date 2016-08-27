

pwd = $(shell pwd)


main.native: *.ml parser.mly lexer.mll
	docker build -t ocaml-core $(pwd)
	docker run -v $(pwd):/home/opam ocaml-core



# ocamlc -c parser.mli
# ocamlc -c lexer.ml
# ocamlc -c parser.ml
# ocamlc -c calc.ml
# ocamlc -o calc lexer.cmo parser.cmo calc.cmo
