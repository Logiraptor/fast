

pwd = $(shell pwd)


main.native: *.ml parser.mly lexer.mll
	docker build -t ocaml-core $(pwd)
	docker run -v $(pwd):/home/opam ocaml-core


nodocker: *.ml parser.mly lexer.mll
	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkg core main.native