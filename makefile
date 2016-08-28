

pwd = $(shell pwd)


main.native: src/*
	docker build -t ocaml-core $(pwd)
	docker run -v $(pwd)/_build:/code/_build ocaml-core


clean:
	rm -rf _build

nodocker: *.ml parser.mly lexer.mll
	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkg core main.native