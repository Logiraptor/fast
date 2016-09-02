

pwd = $(shell pwd)

buildCmd = opam config exec "ocamlbuild \
			-pkgs core,llvm,llvm.bitreader \
			-use-ocamlfind -use-menhir -tag thread $@"


main.native: src/*
	docker build -t ocaml-core $(pwd)
	docker run -v $(pwd)/_build:/code/_build ocaml-core $(buildCmd)


test: test.native
	./_build/test.native


test.native: src/*
	docker build -t ocaml-core $(pwd)
	docker run -v $(pwd)/_build:/code/_build ocaml-core $(buildCmd)


clean:
	rm -rf _build


nodocker: *.ml parser.mly lexer.mll
	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkg core main.native
