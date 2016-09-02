FROM ubuntu

# add add-apt-repository
RUN apt-get update && \
    apt-get install -y software-properties-common

# taken from https://ocaml.org/docs/install.html#Ubuntu
RUN add-apt-repository --yes ppa:avsm/ppa
RUN apt-get install -y opam

RUN opam init

RUN opam update

RUN apt-get install -y pkg-config
RUN apt-get install -y m4

RUN opam install -y core

RUN opam install -y menhir

RUN opam install -y OUnit

RUN apt-get install -y llvm-3.5-dev libllvm-3.5-ocaml-dev llvm-3.5

RUN apt-get install -y python

RUN opam install -y llvm


ADD ./src /code
ADD ./test /test

RUN mv /test/* /code

WORKDIR /code
