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

RUN echo "eval `opam config env`" >> ~/.profile

ADD ./src /code

WORKDIR /code

ENTRYPOINT ocamlbuild -pkg core -use-ocamlfind -use-menhir -tag thread main.native