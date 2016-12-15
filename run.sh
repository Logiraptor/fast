#!/bin/sh

set -e

make

docker run -it -v `pwd`:/code ocaml-core _build/main.native $@