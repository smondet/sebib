#! /bin/sh

GODI_PATH=$HOME/usr/godi311
export PATH=$GODI_PATH/bin:$GODI_PATH/sbin:$PATH;
export MANPATH=$GODI_PATH/man:$MANPATH

ocamlfind batteries/ocamlc -package sexplib.syntax -dtypes -o sebib src/main.ml
