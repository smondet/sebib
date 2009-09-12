#! /bin/sh

GODI_PATH=$HOME/usr/godi311
export PATH=$GODI_PATH/bin:$GODI_PATH/sbin:$PATH;
export MANPATH=$GODI_PATH/man:$MANPATH

prepro() {
    local T=_build/$1
    mkdir -p `dirname $T`
    sed -e 's/##.*$//' $1 > $T
    echo $T
}
SOURCES="src/main.ml"
for i in $SOURCES ; do prepro $i ; done;
cd _build
ocamlfind batteries/ocamlc -g -package xml-light,sexplib.syntax -dtypes \
    -o ../sebib $SOURCES
cd ..


