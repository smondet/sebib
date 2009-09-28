#! /bin/sh

build ()
{
    local TAGOPT="-tags pkg_xml-light,pkg_sexplib.syntax"

    local I_OPT="-I src/app -I src/lib"
    local FLAGS="-cflags -dtypes "
    local ALL_FLAGS="$I_OPT $TAGOPT $BRTXOPT $FLAGS"
    local TARGETS="src/app/sebib_main$1.byte libsebib.cma"
    ocamlfind batteries/ocamlbuild $ALL_FLAGS $TARGETS
    rm -f sebib && ln -s sebib_main$1.byte sebib
}

echo_help ()
{
    echo "\
$0 <cmd>
b: Build all (default action)
bg: Build all with debug symbols
c: Clean
h: This help"
}

if [ $# -eq 0 ]; then
    build
    exit $?
fi

for todo in $* ; do
    case "$todo" in
        "b" ) build ;;
        "bg" ) build ".d" ;;
        "c" ) ocamlbuild -clean ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done
