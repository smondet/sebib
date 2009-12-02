#! /bin/sh

build ()
{
    local APPEXT="byte"
    local LIBEXT="cma"
    local CMEXT="cmo"
    local MKLIB="ocamlc -a"
    case "$1" in
        "opt" )
            APPEXT="native"
            LIBEXT="cmxa"
            CMEXT="cmx"
            MKLIB="ocamlopt -a"
            ;;
        "debug" )
            APPEXT="d.byte"
            ;;
    esac
    local TAGOPT="-tags pkg_xml-light,pkg_sexplib.syntax,pkg_sexplib"

    local I_OPT="-I src/app -I src/lib"
    local FLAGS="-cflags -dtypes "
    local ALL_FLAGS="$I_OPT $TAGOPT $BRTXOPT $FLAGS"
    local TARGETS="src/app/sebib_main.$APPEXT" # libsebib.$LIBEXT"
    ocamlbuild $ALL_FLAGS -tag really_link $TARGETS
    local dir=_build/src/lib/
    local LIB_FILES="$dir/sebib_std.$CMEXT $dir/sebib.$CMEXT "
    $MKLIB -o _build/src/lib/libsebib.$LIBEXT $LIB_FILES
    rm -f sebib && ln -s sebib_main.$APPEXT sebib
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
        "o" ) build "opt";;
        "bg" ) build "debug" ;;
        "c" ) ocamlbuild -clean ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done
