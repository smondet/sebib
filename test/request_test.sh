#! /bin/sh

SOURCES="test/valid.sebib"

do_test() {
    echo "--------------------------"
    echo "  -select \"$1\""
    sebib -format "@{id}: @{title}@{n}" -select "$1" $SOURCES
}

do_test "(ids (most09complete one07author))"
do_test "(ids (minimal07working))"

do_test "(list_and ((ids (most09complete one07author)) (ids (minimal07working))))"
do_test "(list_or ((ids (most09complete one07author)) (ids (minimal07working))))"


do_test "(tags (onetag))"
do_test "(tags (onemoretag))"
do_test "(list_and ((tags (onemoretag)) (not (tags (tag7)))))"

do_test "(matches (authors Joh))"

do_test "(matches (title \"comple.*\"))"
do_test "(not (matches (title comple.*)))"

do_test "(matches (more \"\"))"
