# Source this file with bash to use hlit from a cabal sandbox in hlit-tool.

HLITDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SANDBOX="$HLITDIR/hlit-tool/.cabal-sandbox"

function hlit () {
    HLIT="$SANDBOX/bin/hlit"
    DB=("$SANDBOX"/*.conf.d)
    "$HLIT" -g -package-db -g "$DB" "$@"
}

function open-doc () {
    DOCROOT=("$SANDBOX"/share/doc/*)
    DOC=("$DOCROOT/$1"-*/html/index.html)
    xdg-open "$DOC"
}
