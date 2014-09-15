# Source this file with bash to use hlit from a cabal sandbox.

HLITDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HLITBOX="$HLITDIR/.cabal-sandbox"

function hlit () {
    HLIT="$HLITBOX/bin/hlit"
    DB=("$HLITBOX"/*.conf.d)
    "$HLIT" -g -package-db -g "$DB" "$@"
}

function open-doc () {
    DOCROOT=("$HLITBOX"/share/doc/*)
    DOC=("$DOCROOT/$1"-*/html/index.html)
    xdg-open "$DOC"
}

function show-html () {
    OUTFILE="$(mktemp --suffix .html)"
    cat > "$OUTFILE"
    ( "${BROWSER:-xdg-open}" "$@" "$OUTFILE" & disown )
    ( (sleep 10; rm "$OUTFILE") & disown )
}
