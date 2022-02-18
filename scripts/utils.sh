#!/usr/bin/env bash
arg2nz() { test $# -ge 2 -a ! -z "$2" || usage "empty value for" "$1"; }
fail() { echo "ERROR: $*" >&2; exit 1; }
retry() {
        local tries=$1; arg2nz "iteration count" "$1"; shift
        for i in $(seq 1 "${tries}")
        do if "$@"
           then return 0
           else echo "failed, retry #$i of ${tries}"
           fi
           sleep 5
        done
        fail "persistent failure to exec:  $*"
}
