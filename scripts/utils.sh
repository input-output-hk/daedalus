#!/usr/bin/env bash

usage() {
  test -z "$1" || { echo "ERROR: $*" >&2; echo >&2; }
  cat >&2 <<EOF
  Usage:
    $0 OPTIONS*

  Build a Daedalus installer.

  Options:
    --clusters "[CLUSTER-NAME...]"
                              Build installers for CLUSTERS.  Defaults to "mainnet staging testnet"
    --fast-impure             Fast, impure, incremental build
    --build-id BUILD-NO       Identifier of the build; defaults to '0'

    --nix-path NIX-PATH       NIX_PATH value

    --upload-s3               Upload the installer to S3
    --test-installer          Test the installer for installability

    --verbose                 Verbose operation
    --quiet                   Disable verbose operation

EOF
    test -z "$1" || exit 1
}

validate_arguments() { test $# -ge 2 -a ! -z "$2" || usage "empty value for" "$1"; }

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

retry() {
  local tries=$1
  validate_arguments "iteration count" "$1"
  shift
  for i in $(seq 1 "${tries}"); do
    if "$@"; then
      return 0
    else
      echo "failed, retry #$i of ${tries}"
    fi
    sleep 5
  done
  fail "persistent failure to exec:  $*"
}
