#!/usr/bin/env bash

#
# This script *can* help in minimizing runtime dependencies of
# Daedalus… but only the ones which prevent Electron from starting.
#
# That’s a good starting point before we get this automatically from
# webpack – <https://input-output.atlassian.net/browse/DDW-1103>.
#

set -o errexit
set -o pipefail

if [ -z "$IN_NIX_SHELL" ] ; then
  echo >&2 'fatal: please, enter nix-shell first'
  exit 1
fi

dir=$(git rev-parse --show-toplevel)
srcDir="$dir"/node_modules.pre-minimize-"$(date -Ins)"
dstDir="$dir"/node_modules

yarn build
mv "$dstDir" "$srcDir"
mkdir "$dstDir"

while true ; do

  the_log=$(mktemp)
  export the_log

  bash -c '
    trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
    electron ./ 2>&1 | tee "$the_log"
  ' &
  child_pid=$!

  sleep 2
  kill TERM $child_pid

  module=$(head "$the_log" -n 2 | tail -n 1 | grep -F 'Error: Cannot find module')
  rm "$the_log"

  if [ -z "$module" ]; then
    echo 'done?'
    exit 77
  else
    module=$(echo "$module" | sed -r "s/^.*'(.*)'/\1/")

    echo
    echo
    echo "Missing module is: ‘$module’."

    module=$(echo "$module" | cut -d/ -f1)

    echo "Press <RET> to copy ‘$srcDir/$module’ → ‘$dstDir/’, and continue…"
    read -r

    cp -r "$srcDir/$module" "$dstDir/"
  fi

done
