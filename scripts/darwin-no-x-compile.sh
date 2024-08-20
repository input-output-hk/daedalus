#!/usr/bin/env bash

set -o errexit
set -o pipefail

# x86_64 cross-compilation won’t fly in this pure derivation:
if [[ $system == *darwin* ]]; then
  if [[ $system == *aarch64* ]] ; then
    changeFrom="x86_64"
    changeTo="arm64"
  else
    changeFrom="arm64"
    changeTo="x86_64"
  fi
  find node_modules/ -type f '(' -name '*.gyp' -o -name '*.gypi' ')' \
    | ( xargs grep -F "$changeFrom" || true ; ) | cut -d: -f1 | sort --unique \
    | while IFS= read -r file
  do
    sed -r "s/$changeFrom/$changeTo/g" -i "$file"
  done
fi
