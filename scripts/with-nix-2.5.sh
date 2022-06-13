#!/usr/bin/env bash
set -e

if [ $# -eq 0 ] ; then
  echo >&2 "fatal: usage: $0 <command> [<args>...]"
  exit 1
fi

echo "~~~ Obtaining ‘pkgs.nixUnstable’"

myDir=$(dirname "$0")
nixUnstable=$(nix-build "$myDir"/../default.nix -A pkgs.nixUnstable)

PATH="$nixUnstable/bin:$PATH"

# *Maybe* prevent segfaults on `aarch64-darwin`:
export NIX_DONT_GC=1

export NIX_CONFIG='
  experimental-features = nix-command flakes
'

nix --version
echo

echo "~~~ Running ‘$1’"

exec "$@"
