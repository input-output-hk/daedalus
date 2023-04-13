#!/usr/bin/env bash
set -e

if [ $# -eq 0 ] ; then
  echo >&2 "fatal: usage: $0 <command> [<args>...]"
  exit 1
fi

echo "~~~ Obtaining ‘pkgs.nixUnstable’"

# fe2ecaf706a5907b5e54d979fbde4924d84b65fc is nixos-unstable on 2023-04-13
nixUnstable=$(nix-build https://github.com/NixOS/nixpkgs/archive/fe2ecaf706a5907b5e54d979fbde4924d84b65fc.tar.gz -A pkgs.nixUnstable)

PATH="$nixUnstable/bin:$PATH"

# *Maybe* prevent segfaults on `aarch64-darwin` in `GC_*` code:
export GC_DONT_GC=1 # <https://chromium.googlesource.com/chromiumos/third_party/gcc/+/f4131b9cddd80547d860a6424ee1644167a330d6/gcc/gcc-4.6.0/boehm-gc/doc/README.environment#151>

export NIX_CONFIG='
  experimental-features = nix-command flakes
'

nix --version
echo

echo "~~~ Running ‘$1’"

exec "$@"
