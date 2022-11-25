#!/usr/bin/env bash
set -e

if [ $# -eq 0 ] ; then
  echo >&2 "fatal: usage: $0 <command> [<args>...]"
  exit 1
fi

userNixConfig="$HOME/.config/nix/nix.conf"
if [ -e "$userNixConfig" ] ; then
  echo "~~~ Warning: cleaning user’s Nix config: $userNixConfig"
  echo "Sometimes, a conflicting nix.conf appears in ~/.config/nix, which"
  echo "results in builds not using our global substituters (binary caches)."
  echo
  mv -v "$userNixConfig" "$userNixConfig.$(date -Iseconds)"
fi

echo "~~~ Obtaining ‘nixFlakes’"

# *Maybe* prevent segfaults on `aarch64-darwin` in `GC_*` code:
export GC_DONT_GC=1 # <https://chromium.googlesource.com/chromiumos/third_party/gcc/+/f4131b9cddd80547d860a6424ee1644167a330d6/gcc/gcc-4.6.0/boehm-gc/doc/README.environment#151>

export NIX_CONFIG='
  experimental-features = nix-command flakes
  accept-flake-config = true
'

# 9e96b1562d67a90ca2f7cfb919f1e86b76a65a2c is `nixos-22.05` on 2022-07-06
nixFlakes=$(nix-build --no-out-link 'https://github.com/NixOS/nixpkgs/archive/9e96b1562d67a90ca2f7cfb919f1e86b76a65a2c.tar.gz' -A nixFlakes)

PATH="$nixFlakes/bin:$PATH"

nix --version
echo

echo "~~~ Running ‘$1’"

exec "$@"
