#!/usr/bin/env bash

# This small wrapper over `nix-shell`, called from package.json
# (e.g. `yarn nix:testnet`) allows to specify an initial command to be
# run inside the `nix-shell` once its ready, e.g.:
#
# $ yarn nix:testnet yarn dev
#
# After the command finishes, you will still be left inside the
# nix-shell.

if [ $# -lt 2 ] ; then
   echo >&2 'fatal: usage: '"$0"' <network> <cluster> [<initial command> ...]'
   exit 1
fi

NETWORK="$1" ; shift
cluster="$1" ; shift

# Unfortunately, we need to shell-escape the command:
# cf. <https://github.com/NixOS/nixpkgs/blob/877eb10cfbdb3e3d54bdc7a7f6fd3c4b4e6396da/lib/strings.nix#L310-L328>
command=''
while [ $# -gt 0 ] ; do
  command="$command '""${1//\'/\'\\\'\'}""'" ; shift
done

if [ -z "$command" ] ; then
  command=':'   # no-op, if no command
fi

export NETWORK

# Prevent segfaults on Darwin in `GC_*` code:
export GC_DONT_GC=1

# `return` will make the user stay in `nix-shell` after the initial command finishes:
exec nix-shell --argstr nodeImplementation cardano --argstr cluster "$cluster" --command "$command ; return"
