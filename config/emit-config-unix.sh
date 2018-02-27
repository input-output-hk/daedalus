#!/usr/bin/env bash

fail() { echo "FATAL: $*" >&2; exit 1;
       }

OS=
CLUSTER=
CONFIG=
explain=
while test $# -ge 1
do case "$1"
   in --cluster )         CLUSTER=$2; shift;;
      --os )              OS=$2;      shift;;
      --config )          CONFIG=$2;  shift;;
      --explain )         explain="--explain";;
      --trace )           set -x;;
      "--"* )             fail "$0: unknown option: $1";;
      * )     break;;
   esac
   shift
done

isMainnet=False
isStaging=False
case "$CLUSTER" in
        mainnet ) isMainnet=True;;
        staging ) isStaging=True;;
        * ) fail "unsupported --cluster value '$CLUSTER'.  Valid options: mainnet staging";; esac
isWin64=False
isMacos64=False
case "$OS" in
        macos64 ) isMacos64=True;;
        win64 )   isWin64=True;;
        * ) fail "unsupported --os value '$OS'.  Valid options: macos64 win64";; esac
flags="{ isMainnet=$isMainnet \
       , isStaging=$isStaging \
       , isMacos64=$isMacos64 \
       , isWin64=$isWin64     \
       }"

case "$CONFIG" in
        launcher )    config="./launcher.dhall";;
        topology )    config="./topology.dhall";;
        * ) fail "unsupported --config value '$CONFIG'.  Valid options: launcher topology";; esac

nix-shell --show-trace -p dhall-json dhall \
          --run \
          "bash -c \"echo \\\"${config} ${flags}\\\" | dhall ${explain} | dhall-to-yaml\""
