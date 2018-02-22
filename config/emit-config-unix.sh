#!/usr/bin/env bash

fail() { echo "FATAL: $*" >&2; exit 1;
       }

OS=
CLUSTER=
CONFIG=
while test $# -ge 1
do case "$1"
   in --cluster )         CLUSTER=$2; shift;;
      --os )              OS=$2;      shift;;
      --config )          CONFIG=$2;  shift;;
      "--"* )             fail "$0: unknown option: $1";;
      * )     break;;
   esac
   shift
done

case "$CLUSTER" in
        mainnet | staging ) true;;
        * ) fail "unsupported --cluster value '$CLUSTER'.  Valid options: mainnet staging";; esac
case "$OS" in
        macos64 | win64 ) true;;
        * ) fail "unsupported --os value '$OS'.  Valid options: macos64 win64";; esac
case "$CONFIG" in
        launcher )        components="./common.dhall // ./${CLUSTER}.dhall // ./${OS}.dhall // ./${CLUSTER}-${OS}.dhall";;
        wallet-topology ) components="./topology-${CLUSTER}.dhall";;
        * ) fail "unsupported --config value '$CONFIG'.  Valid options: launcher wallet-topology";; esac

nix-shell --show-trace -p dhall-json dhall \
          --run \
          "bash -c \"echo \\\"${components}\\\" | dhall | dhall-to-yaml\""
