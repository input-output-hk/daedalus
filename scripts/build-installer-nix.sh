#!/usr/bin/env bash

set -ex

VERSION=$1
BUILDKITE_BUILD_NUMBER=$2

rm -rf dist || true

echo '~~~ Pre-building node_modules with nix'
nix-build default.nix -A rawapp.deps -o node_modules.root -Q

echo '~~~ Building mainnet installer'
nix-build release.nix -A mainnet.installer --argstr buildNr $BUILDKITE_BUILD_NUMBER --argstr version $VERSION
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
  buildkite-agent artifact upload result/Daedalus*installer*.bin --job $BUILDKITE_JOB_ID
  daedalus_config=$(nix-build -A daedalus-config --no-out-link                           ./default.nix)
  for cf in launcher-config wallet-topology
  do cp ${daedalus_config}/$cf.yaml  $cf-mainnet.linux.yaml
     buildkite-agent artifact upload $cf-mainnet.linux.yaml --job $BUILDKITE_JOB_ID
  done
fi

echo '~~~ Building staging installer'
nix-build release.nix -A staging.installer --argstr buildNr $BUILDKITE_BUILD_NUMBER --argstr version $VERSION
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
  buildkite-agent artifact upload result/Daedalus*installer*.bin --job $BUILDKITE_JOB_ID
  daedalus_config=$(nix-build -A daedalus-config --no-out-link --argstr cluster staging ./default.nix)
  for cf in launcher-config wallet-topology
  do cp ${daedalus_config}/$cf.yaml  $cf-staging.linux.yaml
     buildkite-agent artifact upload $cf-staging.linux.yaml --job $BUILDKITE_JOB_ID
  done
fi
