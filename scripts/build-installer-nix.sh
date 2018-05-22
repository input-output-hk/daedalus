#!/usr/bin/env bash

set -ex

BUILDKITE_BUILD_NUMBER=$1

upload_artifacts() {
    buildkite-agent artifact upload "$@" --job $BUILDKITE_JOB_ID
}

upload_artifacts_public() {
    buildkite-agent artifact upload "$@" ${ARTIFACT_BUCKET:-} --job $BUILDKITE_JOB_ID
}

rm -rf dist || true

echo '~~~ Pre-building node_modules with nix'
nix-build default.nix -A rawapp.deps -o node_modules.root -Q

echo '~~~ Building mainnet installer'
nix-build release.nix -A mainnet.installer --argstr buildNr $BUILDKITE_BUILD_NUMBER -o csl-daedalus
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
  upload_artifacts_public csl-daedalus/daedalus*.bin
  daedalus_config=$(nix-build -A daedalus.cfg --no-out-link                           ./default.nix)
  for cf in launcher-config wallet-topology
  do cp ${daedalus_config}/etc/$cf.yaml  $cf-mainnet.linux.yaml
     upload_artifacts $cf-mainnet.linux.yaml
  done
fi

echo '~~~ Building staging installer'
nix-build release.nix -A staging.installer --argstr buildNr $BUILDKITE_BUILD_NUMBER -o csl-daedalus
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
  upload_artifacts_public csl-daedalus/daedalus*.bin
  daedalus_config=$(nix-build -A daedalus.cfg --no-out-link --argstr cluster staging ./default.nix)
  for cf in launcher-config wallet-topology
  do cp ${daedalus_config}/etc/$cf.yaml  $cf-staging.linux.yaml
     upload_artifacts $cf-staging.linux.yaml
  done
fi
