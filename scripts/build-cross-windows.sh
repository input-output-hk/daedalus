#!/usr/bin/env bash

set -e

BUILDKITE_BUILD_NUMBER="$1"

upload_artifacts() {
  buildkite-agent artifact upload "$@" --job "$BUILDKITE_JOB_ID"
}

upload_artifacts_public() {
  buildkite-agent artifact upload "$@" "${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
}

CLUSTERS="$(xargs echo -n < "$(dirname "$0")/../installer-clusters.cfg")"

for cluster in ${CLUSTERS}; do
  echo '~~~ Building '"${cluster}"' installer'
  # to enable signing, re-add --arg signingKeys '{ spc = ./dummy-certs/authenticode.spc; pvk = ./dummy-certs/authenticode.pvk; }'
  nix-build default.nix -A windows-installer --show-trace  --allow-unsafe-native-code-during-evaluation --argstr cluster "$cluster" --argstr buildNum "$BUILDKITE_BUILD_NUMBER" --argstr target "x86_64-windows"
  if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    upload_artifacts_public result/daedalus-*-windows*.exe
  fi
done
