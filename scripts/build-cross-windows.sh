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
  nix-build default.nix -A windows-installer --arg signingKeys '{ spc = ./dummy-certs/authenticode.spc; pvk = ./dummy-certs/authenticode.pvk; }' --show-trace  --allow-unsafe-native-code-during-evaluation --argstr cluster "$cluster" --argstr buildNum "$BUILDKITE_BUILD_NUMBER"
  if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    upload_artifacts_public result/daedalus-*-windows*.exe
  fi
done
