#!/usr/bin/env bash

set -e

BUILDKITE_BUILD_NUMBER="$1"

upload_artifacts() {
    buildkite-agent artifact upload "$@" --job "$BUILDKITE_JOB_ID"
}

upload_artifacts_public() {
    buildkite-agent artifact upload "$@" "${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
}

rm -rf dist || true

CLUSTERS="$(xargs echo -n < "$(dirname "$0")/../installer-clusters.cfg")"

echo '~~~ Pre-building node_modules with nix'
nix-build default.nix -A rawapp.deps -o node_modules.root -Q

for cluster in ${CLUSTERS}
do
  echo '~~~ Building '"${cluster}"' installer'
  nix-build -Q default.nix -A "wrappedBundle" --argstr cluster "${cluster}" --argstr buildNum "$BUILDKITE_BUILD_NUMBER" -o csl-daedalus
  if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    upload_artifacts_public csl-daedalus/daedalus*.bin
    nix-build -A daedalus.cfg  --argstr cluster "${cluster}"
    cp result/etc/launcher-config.yaml  "launcher-config-${cluster}.linux.yaml"
    upload_artifacts "launcher-config-${cluster}.linux.yaml"
  fi
done
