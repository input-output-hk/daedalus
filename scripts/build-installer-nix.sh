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

for cluster in ${CLUSTERS}
do
  echo '~~~ Building '"${cluster}"' installer'
  nix-build -Q release.nix -A "${cluster}.appImage" --argstr buildNum "$BUILDKITE_BUILD_NUMBER" -o csl-daedalus
  if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    upload_artifacts_public csl-daedalus/*.AppImage
  fi
  nix-build -Q release.nix -A "${cluster}.linuxInstaller" --argstr buildNum "$BUILDKITE_BUILD_NUMBER" -o csl-daedalus
  if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    upload_artifacts_public csl-daedalus/*.bin
  fi
done
