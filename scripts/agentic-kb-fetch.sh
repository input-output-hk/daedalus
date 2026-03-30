#!/usr/bin/env bash

set -euo pipefail

readonly SHARED_DIR_NAME="Daedalus_KB"
readonly SNAPSHOTS_DIR="agentic/snapshots"

fail() {
  printf '%s\n' "$1" >&2
  exit 1
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"

require_shared_dir() {
  local shared_dir="${AGENTIC_KB_SHARED_DIR:-}"

  [ -n "$shared_dir" ] || fail "AGENTIC_KB_SHARED_DIR is required and must point to the local Dropbox-synced Daedalus_KB folder"
  [ -d "$shared_dir" ] || fail "AGENTIC_KB_SHARED_DIR does not exist or is not a directory: $shared_dir"
  [ "$(basename -- "$shared_dir")" = "$SHARED_DIR_NAME" ] || fail "AGENTIC_KB_SHARED_DIR must point to the '$SHARED_DIR_NAME' folder: $shared_dir"
  [ -r "$shared_dir" ] || fail "AGENTIC_KB_SHARED_DIR is not readable: $shared_dir"

  printf '%s\n' "$shared_dir"
}

[ "$#" -eq 1 ] || fail "usage: yarn agentic:kb:fetch -- <snapshot-basename|snapshot.dump|snapshot.manifest.json>"

snapshot_arg="$1"
case "$snapshot_arg" in
  */*) fail "fetch helper accepts an explicit snapshot basename or sibling filename only, not a path: $snapshot_arg" ;;
  *.dump) snapshot_arg="${snapshot_arg%.dump}" ;;
  *.manifest.json) snapshot_arg="${snapshot_arg%.manifest.json}" ;;
esac

[ -n "$snapshot_arg" ] || fail "fetch helper requires a non-empty snapshot basename"

shared_dir="$(require_shared_dir)"
shared_dump_path="$shared_dir/$snapshot_arg.dump"
shared_manifest_path="$shared_dir/$snapshot_arg.manifest.json"
local_dump_path="$repo_root/$SNAPSHOTS_DIR/$snapshot_arg.dump"
local_manifest_path="$repo_root/$SNAPSHOTS_DIR/$snapshot_arg.manifest.json"

[ -f "$shared_dump_path" ] || fail "shared snapshot dump does not exist: $shared_dump_path"
[ -f "$shared_manifest_path" ] || fail "shared snapshot manifest does not exist: $shared_manifest_path"

mkdir -p "$repo_root/$SNAPSHOTS_DIR"
cp "$shared_dump_path" "$local_dump_path"
cp "$shared_manifest_path" "$local_manifest_path"

[ -f "$local_dump_path" ] || fail "fetch copy failed for dump file: $local_dump_path"
[ -f "$local_manifest_path" ] || fail "fetch copy failed for manifest file: $local_manifest_path"

printf 'fetched snapshot pair: %s\n' "$snapshot_arg"
printf 'shared files: %s %s\n' "$shared_dump_path" "$shared_manifest_path"
printf 'local files: %s %s\n' "$local_dump_path" "$local_manifest_path"
