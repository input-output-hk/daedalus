#!/usr/bin/env bash

set -euo pipefail

readonly SHARED_DIR_NAME="Daedalus_KB"
readonly COMPOSE_FILE="docker-compose.agentic.yml"
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
  [ -w "$shared_dir" ] || fail "AGENTIC_KB_SHARED_DIR is not writable: $shared_dir"

  printf '%s\n' "$shared_dir"
}

snapshot_arg="${1:-}"
if [ -n "$snapshot_arg" ]; then
  case "$snapshot_arg" in
    */*) fail "publish helper accepts a snapshot basename only, not a path: $snapshot_arg" ;;
    *.dump) snapshot_arg="${snapshot_arg%.dump}" ;;
    *.manifest.json) snapshot_arg="${snapshot_arg%.manifest.json}" ;;
  esac
fi

snapshot_basename="${snapshot_arg:-agentic-kb-$(date -u +%Y%m%dT%H%M%SZ)}"
dump_path="$SNAPSHOTS_DIR/$snapshot_basename.dump"
manifest_path="$SNAPSHOTS_DIR/$snapshot_basename.manifest.json"
shared_dir="$(require_shared_dir)"

cd "$repo_root"
mkdir -p "$SNAPSHOTS_DIR"

docker compose -f "$COMPOSE_FILE" run --rm kb-tools sync all
docker compose -f "$COMPOSE_FILE" run --rm kb-tools snapshot export "$dump_path"

[ -f "$dump_path" ] || fail "snapshot export did not create dump file: $dump_path"
[ -f "$manifest_path" ] || fail "snapshot export did not create sibling manifest file: $manifest_path"

cp "$dump_path" "$shared_dir/$snapshot_basename.dump"
cp "$manifest_path" "$shared_dir/$snapshot_basename.manifest.json"

[ -f "$shared_dir/$snapshot_basename.dump" ] || fail "publish copy failed for dump file: $shared_dir/$snapshot_basename.dump"
[ -f "$shared_dir/$snapshot_basename.manifest.json" ] || fail "publish copy failed for manifest file: $shared_dir/$snapshot_basename.manifest.json"

printf 'published snapshot pair: %s\n' "$snapshot_basename"
printf 'local files: %s %s\n' "$dump_path" "$manifest_path"
printf 'shared files: %s %s\n' "$shared_dir/$snapshot_basename.dump" "$shared_dir/$snapshot_basename.manifest.json"
