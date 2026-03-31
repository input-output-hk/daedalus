#!/usr/bin/env bash

set -euo pipefail

readonly SNAPSHOTS_DIR="agentic/snapshots"

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
env_file="$repo_root/agentic/.env"

if [ -f "$env_file" ]; then
  set -a
  source "$env_file"
  set +a
fi

fail() {
  printf '%s\n' "$1" >&2
  exit 1
}

[ "$#" -eq 1 ] || fail "usage: yarn agentic:kb:import -- <snapshot-basename|snapshot.dump>"

snapshot_arg="$1"
case "$snapshot_arg" in
  *.dump) snapshot_arg="${snapshot_arg%.dump}" ;;
esac

[ -n "$snapshot_arg" ] || fail "import helper requires a non-empty snapshot basename"

dump_path="$repo_root/$SNAPSHOTS_DIR/$snapshot_arg.dump"
manifest_path="$repo_root/$SNAPSHOTS_DIR/$snapshot_arg.manifest.json"

[ -f "$dump_path" ] || fail "snapshot dump not found: $dump_path"
[ -f "$manifest_path" ] || fail "snapshot manifest not found: $manifest_path"

cd "$repo_root"
docker compose -f docker-compose.agentic.yml run --rm kb-tools snapshot import "$dump_path" --yes
