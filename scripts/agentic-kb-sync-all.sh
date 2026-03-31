#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
env_file="$repo_root/agentic/.env"

if [ -f "$env_file" ]; then
  set -a
  source "$env_file"
  set +a
fi

cd "$repo_root"
docker compose -f docker-compose.agentic.yml run --rm kb-tools sync all
