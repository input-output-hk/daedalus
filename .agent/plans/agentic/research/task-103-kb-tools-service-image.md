# Task 103 KB-Tools Service Image Research

- Date: 2026-03-27
- Task: `task-103`
- Evidence: `agentic/Dockerfile`, `agentic/src/agentic_kb/commands/status.py`, `docker-compose.agentic.yml`

## Durable Findings

- `kb-tools` now uses a local Docker build rooted at `agentic/` with `ENTRYPOINT ["agentic-kb"]` and default `CMD ["service"]`, which preserves both `docker compose up -d kb-tools` service mode and `docker compose run --rm kb-tools --help` CLI help behavior.
- The first real tools image uses `python:3.12-slim-bookworm` instead of Alpine to avoid likely follow-up friction when later tasks add PostgreSQL-adjacent tooling and Python dependencies.
- Stable in-container paths are now part of the platform contract: the repo mounts at `/workspace` and snapshots mount at `/workspace/agentic/snapshots`.
- `kb-tools` health is now driven by `agentic-kb status --healthcheck`, which checks config parsing, mount visibility, ParadeDB TCP reachability, Ollama API reachability, and configured embedding-model presence without requiring schema tables or sync state.
- The durable CLI surface now exists even though most behavior is still deferred: `status`, `service`, `sync all|changed|docs|code|github|project`, and `snapshot export|import`.

## Gotchas And Constraints

- `status` and `service` are intentionally Compose-oriented; outside the expected container runtime they can report degraded status because `/workspace` mounts or the configured Ollama model are not present.
- The healthcheck depends on the configured embedding model already being visible through Ollama, so `ollama-init` remains an important readiness gate for later automation.
- `docker compose run` may still emit dependency startup noise unless callers use flags like `--no-deps`; that is workflow polish for later tasks rather than a task-103 bug.
