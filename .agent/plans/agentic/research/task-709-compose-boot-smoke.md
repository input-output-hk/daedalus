# Task 709 Compose Boot Smoke Research

- Date: 2026-03-30
- Task: `task-709`
- Evidence: `agentic/tests/test_compose_bootstrap.py`, `docker-compose.agentic.yml`

## Durable Findings

- The repo-root Compose smoke regression should stay host-side and explicitly gated. Running it through normal unittest discovery without `AGENTIC_RUN_COMPOSE_SMOKE=1` would otherwise unexpectedly boot the full Docker stack and potentially trigger a cold-cache Ollama model pull.
- A unique Compose project name is not enough to isolate this stack because `docker-compose.agentic.yml` publishes fixed loopback host ports by default. The smoke suite must set both `AGENTIC_DB_PORT` and `OLLAMA_PORT` per run to avoid collisions with an existing local stack.
- `docker compose up -d` needs its own generous timeout and failure-context capture separate from the later readiness poll. Cold-cache `ollama-init` work is part of the real boot contract, so failures are much easier to diagnose when the test records the timed command output plus isolated-project `docker compose ps` and bounded `docker compose logs --no-color --tail 200` output.
- `docker compose port` is a cheap way to prove the smoke run actually used the per-run host-port overrides rather than silently reusing the checked-in defaults.

## No New Research

- The teardown-visibility fix did not add new durable platform findings; it only corrected the regression harness to fail truthfully when `docker compose down -v` exits non-zero.
