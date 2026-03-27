# Task 101 Compose Foundation Research

- Date: 2026-03-27
- Task: `task-101`
- Evidence: `docker-compose.agentic.yml`

## Durable Findings

- The v1 compose scaffold is confirmed to use five services: `paradedb`, `ollama`, `ollama-init`, `kb-tools`, and `mcp-search`.
- Container images are pinned by digest for reproducible local boot behavior.
- Host-exposed ports are loopback-only by default: ParadeDB on `127.0.0.1:5445` and Ollama on `127.0.0.1:11434`.
- `ollama-init` is the bootstrap gate for the embedding model and downstream placeholder services wait on it with `service_completed_successfully`.
- `kb-tools` and `mcp-search` are intentionally placeholder long-running containers in this phase. Their healthchecks validate scaffold readiness only, not real ingest or MCP behavior.
- Persistent state is isolated in named volumes for database, Ollama model cache, and placeholder service data.
- The compose scaffold establishes the early environment contract for follow-up tasks: `AGENTIC_DB_PORT`, `AGENTIC_DB_NAME`, `AGENTIC_DB_USER`, `AGENTIC_DB_PASSWORD`, `OLLAMA_PORT`, `OLLAMA_EMBED_MODEL`, `DATABASE_URL`, `OLLAMA_BASE_URL`, and `GITHUB_TOKEN`.

## Gotchas And Constraints

- The ParadeDB container in this environment needed its volume mounted at `/var/lib/postgresql` for successful startup.
- Host port `5432` was already in use locally, so the scaffold defaults ParadeDB to host port `5445` to reduce conflicts.
- `mcp-search` should not publish a host port until the real server exists, otherwise compose health can misleadingly suggest a working endpoint.

## No New Research

- No additional durable findings beyond the compose scaffold behavior above.
