# Task 501 Ollama Embedding Client Research

- Date: 2026-03-28
- Task: `task-501`
- Evidence: `docker-compose.agentic.yml`, `agentic/src/agentic_kb/embed/client.py`, live probes against the pinned Ollama image

## Durable Findings

- The pinned Ollama image supports `POST /api/embed` with JSON payloads shaped as `{"model": "all-minilm", "input": "mithril bootstrap"}` for a single string and `{"model": "all-minilm", "input": ["alpha", "beta"]}` for batch input.
- For the pinned image and default `all-minilm` model, `POST /api/embed` returns an `embeddings` field for both single and batch input; single input is still normalized as a one-item list.
- The pinned image also still answers `POST /api/embeddings` with a legacy `embedding` field for single-input requests, so the task-501 client keeps response normalization support for both `embedding` and `embeddings` payloads even though its primary transport path is `POST /api/embed`.
- Live verification confirmed the default configured model `all-minilm` produces vectors of length `384`, matching the schema contract introduced in `agentic/schema/init.sql` for `VECTOR(384)` columns.
- `ollama-init` remains the readiness gate for safe live verification: successful embedding smoke checks depended on the configured model already being visible through `/api/tags` as `all-minilm:latest`.

## Gotchas And Constraints

- `docker compose run` against `kb-tools` inherits the service entrypoint `agentic-kb`, so ad hoc Python verification commands must override the entrypoint, for example with `--entrypoint python`, instead of appending `python ...` after the service name.
- Model-missing failures can still happen outside the happy-path Compose boot flow, so downstream callers should preserve the task-local `EmbeddingModelNotFoundError` instead of assuming the configured model always exists.
- The client should keep the dimension guardrail at the embedding boundary because a model swap that changes vector length would otherwise fail later and less clearly when writing or querying `VECTOR(384)` columns.
