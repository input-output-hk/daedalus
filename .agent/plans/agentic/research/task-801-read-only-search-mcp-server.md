# Task 801 Read-Only Search MCP Server Research

- Date: 2026-03-29
- Task: `task-801`
- Evidence: `agentic/src/agentic_kb/mcp/search_server.py`, `agentic/src/agentic_kb/cli.py`, `docker-compose.agentic.yml`, `agentic/tests/test_mcp_search_server.py`, `agentic/tests/test_mcp_search_server_db.py`

## Durable Findings

- The accepted packaged runtime shape is a stdio JSON-RPC MCP server launched only through `agentic-kb mcp-search`; the Compose `mcp-search` service now runs that same packaged entrypoint as a parity/smoke harness and does not expose an alternate HTTP daemon.
- The MCP layer can stay thin and fully read-only by reusing the existing packaged CLI/search seams: `serialize_search_result_set`, `serialize_search_hit`, `serialize_entity_payload`, `serialize_status_report`, registry-backed `get_entity_payload(...)`, and `PostgresSearchStore.search(...)` cover the approved success-payload contract without inventing a second retrieval stack.
- The pinned v1 tool surface is implemented as exactly seven tools: `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`. Validation failures, incompatible filter/entity combinations, and missing entities surface as MCP tool-call errors rather than success payloads with `ok: false`; `kb_status` remains the one observational exception and returns degraded readiness as a successful payload with `ok: false`.
- `find_related` is implemented as the approved BM25-only wrapper: it resolves the seed row through the generic entity lookup path, derives seed text only from the entity config's `text_columns`, normalizes whitespace, caps the query text at `4000` characters, runs one bounded over-fetch BM25 search, filters out the seed `(entity_type, id)` in the MCP layer, and never calls Ollama.
- `search`, `search_docs`, `search_code`, and `search_github` preserve the current search-store behavior for `bm25`, `vector`, and `hybrid` modes. Because the MCP layer reuses `PostgresSearchStore.from_config(...)`, vector and hybrid calls still fail explicitly when Ollama connectivity or model availability breaks; there is no silent MCP-side fallback to BM25.

## Verification Notes

- `python3 -m py_compile` passed for the new MCP module, touched CLI/entity helpers, and the new MCP-focused tests.
- Focused MCP unit coverage passed in `agentic/tests/test_mcp_search_server.py`, including registered tool surface, passthrough payload shape, domain-wrapper filter injection, MCP error semantics, degraded `kb_status`, BM25-only `find_related`, and explicit vector/hybrid error propagation.
- Seeded DB-backed MCP coverage passed in `agentic/tests/test_mcp_search_server_db.py`, including stdio `initialize`, `tools/list`, `tools/call` for `search`, `search_docs`, `get_entity`, `kb_status`, and `find_related` against seeded KB rows.
- `docker compose -f docker-compose.agentic.yml config` passed after replacing the placeholder `mcp-search` service with the packaged build-backed server shape.

## No New Research Beyond Task Scope

- Task-801 did not add write-capable MCP behavior, setup-doc rollouts for local agent clients, workflow finalization beyond minimum truth maintenance, or any new indexing/retrieval subsystem.
