# Task Plan: task-801 Implement read-only Search MCP server

- Task ID: `task-801`
- Title: `Implement read-only Search MCP server`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-801` is the next unblocked critical-path MCP task after the search/query and local command foundations from `task-502` and `task-503` completed.
- The platform plan, workflow doc, and Compose stack all already reserve a read-only `mcp-search` surface, but repo reality is still a placeholder shell container in `docker-compose.agentic.yml`.
- Downstream tasks `task-802`, `task-803`, and `task-901` all need a real MCP server contract before setup docs, workflow finalization, and clean-machine validation can be completed.

## Scope

- Replace the placeholder `mcp-search` service with a real packaged read-only MCP server for KB search.
- Implement the MCP tool surface already named in the platform plan: `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`.
- Reuse the existing packaged search, entity, and status contracts from `agentic/src/agentic_kb/search/query.py`, `agentic/src/agentic_kb/commands/search.py`, `agentic/src/agentic_kb/commands/entity.py`, and `agentic/src/agentic_kb/commands/status.py` instead of inventing a separate retrieval stack.
- Keep the server read-only against ParadeDB, GitHub, and the repository.
- Add focused automated coverage plus at least one seeded DB-backed MCP smoke path.

## Non-Goals

- Do not write OpenCode, Claude Code, or `.mcp.json` setup documentation; that remains `task-802`.
- Do not do the broader workflow reconciliation in `.agent/workflows/agentic-kb.md`; that remains `task-803`.
- Do not add write-capable MCP tools, GitHub/project-management MCP behavior, sync orchestration, stale detection, or a new retrieval/indexing subsystem.
- Do not redesign the existing CLI JSON contracts from `task-503` unless a concrete MCP blocker requires a narrowly documented compatibility change.

## Relevant Dependencies

- Declared completed prerequisites:
  - `task-502` - packaged BM25/vector/hybrid search exists in `agentic/src/agentic_kb/search/query.py`
  - `task-503` - packaged local `search`, `entity get`, and `status --json` contracts exist in `agentic/src/agentic_kb/cli.py` and command modules
- Relevant completed foundations already in repo:
  - `task-101` and `task-103` - Compose stack and packaged Python project exist, but `mcp-search` is still placeholder-only
  - `task-205` - status and snapshot-capable package/runtime conventions already exist
  - `task-301`, `task-402`, `task-403`, `task-404`, `task-701` - searchable KB data and current sync workflow already exist
- Downstream boundaries to preserve:
  - `task-802` owns agent setup examples and environment-variable docs
  - `task-803` owns the broader workflow-document update once MCP behavior is real
  - `task-804` owns index/doc pointer cleanup after the workflow is finalized
- Tracker drift to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-801.targetPath` at stale un-packaged path `agentic/src/mcp/search_server.py`; repo reality should stay under the packaged `agentic_kb` module tree, expected at `agentic/src/agentic_kb/mcp/search_server.py`.

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - correct `task-801.targetPath` to the packaged MCP server path and later mark the task completed.
- `docker-compose.agentic.yml` - replace the current placeholder `mcp-search` service with a real build-backed packaged server command and remove any now-unused placeholder-only volume/state.
- `agentic/pyproject.toml` - add the smallest required MCP runtime dependency and any packaged entrypoint wiring if needed.
- `agentic/README.md` - only if a minimal command-surface truth update is required once `mcp-search` stops being placeholder-only; full setup instructions remain `task-802`.
- `agentic/src/agentic_kb/cli.py` - add the `mcp-search` subcommand under the existing packaged `agentic-kb` entrypoint.
- `agentic/src/agentic_kb/mcp/__init__.py` - packaged MCP module namespace.
- `agentic/src/agentic_kb/mcp/search_server.py` - canonical packaged MCP server implementation.
- `agentic/src/agentic_kb/commands/search.py` - only if a small reusable serializer/helper extraction is needed so MCP and CLI share one result contract.
- `agentic/src/agentic_kb/commands/entity.py` - only if a small reusable helper extraction is needed so MCP and CLI share one entity-lookup contract.
- `agentic/src/agentic_kb/commands/status.py` - only if a small reusable serializer/helper extraction is needed so MCP and CLI share one status contract.
- `agentic/tests/test_mcp_search_server.py` - focused tool-surface and contract coverage.
- `agentic/tests/test_mcp_search_server_db.py` - seeded DB-backed verification for real MCP tool behavior.
- `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md` - durable findings about the accepted MCP runtime, tool contracts, and verification constraints.

## Implementation Approach

- **Keep the packaged Python project as the only implementation home**: implement MCP under `agentic/src/agentic_kb/` and do not create a second un-packaged `agentic/src/mcp/` tree.
- **Prefer one small MCP dependency**: if the server would otherwise require hand-rolled MCP protocol code, add exactly one MCP runtime dependency in `agentic/pyproject.toml` and avoid adding a separate web framework or service stack.
- **Primary server transport**: implement the server as a stdio MCP process because that is the integration shape expected by OpenCode and Claude Code.
- **Pinned packaged entrypoint**: use one packaged runtime entrypoint, `agentic-kb mcp-search`, wired through `agentic/src/agentic_kb/cli.py`. Do not add a second console script and do not keep any container-only bootstrap script.
- **Pinned Compose contract**: replace the current `python:3.12-alpine` placeholder `mcp-search` container in `docker-compose.agentic.yml` with a build-backed service from the local `agentic/` package, using the same image family and environment contract as `kb-tools`, and running `agentic-kb mcp-search` as its only long-lived process. Set `stdin_open: true` so the stdio MCP process stays attachable and long-lived in Compose, but do not expose a network port or invent an HTTP wrapper.
- **Compose service role**: the Compose `mcp-search` service is a packaged parity and smoke-verification harness for the exact same stdio server that local MCP clients will launch. It is not a separate user-facing daemon protocol and must not diverge from the packaged local process behavior.
- **Pinned readiness contract**: `mcp-search` health/readiness in Compose must reuse `agentic-kb status --healthcheck` from the same image and environment. That proves DB/Ollama runtime readiness without inventing a second MCP-specific probe protocol. The healthcheck does not need to prove an active client session.
- **Read-only contract**: the MCP layer must only read from the KB. It may reuse `DATABASE_URL`, `OLLAMA_BASE_URL`, and optional `GITHUB_TOKEN` environment parsing already present in the package, but it must not mutate KB rows, repository files, sync state, or GitHub state.
- **`search` tool**: wrap the existing `PostgresSearchStore` and `SearchRequest` contract directly. Preserve the current query modes and registry-backed filters rather than inventing MCP-only search semantics.
- **Domain-specific search tools**: implement `search_docs`, `search_code`, and `search_github` as thin wrappers around the same search store with pre-applied `entity_type` selections that map back to the current registry/entity tables. They should not become separate backends.
- **`get_entity` tool**: reuse the current registry-backed generic entity lookup path so MCP entity reads stay aligned with `agentic-kb entity get` semantics.
- **`kb_status` tool**: reuse the current status collection/serialization boundary so MCP status reflects the same runtime and KB readiness contract as `agentic-kb status --json`, without inventing a second health/status format.
- **Pinned `find_related` behavior**: keep the first version minimal, search-based, and BM25-only. Resolve the seed entity through the existing generic entity lookup helper, derive seed text only from the entity config's declared `text_columns` in order, concatenate non-empty values after whitespace normalization, and cap the derived query text at a small fixed bound suitable for BM25 input. If no usable seed text remains, return a tool error instead of falling back to embeddings or extra lookup logic. Execute one over-fetched BM25 search, filter out the seed entity tuple `(entity_type, id)` in the MCP layer, and return the first remaining results. Do not add graph traversal, new indexes, or a separate similarity pipeline.
- **Pinned embedding dependency behavior**: `search`, `search_docs`, `search_code`, and `search_github` preserve the existing `bm25`, `vector`, and `hybrid` modes. `bm25` mode must work with only ParadeDB reachability. `vector` and `hybrid` modes require the configured Ollama API and embed model to be reachable through the existing `OllamaEmbeddingClient`; if Ollama is unavailable, the model is missing, or embedding generation fails, the MCP call must fail explicitly and must not silently fall back to BM25. `find_related` is intentionally BM25-only and must never trigger live embedding generation.
- **Tool payload stability**: prefer returning the same JSON-safe payload shapes already established for CLI success paths where practical, so MCP and CLI consumers share stable fields and serialization rules.
- **Healthcheck shape**: because stdio MCP does not naturally expose an HTTP health endpoint, keep container healthchecks narrow and reuse existing package/runtime readiness checks where possible instead of inventing a second probe protocol.
- **Task boundary with docs**: limit `task-801` docs changes to the minimum truth-maintenance updates needed if runtime packaging changes make current placeholder wording false. The actual user-facing setup examples and workflow narrative stay with `task-802` and `task-803`.

## Pinned MCP Tool Contracts

- **General success contract**: every MCP tool returns one JSON-safe object and never encodes success as free-form text. The implementation should reuse `agentic/src/agentic_kb/commands/output.py` normalization rules so enums, timestamps, paths, UUID-like ids, mappings, and lists serialize the same way as the CLI JSON commands.
- **General failure contract**: validation failures, not-found conditions, and runtime dependency failures must surface as MCP tool-call errors from the chosen MCP runtime, not as success payloads with `ok: false`. `kb_status` is the one exception: degraded readiness is represented as a successful status payload with `ok: false` because status is observational.
- **`search` request**: input object with required `query_text: string`; optional `mode: "bm25" | "vector" | "hybrid"` default `"hybrid"`; optional `limit: positive integer` default `10`; optional `entity_types: string[]` containing unique values from the current registry (`documents`, `code_chunks`, `github_issues`, `github_issue_comments`, `github_prs`, `github_pr_comments`, `project_items`); optional `filters: object<string,string>` limited to the current registry-backed filter keys.
- **`search` response**: exactly the current search serializer shape from `serialize_search_result_set`, with top-level keys `query_text`, `mode`, `limit`, `filters`, `entity_types`, and `hits`; each hit contains `entity_type`, `source_domain`, `id`, `fields`, `bm25_score`, `vector_distance`, `fused_score`, `bm25_rank`, `vector_rank`, and `hybrid_rank`.
- **`search_docs` request**: same as `search` except `entity_types` is not accepted; `filters` may only contain `doc_kind` and `source_path_prefix`; the handler injects `entity_type=["documents"]` before calling the existing search layer.
- **`search_docs` response**: the same payload shape as `search`, including the injected `filters.entity_type` and `entity_types=["documents"]` in the successful response.
- **`search_code` request**: same as `search` except `entity_types` is not accepted; `filters` may only contain `repo_path_prefix`, `language`, and `symbol_kind`; the handler injects `entity_type=["code_chunks"]`.
- **`search_code` response**: the same payload shape as `search`, including the injected `filters.entity_type` and `entity_types=["code_chunks"]` in the successful response.
- **`search_github` request**: same as `search` but `entity_types`, when provided, is restricted to `github_issues`, `github_issue_comments`, `github_prs`, and `github_pr_comments`; `filters` may only contain `repo`, `state`, and `comment_type`; when `entity_types` is omitted the handler injects all four GitHub entity types and intentionally excludes `project_items` because that table belongs to the `project` source domain.
- **`search_github` narrowing behavior**: `repo` is valid for all four GitHub entity types; `state` narrows the effective search set to issues and PRs because comment tables do not support it; `comment_type` narrows the effective search set to PR comments only. Incompatible caller-supplied `entity_types` plus filters must fail through the existing search validation path rather than silently dropping constraints.
- **`search_github` response**: the same payload shape as `search`, with `entity_types` reflecting the resolved GitHub entity types after any supported filter-driven narrowing.
- **`get_entity` request**: input object with required `entity_type: string` from the current registry and required `id: string`.
- **`get_entity` response**: exactly the current entity serializer shape from `serialize_entity_payload`, with top-level keys `entity_type`, `id`, `table_name`, and `row`.
- **`get_entity` failure semantics**: unsupported `entity_type` is an MCP validation error; a valid-but-missing row is an MCP not-found error; neither condition returns a success payload.
- **`find_related` request**: input object with required `entity_type: string`, required `id: string`, and optional `limit: positive integer` default `5`. No mode override, filter override, or embedding override is accepted in v1.
- **`find_related` search scope**: search across the existing global KB search registry, not just the seed entity type, so related hits can come from docs, code, GitHub, or project rows that match the derived seed text.
- **`find_related` derivation rules**: fetch the seed row through the existing generic entity lookup path; read only the configured `text_columns` for that entity type; join non-empty values in declaration order after whitespace normalization; cap the derived query text at `4000` characters; fail if the resulting text is empty or whitespace-only.
- **`find_related` execution rules**: run one BM25 search with internal over-fetch limit `min(max(limit * 3, 20), 50)`, exclude the exact seed `(entity_type, id)` from the resulting hits, do not perform a second search pass, and return up to the requested `limit` remaining hits.
- **`find_related` response**: JSON object with top-level keys `seed_entity_type`, `seed_id`, `query_text`, `mode` (always `"bm25"`), `limit`, and `hits`, where `hits` uses the same hit-object shape as `search`.
- **`find_related` dependency semantics**: the tool must not call Ollama, must not require stored embeddings on the seed row, and must fail only for the same classes of lookup/validation/database errors already available from the reused helpers.
- **`kb_status` request**: no arguments.
- **`kb_status` response**: exactly the current status serializer shape from `serialize_status_report(collect_status_report(config, healthcheck=False))`, with top-level keys `healthcheck`, `ok`, `config_items`, `environment_items`, `dependency_items`, `database_items`, and `notes`.
- **`kb_status` failure semantics**: ordinary degraded readiness returns a success payload with `ok: false`; only unexpected runtime failures in the MCP wrapper itself should surface as MCP tool errors.

## Acceptance Criteria

- A packaged MCP server module exists under `agentic/src/agentic_kb/mcp/`, with canonical implementation at `agentic/src/agentic_kb/mcp/search_server.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated so `task-801.targetPath` points at the packaged MCP server path instead of stale `agentic/src/mcp/search_server.py`.
- `docker-compose.agentic.yml` no longer runs a placeholder shell loop for `mcp-search`; it builds from the local `agentic/` project, runs `agentic-kb mcp-search` as the only long-lived process, and reuses `agentic-kb status --healthcheck` for readiness instead of inventing a second probe protocol.
- The MCP server exposes only the read-only tool surface planned in the platform doc: `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`.
- The packaged/local MCP process shape and the Compose `mcp-search` service shape are the same runtime contract; Compose remains a parity/smoke harness for the exact packaged stdio server rather than a different daemon implementation.
- `search`, `search_docs`, `search_code`, and `search_github` all reuse the current `task-502` search store and `task-503` output/serialization contracts rather than implementing a second retrieval stack.
- The per-tool MCP request and success-payload contracts match the pinned schemas in this plan closely enough for implementation review, with no extra v1 tools or unplanned optional arguments.
- `get_entity` reuses the current registry-backed generic entity lookup path and stays read-only.
- `kb_status` reuses the current status-report collection/serialization path and stays read-only.
- `find_related` is BM25-only, derives its seed query text only from configured stored text columns, excludes the seed entity from final results, never triggers live embeddings, and does not require new schema objects or indexing pipelines.
- `search`, `search_docs`, `search_code`, and `search_github` preserve current vector/hybrid behavior and fail explicitly when Ollama or the configured embed model is unavailable; they do not silently fall back to BM25.
- The implementation keeps MCP v1 read-only and does not add any GitHub write actions, repo writes, sync commands, or snapshot mutation behavior.
- Focused tests cover actual MCP tool registration/invocation, pinned argument/response contracts, MCP-layer error handling, and at least one DB-backed stdio MCP smoke path against seeded KB data.
- The task stays within its boundary: no setup-doc rollout for OpenCode/Claude Code and no broad workflow rewrite land here beyond any minimal truth-maintenance note strictly required by runtime changes.

## Verification Plan

- Run `python3 -m py_compile` on the new MCP module and touched helpers/tests.
- Add focused MCP-runtime coverage in `agentic/tests/test_mcp_search_server.py` that invokes registered tools through the actual server/runtime layer, not only direct helper calls. Cover at least:
  - tool registration and the exact read-only surface shape
  - generic `search` request validation and success payload passthrough to the existing search-store contract
  - domain-specific tool wrapper filter injection and narrowing behavior for docs, code, and GitHub
  - `get_entity` reuse of the existing registry-backed lookup contract plus MCP not-found behavior
  - `kb_status` reuse of the existing status serialization boundary, including degraded-but-successful `ok: false` payloads
  - `find_related` BM25-only derivation, fixed seed-text-source rules, internal over-fetch plus seed exclusion behavior, and zero-Ollama execution path
  - vector/hybrid failure behavior when the embedding client reports Ollama connection/model errors
  - MCP error behavior where the chosen runtime exposes tool-call failures distinctly from successful JSON payloads
- Add seeded DB-backed coverage in `agentic/tests/test_mcp_search_server_db.py` that proves at least:
  - an actual packaged stdio MCP server process can be started from `agentic-kb mcp-search`
  - `tools/list` exposes only the planned read-only tool set
  - at least one `tools/call` path for `search` returns seeded KB results through the MCP layer
  - one domain-specific tool such as `search_docs` or `search_github` correctly narrows results via existing entity filters through the MCP layer
  - `get_entity` returns a seeded row through the MCP layer
  - `kb_status` returns a real DB-backed status payload
  - `find_related` returns bounded related hits while excluding the seed entity and without requiring Ollama
- Re-run the relevant upstream suites if shared helpers change:
  - `agentic/tests/test_search_command.py`
  - `agentic/tests/test_search_command_db.py`
  - `agentic/tests/test_entity_command.py`
  - `agentic/tests/test_entity_command_db.py`
  - `agentic/tests/test_status_command.py`
  - `agentic/tests/test_status_command_db.py`
- Run `docker compose -f docker-compose.agentic.yml config` after the `mcp-search` service replacement.
- Rebuild the packaged image(s) with `docker compose -f docker-compose.agentic.yml build kb-tools` and, if the Compose file uses a distinct build target/service name, the matching `mcp-search` build path too.
- Run at least one packaged/containerized smoke path that exercises the installed MCP server against seeded KB data by calling the real MCP stdio surface, rather than only calling Python helpers directly.

## Risks / Open Questions

- **MCP runtime package API**: the exact Python MCP dependency still needs to be chosen, but it now has to support the pinned stdio server contract, runtime-exposed tool errors, and testable tool invocation without changing the approved request/response schemas.
- **Container stdio behavior under Compose**: the implementation must ensure the chosen MCP runtime behaves predictably when run as `agentic-kb mcp-search` with `stdin_open: true` and no attached human client, because Compose is now explicitly a parity/smoke harness for that same stdio process.
- **Large seed text rows**: `find_related` now intentionally caps derived seed text to keep BM25 input bounded, but very large document/github rows can still influence result quality; this is an accepted v1 tradeoff versus introducing a second similarity pipeline.
- **Doc-boundary pressure**: replacing the placeholder service may tempt broader README/workflow work, but task ownership for setup and workflow docs remains with `task-802` and `task-803`.

## Required Docs / Tracking / Research Updates

- Update this canonical task plan doc as planning and implementation progress so it remains the single source of truth for the approved plan, build state, verification state, and final outcome.
- Append planning review decisions in `.agent/plans/agentic/task-plans/task-801-plan-review.md` and create the implementation review log separately when implementation begins.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` to correct the stale `task-801.targetPath` and later mark the task completed.
- Add `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md` with durable findings about the accepted MCP dependency/runtime shape, tool contracts, and verification caveats.
- Update `agentic/README.md` and `.agent/workflows/agentic-kb.md` only if minimal shipped-runtime truth maintenance is required; the main setup and workflow documentation work remains `task-802` and `task-803`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-801-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-801-impl-review.md`

## Implementation Notes

- Replaced the placeholder Compose `mcp-search` service with a build-backed packaged parity harness that explicitly runs `agentic-kb mcp-search`, keeps `stdin_open: true`, and reuses `agentic-kb status --healthcheck` for readiness.
- Added the packaged read-only MCP server under `agentic/src/agentic_kb/mcp/search_server.py` plus namespace export in `agentic/src/agentic_kb/mcp/__init__.py` and CLI wiring in `agentic/src/agentic_kb/cli.py`.
- Reused the existing packaged search, entity, and status seams rather than adding a second retrieval layer, including a small shared `get_entity_payload(...)` extraction in `agentic/src/agentic_kb/commands/entity.py` so MCP and CLI share the same entity contract.
- Implemented the approved seven-tool read-only MCP surface: `search`, `search_docs`, `search_code`, `search_github`, `get_entity`, `find_related`, and `kb_status`.
- Kept `find_related` aligned with the approved minimal v1 contract: BM25-only, seed text derived only from configured `text_columns`, one bounded over-fetched search, and explicit seed-result exclusion in the MCP layer.
- Added focused MCP coverage in `agentic/tests/test_mcp_search_server.py` and seeded DB-backed stdio MCP coverage in `agentic/tests/test_mcp_search_server_db.py`.

## Implementation Outcome

- `task-801` is implemented and approved.
- The shipped MCP runtime stays fully read-only and remains under the packaged `agentic_kb` module tree at `agentic/src/agentic_kb/mcp/search_server.py`.
- The Compose `mcp-search` service, packaged CLI entrypoint, tracker path, research note, and minimal truth-maintenance docs now all align with the delivered stdio MCP contract.
- No product-scope deviations from the approved plan were recorded in implementation review.

## Review Outcome

- Planning review outcome: `approved`.
- Implementation review outcome: `approved` after one focused follow-up iteration.
- The only implementation-review corrections were:
  - making the Compose parity harness state the packaged runtime explicitly via `entrypoint: ["agentic-kb"]` plus `command: ["mcp-search"]`
  - correcting the focused `search_github` narrowing test to expect `comment_type` narrowing to `github_pr_comments`

## Final Verification Summary

- `python3 -m py_compile` passed for the new MCP module, CLI wiring, shared entity helper, and new MCP-focused tests.
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_mcp_search_server` passed (`Ran 11 tests`, `OK`).
- `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_search_command agentic.tests.test_entity_command agentic.tests.test_status_command` passed (`Ran 22 tests`, `OK`).
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -p task801-verify -f docker-compose.agentic.yml build kb-tools mcp-search` passed.
- Packaged DB-backed MCP verification passed via `docker compose -p task801-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb -e OLLAMA_BASE_URL=http://127.0.0.1:1 -e OLLAMA_EMBED_MODEL=all-minilm kb-tools -m unittest agentic.tests.test_mcp_search_server_db` (`Ran 4 tests`, `OK`).
- Packaged live stdio smoke verification passed with an in-container `agentic-kb mcp-search` probe covering `initialize`, `tools/list`, and `tools/call` for `kb_status`.
- Verification constraint notes:
  - the host-local DB-backed MCP suite skipped because the host Python environment does not include `psycopg`
  - an existing host Ollama bind on `127.0.0.1:11434` prevented starting the isolated Compose `ollama` service, so packaged smoke coverage intentionally used BM25/status MCP paths with `OLLAMA_BASE_URL=http://127.0.0.1:1`
  - explicit vector/hybrid failure behavior remains covered by focused unit tests

## Final Required Docs / Tracking / Research Updates

- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` so `task-801` points at `agentic/src/agentic_kb/mcp/search_server.py` and is marked `completed`.
- Added durable implementation findings and verification notes in `.agent/plans/agentic/research/task-801-read-only-search-mcp-server.md`.
- Applied the minimal truth-maintenance updates that became necessary once `mcp-search` stopped being placeholder-only:
  - `agentic/README.md`
  - `.agent/workflows/agentic-kb.md`
- No further task-tracking, research, or review-log updates are required for consistency.

## Outcome

- The canonical `task-801` plan now records the final approved plan state, delivered implementation, review outcome, verification notes, and supporting doc/tracker references as the single source of truth.
