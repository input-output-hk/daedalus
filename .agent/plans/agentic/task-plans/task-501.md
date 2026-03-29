# Task Plan: task-501 Implement Ollama embedding client

- Task ID: `task-501`
- Title: `Implement Ollama embedding client`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-501` is the first critical-path task in Phase 5 and is already a direct dependency for `task-301`, `task-401`, `task-403`, and `task-502` in `.agent/plans/agentic/knowledge-base-platform-tasks.json`.
- The stack already has the runtime contract needed for this work: `docker-compose.agentic.yml` provisions `ollama`, `ollama-init`, and `kb-tools`, while `task-103` established `OLLAMA_BASE_URL` and `OLLAMA_EMBED_MODEL` as stable config inputs inside the Python package.
- `task-202` already fixed searchable tables on `VECTOR(384)`, so the embedding client needs to land before ingestion and hybrid search tasks start writing or querying vectors.

## Scope

- Add a reusable Python embedding client inside the `agentic_kb` package for use by later ingestion, search, and MCP tasks.
- Reuse the existing Ollama config contract instead of inventing a parallel embedding configuration surface.
- Normalize Ollama embedding responses into Python vectors that downstream tasks can store in the `VECTOR(384)` schema columns introduced by `task-202`.
- Provide clear failure handling for unreachable Ollama, malformed responses, missing configured model availability, and unexpected vector dimensions.
- Add focused automated coverage for the client behavior and a live smoke-verification path against the Compose-managed Ollama service.

## Non-Goals

- Do not implement document, code, GitHub, or project ingestion; those remain `task-301`, `task-401`, `task-403`, and `task-404`.
- Do not implement BM25, vector, or RRF query logic; that remains `task-502`.
- Do not add new CLI commands, schema changes, search indexes, sync commands, or MCP behavior in this task.
- Do not change the Compose service layout unless implementation discovers a real gap in the already-established Ollama runtime contract.
- Do not add batching heuristics, caching, persistence, or retry policy beyond what is minimally needed for a safe reusable client.

## Relevant Dependencies

- **Blocking implementation prerequisite**: `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently mismatches repo reality for `task-501`. It still points `targetPath` at `agentic/src/embed/client.py` and lists only `task-101` as a dependency, while the actual package layout and this plan rely on `agentic/src/agentic_kb/embed/client.py` plus completed upstream work from `task-103` and `task-202`. This mismatch is blocking for task start and must be corrected as the first implementation step before any embedding-client code changes land.
- Completed upstream tasks:
  - `task-101` - established the Compose stack with `ollama`, `ollama-init`, and `OLLAMA_EMBED_MODEL`
  - `task-103` - established the `agentic_kb` Python package, config loader, and Ollama reachability/model-presence checks in `status`
  - `task-201` - enabled the `vector` extension in ParadeDB
  - `task-202` - created searchable tables with nullable `VECTOR(384)` embedding columns
- Direct downstream tasks unblocked by this work:
  - `task-301` - docs ingestion
  - `task-401` - TypeScript symbol chunking
  - `task-403` - GitHub issues, PRs, and comments ingestion
  - `task-502` - hybrid BM25/vector/RRF search queries
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-201-schema-foundation.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `docker-compose.agentic.yml`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/schema/init.sql`

## Files Expected To Change

- `agentic/src/agentic_kb/embed/__init__.py` - package entry for embedding utilities and the stable public import surface
- `agentic/src/agentic_kb/embed/client.py` - Ollama embedding client implementation
- `agentic/src/agentic_kb/config.py` - only if needed to expose reusable Ollama client settings or shared constants
- `agentic/tests/test_embed_client.py` - focused automated coverage for response parsing and error handling
- `agentic/pyproject.toml` - only if a dependency is truly required; the preferred plan is to keep dependencies unchanged

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` for `task-501` before adding code so its `targetPath` matches `agentic/src/agentic_kb/embed/client.py` and its dependency list reflects the upstream task reality used by this plan. No client code changes should be made until that tracking mismatch is fixed.
- **Package placement**: add a new `agentic/src/agentic_kb/embed/` package so later ingest and search tasks can import a stable client module without routing embedding logic through CLI commands.
- **Public import contract**: expose the supported import surface via `agentic_kb.embed`, with `agentic_kb.embed.client` remaining the concrete implementation module. Do not add a top-level re-export from `agentic_kb.__init__`; downstream code should import from `agentic_kb.embed` or `agentic_kb.embed.client` only.
- **Concrete API contract**: make `agentic_kb.embed.OllamaEmbeddingClient` the public class for this task. It should provide `from_config(config: AgenticConfig) -> OllamaEmbeddingClient`, `embed_text(text: str) -> list[float]`, and `embed_texts(texts: list[str]) -> list[list[float]]`. Verification, later ingestion work, and downstream imports should treat those names as the task-501 contract.
- **Reuse existing config**: use `AgenticConfig.from_env()` and the existing `OLLAMA_BASE_URL` / `OLLAMA_EMBED_MODEL` contract already wired into `kb-tools`. Do not introduce task-specific environment variables unless implementation uncovers a concrete gap that must be documented first.
- **Client surface**: provide a small reusable API that supports both single-text and multi-text embedding requests. The surface should preserve input order, make empty-input behavior explicit, and return plain Python float vectors that later tasks can pass directly into DB writes or search code.
- **Empty-input contract**: define this behavior concretely in the client API and tests:
  - single-text API: reject `""` and whitespace-only input such as `"   "` with a validation error before making any HTTP request
  - batch API: return `[]` immediately for an empty input list without contacting Ollama
  - batch API with any empty or whitespace-only item: raise a validation error that identifies the invalid item index instead of silently dropping, trimming away, or partially embedding the batch
  - non-empty strings should be passed through as provided; validation should check emptiness via `text.strip()` without mutating otherwise valid input content
- **Transport choice**: prefer the Python standard library HTTP stack used elsewhere in `agentic_kb` today rather than adding `requests` or another client dependency. `agentic/pyproject.toml` is currently dependency-free, and `agentic/src/agentic_kb/commands/status.py` already uses `urllib` successfully for Ollama reachability.
- **Ollama API contract**: implement against the embedding endpoint exposed by the pinned Ollama image in `docker-compose.agentic.yml`, and record the exact request/response shape discovered during implementation in the task research note. Current repo evidence confirms `/api/tags` availability and model preloading via `ollama-init`, but the precise embedding endpoint shape still needs implementation-time verification.
- **Dimension guardrail**: enforce a durable expected embedding size of `384` at the client boundary because `task-202` confirmed the schema uses `VECTOR(384)`. If Ollama returns a different length for the configured model, the client should fail fast with a specific error rather than letting later DB writes or search code fail indirectly.
- **Error model**: raise clear, task-local exceptions for connection failures, non-success HTTP responses, malformed JSON, missing embedding payloads, and dimension mismatches. Downstream tasks should be able to surface those errors without reinterpreting raw transport failures.
- **Model-readiness behavior**: rely on the existing Compose contract where `ollama-init` pre-pulls the configured model, but keep a client-side error path for model-not-found responses because `docker compose run` and local operator mistakes can still bypass the happy path.
- **Scope guardrail**: keep the client library independent from DB code and query code. This task should not decide how embeddings are persisted, how search queries are fused, or how ingestion batches are scheduled; it should only provide reliable vector generation.
- **Testing strategy**: add focused automated tests in `agentic/tests/test_embed_client.py` for response normalization, error handling, empty-input behavior, whitespace-only validation, and dimension validation. Use an exact discovery command against `/workspace/agentic/tests` so verification does not depend on optional package-import behavior. Prefer unit-style tests that stub HTTP responses plus one live Compose smoke path to confirm the pinned Ollama image and default model actually produce vectors compatible with the current schema contract.

## Acceptance Criteria

- A reusable Ollama embedding client exists under `agentic/src/agentic_kb/embed/client.py` and is publicly imported via `agentic_kb.embed`, with no new top-level re-export added to `agentic_kb.__init__`.
- The public task-501 API is `agentic_kb.embed.OllamaEmbeddingClient` with methods `from_config`, `embed_text`, and `embed_texts` using the signatures described in this plan.
- The client uses the existing `OLLAMA_BASE_URL` and `OLLAMA_EMBED_MODEL` configuration contract and does not require new environment variables for the base implementation.
- The client supports embedding a single string and multiple strings through a stable Python API intended for later ingestion and search tasks.
- Successful calls return vectors as Python float lists in the same order as the requested inputs.
- The client validates that returned vectors match the schema-backed expected dimension of `384` and raises a clear error when they do not.
- The client fails with clear task-local errors for unreachable Ollama, non-2xx responses, malformed payloads, and missing or invalid embedding data.
- Empty-input handling is explicit and deterministic: the single-text API rejects empty and whitespace-only strings, the batch API returns `[]` for `[]` without making an HTTP call, and the batch API rejects any empty or whitespace-only item with an index-specific validation error.
- Automated tests cover the main success path plus the expected error and validation paths.
- A live smoke verification path against the Compose-managed Ollama service confirms the default configured model can be embedded successfully and produces vectors compatible with `VECTOR(384)`.
- No schema changes, CLI-surface changes, search-query logic, MCP changes, or ingest-specific behavior are introduced by this task.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the existing Compose contract still resolves after any package-level changes.
- Rebuild the tools image before automated and live verification with `docker compose -f docker-compose.agentic.yml build kb-tools` so verification cannot accidentally run against a stale previously installed package layer.
- Start the embedding dependencies with `docker compose -f docker-compose.agentic.yml up -d ollama ollama-init kb-tools` so live verification uses the real Compose-managed model bootstrap path.
- Run the focused automated tests against the workspace code with `docker compose -f docker-compose.agentic.yml run --rm -e PYTHONPATH=/workspace/agentic/src kb-tools python -m unittest discover -s /workspace/agentic/tests -p 'test_embed_client.py'`.
- Run a live smoke command against the workspace code with `docker compose -f docker-compose.agentic.yml run --rm -e PYTHONPATH=/workspace/agentic/src kb-tools python -c "from agentic_kb.config import AgenticConfig; from agentic_kb.embed import OllamaEmbeddingClient; client = OllamaEmbeddingClient.from_config(AgenticConfig.from_env()); vector = client.embed_text('mithril bootstrap'); print(len(vector))"` and confirm it prints `384`.
- Confirm the live smoke path succeeds only after `ollama-init` has completed and the configured model is visible, matching the readiness assumptions established in `task-101` and `task-103`.
- If implementation adds any optional helper in `config.py`, run `docker compose -f docker-compose.agentic.yml run --rm -e PYTHONPATH=/workspace/agentic/src kb-tools agentic-kb status` as a regression check so the task does not break the bootstrap status behavior established by `task-103`.

## Risks / Open Questions

- The current tasks tracker is out of sync with repo reality for this task's target path and dependency chain. That mismatch is a task-start blocker until the tracker is corrected, because leaving it unresolved would make later automation and critique work point at the wrong file and understate the true upstream requirements.
- The current repo proves Ollama reachability through `/api/tags`, but it does not yet capture the exact embedding endpoint path or response shape for the pinned image; implementation should verify and document that contract instead of assuming it.
- `task-202` confirmed `VECTOR(384)` is accepted by ParadeDB, but the default `all-minilm` model still needs live confirmation that it emits 384-length vectors in this stack.
- If Ollama returns different payload shapes for single-input versus batch-input calls, the client should normalize them behind one API and capture that durable behavior in research notes.
- Timeout and retry policy is intentionally minimal in this plan. If implementation discovers the need for new operator-facing knobs, that should be surfaced as a follow-up planning/design update rather than expanded ad hoc.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Correct `.agent/plans/agentic/knowledge-base-platform-tasks.json` as the first implementation step for `task-501` so `task-501` points at `agentic/src/agentic_kb/embed/client.py` and includes the upstream dependency reality captured by this plan before the embedding client code lands.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` again when `task-501` is completed so status and completion metadata stay aligned with the corrected target path and dependencies.
- Add a research note under `.agent/plans/agentic/research/` capturing the exact Ollama embedding endpoint contract, observed response normalization needs, confirmed vector dimension behavior, and any durable gotchas discovered during live verification.
- Only update `.agent/plans/agentic/knowledge-base-platform-prd.md`, `.agent/workflows/agentic-kb.md`, or `agentic/README.md` if implementation reveals a durable platform-contract change rather than a task-local client detail.

## Planning Status Rationale

- This plan is `approved` because the key repo contracts are already established and evidenced locally: Compose provides `ollama` plus model bootstrap, `agentic_kb` already has reusable config loading and Ollama reachability checks, and the schema already fixes the expected vector shape at `384`.
- The remaining uncertainty is the exact Ollama embedding request/response wire shape for the pinned image. That is a real implementation verification item, but it does not block planning because the task scope, boundaries, dependencies, acceptance criteria, tracking-fix requirement, and validation path are already concrete.

## Implementation Notes

- Corrected `.agent/plans/agentic/knowledge-base-platform-tasks.json` first so `task-501` now points at `agentic/src/agentic_kb/embed/client.py` and depends on `task-101`, `task-103`, and `task-202` before any client code landed.
- Added `agentic/src/agentic_kb/embed/` with `agentic_kb.embed.OllamaEmbeddingClient` as the stable public surface and kept `agentic_kb.__init__` unchanged.
- Implemented the client against Ollama `POST /api/embed` using the existing `AgenticConfig` contract and standard-library `urllib`, with task-local exceptions for validation, connectivity, malformed responses, model-not-found conditions, and dimension mismatches.
- Implemented the empty-input contract from this plan exactly: `embed_text` rejects empty and whitespace-only strings, `embed_texts([])` returns `[]` without an HTTP call, and batch validation reports the failing item index.
- Normalized both `embedding` and `embeddings` response payloads into plain Python `list[float]` vectors and enforced a hard `384`-dimension guardrail for schema compatibility.
- Added focused unit coverage in `agentic/tests/test_embed_client.py` for success paths, request shape, validation behavior, connection failures, malformed payloads, HTTP failures, missing-model handling, and dimension mismatches.

## Verification Notes

- `docker compose -f docker-compose.agentic.yml config` succeeded before and after the implementation.
- `docker compose -f docker-compose.agentic.yml build kb-tools` succeeded and rebuilt the tools image with the new package code.
- `docker compose -f docker-compose.agentic.yml up -d ollama ollama-init kb-tools` succeeded; `ollama-init` exited with status `0` and `/api/tags` showed `all-minilm:latest`.
- The exact automated-test command from the plan needed a repo-local adaptation because `kb-tools` has `ENTRYPOINT ["agentic-kb"]`; the equivalent working command was `docker compose -f docker-compose.agentic.yml run --rm --entrypoint python -e PYTHONPATH=/workspace/agentic/src kb-tools -m unittest discover -s /workspace/agentic/tests -p 'test_embed_client.py'`, which passed with `Ran 12 tests ... OK`.
- The live smoke command from the plan needed the same entrypoint adaptation: `docker compose -f docker-compose.agentic.yml run --rm --entrypoint python -e PYTHONPATH=/workspace/agentic/src kb-tools -c "from agentic_kb.config import AgenticConfig; from agentic_kb.embed import OllamaEmbeddingClient; client = OllamaEmbeddingClient.from_config(AgenticConfig.from_env()); vector = client.embed_text('mithril bootstrap'); print(len(vector))"`, which printed `384`.
- Because `config.py` stayed unchanged, the optional regression check still ran and passed as `docker compose -f docker-compose.agentic.yml run --rm -e PYTHONPATH=/workspace/agentic/src kb-tools status`, reporting `agentic-kb status: ok`.
- Review follow-up verification reran the focused unit suite after tightening 404 classification and adding invalid-payload tests; the same adapted unittest command passed with `Ran 16 tests ... OK`.

## Outcome

- `task-501` is complete and now provides the reusable embedding client contract needed by `task-301`, `task-401`, `task-403`, and `task-502`.
- Tracking is aligned with repo reality in `.agent/plans/agentic/knowledge-base-platform-tasks.json`, including the corrected target path, dependency chain, and completion metadata.
- Durable implementation findings were recorded in `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`, including the verified Ollama endpoint contract, legacy payload compatibility note, `384`-dimension confirmation, and the `docker compose run` entrypoint gotcha.
- Review-required fixes landed without widening scope: missing-model classification now requires an explicit model-specific not-found message, and test coverage now includes invalid embedding payload shapes and non-numeric vector content.

## Review Outcome

- Final review result: clean.
- Scribe reconciliation confirmed `.agent/plans/agentic/task-plans/task-501.md`, `.agent/plans/agentic/knowledge-base-platform-tasks.json`, `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`, `agentic/src/agentic_kb/embed/client.py`, `agentic/src/agentic_kb/embed/__init__.py`, and `agentic/tests/test_embed_client.py` all agree on the final task-501 API contract, corrected dependency chain, target path, verification notes, and completion state.
- No broader platform or workflow doc updates were required because task-501 reused the existing Compose and config contract from `task-101` and `task-103` without introducing a new durable operator-facing surface.
