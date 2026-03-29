# Task Plan: task-301 Ingest agent and repo documentation

- Task ID: `task-301`
- Title: `Ingest agent and repo documentation`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-301` is the next unblocked critical-path task after `task-202` and `task-501`, which already established the `agentic.kb_documents` schema contract and a working Ollama embedding client.
- Phase 3 needs a first real repository-ingestion path before later work can add markdown section chunking (`task-302`), structured plan/workflow metadata extraction (`task-303`), and skip-unchanged/idempotent sync behavior (`task-304`).
- The current repo already contains a large body of agent-facing documentation under `.agent/` plus root and subsystem READMEs, so this task can deliver immediate KB value without waiting for code or GitHub ingestion.

## Scope

- Add the first real documentation ingestor inside the existing `agentic_kb` Python package.
- Discover and ingest agent and repo documentation from the current approved source set into `agentic.kb_documents`.
- Store one KB row per source file for this phase, including required schema fields, whole-document content, lightweight path-derived metadata, and embeddings.
- Make repeat runs safe at the database-write level by using deterministic keys and upserts, without claiming the fuller unchanged-content skip behavior reserved for `task-304`.
- Add focused automated coverage plus a live smoke-verification path against the Compose-managed stack.

## Non-Goals

- Do not implement heading-boundary chunking, section hierarchy splitting, or subsection chunk records; that remains `task-302`.
- Do not implement structured workflow / plan / task metadata extraction beyond lightweight path-based classification; that remains `task-303`.
- Do not implement sync-state tracking, skip-unchanged behavior, watermark comparison, or generalized content-hash idempotency logic; that remains `task-304` and `task-405`.
- Do not implement deletion detection, stale-row cleanup, or removal of rows whose source files no longer match the allowlist or no longer exist; that remains future sync/idempotency work beyond `task-301`.
- Do not implement code ingestion, GitHub ingestion, project ingestion, or search-query behavior; those remain `task-401+`, `task-403+`, `task-404+`, and `task-502+`.
- Do not replace the placeholder `agentic-kb sync docs` CLI surface; `task-701` still owns the real sync command orchestration.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-101` - Compose stack and runtime env contract in `docker-compose.agentic.yml`
  - `task-103` - `kb-tools` image, packaged CLI, stable `/workspace` mount, and placeholder sync surface
  - `task-201` - ParadeDB bootstrap and `agentic` schema foundation
  - `task-202` - `agentic.kb_documents` table and required columns/constraints
  - `task-501` - reusable Ollama embedding client at `agentic/src/agentic_kb/embed/client.py`
- Direct downstream tasks unblocked by this work:
  - `task-302` - markdown chunking and heading-boundary splitting
  - `task-303` - structured plan/workflow metadata extraction
  - `task-304` - unchanged-doc skip logic and content-hash driven idempotency behavior
  - `task-502` - docs become available as one indexed entity class for hybrid search
- Current tracking mismatch to correct during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` currently points `task-301.targetPath` at `agentic/src/ingest/docs.py`, but repo reality after `task-103` / `task-501` is the `agentic/src/agentic_kb/` package. The first implementation step should update the tracker to the real package path before code lands.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-201-schema-foundation.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`
  - `.agent/plans/agentic/task-plans/task-201.md`
  - `.agent/plans/agentic/task-plans/task-202.md`
  - `.agent/plans/agentic/task-plans/task-501.md`
  - `AGENTS.md`
  - `CLAUDE.md`
  - `.agent/readme.md`
  - `README.md`
  - `tests/README.md`
  - `installers/README.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/embed/client.py`

## Files Expected To Change

- `agentic/src/agentic_kb/ingest/__init__.py` - package surface for ingestion helpers
- `agentic/src/agentic_kb/ingest/docs.py` - source discovery, file-to-row transformation, embedding, and DB write path for docs
- `agentic/src/agentic_kb/ingest/db.py` - minimal shared DB connection / upsert helper only if needed to keep SQL isolated from the docs ingestor
- `agentic/tests/test_docs_ingest.py` - focused automated coverage for discovery, classification, row shaping, and safe re-run behavior
- `agentic/tests/fixtures/docs/` - representative markdown fixtures only if unit tests need file-backed samples
- `agentic/pyproject.toml` - only if a PostgreSQL client dependency is required for the first DB write path

## Implementation Approach

- **First implementation step**: fix the `task-301` tracker path in `.agent/plans/agentic/knowledge-base-platform-tasks.json` so the task points at `agentic/src/agentic_kb/ingest/docs.py` instead of the pre-package path.
- **Package placement**: add the docs ingestor under `agentic/src/agentic_kb/ingest/` to match the current Python package layout established by `task-103` and already used by `task-501`.
- **Invocation boundary**: implement this task as library functionality, not a new top-level CLI command. Verification can call the library through `python -c` or unit tests. The real operator-facing `sync docs` command remains future work in `task-701`.
- **Initial source set**: ingest markdown documentation only from the currently approved agent/repo doc corpus:
  - root docs: `AGENTS.md`, `CLAUDE.md`, `README.md`
  - agent docs: `.agent/readme.md`, `.agent/workflows/**/*.md`, `.agent/skills/**/*.md`, `.agent/SOPs/**/*.md`, `.agent/plans/**/*.md`
  - selected subsystem READMEs already present in repo reality: `tests/README.md`, `tests/news/README.md`, `installers/README.md`, `installers/icons/README.md`, `source/renderer/app/themes/README.md`
- **Discovery contract**: use explicit allowlisted glob patterns rooted at `/workspace` rather than an open-ended crawl. This keeps the task bounded to "agent and repo documentation" and avoids quietly ingesting unrelated markdown.
- **Path contract**: normalize every discovered file to a repo-relative POSIX `source_path`, for example `AGENTS.md`, `.agent/readme.md`, and `source/renderer/app/themes/README.md`. Do not persist absolute container paths like `/workspace/...`, OS-native separators, or `./` prefixes in `kb_documents.source_path`.
- **File-level row shape for this task**: create exactly one `kb_documents` row per source file with `chunk_index = 0`. Store the whole file body in `content`, keep `section_title` and `subsection_title` null, and keep `heading_path` as `[]`. This intentionally defers heading-aware chunking to `task-302`.
- **Lightweight title extraction**: set `title` from the first markdown H1 when present; otherwise fall back to a basename-derived title. This is allowed because it only fills the required top-level title field and does not introduce section splitting.
- **Minimal metadata contract**: keep `source_domain = 'docs'` for all rows in this task. Set `doc_kind` from path-based classification only, for example `agent_index`, `agent_instruction`, `workflow`, `skill`, `sop`, `plan`, or `readme`. Store additional lightweight facts in `metadata`, such as relative path, file size, top-level source group, and whether the title came from H1 fallback logic.
- **Structured metadata boundary**: do not extract workflow command triggers, plan task ids, research-note semantics, status badges, or field-level plan metadata in this task. `task-303` should extend the row metadata later without redesigning the base ingestion flow.
- **Preview text**: derive `preview_text` from the normalized leading non-empty content, trimmed to a stable short preview length. Keep this simple and deterministic so later search/UI tasks can display useful snippets even before section chunking lands.
- **Commit and timestamp fields**: capture the repo commit hash once per ingestion run from local `HEAD` and populate `repo_commit_hash` on all inserted rows. Populate `source_updated_at` from the file's filesystem mtime in UTC. This gives later freshness tasks a real watermark without requiring sync-state tables yet.
- **Embedding usage**: use `agentic_kb.embed.OllamaEmbeddingClient` from `task-501` to embed the whole-document content for each row. Batched `embed_texts` calls are preferred where safe, but if the pinned model rejects a whole file as one embedding input, a minimal embedding-only segmentation step is allowed as long as the stored KB row remains one row per file with the original whole-file `content` unchanged.
- **Embedding-only segmentation boundary**: if a whole file exceeds the model context length on the pinned stack, split only the embedding input into deterministic text segments, embed those segments, and aggregate them back into one document embedding for the single stored row. Do not store multiple chunks, do not change `chunk_index`, do not add heading-boundary storage records, and do not truncate persisted content.
- **Database write contract**: write into `agentic.kb_documents` with deterministic row ids and `INSERT ... ON CONFLICT (source_path, chunk_index) DO UPDATE` behavior. This makes reruns safe against the existing uniqueness constraint without claiming the fuller skip-unchanged semantics reserved for `task-304`.
- **Stable id contract**: derive `id` deterministically from the normalized repo-relative POSIX `source_path` plus file-level `chunk_index = 0` so `task-302` can later replace file-level records with section-level records under the same source path model without introducing opaque ids.
- **Content hash boundary**: populate the schema-required `content_hash` field deterministically from the normalized repo-relative POSIX `source_path` plus current normalized file content, but do not use stored hashes to skip work, compare prior runs, or drive sync-state behavior yet. `task-304` owns turning hashes into formal idempotency behavior.
- **Allowlist completeness contract**: centralize the approved allowlist in code so the same definition drives discovery, tests, and live verification. The ingestor should be able to surface the resolved matched path set for deterministic assertions that the approved set is complete and exclusive.
- **Upsert update contract**: on conflict, update the full mutable document payload for the existing row at `(source_path, chunk_index)`, including `content`, `preview_text`, `content_hash`, `source_updated_at`, `repo_commit_hash`, `embedding`, `metadata`, and local `updated_at`. This verifies safe reruns and changed-content refresh without claiming skip-unchanged optimization.
- **Write strategy**: prefer a single ingestion transaction for a run so a failed embed or DB write does not leave a partially updated document set. If a new PostgreSQL client is required, add the smallest practical dependency and keep the DB seam narrow and reusable.
- **Testing approach**: cover path discovery, repo-relative path normalization, doc-kind classification, title/preview derivation, deterministic id/content-hash generation, allowlist completeness/exclusivity, embedding-call ordering, and `ON CONFLICT` safe reruns including changed-content updates. Use stubbed embedding client responses in unit tests and reserve real Ollama + ParadeDB interaction for a smoke test.

## Acceptance Criteria

- A docs ingestor exists under `agentic/src/agentic_kb/ingest/docs.py` in the packaged `agentic_kb` module layout.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated so `task-301.targetPath` points at `agentic/src/agentic_kb/ingest/docs.py`; this tracker fix is treated as a required task artifact, not an optional follow-up note.
- The ingestor discovers exactly and only the approved documentation source set from this plan, using repo-relative normalized POSIX paths and excluding non-allowlisted markdown plus non-markdown trackers such as `*.json`.
- `agentic.kb_documents` receives one row per source file for this phase with `chunk_index = 0` and whole-document `content`.
- Inserted rows populate the schema-required fields at minimum: `id`, `source_domain`, `doc_kind`, `source_path`, `title`, `chunk_index`, `content`, `preview_text`, `content_hash`, and the embedding field.
- `source_path` values are stored only as normalized repo-relative POSIX paths like `AGENTS.md` and `.agent/readme.md`, never as `/workspace/...` absolute paths or platform-native separator variants.
- Deterministic `id` and `content_hash` generation are both anchored to that normalized `source_path` contract.
- `section_title`, `subsection_title`, and heading-derived chunk hierarchy remain intentionally unpopulated or empty in this task, preserving the boundary with `task-302`.
- `source_domain` is consistently `docs`, while `doc_kind` is assigned through lightweight path-based classification only.
- Each ingested row stores `repo_commit_hash` from the current repo state and `source_updated_at` from the file timestamp.
- Embeddings are generated through `OllamaEmbeddingClient` and stored on ingested docs with the existing `VECTOR(384)` schema contract.
- Re-running the ingestor against the same source set succeeds without unique-constraint failures by updating existing `(source_path, chunk_index)` rows rather than duplicating them.
- Re-running after a source document changes updates the existing row in place via upsert, including `content`, `preview_text`, `content_hash`, `source_updated_at`, `repo_commit_hash`, `embedding`, and local `updated_at`.
- Deletion detection and stale-row cleanup are explicitly not implemented by this task.
- The implementation does not add heading-boundary chunking, structured workflow/plan metadata extraction, sync-state writes, or unchanged-doc skip logic.

## Verification Plan

- Run `docker compose -f docker-compose.agentic.yml config` to confirm the Compose contract still resolves after any package or dependency changes.
- Rebuild the tools image with `docker compose -f docker-compose.agentic.yml build kb-tools` so verification uses the current installed package.
- Start a clean isolated verification stack with a unique project name and fresh volumes, for example `AGENTIC_DB_PORT=5751 docker compose -p agentic-task-301 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools`, then wait for the known ParadeDB first-boot restart handoff to settle before DB checks.
- Run focused automated tests with the current package path, for example `docker compose -p agentic-task-301 -f docker-compose.agentic.yml run --rm --entrypoint python -e PYTHONPATH=/workspace/agentic/src kb-tools -m unittest discover -s /workspace/agentic/tests -p 'test_docs_ingest.py'`.
- Verify the tracker artifact directly by reading `.agent/plans/agentic/knowledge-base-platform-tasks.json` and confirming `task-301.targetPath` is `agentic/src/agentic_kb/ingest/docs.py` before final task signoff.
- Run a live library smoke command through `--entrypoint python` that invokes the docs ingestor against `/workspace` and writes into the live verification DB. Keep this as a direct module invocation rather than a new CLI surface.
- Verify discovery completeness and exclusivity deterministically by comparing the ingestor's resolved matched path set against the expected allowlist expansion for the current repo state. At minimum assert the exact singleton roots (`AGENTS.md`, `CLAUDE.md`, `README.md`, `.agent/readme.md`, `tests/README.md`, `tests/news/README.md`, `installers/README.md`, `installers/icons/README.md`, `source/renderer/app/themes/README.md`) plus full per-glob expansions for `.agent/workflows/**/*.md`, `.agent/skills/**/*.md`, `.agent/SOPs/**/*.md`, and `.agent/plans/**/*.md`.
- Verify negative discovery assertions by confirming the resolved matched path set excludes at minimum `.agent/plans/agentic/knowledge-base-platform-tasks.json`, `agentic/README.md`, `docker-compose.agentic.yml`, and any absolute `/workspace/...` path forms.
- Verify file-level shape with SQL by confirming representative rows use `chunk_index = 0`, `source_domain = 'docs'`, normalized repo-relative POSIX `source_path`, non-empty `content`, non-empty `preview_text`, non-null `content_hash`, and null or empty section-hierarchy fields.
- Verify embeddings are populated, for example by confirming `embedding IS NOT NULL` on representative rows.
- Verify the stored `source_path` contract with SQL by confirming representative rows equal values like `AGENTS.md` and `.agent/workflows/agentic-kb.md` and that no row has `source_path LIKE '/workspace/%'`.
- Re-run the live ingestion command once more against the same verification DB and confirm row counts for the ingested source paths do not increase, proving safe upsert behavior without asserting unchanged-content skip optimization.
- Modify a temporary verification copy of one allowlisted source file, re-run ingestion, and confirm the existing row for that same normalized `source_path` updates in place: `content`, `preview_text`, `content_hash`, `source_updated_at`, `repo_commit_hash`, `embedding`, and local `updated_at` all change as expected while row count remains constant.
- Verify the non-goal deletion boundary by noting that removing or excluding a previously ingested source file is not part of this task's verification; no stale-row cleanup assertion should be added for `task-301`.
- Tear down the isolated verification stack with `docker compose -p agentic-task-301 -f docker-compose.agentic.yml down -v` after validation.

## Risks / Open Questions

- **Tracker mismatch**: `task-301.targetPath` in `.agent/plans/agentic/knowledge-base-platform-tasks.json` still reflects a pre-`agentic_kb` package path and is now a required task output, not a best-effort cleanup.
- **DB client gap**: the current Python package has no PostgreSQL client dependency yet. Implementation may need to introduce a minimal dependency or a very narrow DB access seam; that is acceptable if kept tightly scoped to document writes.
- **Whole-file embeddings before chunking**: some longer markdown files may stress model input limits or reduce embedding quality. This plan chooses loud failure over silent truncation so `task-302` can solve the real chunking problem explicitly.
- **Doc-kind vocabulary**: this plan intentionally keeps `doc_kind` coarse and path-derived. If implementation uncovers a better minimal taxonomy that still avoids structured extraction, record it in research notes and keep it stable.
- **Source-set growth**: selected subsystem READMEs are concrete today, but more may become worth ingesting later. The implementation should centralize the allowlist so follow-up expansion does not require redesign.

## Required Docs / Tracking / Research Updates

- Update this task plan doc during implementation with final build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` before implementation to fix the real `task-301` target path, verify that correction explicitly during task validation, and update it again when the task is completed.
- Add a research note under `.agent/plans/agentic/research/` capturing durable findings such as the final doc allowlist, chosen `doc_kind` mapping, any DB-client decision, and any embedding-size / oversized-document constraints discovered in live verification.
- Only update `.agent/plans/agentic/knowledge-base-platform-prd.md`, `.agent/workflows/agentic-kb.md`, or `agentic/README.md` if implementation reveals a durable operator-facing contract change beyond this task's internal library behavior.

## Planning Status Rationale

- This plan is `approved` because the upstream runtime, schema, and embedding prerequisites already exist and the remaining work is now mostly about converting a bounded markdown source set into valid `kb_documents` rows.
- The boundary with later tasks is explicit: this task lands file-level ingestion only, while `task-302` owns markdown chunking, `task-303` owns structured metadata extraction, and `task-304` owns unchanged-content idempotency behavior.
- The main uncertainties are implementation details around the first DB write seam and any whole-file embedding limits, which are real execution risks but not planning blockers.

## Implementation Notes

- Fixed `.agent/plans/agentic/knowledge-base-platform-tasks.json` first so `task-301.targetPath` now points at `agentic/src/agentic_kb/ingest/docs.py`.
- Added `agentic/src/agentic_kb/ingest/` with a file-level docs ingestor that centralizes the approved allowlist, normalizes repo-relative POSIX `source_path` values, derives lightweight `doc_kind` and metadata from paths, extracts top-level titles, computes deterministic ids and content hashes, and keeps `section_title`, `subsection_title`, and `heading_path` empty for this phase.
- Added a narrow `PostgresDocsStore` write seam using `psycopg[binary]` and `INSERT ... ON CONFLICT (source_path, chunk_index) DO UPDATE` so reruns refresh the existing row in place.
- Kept whole-file storage intact with exactly one row per source file and `chunk_index = 0`; no heading chunking, metadata extraction expansion, deletion cleanup, or sync orchestration was added.
- Added focused unit coverage in `agentic/tests/test_docs_ingest.py` for allowlist discovery, normalized paths, classification, title/preview derivation, deterministic ids and hashes, embedding-only segmentation and aggregation, rerun upserts, and repo-commit fallback when the `git` binary is unavailable inside `kb-tools`.
- Minimal implementation-time deviation: when the pinned Ollama stack rejects a whole file as one embedding input, the ingestor now splits only the embedding input into deterministic text segments, embeds them, and aggregates them back into one document embedding while still storing one unsplit KB row per file.

## Verification Notes

- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_docs_ingest.py'` passed locally with `Ran 11 tests ... OK`.
- `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_embed_client.py'` passed locally with `Ran 16 tests ... OK`.
- `docker compose -f docker-compose.agentic.yml config` succeeded.
- `docker compose -f docker-compose.agentic.yml build kb-tools` succeeded after adding the PostgreSQL client dependency.
- `AGENTIC_DB_PORT=5751 OLLAMA_PORT=11444 docker compose -p agentic-task-301 -f docker-compose.agentic.yml up -d paradedb ollama ollama-init kb-tools` succeeded after adapting the Ollama host port because `11434` was already allocated locally.
- `docker compose -p agentic-task-301 -f docker-compose.agentic.yml exec -T paradedb psql -U agentic -d agentic_kb -c "SELECT 1;"` succeeded after the known ParadeDB first-boot handoff.
- `docker compose -p agentic-task-301 -f docker-compose.agentic.yml exec -T kb-tools env PYTHONPATH=/workspace/agentic/src python -m unittest discover -s /workspace/agentic/tests -p 'test_docs_ingest.py'` passed with `Ran 11 tests ... OK`.
- `docker compose -p agentic-task-301 -f docker-compose.agentic.yml exec -T kb-tools python -m unittest discover -s /workspace/agentic/tests -p 'test_embed_client.py'` passed with `Ran 16 tests ... OK`.
- Discovery verification inside `kb-tools` confirmed the resolved allowlisted path set includes the required singleton docs, expands the approved globs, excludes `.agent/plans/agentic/knowledge-base-platform-tasks.json`, `agentic/README.md`, and `docker-compose.agentic.yml`, and stores no `/workspace/...` path forms.
- Live DB smoke verification succeeded for a representative subset by ingesting `AGENTS.md`, `.agent/workflows/agentic-kb.md`, and `installers/README.md`, then re-ingesting after a temporary content change. SQL checks confirmed normalized `source_path`, `chunk_index = 0`, `source_domain = 'docs'`, null section fields, empty `heading_path`, populated embeddings, stable row counts, and in-place updates to `content`, `content_hash`, `repo_commit_hash`, and `updated_at`.
- Full-corpus live ingestion against the approved allowlist now succeeds on the pinned stack. Live verification ingested 67 allowlisted docs end-to-end, confirmed all rows use `chunk_index = 0` and normalized repo-relative `source_path` values, confirmed embeddings are populated for all ingested rows, confirmed rerunning ingestion keeps row counts stable, and confirmed re-ingesting a modified temporary verification copy updates the existing row in place.

## Outcome

- `task-301` landed the approved file-level docs ingestor, tracker fix, deterministic row shaping, DB upsert path, and focused automated coverage in the current `agentic_kb` package layout.
- The implementation now ingests the full approved allowlist end-to-end on the pinned stack by using embedding-only segmentation and aggregation for oversized files while preserving the one-row-per-file storage contract.
- The final task state is signoff-ready: task tracking is aligned, research notes match the implementation, and live full-corpus verification now passes.

## Review Outcome

- Review-required fixes were addressed: unrelated tracker edits outside `task-301` were reverted, task tracking is now consistent across the task plan, tasks JSON, and research note, and full approved-corpus live ingestion now passes on the supported pinned stack.
