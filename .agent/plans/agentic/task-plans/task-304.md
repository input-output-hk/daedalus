# Task Plan: task-304 Add idempotency and content hashing

- Task ID: `task-304`
- Title: `Add idempotency and content hashing`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-304` is the remaining high-priority Phase 3 docs-ingestion gap after `task-301` and `task-302`: the repo already stores deterministic per-chunk `content_hash` values in `agentic.kb_documents`, but explicit `sync docs`, library ingest, and incremental docs sync still re-read, re-embed, and rewrite unchanged docs on every run.
- Current repo reality now supports a narrower, well-grounded version of the task than the stale tracker path suggests: docs ingestion lives in `agentic/src/agentic_kb/ingest/docs.py`, and repo-level docs sync baselines live in `agentic/src/agentic_kb/commands/sync.py` plus `agentic/src/agentic_kb/sync/state.py`.
- This task is worth planning now because the docs path already has the stable inputs needed for cheap unchanged-doc detection: deterministic chunk ids, deterministic row-scoped content hashes, normalized repo-relative `source_path`, and one atomic replacement seam for changed paths.

## Scope

- Add unchanged-doc skip behavior for the allowlisted docs corpus handled by `agentic/src/agentic_kb/ingest/docs.py`.
- Reuse the existing deterministic per-chunk `content_hash` contract from task-302 as the idempotency key for docs rows.
- Make docs ingest and docs sync avoid embedding and rewriting source paths whose chunk set is unchanged.
- Preserve the current atomic replacement behavior for changed docs and deleted allowlisted docs.
- Update docs sync-state metadata only if a small addition is needed to make the docs skip behavior observable or safely reusable in current repo design.

## Non-Goals

- Do not create or revive the stale tracker target `agentic/src/ingest/base.py`; current repo reality keeps this task in the packaged docs-ingestion and sync modules.
- Do not broaden into code ingestion idempotency, GitHub/project incremental replay changes, or a generalized cross-source hashing framework.
- Do not redesign `agentic.kb_sync_state` into per-document or per-chunk watermark storage unless the current design proves insufficient for docs-only skip behavior.
- Do not change the docs allowlist, markdown chunking rules, embedding model contract, or operator-facing `sync changed` baseline rules beyond what unchanged-doc skipping directly requires.
- Do not treat repo-level `repo_commit_hash` alone as sufficient unchanged proof; local docs skip logic should remain content-based because one repo commit can contain both changed and unchanged allowlisted docs.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-301` - allowlisted docs ingestion in `agentic/src/agentic_kb/ingest/docs.py`
  - `task-302` - markdown chunking, deterministic per-chunk ids/hashes, and atomic replacement for docs rows
  - `task-405` - repo-scoped docs sync state in `agentic/src/agentic_kb/sync/state.py`
  - `task-701` - explicit `sync docs` and incremental `_sync_docs_changed(...)` command paths in `agentic/src/agentic_kb/commands/sync.py`
- Current downstream boundary to preserve:
  - `task-303` - structured doc metadata extraction remains separate
  - later freshness tasks should be able to rely on docs sync-state, but they are not part of this task
- Reviewed references:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-302-markdown-chunking.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/sync/state.py`
  - `agentic/tests/test_docs_ingest.py`
  - `agentic/tests/test_sync_command.py`
  - `agentic/tests/test_sync_command_db.py`
  - `agentic/schema/init.sql`

## Current Repo State To Reconcile

- The task tracker still points `task-304.targetPath` at stale path `agentic/src/ingest/base.py`, but the shipped docs ingestor lives in `agentic/src/agentic_kb/ingest/docs.py`.
- Task-302 already shipped the core hashing primitive this task should build on: docs rows are chunked and use deterministic ids `docs:{source_path}#{chunk_index}` plus deterministic row-scoped `content_hash = sha256(source_path + NUL + chunk_index + NUL + chunk_content)`.
- `agentic.kb_documents` already persists `content_hash` and is unique on `(source_path, chunk_index)`, so this task does not need a schema change just to compare unchanged chunk rows.
- Current docs write paths are intentionally safe for changed files because they use `replace_documents_for_paths(...)`, but they still prepare embeddings for every discovered/changed doc before deciding whether any row actually changed.
- Current docs sync-state is intentionally repo-scoped and minimal: docs rows in `agentic.kb_sync_state` store `repo_commit_hash`, attempt/success timestamps, bounded errors, and summary metadata, but they do not track per-file hashes.
- `sync docs` currently discovers the full allowlist, prunes stale allowlisted paths, prepares all discovered documents, replaces rows for all discovered plus stale paths, and records docs sync state. `_sync_docs_changed(...)` already narrows to git-derived changed/deleted paths, so task-304 should improve unchanged reruns within that current design rather than widen baseline semantics.

## Files Expected To Change

- `agentic/src/agentic_kb/ingest/docs.py` - add minimal docs-row comparison helpers and unchanged-doc skip behavior around the existing preparation/replacement path
- `agentic/src/agentic_kb/commands/sync.py` - reuse the docs skip path for explicit `sync docs` and incremental `_sync_docs_changed(...)`, keeping deleted-path handling explicit
- `agentic/tests/test_docs_ingest.py` - focused coverage for unchanged-doc detection, hash comparison, and skip behavior without regressing atomic replacement for changed docs
- `agentic/tests/test_sync_command.py` - explicit and changed docs sync coverage for unchanged reruns and sync metadata/reporting shape
- `agentic/tests/test_sync_command_db.py` - DB-backed verification of unchanged-doc skip behavior if the current environment-backed suite remains the repo’s chosen place for real DB sync checks
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update `task-304.targetPath` to the real packaged path when implementation lands
- `.agent/plans/agentic/research/` - add a durable task-304 research note after implementation
- `.agent/workflows/agentic-kb.md` - update only if the shipped docs sync wording needs a narrow note about unchanged-doc skipping or result semantics

## Implementation Approach

- Keep task-304 implementation on the current packaged docs path and command path:
  - docs ingest logic in `agentic/src/agentic_kb/ingest/docs.py`
  - docs sync orchestration in `agentic/src/agentic_kb/commands/sync.py`
  - docs sync-state usage in `agentic/src/agentic_kb/sync/state.py` only for repo-scoped summary reporting, not per-doc hash storage
- Build on the existing task-302 hashing contract instead of inventing a second checksum scheme. The durable unchanged check must compare the current per-path prepared chunk set to the stored rows for that same `source_path`, using deterministic row identity and `content_hash` as the equality signal.
- Make pre-embedding unchanged detection mandatory. The implementation must introduce a two-phase docs path, or an equivalent helper, that computes chunk payloads and deterministic hashes before any embedding call so unchanged docs are filtered out before embeddings are requested.
- Keep the change minimal by adding a docs-store read seam rather than a schema redesign. Minimal acceptable shape:
  - list existing rows for one or more `source_path` values with enough fields to compare the current stored chunk set to the newly prepared chunk set, or
  - expose an equivalent helper that returns existing per-path chunk identity/hash state from `agentic.kb_documents`
- Required execution sequence:
  1. discover candidate docs for the run
  2. load current stored chunk identity/hash state for those candidate `source_path` values
  3. prepare chunk content plus deterministic chunk metadata and `content_hash` values without embeddings
  4. partition candidate paths into `updated_paths` and `skipped_paths` by exact chunk-set equality
  5. request embeddings only for `updated_paths`
  6. atomically replace rows only for `updated_paths`
  7. separately delete stale allowlisted paths that are no longer discovered or were deleted in the incremental delta
- Keep comparison path-focused, not repo-global. `sync docs` should still scan the allowlisted docs set and delete stale allowlisted paths, but unchanged files within that set should not trigger new embeddings or writes.
- Preserve current task-302 chunk semantics exactly: if chunk boundaries or content change, the file is treated as changed and all rows for that source path are replaced via the existing atomic replacement seam.
- Define persisted row metadata for skipped unchanged docs as content-versioned, not sync-attempt-versioned:
  - if a path is classified as unchanged and skipped, its persisted `repo_commit_hash` and `source_updated_at` values remain untouched in `agentic.kb_documents`
  - repo-scoped docs sync state may advance to the newer sync commit independently
  - downstream contract: row metadata reflects the last content-changing write for that document path, while `kb_sync_state` reflects the last repo-scoped docs sync attempt/success
- Preserve current sync-state shape unless one small summary addition is clearly useful. Do not store a second source of truth for per-file hashes in `kb_sync_state`.
- Keep deleted-path handling unchanged in principle:
  - explicit `sync docs` still removes allowlisted rows whose source paths are no longer discovered
  - incremental `_sync_docs_changed(...)` still deletes removed paths from the git delta
  - deletion remains independent from unchanged-doc skip logic
- Lock down operator-visible result contracts so candidate discovery is not conflated with work performed:
  - `updated_paths` means paths that were embedded and rewritten
  - `skipped_paths` means candidate paths whose prepared chunk set matched stored rows exactly and therefore had no embedding call and no row rewrite
  - `deleted_paths` means paths removed from storage
  - any existing candidate/discovered/changed path list must not be used as the reported updated set unless it exactly matches rewritten paths

## Acceptance Criteria

- The canonical implementation plan and tracker path for `task-304` are reconciled to repo reality: no new stale-path-only module is created, and the plan targets `agentic/src/agentic_kb/ingest/docs.py` plus docs sync code in `agentic/src/agentic_kb/commands/sync.py`.
- Re-running docs ingest against unchanged allowlisted docs must skip those paths before any embedding call, and it must not rewrite `kb_documents` rows for those unchanged `source_path` values.
- The unchanged decision is grounded in the existing deterministic per-chunk docs hash contract, not only repo commit baselines.
- If any chunk count, chunk order, or chunk content changes for a `source_path`, that path is treated as changed and is re-embedded and atomically replaced.
- Deleted allowlisted docs are still removed correctly on explicit docs sync and incremental docs-changed sync.
- The implementation does not require a schema migration unless a concrete blocker is discovered during implementation.
- For skipped unchanged docs, persisted row `repo_commit_hash` and `source_updated_at` remain unchanged by design, while repo-scoped docs sync state may advance independently.
- Explicit `sync docs` and incremental docs sync report unchanged paths separately from updated paths, so unchanged candidates are not misreported as updated work.
- Docs sync-state remains repo-scoped and minimal unless a small summary metadata addition is explicitly justified by implementation.
- The task does not broaden into code/GitHub/project idempotency or a generalized multi-source sync-state redesign.

## Verification Plan

- Extend `agentic/tests/test_docs_ingest.py` to cover:
  - deterministic equality checks between newly prepared chunk sets and stored docs rows
  - the pre-embed comparison path, proving unchanged reruns skip embedding calls for identical docs content
  - unchanged reruns skipping writes for identical docs content
  - changed reruns still replacing rows when chunk content or count changes
  - deletion behavior remaining separate from unchanged skip behavior
  - preservation of current atomic replacement guarantees for changed docs
  - skipped unchanged docs leaving row `repo_commit_hash` and `source_updated_at` untouched
- Extend `agentic/tests/test_sync_command.py` to cover:
  - explicit `sync docs` on an unchanged corpus does not rewrite unchanged paths and reports them in `skipped_paths`, not `updated_paths`
  - incremental `_sync_docs_changed(...)` skips unchanged changed-path candidates when stored hashes still match current content, with no embedding calls for skipped paths
  - stale/deleted paths are still deleted
  - reported counts and path lists distinguish candidate paths from actually updated paths for both explicit and incremental docs sync
  - docs sync-state metadata remains correct if a narrow summary field is added
- Extend `agentic/tests/test_sync_command_db.py` if needed to verify the real DB-backed docs path can compare against stored rows and skip unchanged rewrites without breaking delete/replace semantics, including no row rewrite and unchanged row metadata for skipped paths.
- Run the focused docs ingest and sync unit suites locally:
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command`
- Run the DB-backed sync suite when `AGENTIC_TEST_DATABASE_URL` is available:
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db`
- If implementation adds a new store read helper against ParadeDB, verify unchanged-doc reruns against a real DB so the optimization is proven at the actual SQL seam rather than only in-memory.

## Risks / Open Questions

- **Stored-row comparison shape**: current docs store protocol exposes path listing and replace/delete writes, but not per-path row reads. A minimal read helper is likely justified, but the implementation should keep it docs-specific and small.
- **Result compatibility**: current command callers may already rely on existing summary fields. Implementation should keep backward-compatible summaries only if they do not blur `updated_paths` versus `skipped_paths`; otherwise the plan favors adding explicit fields over preserving ambiguous names.
- **Docs sync-state necessity**: current repo design should not require any docs watermark/hash addition in `kb_sync_state` because `kb_documents` already stores the durable row hashes and row metadata is now explicitly content-versioned.

## Required Docs / Tracking / Research Updates

- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` during implementation so `task-304.targetPath` matches current repo reality instead of stale `agentic/src/ingest/base.py`.
- Add a durable research note under `.agent/plans/agentic/research/` capturing the final unchanged-doc comparison contract, whether embeddings are skipped before preparation completes, and any docs sync-state decision made during implementation.
- Update this canonical task plan with final implementation notes, verification notes, and outcome after implementation.
- Update `.agent/workflows/agentic-kb.md` only if task-304 changes the operator-visible docs sync contract enough that the current workflow wording becomes stale.
- Keep planning review history in `.agent/plans/agentic/task-plans/task-304-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-304-impl-review.md` during this planning pass.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-304-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-304-impl-review.md`

## Planning Status Rationale

- Planning status is `approved` because `.agent/plans/agentic/task-plans/task-304-plan-review.md` ends with `Decision: approved` after the revised canonical plan addressed the critique items around pre-embed skip detection, content-versioned skipped-row metadata, and explicit docs sync result semantics.
- Build status is `completed` because `.agent/plans/agentic/task-plans/task-304-impl-review.md` ends with `Decision: approved` after the shipped implementation, tracker update, workflow/research updates, and focused verification were re-reviewed and accepted.
- This canonical plan is the single source of truth for the final approved plan, current build state, and final task outcome; the review logs remain preserved audit history and are not rewritten.

## Implementation Notes

- Implemented the task on the current packaged docs ingestion path in `agentic/src/agentic_kb/ingest/docs.py` and docs sync orchestration path in `agentic/src/agentic_kb/commands/sync.py`; no stale-path module was introduced.
- Added a two-phase docs flow that prepares deterministic chunk drafts and hashes before embedding, compares them against stored `(source_path, chunk_index, content_hash)` rows, and embeds only `updated_paths`.
- Preserved content-versioned docs row metadata for skipped unchanged docs by leaving stored `repo_commit_hash` and `source_updated_at` untouched while still advancing repo-scoped docs sync state.
- Tightened docs sync result semantics so explicit and incremental docs sync now report `candidate_paths`, `updated_paths`, `skipped_paths`, and `deleted_paths` separately from chunk-row `processed_count`.
- Extended repo-scoped docs sync state reporting in `agentic/src/agentic_kb/sync/state.py` with the explicit docs result fields above while keeping `kb_sync_state` minimal and repo-scoped.
- Kept deletions independent from unchanged-doc skipping and limited all changes to docs ingestion/sync plus narrow tracking/workflow/research updates.
- Synchronized durable task artifacts alongside the code changes: `.agent/plans/agentic/knowledge-base-platform-tasks.json` now records `task-304` as completed against `agentic/src/agentic_kb/ingest/docs.py`, `.agent/plans/agentic/research/task-304-idempotent-docs-sync.md` captures the shipped contract, and `.agent/workflows/agentic-kb.md` reflects the operator-visible sync behavior.
- The implementation history in `.agent/plans/agentic/task-plans/task-304-impl-review.md` intentionally preserves sequencing anomalies from the subagent loop, including implementation entries appearing before the first code-review entry; that transcript shape was not rewritten so future readers can audit the original review flow directly.

## Verification Notes

- Focused unit coverage was extended in `agentic/tests/test_docs_ingest.py` and `agentic/tests/test_sync_command.py` for no-embed/no-rewrite unchanged reruns and explicit/incremental docs sync result semantics.
- DB-backed coverage in `agentic/tests/test_sync_command_db.py` was extended for unchanged incremental docs candidates when `AGENTIC_TEST_DATABASE_URL` is available, including the SQL-backed `list_document_versions(...)` comparison seam and preservation of skipped-row metadata while repo-scoped sync state advances.
- Recorded verification from `.agent/plans/agentic/task-plans/task-304-impl-review.md`:
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest` passed (`Ran 17 tests`, `OK`).
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command` passed locally (`Ran 17 tests`, `OK`, `1 skipped` for optional tree-sitter coverage).
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db` remained environment-gated in this host session and skipped cleanly (`Ran 4 tests`, `OK`, `skipped=4`).
- Final review accepted the current verification state as sufficient for this environment because no task-304-specific blocking gaps remained after the DB-backed candidate-path test was corrected.

## Outcome

- Status: completed
- Final signoff: approved in `.agent/plans/agentic/task-plans/task-304-plan-review.md` for the canonical plan and approved in `.agent/plans/agentic/task-plans/task-304-impl-review.md` for the shipped implementation.
- Final outcome: task-304 delivered chunk-hash-based unchanged-doc skipping before embedding, preserved content-versioned skipped-row metadata, shipped explicit docs sync result reporting, and synchronized the tracker/research/workflow artifacts required by the approved plan.
- Canonical-record note: this document is the final durable summary of approved scope, completed build state, verification posture, and outcome; the two review logs remain preserved historical transcripts, including the implementation log's intentional sequencing anomalies.
