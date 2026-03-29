# Task Plan: task-302 Implement markdown chunking

- Task ID: `task-302`
- Title: `Implement markdown chunking`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-302` is the next unblocked high-priority backlog gap after `task-301`: the current docs ingestor in `agentic/src/agentic_kb/ingest/docs.py` still stores one row per markdown file with `section_title=None`, `subsection_title=None`, `heading_path=[]`, and `chunk_index=0`.
- Phase 3 explicitly requires markdown/docs splitting on heading boundaries before later docs work can rely on meaningful section-level search previews and hierarchy.
- The current repo already has the right schema fields in `agentic.kb_documents` and an established allowlisted docs-ingestion path, so this task can stay small and build directly on shipped repo reality.

## Scope

- Add heading-boundary chunking to the existing docs ingestor in `agentic/src/agentic_kb/ingest/docs.py`.
- Preserve the current task-301 docs-ingestion contract for allowlisted sources, normalized repo-relative POSIX `source_path` values, deterministic ids and hashes, repo commit capture, and embedding generation.
- Populate `section_title`, `subsection_title`, `heading_path`, and multi-row `chunk_index` values in `agentic.kb_documents` for markdown docs that contain headings.
- Make all docs write paths that matter for task-302 use one atomic source-path replacement seam so shrinking files do not leave stale rows behind and failed re-ingests do not silently empty documents.
- Add focused tests in `agentic/tests/test_docs_ingest.py` and the relevant docs-sync tests for chunk parsing, row shaping, deterministic ordering, source-path replacement behavior, and sync-state metadata semantics.

## Non-Goals

- Do not broaden into task-303 structured plan/workflow metadata extraction beyond the heading hierarchy already represented by `section_title`, `subsection_title`, and `heading_path`.
- Do not broaden into task-304 generalized unchanged-content skip logic or sync-state redesign.
- Do not create a separate generic markdown chunking package or a new `agentic/src/ingest/markdown_chunker.py` module unless implementation proves the existing `docs.py` file cannot hold the minimal logic cleanly.
- Do not redesign the docs allowlist, docs CLI surface, or embedding model contract.
- Do not implement arbitrary size-window chunking or generalized semantic chunking; this task is specifically heading-boundary chunking.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-301` - allowlisted docs ingestion in `agentic/src/agentic_kb/ingest/docs.py`
  - `task-501` - Ollama embedding client used by docs ingestion
  - `task-701` - docs sync commands that already call `prepare_documents(...)` and persist docs sync state
- Downstream boundaries to preserve:
  - `task-303` - structured metadata extraction
  - `task-304` - generalized idempotency / unchanged-content skip behavior
  - `task-502` and later search tasks - benefit from better chunked docs rows but are not part of this implementation
- Reviewed references:
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-701-sync-commands.md`
  - `.agent/plans/agentic/research/task-901-clean-machine-bootstrap.md`
  - `.agent/plans/agentic/research/task-803-804-tracking-adjustment.md`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/tests/test_docs_ingest.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/schema/init.sql`

## Current Repo State To Reconcile

- Before implementation, `.agent/plans/agentic/knowledge-base-platform-tasks.json` pointed `task-302.targetPath` at stale pre-package path `agentic/src/ingest/markdown_chunker.py` even though current repo reality kept docs ingestion in `agentic/src/agentic_kb/ingest/docs.py`.
- `agentic.kb_documents` already had the required hierarchy columns and uniqueness constraint `UNIQUE (source_path, chunk_index)`.
- Before implementation, `prepare_documents(...)` returned exactly one `PreparedDocument` per source file and `ingest_docs(...)` simply upserted those rows, which was not sufficient once per-file chunk counts could grow or shrink across reruns.
- `sync docs` and `sync changed` already used `prepare_documents(...)`, so the task preserved those entrypoints rather than creating a second docs-ingestion path.

## Files Expected To Change

- `agentic/src/agentic_kb/ingest/docs.py` - heading parsing, chunk preparation, deterministic chunk row shaping, and one atomic source-path replacement seam reused by all docs write paths
- `agentic/tests/test_docs_ingest.py` - focused coverage for chunk boundaries, heading hierarchy, intro chunks, replacement semantics, and failure-safe rerun behavior
- `agentic/src/agentic_kb/ingest/__init__.py` - only if exports need to expose new or renamed docs-ingest helpers
- `agentic/src/agentic_kb/commands/sync.py` - update explicit and changed docs sync paths to use the shared replacement seam and to keep sync metadata contracts explicit under chunked docs
- `agentic/tests/test_sync_command.py` - extend docs-sync coverage for explicit `sync docs` and incremental docs-changed flows under chunked rows
- `agentic/tests/test_sync_command_db.py` - extend DB-backed coverage for chunk replacement behavior and docs sync-state metadata where environment-backed tests already exist
- `.agent/workflows/agentic-kb.md` - update only if the shipped operator-visible docs sync contract changes materially, especially if `processed_count` now represents chunk rows rather than file count
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update `task-302.targetPath` to the real packaged docs-ingestion path when implementation lands
- `.agent/plans/agentic/research/` - add a task-302 research note with durable chunking findings after implementation

## Implementation Approach

- Keep the implementation in `agentic/src/agentic_kb/ingest/docs.py` unless the diff clearly proves a small helper split is needed. The task tracker path should be corrected to repo reality during implementation rather than driving a new module into existence.
- Preserve the current docs source set and normalization rules from task-301. Chunking changes row shaping, not source discovery.
- Use the smallest markdown parser boundary that fits current repo reality:
  - chunk on ATX headings `#` through `######`
  - keep non-heading documents as a single chunk with `chunk_index = 0`, `section_title = None`, `subsection_title = None`, and `heading_path = []`
  - keep content that appears before the first section heading as an intro chunk with empty heading metadata when the file has leading body text outside later sections
  - do not widen scope into full CommonMark parsing unless allowlisted docs or tests prove ATX-only handling is insufficient
- Preserve the existing top-level title behavior:
  - keep `title` sourced from the first H1 when present, otherwise fallback basename logic
  - when the opening H1 is acting as the document title, do not duplicate that title into `heading_path`; section hierarchy should describe navigational sections under the document title
  - later headings still participate in chunking normally
- Use a simple hierarchy mapping for stored rows:
  - `heading_path` is the ordered heading trail for the chunk beneath the document title
  - `section_title` is `heading_path[0]` when present
  - `subsection_title` is `heading_path[1]` when present
  - deeper heading levels stay only in `heading_path`
- Each stored chunk should include the heading line and the content that belongs to that section so search and previews retain local context.
- Keep chunk ordering deterministic and file-local. `chunk_index` should increment in document order starting at `0` for each `source_path`.
- Keep ids deterministic with the existing shape `docs:{source_path}#{chunk_index}`.
- Update `content_hash` generation to be row-scoped for chunked docs, for example by anchoring it to normalized `source_path`, `chunk_index`, and normalized chunk content. This preserves determinism without widening into task-304 skip logic.
- Reuse the current embedding flow per stored chunk row. If an individual chunk still exceeds Ollama context limits, keep the existing embedding-only segmentation fallback for that chunk rather than changing stored row boundaries.
- Lock one atomic replacement seam for chunked docs writes across all relevant paths:
  - `ingest_docs(...)`
  - `ingest_docs_from_config(...)` through `ingest_docs(...)`
  - explicit `sync docs` in `agentic/src/agentic_kb/commands/sync.py`
  - incremental `_sync_docs_changed(...)` in `agentic/src/agentic_kb/commands/sync.py`
- The replacement seam must be transactionally atomic per affected `source_path` set. Minimal acceptable shape:
  - add one store helper such as `replace_documents_for_paths(source_paths, documents)` that deletes existing rows for those `source_path` values and inserts the replacement rows in the same transaction, or
  - centralize an equivalent one-transaction helper in `docs.py` that all docs write callers reuse
  Separate `delete_documents_for_paths(...)` then `upsert_documents(...)` calls are not acceptable for task-302 because a failure between them can silently empty a document.
- Failure-safety requirement:
  - if chunk preparation, embedding, or database insert fails for a document during re-ingest, the previously stored rows for that `source_path` must remain intact
  - replacement must happen only after the new chunk set for the affected file paths is fully prepared and inside the same DB transaction as the delete/insert work
  - no docs write path may keep its own ad hoc delete-then-upsert sequence once task-302 lands
- Preserve current library and sync entrypoints where possible:
  - `prepare_documents(...)` should now return one `PreparedDocument` per chunk
  - `ingest_docs(...)` should derive file-unique `source_paths` in result metadata even when multiple chunks are written per file
  - explicit `sync docs` and `_sync_docs_changed(...)` should also report file-unique `source_paths` / `changed_paths` while treating `processed_count` as chunk rows written
  - `build_docs_sync_state(...)` should continue storing unique source-file paths in metadata while `processed_count` intentionally reflects chunk-row count after task-302

## Acceptance Criteria

- Markdown docs ingestion is implemented on the real packaged path `agentic/src/agentic_kb/ingest/docs.py`; no new stale-path-only module is required.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated so `task-302.targetPath` matches the packaged docs-ingestion path used by the implementation.
- For markdown files with headings, `prepare_documents(...)` returns multiple ordered chunk rows under the same `source_path` with deterministic `chunk_index` values.
- For heading-less markdown files, docs ingestion still produces one row with empty heading metadata.
- Stored chunk rows populate hierarchy fields correctly:
  - `heading_path` preserves the chunk's heading trail
  - `section_title` and `subsection_title` are derived from that path
  - document `title` remains the top-level document title rather than being redundantly copied into every chunk path
- `source_path` remains repo-relative POSIX and `source_domain` remains `docs` for all chunked rows.
- Deterministic ids and content hashes remain stable for unchanged chunk content under unchanged source path ordering.
- Re-ingesting a changed document with fewer chunks removes stale old chunk rows instead of leaving orphaned higher `chunk_index` rows behind.
- Re-ingesting unchanged docs succeeds without uniqueness failures and preserves deterministic row identities.
- All docs write paths that matter for this task use the same atomic source-path replacement behavior: library ingest, explicit `sync docs`, and incremental `_sync_docs_changed(...)`.
- A failed re-ingest does not leave an affected document empty or partially replaced; previously stored rows remain intact if replacement fails.
- Result and sync-state metadata remain explicit after chunking: file-path collections are unique by source file while `processed_count` intentionally represents chunk rows rather than file count.
- The implementation stays within task scope and does not add task-303 structured metadata extraction or task-304 generalized unchanged-doc skip behavior.

## Verification Plan

- Extend `agentic/tests/test_docs_ingest.py` to cover:
  - heading-less docs remaining single-row
  - one file splitting into multiple chunks on headings
  - nested heading hierarchy mapping into `heading_path`, `section_title`, and `subsection_title`
  - intro content before the first section heading
  - deterministic `chunk_index` ordering and ids
  - row-scoped deterministic `content_hash` generation for chunked rows
  - rerun replacement when a file shrinks from multiple chunks to fewer chunks
  - atomic replacement behavior for a shrinking rerun in the in-memory store shape
  - failure safety: a simulated replacement failure leaves the prior rows intact rather than emptying the file
  - current embedding fallback behavior still working on oversized chunk content
- Verify that direct `prepare_documents(...)` / `ingest_docs(...)` results keep `source_paths` unique by source file even when multiple chunks are produced.
- Extend sync tests to cover the repo's real docs-sync write paths:
  - explicit `sync docs` uses the shared replacement seam and removes stale higher `chunk_index` rows when a file shrinks
  - incremental `_sync_docs_changed(...)` uses the same replacement seam for changed files
  - docs sync-state metadata written through `build_docs_sync_state(...)` keeps unique source-file paths while `processed_count` intentionally reflects chunk rows
  - no docs sync path regresses to ad hoc delete-then-upsert sequencing
- Run the focused docs ingest unit suite locally with `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_docs_ingest.py'`.
- Run the relevant sync test suites that exercise explicit docs sync and changed-only docs sync under chunked rows.
- Rebuild `kb-tools` and run the same docs ingest test suite in-container so packaged runtime behavior matches source execution.
- For live verification, run docs ingest against a small representative subset and confirm in SQL that one source path now has multiple rows with ordered `chunk_index`, populated hierarchy fields, normalized `source_path`, and no stale chunk rows after a shrinking rerun.
- If live sync verification is performed, confirm the resulting docs sync-state metadata shows unique file paths and the intended chunk-row `processed_count` value.

## Risks / Open Questions

- **ATX-only first pass**: setext headings and broader CommonMark constructs remain out of scope for v1 and may need a follow-up task if a real allowlisted document requires them.
- **Result metadata semantics**: `DocsIngestResult.processed_count` now means chunk-row count rather than source-file count, so downstream operator-facing wording must continue to keep that distinction explicit.
- **Chunk granularity**: heading-boundary chunking can still produce very large sections in some docs. This task intentionally relies on existing embedding-only segmentation fallback rather than widening into non-heading secondary chunking.

## Required Docs / Tracking / Research Updates

- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` during implementation to correct `task-302.targetPath` to the real packaged path.
- Add a durable research note under `.agent/plans/agentic/research/` capturing the final chunking contract, hierarchy mapping, replacement strategy for shrinking files, and any heading-parser limitations accepted for v1.
- Update this canonical task plan with final implementation notes, verification notes, and outcome after implementation.
- Update `.agent/workflows/agentic-kb.md` if task-302 changes the operator-visible docs-sync contract, especially if docs `processed_count` in sync summaries now means chunk rows rather than file count.
- Keep any operator-doc update narrow and task-owned: only refresh docs-sync wording that would become stale from the task-302 contract change, not broader workflow content reserved for other tasks.
- Keep planning review history in `.agent/plans/agentic/task-plans/task-302-plan-review.md`.
- Do not create `.agent/plans/agentic/task-plans/task-302-impl-review.md` during this planning pass.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-302-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-302-impl-review.md`

## Final Implementation Notes

- The approved plan landed on the packaged docs ingestor path `agentic/src/agentic_kb/ingest/docs.py` with ATX-heading chunking, intro-chunk preservation, top-level H1 title preservation, deterministic chunk ids/hashes, and heading hierarchy stored through `section_title`, `subsection_title`, and `heading_path`.
- Docs writes now use one shared atomic `replace_documents_for_paths(...)` seam across library ingest, explicit `sync docs`, and incremental `_sync_docs_changed(...)`, so shrinking reruns remove stale higher `chunk_index` rows without risking empty docs on failed replacement.
- The final implementation also hardens heading detection by ignoring ATX-looking lines inside fenced code blocks, matching the approved chunking contract for real allowlisted docs.
- Task-owned follow-through shipped alongside the code: `task-302.targetPath` in `.agent/plans/agentic/knowledge-base-platform-tasks.json` now matches repo reality, `.agent/workflows/agentic-kb.md` reflects docs `processed_count` as chunk rows with file-unique `source_paths`, and the durable research note lives at `.agent/plans/agentic/research/task-302-markdown-chunking.md`.

## Final Verification Notes

- Recorded implementation verification from `.agent/plans/agentic/task-plans/task-302-impl-review.md`:
  - `python3 -m py_compile "agentic/src/agentic_kb/ingest/docs.py" "agentic/src/agentic_kb/commands/sync.py" "agentic/tests/test_docs_ingest.py" "agentic/tests/test_sync_command.py" "agentic/tests/test_sync_command_db.py"`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_docs_ingest`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command`
  - `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command_db`
- Outcome:
  - compile passed
  - `agentic.tests.test_docs_ingest` passed after the fenced-code regression fix (`Ran 15 tests`, `OK`)
  - `agentic.tests.test_sync_command` passed (`Ran 15 tests`, `OK`, `1 skipped` for optional tree-sitter coverage)
  - `agentic.tests.test_sync_command_db` remains environment-gated in the recorded run (`Ran 3 tests`, `OK`, all skipped) because `AGENTIC_TEST_DATABASE_URL` was not set

## Final Outcome

- Planning is approved per `.agent/plans/agentic/task-plans/task-302-plan-review.md` iteration 2 (`Decision: approved`).
- Implementation and code review are approved per `.agent/plans/agentic/task-plans/task-302-impl-review.md` iteration 2 (`Decision: approved`).
- `task-302` is complete: markdown docs are chunked on approved heading boundaries, sync/reporting metadata semantics are explicit, stale chunk rows are removed safely on rerun, and no scope creep into task-303 or task-304 was introduced.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-302-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the approved implementation landed, verification was recorded, and iteration 2 of `.agent/plans/agentic/task-plans/task-302-impl-review.md` ended with `Decision: approved`.
