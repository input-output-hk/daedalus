# Task 405 Sync State Research

- Date: 2026-03-28
- Task: `task-405`
- Evidence: `agentic/src/agentic_kb/sync/state.py`, `agentic/src/agentic_kb/ingest/docs.py`, `agentic/src/agentic_kb/ingest/code.py`, `agentic/src/agentic_kb/ingest/github.py`, `agentic/src/agentic_kb/ingest/project.py`, `agentic/tests/test_sync_state.py`

## Durable Findings

- The accepted packaged destination for sync-state logic is `agentic/src/agentic_kb/sync/state.py`, with `agentic/src/agentic_kb/sync/__init__.py` re-exporting the public helpers for downstream tasks.
- `agentic.kb_sync_state` is sufficient for the current v1 needs without a schema change when state is partitioned by scope: one repo-scoped row each for docs and code, one row per GitHub stream (`issues`, `pulls`, `issue_comments`, `review_comments`), and one project-scoped row for `DripDropz` Project 5.
- Docs and code sync-state rows stay intentionally minimal in `task-405`: they persist `repo_commit_hash`, `last_attempted_at`, `last_succeeded_at`, and bounded `last_error`, while `cursor_text`, `watermark_text`, and `watermark_timestamp` remain unset.
- GitHub sync-state rows derive directly from `GithubIngestResult.stream_progress`; `watermark_timestamp` stores the latest observed `source_updated_at` for the stream, while `metadata` stores bounded fetch details such as stream name, page size, hit-bound flag, and per-entity write counts.
- Project sync-state rows derive directly from `ProjectIngestResult`; `cursor_text` stores `final_cursor`, `watermark_timestamp` stores the latest observed project-item update timestamp, and `metadata` preserves owner/number/title/url plus bounded fetch details.
- Config-backed ingestion entrypoints now record attempt state before ingest, record failure state before re-raising if ingest fails, and persist success state only after the ingest succeeds, so sync-state write failures are loud instead of producing false-success results.
- The task also needs the sync lookup helpers re-exported through `agentic_kb.ingest` if downstream code is meant to consume the ingestion package surface; implementation review caught a missing `get_sync_state` / `list_sync_states` import in `agentic/src/agentic_kb/ingest/__init__.py`, and adding an explicit import-surface test prevents that regression from recurring.
- The stale `task-304` dependency for `task-405` in `.agent/plans/agentic/knowledge-base-platform-tasks.json` was confirmed to be tracker drift rather than a real blocker and was removed as part of this task's required tracking cleanup.

## Verification Notes

- Local unit coverage passed for deterministic ids, scope-key helpers, docs/code minimal state mapping, GitHub per-stream watermark mapping, project cursor mapping, in-memory attempt/failure/success bookkeeping, and source-specific config-wrapper integration.
- Deterministic ParadeDB-backed verification passed using `AGENTIC_TEST_DATABASE_URL` against a live `paradedb` service, proving the real `PostgresSyncStateStore` contract for `ON CONFLICT` updates, typed timestamp/cursor fields, JSON metadata, and attempt/success/failure sequencing without any GitHub or Project credentials.
- In-container unit coverage passed inside `kb-tools` for the sync-state suite and the touched GitHub/project wrapper tests.

## No New Research Beyond Task Scope

- No new operator-facing workflow requirement was discovered beyond the library-first sync-state contract already captured here; real `sync` CLI behavior remains deferred to `task-701`.
