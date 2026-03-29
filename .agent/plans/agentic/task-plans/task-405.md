# Task Plan: task-405 Track sync cursors and watermarks

- Task ID: `task-405`
- Title: `Track sync cursors and watermarks`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-405` is the next critical-path task after `task-403` and `task-404`: the repo now has real GitHub and Project ingestion result contracts, but no durable place yet to persist the watermarks and cursors those tasks expose.
- The remaining declared dependency on `task-304` looks stale rather than truly blocking for this task's scope, but the tracker inconsistency must be reconciled in `.agent/plans/agentic/knowledge-base-platform-tasks.json` before implementation begins. `task-301` already writes `content_hash` and `repo_commit_hash`, so `task-405` can persist sync-state records once that tracker cleanup is done even though skip-unchanged docs behavior still belongs to `task-304`.
- This task unblocks the first implementation-ready path for `task-601`, `task-604`, `task-701`, and `task-702`, all of which need durable sync state before snapshot manifests, delta sync, and stale-index detection can be credible.

## Scope

- Add a packaged sync-state library under `agentic/src/agentic_kb/` that reads and writes `agentic.kb_sync_state`.
- Persist repo commit hashes for docs and code ingestion runs.
- Persist GitHub stream watermarks for issues, pull requests, issue comments, and review comments.
- Persist GitHub Project cursor and watermark state for Project 5 ingestion runs.
- Expose reusable state records and source-specific helper functions so later sync orchestration can read prior state and pass bounded fetch parameters without redesigning ingestion modules.

## Non-Goals

- Do not implement real `agentic-kb sync ...` command behavior; `task-701` still owns CLI orchestration.
- Do not implement docs skip-unchanged logic, content-hash diffing, or file-level pruning; that remains `task-304` and later sync work.
- Do not implement snapshot export/import behavior, stale-index warnings, or search-status UX; those remain `task-601`, `task-602`, `task-702`, and `task-503`.
- Do not redesign the `agentic.kb_sync_state` schema from `task-202` unless implementation proves a hard blocker, which this plan does not expect.
- Do not add GitHub/project mutation behavior or broaden token requirements beyond the already accepted read-only ingest contracts.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-103` - established the packaged `agentic_kb` module layout and current CLI/package boundaries.
  - `task-202` - created `agentic.kb_sync_state` with the columns this task must now populate.
  - `task-301` - already captures repo-relative doc identities, `content_hash`, `repo_commit_hash`, and file timestamps during docs ingestion.
  - `task-401` - established the packaged code-ingestion path that can publish repo-level sync state.
  - `task-403` - established reusable GitHub bounds and per-stream progress reporting with latest `updated_at` watermarks.
  - `task-404` - established reusable Project bounds plus final cursor and latest item watermark reporting.
- Tracker inconsistencies reconciled during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` no longer lists the stale `task-304` dependency for `task-405`, matching current repo reality that `task-304` still matters for skip-unchanged docs behavior but does not block bounded sync-state persistence.
- Direct downstream tasks unblocked by this work:
  - `task-601` - snapshot manifests need exported GitHub cursors and watermarks.
  - `task-604` - import-then-sync-changed bootstrap depends on prior sync state.
  - `task-701` - real sync orchestration needs persisted cursors/watermarks plus read APIs.
  - `task-702` - stale-index detection needs repo commit and remote watermark comparisons.
- Tracking mismatch reconciled during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` now points `task-405.targetPath` at `agentic/src/agentic_kb/sync/state.py`, matching the packaged module layout.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-301-docs-ingestion.md`
  - `.agent/plans/agentic/research/task-403-github-ingestion.md`
  - `.agent/plans/agentic/research/task-404-project-ingestion.md`
  - `.agent/plans/agentic/research/task-501-ollama-embedding-client.md`
  - `.agent/plans/agentic/task-plans/task-103.md`
  - `.agent/plans/agentic/task-plans/task-202.md`
  - `.agent/plans/agentic/task-plans/task-301.md`
  - `.agent/plans/agentic/task-plans/task-403.md`
  - `.agent/plans/agentic/task-plans/task-404.md`
  - `.agent/plans/agentic/task-plans/task-501.md`
  - `agentic/schema/init.sql`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/ingest/__init__.py`
  - `agentic/src/agentic_kb/ingest/docs.py`
  - `agentic/src/agentic_kb/ingest/code.py`
  - `agentic/src/agentic_kb/ingest/github.py`
  - `agentic/src/agentic_kb/ingest/project.py`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile the stale `task-304` dependency before implementation begins, fix `task-405.targetPath` to the packaged module path, and update task status when implementation lands.
- `agentic/src/agentic_kb/sync/state.py` - new sync-state models, deterministic ids, store implementations, and source-specific state helpers.
- `agentic/src/agentic_kb/sync/__init__.py` - export the approved sync-state surface.
- `agentic/src/agentic_kb/ingest/docs.py` - wire the existing config-backed docs ingest path into repo-scoped sync-state persistence without broadening `DocsIngestResult`.
- `agentic/src/agentic_kb/ingest/code.py` - wire the existing config-backed code ingest path into repo-scoped sync-state persistence without broadening `CodeIngestResult`.
- `agentic/src/agentic_kb/ingest/github.py` - wire GitHub stream progress into persisted sync-state updates without changing the existing library-first fetch/write contract.
- `agentic/src/agentic_kb/ingest/project.py` - wire final cursor and watermark into persisted project sync-state updates.
- `agentic/src/agentic_kb/ingest/__init__.py` - export the new sync-state helpers if downstream callers need them through the ingestion package surface.
- `agentic/tests/test_sync_state.py` - focused unit coverage for sync-state records, keying, source-specific update mapping, and a deterministic ParadeDB-backed round-trip for the real `kb_sync_state` table.
- `agentic/tests/test_docs_ingest.py` - only if needed to lock the config-backed docs state-persistence path without broadening `DocsIngestResult`.
- `agentic/tests/test_github_ingest.py` - only if needed to lock GitHub watermark mapping and state-update integration.
- `agentic/tests/test_project_ingest.py` - only if needed to lock project cursor persistence and state-update integration.
- `.agent/plans/agentic/research/task-405-sync-state.md` - durable implementation findings and caveats discovered during execution.

## Expected Library Surface

- `agentic/src/agentic_kb/sync/state.py` should be the canonical packaged destination for this task, replacing the stale tracker path `agentic/src/sync/state.py`.
- Expected public dataclasses and stores:
  - `PreparedSyncState`
  - `SyncStateRecord`
  - `SyncStateReadResult` or similarly narrow read model for downstream orchestration
  - `PostgresSyncStateStore`
  - `InMemorySyncStateStore`
- Expected public helpers:
  - `deterministic_sync_state_id(source_name, scope_key)`
  - `build_docs_sync_state(...)`
  - `build_code_sync_state(...)`
  - `build_github_sync_state_updates(...)`
  - `build_project_sync_state(...)`
  - `persist_sync_state_updates(...)` or equivalent narrow upsert helper
  - lookup helpers such as `get_sync_state(source_name, scope_key)` and `list_sync_states(source_name=None)` for later `task-701` / `task-702` reuse

## Implementation Approach

- **First implementation step**: update `.agent/plans/agentic/knowledge-base-platform-tasks.json` before production code lands so the stale `task-304` dependency is removed or otherwise explicitly reconciled and `task-405.targetPath` points at `agentic/src/agentic_kb/sync/state.py`.
- **Package placement**: create a new packaged sync module at `agentic/src/agentic_kb/sync/` instead of using the stale un-packaged path from the tracker.
- **State identity contract**: use deterministic `kb_sync_state.id` values derived from `(source_name, scope_key)`, for example `sync-state:docs:repo:DripDropz/daedalus`, to keep reruns stable and snapshot-safe.
- **Store contract**: implement a narrow sync-state store around `agentic.kb_sync_state` with `INSERT ... ON CONFLICT (source_name, scope_key) DO UPDATE`, plus read helpers that future sync commands can call without reaching into raw SQL.
- **Source partitioning**: persist one logical state row per independently refreshable source scope instead of one global blob:
  - docs: one row keyed by repo scope
  - code: one row keyed by repo scope
  - GitHub: one row per REST stream (`issues`, `pulls`, `issue_comments`, `review_comments`) for the repo
  - project: one row for `DripDropz` Project 5 scope
- **Docs/code state mapping**: keep this task explicitly minimal for docs and code. Persist only `repo_commit_hash`, `last_attempted_at`, `last_succeeded_at`, and bounded `last_error` for the repo-scoped docs/code rows; leave `cursor_text`, `watermark_text`, and `watermark_timestamp` unset for those sources in `task-405`. Do not expand `DocsIngestResult` or `CodeIngestResult` in this task.
- **GitHub state mapping**: convert `GithubIngestResult.stream_progress` into per-stream sync-state rows. For each stream, persist the repo scope in `scope_key`, the latest observed `source_updated_at` in `watermark_timestamp`, a readable ISO form in `watermark_text`, and metadata describing bounds used, pages fetched, and whether a page limit truncated the run.
- **Project state mapping**: persist `ProjectIngestResult.final_cursor` into `cursor_text`, persist `latest_source_updated_at` into `watermark_timestamp`, and preserve project owner/number plus truncation facts in metadata.
- **Attempt vs success bookkeeping**: set `last_attempted_at` at the start of a stateful ingest wrapper and `last_succeeded_at` only after the underlying ingest plus sync-state upsert succeed. On failure, capture a bounded `last_error` without masking the original exception.
- **Integration boundary**: keep the task library-first by wiring persistence into the existing config-backed ingestion entrypoints (`ingest_*_from_config`) or into explicit wrapper helpers in the new sync module. Do not add real CLI sync behavior yet.
- **Bounds reuse contract**: design read helpers so `task-701` can easily translate prior sync-state rows back into `GithubFetchBounds(updated_since=...)` and `ProjectFetchBounds(after_cursor=...)` without reparsing opaque blobs.
- **Metadata discipline**: use typed columns for the canonical watermark fields and reserve `metadata` for source-specific extras such as `pages_fetched`, `hit_bound`, `repo`, `project_owner`, `project_number`, and the exact stream name. Docs/code rows should not invent synthetic watermark metadata in this task.
- **Schema boundary**: reuse the existing `kb_sync_state` columns from `task-202`; this task should prove the schema is sufficient before any future migration is considered.
- **Failure model**: fail loudly for malformed sync-state writes or impossible source/result mappings. A failed state write should prevent a config-backed ingest wrapper from reporting success, because silent stale state would undermine later incremental sync logic.
- **Testing strategy**: keep tests focused and deterministic with in-memory stores plus stubbed ingest results, and add one required ParadeDB-backed round-trip that exercises the real `PostgresSyncStateStore` against `agentic.kb_sync_state` with no GitHub or Project credentials. Add only the minimum integration coverage needed in source-specific ingest tests to prove state persistence hooks are called with the right contracts.

## Acceptance Criteria

- A packaged sync-state module exists at `agentic/src/agentic_kb/sync/state.py`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is updated before implementation begins so the stale `task-304` dependency is reconciled and `task-405.targetPath` points at `agentic/src/agentic_kb/sync/state.py`.
- The sync-state module can read and upsert rows in `agentic.kb_sync_state` using deterministic ids plus the existing unique constraint on `(source_name, scope_key)`.
- Docs and code ingestion have a bounded, reusable path that records repo sync state using only `repo_commit_hash`, `last_attempted_at`, `last_succeeded_at`, and failure bookkeeping, without adding docs/code watermarks or implementing skip-unchanged orchestration.
- GitHub ingestion persists per-stream watermarks for `issues`, `pulls`, `issue_comments`, and `review_comments` using the existing `GithubIngestResult.stream_progress` contract.
- Project ingestion persists final cursor and latest observed item watermark for Project 5 using the existing `ProjectIngestResult` contract.
- Sync-state rows preserve typed watermark fields in columns and source-specific context in `metadata` without requiring schema changes.
- Read helpers expose prior cursor/watermark state in a form that later `task-701` and `task-702` work can reuse directly.
- Rerunning a successful ingest updates the existing sync-state rows in place rather than creating duplicates.
- The implementation remains library-first and does not add real `sync` CLI behavior, snapshot logic, staleness warnings, or search UX.

## Verification Plan

- Run focused unit tests for the new sync-state module, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'`.
- Add deterministic unit coverage for:
  - sync-state id and scope-key generation
  - `ON CONFLICT` upsert behavior for the same `(source_name, scope_key)` pair
  - docs/code state mapping from ingestion results into `repo_commit_hash`, `last_attempted_at`, `last_succeeded_at`, and `last_error`, with docs/code watermark fields remaining unset
  - GitHub per-stream watermark mapping from `GithubIngestResult.stream_progress`
  - project cursor and watermark mapping from `ProjectIngestResult`
  - read helpers that reconstruct prior state for downstream sync orchestration
  - attempt/success/error bookkeeping semantics
- Re-run the focused source-specific ingest tests touched by the implementation, likely `test_github_ingest.py`, `test_project_ingest.py`, and any docs/code tests whose persistence hooks changed without broadening docs/code result contracts.
- Run `docker compose -f docker-compose.agentic.yml config` to confirm the package layout still resolves cleanly.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so in-container verification uses the current installed package.
- Run at least one deterministic ParadeDB-backed verification path for the real store with no remote credentials, for example:
  - `docker compose -f docker-compose.agentic.yml up -d paradedb`
  - `PYTHONPATH=agentic/src DATABASE_URL=postgresql://agentic:agentic@127.0.0.1:5445/agentic_kb python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'`
  - That suite must exercise `PostgresSyncStateStore` against the real `agentic.kb_sync_state` table using stubbed/local docs, code, GitHub, and project sync results, and assert deterministic ids, `ON CONFLICT` updates, JSON `metadata`, `cursor_text`, `watermark_timestamp`, `repo_commit_hash`, and attempt/success timestamp behavior.
- Run the touched unit suites inside `kb-tools` with `--entrypoint python` because `kb-tools` still uses `ENTRYPOINT ["agentic-kb"]`.
- If credentials are available, run a bounded live smoke path for GitHub/project ingestion and verify corresponding rows appear in `agentic.kb_sync_state` with expected `source_name`, `scope_key`, cursor, watermark, and timestamp fields; otherwise treat live remote verification as credential-blocked rather than silently skipped.

## Risks / Open Questions

- **Docs/code result thinness**: `task-405` intentionally keeps docs/code sync-state rows limited to repo commit plus attempt/success/error bookkeeping. If later `task-701` needs richer repo-level watermarks, it should widen the contract deliberately instead of retrofitting hidden behavior here.
- **Stream granularity**: one GitHub sync-state row per stream matches the current `GithubIngestResult.stream_progress` surface and existing schema, but later incremental-sync work may still discover a need for finer-grained bounds or replay windows.
- **Failure semantics**: config-backed ingest wrappers now fail loudly if sync-state persistence fails, which is the right v1 default but means state-table regressions can block otherwise successful ingestion runs.
- **Schema sufficiency**: `kb_sync_state` proved sufficient for current docs/code commit state, GitHub stream watermarks, and Project 5 cursor/watermark storage without a migration, but later snapshot or stale-detection work may still want stronger typed metadata conventions.

## Required Docs / Tracking / Research Updates

- Updated this task plan doc with final planning/build status, implementation notes, verification notes, and outcome.
- Updated `.agent/plans/agentic/knowledge-base-platform-tasks.json` to remove the stale `task-304` dependency, correct `task-405.targetPath`, and mark the task completed.
- Added `.agent/plans/agentic/research/task-405-sync-state.md` with durable findings about scope keys, row partitioning, failure semantics, and config-wrapper behavior.
- `.agent/workflows/agentic-kb.md` did not need a user-facing change yet because the implementation stayed library-first and did not add new operator commands beyond existing config-backed ingestion entrypoints.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-405-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-405-impl-review.md`

## Implementation Notes

- Added the packaged sync-state module at `agentic/src/agentic_kb/sync/state.py` plus `agentic/src/agentic_kb/sync/__init__.py` with deterministic ids, repo/GitHub/project scope-key helpers, typed state builders, failure/attempt helpers, and both in-memory and Postgres-backed stores for `agentic.kb_sync_state`.
- Wired `ingest_docs_from_config`, `ingest_code_from_config`, `ingest_github_from_config`, and `ingest_project_items_from_config` to record sync attempts first, persist success state after the ingest completes, and record bounded failure state before re-raising exceptions.
- Kept the docs/code contract intentionally minimal: repo-scoped sync-state rows persist `repo_commit_hash`, attempt/success timestamps, and bounded `last_error`, while `cursor_text`, `watermark_text`, and `watermark_timestamp` remain unset for those sources.
- Persisted GitHub sync state as one row per stream (`issues`, `pulls`, `issue_comments`, `review_comments`) with typed watermark columns plus metadata for page counts, bounds, and row totals.
- Persisted Project 5 sync state as one row keyed by project scope with `cursor_text` from `ProjectIngestResult.final_cursor`, typed watermark columns from the latest observed item timestamp, and metadata for owner/number/title/url and bounded fetch details.
- Exported the new sync-state helpers through `agentic/src/agentic_kb/ingest/__init__.py`, including the promised `get_sync_state` and `list_sync_states` re-exports after implementation review caught the missing imports, and added focused tests in `agentic/tests/test_sync_state.py`, `agentic/tests/test_docs_sync_state.py`, `agentic/tests/test_code_sync_state.py`, plus sync-state integration coverage in `agentic/tests/test_github_ingest.py` and `agentic/tests/test_project_ingest.py`.

## Verification Notes

- `python3 -m py_compile "agentic/src/agentic_kb/sync/state.py" "agentic/src/agentic_kb/sync/__init__.py" "agentic/src/agentic_kb/ingest/__init__.py" "agentic/src/agentic_kb/ingest/docs.py" "agentic/src/agentic_kb/ingest/code.py" "agentic/src/agentic_kb/ingest/github.py" "agentic/src/agentic_kb/ingest/project.py" "agentic/tests/test_sync_state.py" "agentic/tests/test_docs_sync_state.py" "agentic/tests/test_code_sync_state.py"` passed.
- Local unit tests passed: `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'`, `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_docs_sync_state.py'`, `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_code_sync_state.py'`, `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_github_ingest.py'`, and `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_project_ingest.py'`, including an explicit import-surface assertion that `agentic_kb.ingest` re-exports `get_sync_state` and `list_sync_states`.
- `docker compose -f docker-compose.agentic.yml config` passed.
- `docker compose -f docker-compose.agentic.yml build kb-tools` passed.
- Deterministic ParadeDB-backed verification passed by starting an isolated DB service, waiting for readiness, and running `AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@127.0.0.1:5755/agentic_kb PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_sync_state.py'`, which exercised the real `PostgresSyncStateStore` against `agentic.kb_sync_state` with stubbed results and asserted `ON CONFLICT` updates, typed cursor/watermark fields, JSON metadata, and attempt/success timestamps.
- In-container unit tests passed with `docker compose -p agentic-task-405 -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_sync_state.py'`, plus the matching `kb-tools` runs for `test_docs_sync_state.py`, `test_code_sync_state.py`, `test_github_ingest.py`, and `test_project_ingest.py`.
- Live remote GitHub/Project verification remained optional and was not required because deterministic local plus ParadeDB-backed verification fully covered the new sync-state contracts.

## Outcome

- `task-405` is complete: the packaged sync-state layer now persists repo commit state for docs/code, per-stream GitHub watermarks, and Project 5 cursor/watermark state without widening into CLI sync orchestration.
- The task tracker is synchronized with implementation reality: the stale `task-304` dependency is removed, `task-405.targetPath` points at the packaged module path, and the task is marked completed.
- The implementation stayed within scope: no new CLI sync commands, stale-index warnings, snapshot logic, or docs skip-unchanged behavior were added.

## Planning Status Rationale

- Planning status is `approved` because the planning review loop converged with `Decision: approved` in `.agent/plans/agentic/task-plans/task-405-plan-review.md`.
- Build status is `completed` because the production code, focused tests, tracker updates, and research note are in place and verification passed.
