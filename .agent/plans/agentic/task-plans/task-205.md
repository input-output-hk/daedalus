# Task Plan: task-205 Add DB status and snapshot commands

- Task ID: `task-205`
- Title: `Add DB status and snapshot commands`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-205` is the next unblocked critical-path task directly in front of `task-503`: `task-503` depends on `task-205` and `task-502`, and `task-502` is already completed.
- `task-103` intentionally shipped only the packaged CLI surface plus placeholder `snapshot` behavior and bootstrap-only `status`; this task owns replacing those placeholders with real database-aware behavior without waiting for later sync orchestration or MCP work.
- The repo now has enough completed foundation work from `task-202`, `task-203`, `task-204`, `task-405`, and `task-502` to report meaningful KB database readiness and export or restore a portable database snapshot from the existing `kb-tools` container.

## Scope

- Extend the packaged `agentic-kb` CLI in `agentic/src/agentic_kb/cli.py` with real database-aware `status` behavior.
- Replace the task-103 placeholder `snapshot export` and `snapshot import` subcommands with real export and restore behavior for the knowledge database.
- Reuse the existing packaged CLI and current command surface where practical, especially `status`, `snapshot export`, and `snapshot import`.
- Report only the current schema, table, index, and sync-state readiness that already exists today: schema migrations `1`, `2`, and `3`; the current `agentic` tables from `agentic/schema/init.sql`; the seven searchable tables from the task-204 registry; and their task-203 BM25/HNSW index contracts.
- Support local developer and in-container usage through the existing `kb-tools` image and `/workspace/agentic/snapshots` mount.

## Non-Goals

- Do not implement search CLI commands, entity inspection, or query UX; that remains `task-503`.
- Do not implement `sync` orchestration, freshness detection, or MCP integration; those remain later tasks.
- Do not define the canonical snapshot manifest JSON schema or team-sharing publication flow; that remains `task-601` through `task-603`.
- Do not redesign the schema, search registry, or sync-state model unless a narrow blocker appears during implementation.
- Do not rename or broaden the existing placeholder command surface unless implementation finds a concrete compatibility blocker; `snapshot import` remains the only required operator-facing restore verb in task-205, and any `restore` alias is explicitly deferred unless implementation proves it is necessary.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-101` - established the Compose stack, stable service names, and the mounted `agentic/snapshots` path.
  - `task-103` - created the packaged `agentic-kb` CLI and reserved the `status`, `snapshot export`, and `snapshot import` surfaces.
  - `task-201`, `task-202`, `task-203` - created the `agentic` schema, KB tables, and stable BM25/HNSW index contracts that status can now inspect.
  - `task-204` - created the packaged search registry, which can supply the current searchable-entity/table vocabulary instead of re-hardcoding it in status logic.
  - `task-405` - created the packaged sync-state helpers and durable `agentic.kb_sync_state` contract that status can summarize without requiring `sync` commands.
  - `task-502` - completed the current search foundation and confirms the searchable entity set that status should treat as the present search-ready surface.
- Direct downstream tasks unblocked by this work:
  - `task-503` - can build user-facing status/search CLI behavior on top of real DB-aware command infrastructure instead of placeholders.
  - `task-601` - can add the formal snapshot manifest format on top of a real export or restore workflow rather than designing against placeholders.
  - `task-602` - can extend the snapshot workflow into team-sharing polish and manifest-aware behavior rather than first introducing raw dump or restore support.
- Tracking mismatch to reconcile during implementation:
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json` still points `task-205.targetPath` at `agentic/src/cli.py`, but repo reality uses the packaged path `agentic/src/agentic_kb/cli.py`.
- Reference materials reviewed for this plan:
  - `.agent/plans/agentic/prompt.md`
  - `.agent/plans/agentic/knowledge-base-platform.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/workflows/agentic-kb.md`
  - `.agent/plans/agentic/research/task-101-compose-foundation.md`
  - `.agent/plans/agentic/research/task-103-kb-tools-service-image.md`
  - `.agent/plans/agentic/research/task-202-core-knowledge-tables.md`
  - `.agent/plans/agentic/research/task-203-bm25-hnsw-indexes.md`
  - `.agent/plans/agentic/research/task-204-search-config-registry.md`
  - `.agent/plans/agentic/research/task-405-sync-state.md`
  - `.agent/plans/agentic/research/task-502-search-queries.md`
  - `agentic/src/agentic_kb/cli.py`
  - `agentic/src/agentic_kb/commands/status.py`
  - `agentic/src/agentic_kb/commands/snapshot.py`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/config.py`
  - `agentic/src/agentic_kb/sync/state.py`
  - `agentic/schema/init.sql`
  - `agentic/schema/create_indexes.sql`
  - `agentic/Dockerfile`

## Files Expected To Change

- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - reconcile only the stale `task-205.targetPath` and later the `task-205` status/completed metadata; record any unrelated tracker drift as follow-up instead of broad cleanup here.
- `agentic/src/agentic_kb/cli.py` - preserve the packaged CLI entrypoint while wiring real task-205 command behavior.
- `agentic/src/agentic_kb/commands/status.py` - replace bootstrap-only reporting with real DB status while preserving healthcheck behavior.
- `agentic/src/agentic_kb/commands/snapshot.py` - replace task-103 placeholders with real export and restore behavior behind `snapshot export` and `snapshot import`.
- `agentic/src/agentic_kb/commands/__init__.py` - only if needed for stable exports.
- `agentic/Dockerfile` - likely add the PostgreSQL client tools needed for `pg_dump` and `pg_restore` inside `kb-tools`.
- `agentic/tests/test_status_command.py` - focused unit coverage for database-aware status behavior.
- `agentic/tests/test_snapshot_command.py` - focused unit coverage for snapshot path resolution, subprocess invocation, and error handling.
- `agentic/tests/test_status_command_db.py` and or `agentic/tests/test_snapshot_command_db.py` - DB-backed verification if implementation splits pure unit tests from live DB tests.
- `.agent/workflows/agentic-kb.md` - update the operator-facing workflow note once snapshot and DB status behavior are real.
- `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md` - capture durable findings from the actual implementation and verification.

## Implementation Approach

- **CLI surface preservation**: keep `agentic/src/agentic_kb/cli.py` as the single packaged entrypoint and preserve the current top-level commands. `status` remains the canonical status command, and restore semantics are implemented and documented behind the existing `snapshot import` surface so task-103 callers do not need a rename migration. Task-205 should not add a `restore` alias unless implementation uncovers a concrete compatibility need that is also covered by docs and tests.
- **Status command layering**: preserve the current environment and dependency checks from task-103, but extend default `status` output to query the database when `DATABASE_URL` is usable. `--healthcheck` should continue to produce a simple exit-code-oriented readiness result, while normal `status` should include richer DB inspection.
- **Real DB status contract**: status should report only what the current repo can verify today without depending on future tasks:
  - database connectivity and parsed target endpoint
  - applied schema migration versions exactly `1`, `2`, and `3`, matching `agentic.kb_schema_migrations`
  - presence of the current `agentic` tables defined in `agentic/schema/init.sql`: `kb_schema_migrations`, `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_issue_comments`, `kb_github_prs`, `kb_github_pr_comments`, `kb_project_items`, `kb_sync_state`, and `kb_snapshot_manifest`
  - presence of the seven searchable tables from the task-204 registry: `agentic.kb_documents`, `agentic.kb_code_chunks`, `agentic.kb_github_issues`, `agentic.kb_github_issue_comments`, `agentic.kb_github_prs`, `agentic.kb_github_pr_comments`, and `agentic.kb_project_items`
  - presence of the task-203 index contracts for those searchable tables only: `kb_documents_bm25_idx`, `kb_documents_embedding_hnsw_idx`, `kb_code_chunks_bm25_idx`, `kb_code_chunks_embedding_hnsw_idx`, `kb_github_issues_bm25_idx`, `kb_github_issues_embedding_hnsw_idx`, `kb_github_issue_comments_bm25_idx`, `kb_github_issue_comments_embedding_hnsw_idx`, `kb_github_prs_bm25_idx`, `kb_github_prs_embedding_hnsw_idx`, `kb_github_pr_comments_bm25_idx`, `kb_github_pr_comments_embedding_hnsw_idx`, `kb_project_items_bm25_idx`, and `kb_project_items_embedding_hnsw_idx`
  - row counts for the seven current searchable entity tables, driven by the task-204 search registry rather than a second hard-coded table list
  - sync-state summary from `agentic.kb_sync_state`, such as row counts by source and last attempted or succeeded timestamps, using the existing `task-405` schema rather than depending on `sync` CLI orchestration
- **No premature orchestration coupling**: status should not require `task-701` sync commands, live GitHub credentials, MCP readiness, or an already-populated database. An empty but correctly bootstrapped DB should still report as schema-ready with zero content rows.
- **Snapshot export contract**: implement `snapshot export` as a real database dump flow that targets the configured KB database and writes into `/workspace/agentic/snapshots` by default when no explicit path is provided. The preferred dump shape stays aligned with the platform plan: custom-format `pg_dump` with compression, scoped to the KB database contents rather than a full-cluster backup.
- **Snapshot restore contract**: implement restore semantics behind the existing `snapshot import` command. The command should accept an explicit dump path and may keep the positional optional only if implementation provides a deterministic default, such as the newest dump in `agentic/snapshots`; otherwise it should fail with a precise usage error instead of silently guessing. The required task-205 tests and workflow docs should exercise `snapshot import`, not an unimplemented alias.
- **Schema-scope safety**: export and restore should stay bounded to the KB database contract. The preferred plan is to dump and restore the `agentic` schema so snapshot operations remain isolated from any non-KB objects in the same Postgres instance.
- **Destructive import boundary**: `snapshot import` is explicitly destructive for the `agentic` schema inside the target database and must not be treated as an in-place merge. The plan assumes operator docs and live verification use only fresh, isolated, or otherwise disposable KB databases for import proofs. If implementation performs a drop-and-recreate or clean restore of `agentic`, it should require an explicit destructive-action acknowledgement rather than silently overwriting existing KB state.
- **Manifest boundary**: do not define or enforce a canonical manifest JSON file in this task. `task-601` owns that schema contract. Task-205 may print human-readable export metadata or preserve whatever data already exists inside `agentic.kb_snapshot_manifest`, but it should not invent a new durable manifest file format or schema validation contract.
- **Runtime dependencies**: add the minimal in-container runtime support needed for snapshot commands, most likely PostgreSQL client binaries via the `kb-tools` image. Keep the Python surface light and avoid introducing a second CLI package when the existing packaged `agentic-kb` entrypoint already owns these commands.
- **Implementation seams for tests**: structure status-query helpers and snapshot subprocess helpers so unit tests can verify SQL result shaping, path handling, and `pg_dump` or `pg_restore` invocation without always needing live binaries. Use focused DB-backed tests only for the parts that truly need a running ParadeDB instance.
- **Workflow alignment**: after implementation, update `.agent/workflows/agentic-kb.md` so it no longer says snapshot commands are placeholders and so it explains the current status of DB-aware `status`, real `snapshot export`, and destructive restore-via-`snapshot import` behavior, including the expectation that import verification runs only against disposable KB databases.

## Acceptance Criteria

- The packaged CLI at `agentic/src/agentic_kb/cli.py` continues to be the entrypoint for task-205 behavior.
- `agentic-kb status` reports real DB-aware status when the configured database is reachable, while `agentic-kb status --healthcheck` still behaves as an exit-code-friendly readiness check.
- DB status uses the current completed schema/search/sync-state work only: schema migrations `1`, `2`, and `3`; the current `agentic/schema/init.sql` tables; the seven searchable registry tables; their task-203 BM25/HNSW index presence; searchable row counts; and sync-state summaries.
- DB status does not require `sync` orchestration, MCP services, or live GitHub requests to succeed.
- `snapshot export` creates a real dump file for the KB database.
- `snapshot import` performs real restore behavior for a previously exported dump, replaces the task-103 placeholder message, and is the documented or tested restore verb for task-205.
- `snapshot import` is documented and tested as a destructive `agentic`-schema restore that is safe only against fresh, isolated, or disposable KB databases; task-205 does not promise merge semantics.
- Snapshot commands reuse the existing packaged CLI surface and default snapshot mount conventions under `agentic/snapshots` where appropriate.
- Task-205 does not define a new canonical snapshot manifest schema or team-sharing publication workflow.
- The `kb-tools` image contains the runtime dependencies needed for the real snapshot commands.
- Focused automated tests cover command behavior, and at least one live DB-backed verification path proves status plus snapshot export and restore behavior against ParadeDB.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` is reconciled only for `task-205` metadata, including the packaged CLI path, before the task is marked complete.

## Verification Plan

- Run `python3 -m py_compile` on the touched command modules and new tests.
- Run focused local unit coverage, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_*status*py'` and the matching snapshot-focused suite.
- Add deterministic unit tests that verify:
  - DB status degrades clearly when the database is unreachable or missing required schema objects
  - DB status reports expected readiness for migration versions `1`, `2`, and `3`, the current `init.sql` table set, and the task-203 searchable index names only
  - searchable row counts are shaped from the approved seven-entity registry set
  - sync-state summaries tolerate empty state and populated state
  - snapshot export builds the expected output path and subprocess invocation
  - snapshot import validates dump paths, destructive-action gating, and the expected restore invocation
- Run DB-backed verification against an isolated ParadeDB instance via `AGENTIC_TEST_DATABASE_URL`, reusing the current live-test pattern already used by search and sync-state tests. The test database must be disposable for task-205 runs and must not point at a developer's long-lived KB database.
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` after any Dockerfile change.
- Run the focused test suites inside `kb-tools` with `--entrypoint python` so in-container packaging is exercised, again pointing only at an isolated test database.
- Start required compose dependencies before any `--no-deps` command examples, and run smoke checks inside an isolated compose project or disposable DB volume, for example by bringing up `paradedb` first and then running:
  - `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools status`
  - `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot export`
  - `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot import <dump-path>` against a controlled test dump
- Verify export or restore round-trip behavior only against an isolated DB so the test can prove that a dumped KB can be restored and queried for expected tables or counts without mutating a long-lived local volume, and tear that compose project or volume down after verification.

## Risks / Open Questions

- **Manifest overlap with task-601**: task-205 must avoid inventing a durable manifest-file contract just to make export useful. The likely safe boundary is dump-only behavior plus human-readable summaries.
- **Status verbosity**: default status should be meaningfully richer than task-103 without becoming a full diagnostics subsystem. Implementation should keep the output concise enough for CLI use while still exposing the state task-503 will build on.
- **PostgreSQL client availability**: real snapshot commands probably require `pg_dump` and `pg_restore` in the `kb-tools` image. Implementation should confirm the smallest package change that works cleanly in both local and container verification.
- **Import path defaults**: if selecting the newest dump is not deterministic enough across local and in-container workflows, task-205 should require an explicit dump path rather than guessing.

## Required Docs / Tracking / Research Updates

- Update this canonical plan doc during implementation with final planning status, build status, implementation notes, verification notes, and outcome.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only to fix the stale `task-205.targetPath` and later mark `task-205` completed; note any unrelated tracker drift separately instead of broad cleanup in this task.
- Add `.agent/plans/agentic/research/task-205-db-status-and-snapshot-commands.md` with durable findings about the accepted DB status contract, snapshot command contract, runtime dependencies, and verification caveats.
- Update `.agent/workflows/agentic-kb.md` to reflect that DB-aware `status` plus real snapshot export and destructive restore-via-import behavior now exist, if implementation lands as planned.

## Implementation Notes

- Implemented real DB-aware `status` in `agentic/src/agentic_kb/commands/status.py` while preserving task-103 healthcheck behavior. Normal status now inspects migration versions `1`/`2`/`3`, the current `agentic` table set, searchable registry tables, task-203 BM25/HNSW indexes, per-table row counts, and grouped `kb_sync_state` summaries.
- Replaced the task-103 snapshot placeholders in `agentic/src/agentic_kb/commands/snapshot.py` with real `pg_dump` export and destructive `snapshot import` restore behavior scoped to the `agentic` schema. `snapshot import` now requires `--yes` acknowledgement before it drops and recreates the schema.
- Tightened `snapshot import` to stay bounded to the `agentic` schema even for a non-conforming custom dump by generating a filtered `pg_restore --list` file and restoring only `agentic` TOC entries; this replaced the earlier `pg_restore --schema=agentic` attempt after live verification showed schema creation and extension-related entries from a full dump could still leak outside the approved scope.
- Preserved the packaged CLI surface in `agentic/src/agentic_kb/cli.py`; no new entrypoint or `restore` alias was introduced.
- Added focused unit and DB-backed tests in `agentic/tests/test_status_command.py`, `agentic/tests/test_snapshot_command.py`, `agentic/tests/test_status_command_db.py`, and `agentic/tests/test_snapshot_command_db.py`.
- Updated `agentic/Dockerfile` to install PostgreSQL 18 client binaries from PGDG after verification showed Debian's default PostgreSQL 15 clients fail against ParadeDB 18 with a `pg_dump` server-version mismatch.
- Reconciled task tracking only for `task-205` in `.agent/plans/agentic/knowledge-base-platform-tasks.json` by updating the packaged CLI `targetPath`, keeping the task `in_progress` while implementation review was still open, and finalizing the task as `completed` during orchestrator signoff after the review loop approved.

## Verification Notes

- Passed `python3 -m py_compile "agentic/src/agentic_kb/cli.py" "agentic/src/agentic_kb/config.py" "agentic/src/agentic_kb/commands/status.py" "agentic/src/agentic_kb/commands/snapshot.py" "agentic/tests/test_status_command.py" "agentic/tests/test_snapshot_command.py" "agentic/tests/test_status_command_db.py" "agentic/tests/test_snapshot_command_db.py"`.
- Passed local unit suites with `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_status_command.py'` and `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'`.
- Passed the updated local snapshot unit suite again with `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_snapshot_command.py'`, including restore-list filtering coverage for non-conforming dumps.
- Local DB-backed suites were attempted with `AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@127.0.0.1:5759/agentic_kb`, but they skipped because the host Python environment did not expose `psycopg`; live DB verification was completed inside `kb-tools` instead.
- Passed `docker compose -f docker-compose.agentic.yml build kb-tools` after the Dockerfile update and confirmed in-container client versions with `psql --version`, `pg_dump --version`, and `pg_restore --version` reporting PostgreSQL `18.3`.
- Passed in-container unit suites with `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python kb-tools -m unittest discover -s agentic/tests -p 'test_status_command.py'` and the matching `test_snapshot_command.py` run.
- Passed in-container DB-backed suites with `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_status_command_db.py'` and the matching `test_snapshot_command_db.py` run against an isolated `task205-verify` ParadeDB volume.
- Passed smoke verification after starting `paradedb` in the isolated `task205-verify` compose project: `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools status`, `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot export /workspace/agentic/snapshots/task205-smoke.dump`, and `docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps kb-tools snapshot import /workspace/agentic/snapshots/task205-smoke.dump --yes`, followed by post-import inspection confirming `agentic.kb_documents` row count `1` and applied versions `(1, 2, 3)`.
- Passed a focused in-container DB-backed snapshot suite after the review fix with `AGENTIC_DB_PORT=5760 docker compose -p task205-verify -f docker-compose.agentic.yml up -d paradedb`, `AGENTIC_DB_PORT=5760 docker compose -p task205-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_snapshot_command_db.py'`, and `AGENTIC_DB_PORT=5760 docker compose -p task205-verify -f docker-compose.agentic.yml down -v`; this exercised import from a non-conforming full-database custom dump and confirmed only `agentic` objects were restored.
- Removed the isolated `task205-verify` compose project and volumes with `docker compose -p task205-verify -f docker-compose.agentic.yml down -v` after verification.

## Outcome

- `task-205` is complete: the packaged CLI now provides DB-aware `status`, real snapshot export, and destructive restore-via-`snapshot import` with schema-bounded restore behavior even for non-conforming custom dumps.
- Acceptance criteria are satisfied for the packaged CLI surface, DB-aware `status`, real snapshot export, destructive restore via `snapshot import`, schema-bounded restore behavior even for a non-conforming dump, Docker runtime dependencies, focused automated coverage, workflow updates, research capture, and task-205-only tracker cleanup.
- No canonical snapshot manifest schema was introduced, no `restore` alias was added, and tracker cleanup stayed limited to task-205 metadata as planned.

## Review Outcome

- The implementation review loop is clean: iteration 2 of `.agent/plans/agentic/task-plans/task-205-impl-review.md` ended with `Decision: approved` after tightening restore scoping to filtered `agentic` TOC entries and correcting premature tracker completion.

## Planning Status Rationale

- Planning status is `approved` because iteration 2 of `.agent/plans/agentic/task-plans/task-205-plan-review.md` ended with `Decision: approved`.
- Build status is `completed` because the implementation, verification, workflow/tracking updates, research note, and implementation review loop all finished with `Decision: approved`.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-205-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-205-impl-review.md`
