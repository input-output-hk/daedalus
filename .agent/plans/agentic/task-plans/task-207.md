# Task Plan: task-207 Add schema bootstrap and index creation regression check

- Task ID: `task-207`
- Title: `Add schema bootstrap and index creation regression check`
- Planning Status: `approved`
- Build Status: `completed`
- Interaction Mode: `autonomous`
- Required User Inputs: `None`
- Required Manual Test Steps: `None for task completion; this task is an automated regression check`
- Evidence Needed Back From User: `None`
- Implementation Can Proceed Before User Interaction: `Yes`

## Why This Task Was Chosen Now

- `task-207` is the remaining pending Phase 2 validation task after `task-201`, `task-203`, and `task-205` completed.
- The PRD explicitly promises an automated `schema bootstrap and index creation test`, and current DB-backed tests still bypass the fresh-bootstrap include path by sanitizing `init.sql` and executing `create_indexes.sql` separately.
- This is the smallest missing regression gate before later automation and rollout tasks build on the assumption that a fresh KB still boots the documented schema and index contract.

## Scope

- Add an automated regression test at `agentic/tests/test_schema_bootstrap.py` for the fresh-database schema bootstrap contract.
- Verify that a disposable database can initialize the `agentic` schema from `agentic/schema/init.sql`, including the delegated index creation phase from `agentic/schema/create_indexes.sql`.
- Assert the exact currently documented task-207 contract: migration rows `1`, `2`, and `3`; the expected `agentic` tables; the seven searchable tables; and the derived BM25/HNSW index names.
- Reuse the current DB-backed test pattern with `AGENTIC_TEST_DATABASE_URL`, `psycopg`, and PostgreSQL client tools rather than introducing a second test runner.

## Non-Goals

- Do not add a Compose boot smoke test; that remains `task-709`.
- Do not redesign schema SQL, index names, migration versioning, or the status command contract unless a narrow blocker is required to make the regression test meaningful.
- Do not broaden this task into snapshot, search-ranking, sync, MCP, or workflow validation beyond the schema/bootstrap contract.
- Do not treat manual application of `create_indexes.sql` as sufficient evidence for task-207; the goal is the documented fresh-bootstrap path.

## Relevant Dependencies

- Declared completed dependencies:
  - `task-201` - established `agentic/schema/init.sql`, the schema root entrypoint, and migration rows `1` and `2`.
  - `task-203` - established `agentic/schema/create_indexes.sql`, migration row `3`, and the BM25/HNSW naming contract.
  - `task-205` - established `status.inspect_database()` plus the concrete expected migration, table, and index registries that task-207 can reuse for assertions.
- Practical supporting context:
  - `docker-compose.agentic.yml` defines the shipped include-path layout by mounting `create_indexes.sql` to `/docker-entrypoint-initdb.d/includes/create_indexes.task-203.sql`.
  - Existing DB-backed tests in `agentic/tests/test_status_command_db.py` and `agentic/tests/test_snapshot_command_db.py` provide reusable gating patterns and show the current gap: they sanitize `init.sql` and therefore do not verify the include path.
- Downstream work made safer by this task:
  - Later automation and rollout tasks can rely on an executable proof that fresh KB bootstrap still creates the full schema and index surface.

## Current Build State

- `agentic/tests/test_schema_bootstrap.py` now exists and covers the fresh-bootstrap include path by executing `agentic/schema/init.sql` through `psql -f` from a temporary directory that mirrors the shipped mounted `includes/` layout.
- Existing DB-backed tests still use the sanitized helper path for their own focused concerns, but task-207 now adds the missing regression gate that proves the shipped bootstrap contract still works end to end.
- Task-207 now has an approved implementation review log and focused verification evidence recorded during implementation.

## Files Expected To Change

- `agentic/tests/test_schema_bootstrap.py` - new DB-backed regression suite for fresh bootstrap and index creation.
- `agentic/tests/test_status_command_db.py` or `agentic/tests/test_snapshot_command_db.py` - only if a tiny shared helper extraction materially reduces duplicate bootstrap assertions.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update only `task-207` metadata when implementation lands.
- `.agent/plans/agentic/research/task-207-schema-bootstrap-regression-check.md` - capture durable findings from implementation and authoritative verification.
- This plan doc - update planning status, build status, verification notes, and final outcome as work progresses.

## Implementation Approach

- Add a focused DB-backed unittest module named `agentic/tests/test_schema_bootstrap.py` guarded by `AGENTIC_TEST_DATABASE_URL`, `psycopg`, and `psql` availability.
- Build the test around the real shipped bootstrap shape instead of the current sanitized helper path:
  - create a temporary bootstrap directory that mirrors the Compose include layout
  - place `init.sql` there as the executed root script
  - place `create_indexes.sql` under `includes/create_indexes.task-203.sql` so the existing `\ir` line resolves unchanged
  - drop `agentic` schema in the disposable test database, then execute the root script through `psql -f ...`
- After bootstrap, inspect the database with `status.inspect_database()` and assert:
  - applied versions are exactly `(1, 2, 3)`
  - `status.EXPECTED_AGENTIC_TABLES` are present
  - `status.expected_searchable_tables()` are present
  - `status.expected_searchable_indexes()` are present
  - searchable row counts are zero on a fresh bootstrap
  - sync-state summaries are empty on a fresh bootstrap
- Keep the task minimal by preferring direct inspection over extra helper abstractions. Only extract a shared bootstrap helper if the new test would otherwise duplicate too much fragile setup.
- Keep the verification target disposable. The test should reset only the `agentic` schema in the database named by `AGENTIC_TEST_DATABASE_URL` and should not assume ownership of other schemas or long-lived developer state.

## Acceptance Criteria

- A new automated regression suite exists at `agentic/tests/test_schema_bootstrap.py`.
- The suite verifies the shipped fresh-bootstrap contract from `agentic/schema/init.sql`, including delegated index creation through the existing `\ir includes/create_indexes.task-203.sql` path.
- The regression check proves a fresh disposable KB database ends with migration versions `1`, `2`, and `3`.
- The regression check proves the expected `agentic` tables are present.
- The regression check proves the seven searchable tables and all expected BM25/HNSW indexes are present.
- The regression check proves a fresh bootstrap has zero searchable rows and no sync-state summaries.
- The task does not rely only on sanitizing `init.sql` and manually replaying `create_indexes.sql` as separate proof.
- Focused verification can run in the packaged environment against an isolated disposable ParadeDB target.

## Verification Plan

- Run `python3 -m py_compile` on `agentic/tests/test_schema_bootstrap.py` and any touched helper/test files.
- Run the focused suite locally when dependencies are available, for example `PYTHONPATH=agentic/src python3 -m unittest discover -s agentic/tests -p 'test_schema_bootstrap.py'`.
- Run the same suite in the packaged environment against an isolated disposable ParadeDB instance using `AGENTIC_TEST_DATABASE_URL`.
- Reuse `status.inspect_database()` in assertions so task-207 validates the same concrete migration, table, and index contract exposed to operators by `status`.
- If any helper extraction touches existing DB-backed tests, rerun the affected focused suites such as `test_status_command_db.py` and `test_snapshot_command_db.py`.

## Verification Notes

- `python3 -m py_compile agentic/tests/test_schema_bootstrap.py` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task207-verify -f docker-compose.agentic.yml build kb-tools` passed.
- `AGENTIC_DB_PORT=5761 docker compose -p task207-verify -f docker-compose.agentic.yml run --rm --no-deps --entrypoint python -e PYTHONPATH=/workspace/agentic/src -e AGENTIC_TEST_DATABASE_URL=postgresql://agentic:agentic@paradedb:5432/agentic_kb kb-tools -m unittest discover -s agentic/tests -p 'test_schema_bootstrap.py'` passed against an isolated ParadeDB volume after waiting for the known first-boot restart handoff to finish.
- No helper extraction was needed, so existing `test_status_command_db.py` and `test_snapshot_command_db.py` were not rerun for this task-scoped change.

## Risks / Open Questions

- The repo copy of `agentic/schema/init.sql` references `includes/create_indexes.task-203.sql`, but that include path exists only in the mounted/bootstrap layout, not beside the checked-in file. The regression suite must therefore recreate the mounted layout or it will falsely fail.
- Using `status.inspect_database()` for assertions keeps the contract centralized, but the test must still ensure the bootstrap path itself is real and not reintroduced through the old sanitized-helper shortcut.
- PostgreSQL client tooling availability may make `psql`-driven local execution environment-dependent, so authoritative verification should continue to use the packaged container path when needed.

## Required Docs / Tracking / Research Updates

- Update this plan doc with implementation notes, verification notes, and final outcome when task-207 lands.
- Append planner and critique entries to `.agent/plans/agentic/task-plans/task-207-plan-review.md` until planning is approved.
- Create `.agent/plans/agentic/task-plans/task-207-impl-review.md` only during implementation.
- Update `.agent/plans/agentic/research/task-207-schema-bootstrap-regression-check.md` with final durable findings or verification caveats discovered during implementation.
- Update `.agent/plans/agentic/knowledge-base-platform-tasks.json` only for `task-207` status and metadata when implementation lands.

## Final Outcome

- Completed and approved. The new `agentic/tests/test_schema_bootstrap.py` regression suite proves that a fresh disposable KB can bootstrap from `agentic/schema/init.sql` through the shipped relative `\ir includes/create_indexes.task-203.sql` path and ends with migration versions `1`, `2`, and `3`, the expected `agentic` tables, the expected searchable tables and BM25/HNSW indexes, zero searchable rows, and no sync-state summaries.

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-207-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-207-impl-review.md`
