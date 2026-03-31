# Task Plan: task-713 Add removal reconciliation regression tests

- Task ID: `task-713`
- Title: `Add removal reconciliation regression tests`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

`task-711` implemented removal handling for repo and project sync — adding `delete_archived_items()` and `delete_missing_items()` to project sync, and confirming docs/code sync already handled deletions correctly. The tests added in task-711 cover the mechanics of deletion methods and basic command-level invocation, but do not prove end-to-end convergence: that after a sync run against an already-seeded KB, deleted docs/code files and removed/cleared project metadata actually stop surfacing in the KB.

This task closes that gap by adding regression tests that verify removal convergence across all three sync scopes (docs, code, project) using both in-memory and DB-backed test patterns, matching the existing test suite conventions.

## Scope

- Add regression tests proving that after `sync docs` (explicit), deleted files no longer exist as KB records (combined add+delete scenario)
- Add regression tests proving that after `sync code` (explicit), unsupported paths no longer exist as KB records (supported path filtering)
- Add regression tests proving that after `sync project --full`, archived items and items not seen during the full pass are removed from the KB (multi-item convergence)
- Add regression tests proving that cleared project field values (null) are preserved through sync and do not resurrect stale values
- Tests verify non-appearance in the store (in-memory `rows_by_key` or DB queries), not just method call counts
- Follow existing test patterns: `unittest.TestCase`, `FakeEmbeddingClient` from `test_docs_ingest`, proper `patch` usage

## Non-Goals

- Do not re-test scenarios already covered by task-711 tests in `test_sync_command.py`:
  - `test_sync_project_full_refresh_calls_delete_archived_and_delete_missing` (line 1165)
  - `test_sync_project_incremental_calls_delete_archived_items` (line 1290)
  - `test_in_memory_project_items_store_delete_missing_items_empty_set` (line 1131)
  - `test_in_memory_project_items_store_delete_archived_items` (line 1007)
  - `test_in_memory_project_items_store_delete_missing_items` (line 1069)
  - `test_sync_docs_changed_updates_changed_paths_and_deletes_removed_rows` (line 114)
  - `test_sync_docs_explicit_replaces_shrinking_chunked_doc_and_keeps_unique_path_metadata` (line 299)
- Do not add new sync features or change sync logic
- Do not test GitHub issue/PR removal (append-only per PRD v1 contract)
- Do not add scheduled automation or staleness detection; that is task-702
- Do not test MCP or search-layer behavior for removed records (store-level verification only)
- Do not modify existing sync code or store implementations

## Relevant Dependencies

- **Satisfied**: `task-711` — removal handling implemented in `sync.py`, `project.py`, `docs.py`, `code.py`
- **Satisfied**: `task-701` — sync command family exists and is testable
- **Satisfied**: `task-301` — docs ingestion with `delete_documents_for_paths` exists
- **Satisfied**: `task-402` — code ingestion with `delete_missing_paths` exists
- **Reference**: `task-712` — ingest fixture coverage patterns (idempotent re-ingest, content_hash equality)

## Files Expected To Change

- `agentic/tests/test_removal_reconciliation.py` — NEW file containing all regression tests

No existing source files will be modified. This is a pure test addition.

## Implementation Approach

### Test File Structure

The new test file `agentic/tests/test_removal_reconciliation.py` will contain three test classes:

#### 1. `DocsCodeRemovalReconciliationTests` (in-memory)

Tests that verify docs and code removal convergence using `InMemoryDocsStore` and `InMemoryCodeChunksStore`. These tests focus on scenarios NOT already covered by task-711:

- **`test_sync_docs_combined_add_and_delete_converges`**: Seed KB with docs for AGENTS.md, README.md, and CONTRIBUTING.md. Run `sync_docs` with only AGENTS.md and NEW.md discoverable. Assert README.md and CONTRIBUTING.md rows are deleted, AGENTS.md is updated, and NEW.md is added. This tests the most common real-world scenario (simultaneous adds and deletes) not covered by existing tests.
- **`test_sync_code_supported_path_filtering_removes_stale_chunks`**: Seed KB with code chunks for alpha.ts (TypeScript, supported) and pipeline.yml (YAML, unsupported after filtering). Run `sync_code` with both paths. Assert pipeline.yml chunks are removed by `delete_missing_paths` because it's not in the supported language set. This tests the actual code removal mechanism (supported path filtering), which differs from docs deletion.
- **`test_sync_code_combined_add_and_delete_converges`**: Seed KB with code chunks for alpha.ts and old.ts. Run `sync_code` with alpha.ts and new.ts as source paths. Assert old.ts chunks are removed, alpha.ts chunks remain, and new.ts chunks are added.

#### 2. `ProjectRemovalReconciliationTests` (in-memory)

Tests that verify project removal convergence using `InMemoryProjectItemsStore`. These tests focus on multi-item convergence and edge cases NOT already covered by task-711:

- **`test_sync_project_full_multi_item_convergence`**: Seed KB with 5 project items (3 live, 1 archived, 1 deleted/unseen). Run `sync_project --full` that sees only 3 live items. Assert all 5 items converge to exactly 3 (archived removed, unseen removed, live preserved). This tests the combined effect of both deletion methods in a single scenario, unlike task-711 which tests them separately.
- **`test_sync_project_full_preserves_cleared_field_values`**: Seed KB with a project item that has null status, priority, and size fields. Run `sync_project --full` that sees the same item. Assert null values are preserved through `ON CONFLICT DO UPDATE` and not overwritten with stale non-null values. This verifies the cleared-field preservation requirement from the task description.
- **`test_sync_project_incremental_noop_when_no_archived_items`**: Seed KB with 2 live items (none archived). Run incremental `sync_project`. Assert both items remain and `delete_archived_items()` returns 0. This tests the no-op path for incremental sync when there are no archived items to remove.

#### 3. `RemovalReconciliationDbTests` (DB-backed)

Tests that verify removal convergence against a real PostgreSQL database. Uses `@unittest.skipUnless` with `AGENTIC_TEST_DATABASE_URL` env var, following `test_sync_command_db.py` patterns. Imports `_bootstrap_database` helper from `test_sync_command_db.py` (or duplicates if import causes circular dependency):

- **`test_sync_project_full_removes_archived_items_from_db`**: Seed DB with project items including archived ones via direct INSERT. Run `sync_project --full` with mocked ingest that sees only live items. Query `kb_project_items` to assert archived rows are deleted.
- **`test_sync_project_full_removes_deleted_items_from_db`**: Seed DB with 3 project items. Run `sync_project --full` with mocked ingest that sees 2 items. Query DB to assert the unseen item's row is deleted.
- **`test_sync_project_preserves_cleared_field_values_in_db`**: Seed DB with a project item that has null field values in the metadata JSONB column. Run `sync_project --full`. Query DB to assert null values are preserved (metadata->>'status' IS NULL).

### Key Patterns to Follow

- Import `FakeEmbeddingClient` from `agentic.tests.test_docs_ingest` (not copy)
- Use `InMemoryDocsStore`, `InMemoryCodeChunksStore`, `InMemoryProjectItemsStore` from their respective ingest modules
- Use `InMemorySyncStateStore` for sync state
- Use `patch` for mocking external calls (ingest functions, embedding client factory, etc.)
- DB-backed tests use `AGENTIC_TEST_DATABASE_URL` env var with `@unittest.skipUnless` decorator
- DB-backed tests reuse `_bootstrap_database` from `test_sync_command_db.py` via `from agentic.tests.test_sync_command_db import _bootstrap_database`
- Verify removal by checking store state (`rows_by_key` contents or DB query results), not just mock call counts
- Extract `_make_project_item()` helper method to reduce verbosity of `PreparedProjectItem` construction (following the pattern used in task-711 tests)

### Test Execution

```bash
PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_removal_reconciliation
```

## Acceptance Criteria

- All new regression tests pass with `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_removal_reconciliation`
- Tests prove that deleted docs files stop surfacing as KB records after sync (combined add+delete scenario)
- Tests prove that unsupported code files stop surfacing as KB records after sync (supported path filtering)
- Tests prove that archived + deleted project items are removed after full sync (multi-item convergence)
- Tests prove that cleared project field values are preserved through sync (null not overwritten)
- Tests prove that incremental sync is a no-op when no archived items exist
- Tests follow existing test suite conventions (unittest.TestCase, proper mocking, FakeEmbeddingClient import)
- `python3 -m py_compile` passes on the new test file
- No existing source files are modified
- No test scenarios duplicate existing task-711 tests

## Verification Plan

1. Create `agentic/tests/test_removal_reconciliation.py` with all test classes
2. Run `python3 -m py_compile agentic/tests/test_removal_reconciliation.py`
3. Run `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_removal_reconciliation`
4. Verify all tests pass (in-memory tests should pass without DB; DB-backed tests will skip without `AGENTIC_TEST_DATABASE_URL`)
5. If DB is available, run DB-backed tests to verify PostgreSQL removal behavior
6. Verify no test overlap with existing task-711 tests by reviewing test names and assertions

## Risks / Open Questions

- **DB helper import**: `_bootstrap_database` in `test_sync_command_db.py` is a module-level function (not a class method). Import should work: `from agentic.tests.test_sync_command_db import _bootstrap_database`. If circular dependency issues arise, will duplicate the helper (it's only ~10 lines).
- **Test data verbosity**: `PreparedProjectItem` requires 20+ fields. Will extract `_make_project_item()` helper to reduce duplication, following the pattern in task-711 tests.
- **Code removal mechanism**: Code removal works via `delete_missing_paths` which removes chunks for paths not in the supported language set, not just deleted files. Tests must reflect this actual mechanism.
- **Scope discipline**: Must avoid re-testing scenarios already covered by task-711. Each test must have a distinct convergence scenario.

## Required Docs / Tracking / Research Updates

- This canonical task plan records the approved implementation notes for `task-713`
- Planning review history is preserved in `.agent/plans/agentic/task-plans/task-713-plan-review.md`
- Implementation review history will be preserved in `.agent/plans/agentic/task-plans/task-713-impl-review.md` (created during build phase)
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` will update task-713 status to `completed` after implementation

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-713-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-713-impl-review.md`

## Final Outcome

Implemented and code-review approved. Created `agentic/tests/test_removal_reconciliation.py` with 9 regression tests:

- **6 in-memory tests** across `DocsCodeRemovalReconciliationTests` and `ProjectRemovalReconciliationTests` classes, covering combined add+delete convergence for docs, supported-path filtering for code, multi-item project convergence, cleared field preservation, and incremental no-op paths.
- **3 DB-backed tests** in `RemovalReconciliationDbTests` class, verifying project removal and cleared-field behavior against real PostgreSQL (skipped without `AGENTIC_TEST_DATABASE_URL`).

All 6 in-memory tests pass; 3 DB-backed tests skip gracefully without the database URL. No existing source code was modified. Tests follow existing suite conventions (`unittest.TestCase`, `FakeEmbeddingClient`, `InMemory*Store` patterns, `@unittest.skipUnless` for DB tests).

**Review-log references:**
- Planning review: `.agent/plans/agentic/task-plans/task-713-plan-review.md`
- Implementation review: `.agent/plans/agentic/task-plans/task-713-impl-review.md`
