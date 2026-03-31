Planner: Iteration 1
Timestamp: 2026-03-31T00:00:00Z
Outcome: initial_plan_documented

Created canonical task plan for task-713 at `.agent/plans/agentic/task-plans/task-713.md`.

Summary:
- Target file: `agentic/tests/test_removal_reconciliation.py` (NEW)
- 4 test classes: DocsRemovalReconciliationTests, CodeRemovalReconciliationTests, ProjectRemovalReconciliationTests, ProjectRemovalReconciliationDbTests
- 14 total tests covering docs removal, code removal, project full/incremental removal, and cleared field preservation
- Follows existing patterns: unittest.TestCase, FakeEmbeddingClient from test_docs_ingest, InMemory*Store classes, patch-based mocking
- DB-backed tests use AGENTIC_TEST_DATABASE_URL with skipUnless decorator matching test_sync_command_db.py
- No existing source files modified; pure test addition
- Estimated 3 hours as per tasks.json

Key design decisions:
1. Tests verify store state (rows_by_key contents or DB query results), not just mock call counts — proves actual convergence
2. Separates in-memory unit tests from DB-backed integration tests, matching existing suite structure
3. Includes empty-set guard test to prevent regression on the critical safety check from task-711
4. Includes cleared field value preservation test to verify ON CONFLICT DO UPDATE doesn't resurrect stale metadata
5. Avoids duplicating existing removal tests in test_sync_command.py; focuses on end-to-end convergence scenarios

Critiquer: Iteration 1
Timestamp: 2026-03-31T00:05:00Z
Outcome: requires_changes

<findings>

1. **Significant test overlap with task-711 tests**: The plan proposes 5 in-memory project tests, but task-711 already added 5 tests in test_sync_command.py that cover nearly identical scenarios:
   - `test_sync_project_full_refresh_calls_delete_archived_and_delete_missing` (line 1165) already tests full refresh removing archived + deleted items
   - `test_sync_project_incremental_calls_delete_archived_items` (line 1290) already tests incremental removing archived items
   - `test_in_memory_project_items_store_delete_missing_items_empty_set` (line 1131) already tests the empty-set guard
   
   The proposed tests would be near-duplicates. The plan needs to either: (a) focus on genuinely different scenarios (e.g., multi-item convergence, edge cases), or (b) consolidate and reduce test count to avoid redundancy.

2. **Docs tests overlap with existing coverage**: `test_sync_docs_changed_updates_changed_paths_and_deletes_removed_rows` (test_sync_command.py:114) and `test_sync_docs_explicit_replaces_shrinking_chunked_doc_and_keeps_unique_path_metadata` (test_sync_command.py:299) already cover docs deletion scenarios. The proposed `test_sync_docs_explicit_removes_deleted_file_rows` would be redundant.

3. **Code tests lack specificity**: The plan doesn't account for how code removal actually works — `delete_missing_paths` removes chunks for paths not in the supported set. The proposed tests need to reflect the actual mechanism (supported path filtering, not just "deleted files").

4. **Missing scenario: combined add+delete convergence**: No test covers the case where files are both added and deleted in the same sync run — this is the most common real-world scenario and the one most likely to regress.

5. **DB-backed test helper reuse unclear**: The plan doesn't specify how DB-backed tests will reuse `_bootstrap_database` and other helpers from `test_sync_command_db.py`. Will they import them? Duplicate them? This affects implementation complexity.

6. **Test count vs estimate**: 14 tests in 3 hours is aggressive given the verbosity of `PreparedProjectItem` construction. Consider reducing to 8-10 high-value tests or extracting fixture helpers.

7. **Missing: verify search-layer non-appearance**: The task description says tests should prove items "stop surfacing after the relevant sync or explicit full-refresh path runs." The plan only tests store-level removal. Should also verify that removed items don't appear in any query/search results (though this may require MCP/search-layer access, which could be out of scope).

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-31T00:15:00Z
Outcome: revised_plan_addressing_critique

Revised canonical plan to address all 7 critique findings:

1. **Eliminated test overlap with task-711**: Removed all 5 proposed project tests that duplicated existing task-711 coverage. Replaced with 3 genuinely different scenarios: multi-item convergence (combined archived+deleted), cleared field preservation, and incremental no-op path.

2. **Eliminated docs test overlap**: Removed `test_sync_docs_explicit_removes_deleted_file_rows` (duplicate of test_sync_command.py:114). Replaced with `test_sync_docs_combined_add_and_delete_converges` — a scenario not covered by existing tests.

3. **Fixed code test specificity**: Changed code tests to reflect the actual removal mechanism — `delete_missing_paths` removes chunks for unsupported paths, not just deleted files. Added `test_sync_code_supported_path_filtering_removes_stale_chunks` to test this.

4. **Added combined add+delete scenario**: Both docs and code test classes now include a combined add+delete convergence test, which is the most common real-world scenario.

5. **Clarified DB helper reuse**: Plan specifies importing `_bootstrap_database` from `test_sync_command_db.py` with fallback to duplication if circular dependency arises.

6. **Reduced test count**: From 14 to 9 tests (3 docs/code + 3 project in-memory + 3 DB-backed), fitting comfortably within the 3-hour estimate.

7. **Scoped to store-level verification**: Confirmed non-goals exclude MCP/search-layer testing. Store-level verification (rows_by_key or DB queries) is sufficient for proving removal convergence.

Critiquer: Iteration 2
Timestamp: 2026-03-31T00:20:00Z
Outcome: approved

<findings>

1. **Test overlap resolved**: All 9 proposed tests are distinct from task-711's 5 existing tests. Each tests a unique convergence scenario not already covered.

2. **Scope is tight**: Non-goals explicitly list the 7 existing task-711 tests that will NOT be duplicated. Plan focuses on combined add+delete, multi-item convergence, cleared field preservation, and supported-path filtering — all genuine gaps.

3. **Patterns match existing suite**: FakeEmbeddingClient import source, InMemory*Store usage, patch-based mocking, DB-backed test decorator, and _bootstrap_database reuse all align with test_sync_command.py and test_sync_command_db.py conventions.

4. **Test count is reasonable**: 9 tests in 3 hours is achievable, especially with the proposed `_make_project_item()` helper to reduce verbosity.

5. **DB helper import path is correct**: `_bootstrap_database` is a module-level function in test_sync_command_db.py and can be imported directly. The fallback to duplication is a sensible safety net.

6. **Acceptance criteria are measurable**: Each criterion maps to a specific test or verifiable outcome. No ambiguous requirements.

7. **No source file modifications**: Pure test addition as required. Risk of regressions is minimal.

Decision: approved
