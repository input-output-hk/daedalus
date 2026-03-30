# Research: task-704 Incremental Update Smoke Checks

## Task Summary
Created `agentic/tests/test_incremental_sync.py` with 14 in-memory smoke tests for incremental sync functionality.

## Key Findings

### Test Patterns Established

1. **In-memory smoke tests**: Use `InMemorySyncStateStore`, `InMemoryDocsStore`, `InMemoryGithubStore` - no database required
2. **Proper subset assertions**: Core verification is `len(updated_paths) < total_entity_count` proving incremental vs full rebuild
3. **Seeded baseline pattern**: `_seed_all_required_baselines()` from `test_sync_command.py` reused to simulate imported-baseline workflow
4. **Two-tier seeding helpers**:
   - `_seed_all_required_baselines()` - full seeding with validation
   - `_seed_incomplete_baselines()` - seeds states without validation for failure-path tests
5. **Failure-path tests**: Use `_seed_incomplete_baselines()` then call `load_required_incremental_baselines()` separately to test exception

### Gotchas and Insights

1. **Manual PreparedDocument construction**: When seeding docs store for deletion/rename tests, avoid `prepare_documents()` because it reads from filesystem which fails when files are deleted/renamed by git. Construct `PreparedDocument` objects manually.

2. **GitHub mocking**: Use existing fake GitHub result patterns from `test_sync_command.py` to avoid complex API mocking.

3. **Code ingest optional dependency**: Code tests must skip when tree-sitter unavailable:
   ```python
   @unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
   ```

4. **Factory-patching for full-command tests**: `sync_changed` requires factory patching (`PostgresSyncStateStore.from_database_url()`) per the pattern at `test_sync_command.py:478`.

5. **Proper subset for code**: Code tests use `processed_file_count` as the subset proxy rather than `len(updated_paths)`. Both satisfy the proper-subset requirement.

## Test Coverage

- `test_docs_incremental_updates_only_changed_paths_not_all_docs` - proper subset behavior
- `test_docs_incremental_reports_deleted_paths` - deletion reporting
- `test_docs_incremental_treats_rename_as_delete_plus_add` - rename handling
- `test_code_incremental_*` - same for code (skipped without tree-sitter)
- `test_github_incremental_uses_minimum_watermark_as_updated_since` - GitHub bounds derivation
- 4 failure-path tests for missing baselines (docs/code/project/github_watermark)
- 4 snapshot compatibility tests (legacy manifest, incompatible model, compatible manifest, no manifest)

## Verification
```
PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_incremental_sync -v
```
Result: 14 tests, 12 pass, 2 skipped (tree-sitter), 0 failures. Runtime: ~2 seconds.

## References
- Canonical task plan: `.agent/plans/agentic/task-plans/task-704.md`
- Planning review: `.agent/plans/agentic/task-plans/task-704-plan-review.md`
- Implementation review: `.agent/plans/agentic/task-plans/task-704-impl-review.md`
