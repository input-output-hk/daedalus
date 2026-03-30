# Task 711 Implementation Review Log

## Implementation Entry 1

**Timestamp: 2026-03-30T21:30:00Z**

### Changes Made

**Files Modified:**
1. `agentic/src/agentic_kb/ingest/project.py`
2. `agentic/src/agentic_kb/commands/sync.py`
3. `agentic/tests/test_sync_command.py`

### Details

#### 1. `agentic/src/agentic_kb/ingest/project.py`

**`ProjectIngestResult` dataclass (line 390-402):**
- Added `seen_node_ids: frozenset[str] = frozenset()` field to collect node IDs seen during ingest

**`ingest_project_items()` function (lines 666-706):**
- Added `seen_node_ids: set[str] = set()` local variable
- In the page iteration loop, collects `project_item_node_id` from each item
- Returns `seen_node_ids=frozenset(seen_node_ids)` in the `ProjectIngestResult`

**`PostgresProjectItemsStore` class (lines 1318-1335):**
- Added `delete_archived_items(self) -> int` method:
  - SQL: `DELETE FROM agentic.kb_project_items WHERE metadata->>'is_archived' = 'true'`
  - Returns `cursor.rowcount`
- Added `delete_missing_items(self, valid_node_ids: frozenset[str]) -> int` method:
  - SQL: `DELETE FROM agentic.kb_project_items WHERE NOT (project_item_node_id = ANY(%s))`
  - Returns `cursor.rowcount`

**`InMemoryProjectItemsStore` class (lines 1355-1367):**
- Added `delete_archived_items(self) -> int` method:
  - Filters `rows_by_key` where `metadata.get("is_archived")` is truthy
  - Returns count of deleted items
- Added `delete_missing_items(self, valid_node_ids: frozenset[str]) -> int` method:
  - Filters keys not in `valid_node_ids`
  - Returns count of deleted items

#### 2. `agentic/src/agentic_kb/commands/sync.py`

**`sync_project()` function (lines 482-496):**
- After successful `ingest_project_items()`, calls:
  - `project_store.delete_archived_items()` (always, for all modes)
  - `project_store.delete_missing_items(result.seen_node_ids)` (only in full mode when `seen_node_ids` is non-empty)

**`_sync_project_changed()` function (lines 1212-1223):**
- After successful `ingest_project_items()`, calls `project_store.delete_archived_items()`

#### 3. `agentic/tests/test_sync_command.py`

Added 5 new test methods:

- `test_in_memory_project_items_store_delete_archived_items()`: Verifies archived items are deleted
- `test_in_memory_project_items_store_delete_missing_items()`: Verifies non-seen items are deleted
- `test_in_memory_project_items_store_delete_missing_items_empty_set()`: Verifies empty valid set deletes all items
- `test_sync_project_full_refresh_calls_delete_archived_and_delete_missing()`: Verifies full refresh calls both deletion methods
- `test_sync_project_incremental_calls_delete_archived_items()`: Verifies incremental sync calls delete_archived_items

### Implementation Notes

1. **Deletion ordering**: `delete_archived_items()` is called before `delete_missing_items()` because archived items may or may not be in the seen set.

2. **Empty set behavior**: When `valid_node_ids` is an empty frozenset, `delete_missing_items()` deletes ALL items (since no items are valid). This is correct behavior for the "collect then delete non-seen" approach.

3. **First sync guard**: `delete_missing_items()` is only called when `seen_node_ids` is non-empty (checked via `if mode == "full" and result.seen_node_ids`). On a first sync where GitHub returns 0 items, `seen_node_ids` would be empty and the method would not be called.

4. **In-memory store implementation**: The `InMemoryProjectItemsStore.delete_archived_items()` uses `v.get("metadata", {}).get("is_archived")` which matches the boolean `False`/`True` values stored in metadata.

### Verification

- All modified Python files pass `python3 -m py_compile`
- Unit tests pass: `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command`
- Project ingest tests pass: `PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_project_ingest`

---

## Code Review: Iteration 1

**Timestamp: 2026-03-30T23:15:00Z**

### Summary

Implementation matches the approved plan in all respects. All acceptance criteria from the canonical task plan are satisfied. Code is well-structured, maintainable, and follows existing patterns. No deviations from the approved plan were found.

### Plan Conformance

| Plan Item | Status |
|-----------|--------|
| `seen_node_ids: frozenset[str]` added to `ProjectIngestResult` | ✓ (line 402) |
| `seen_node_ids` populated during `ingest_project_items()` | ✓ (lines 673, 691-692, 705) |
| `delete_archived_items()` added to `PostgresProjectItemsStore` | ✓ (lines 1318-1324) |
| `delete_missing_items()` added to `PostgresProjectItemsStore` | ✓ (lines 1326-1333) |
| `delete_archived_items()` added to `InMemoryProjectItemsStore` | ✓ (lines 1353-1360) |
| `delete_missing_items()` added to `InMemoryProjectItemsStore` | ✓ (lines 1362-1367) |
| `sync_project()` calls `delete_archived_items()` (all modes) | ✓ (line 493) |
| `sync_project()` calls `delete_missing_items()` (full mode, non-empty seen_node_ids) | ✓ (lines 494-495) |
| `_sync_project_changed()` calls `delete_archived_items()` | ✓ (line 1223) |

### SQL Correctness

- `delete_archived_items()`: `DELETE FROM agentic.kb_project_items WHERE metadata->>'is_archived' = 'true'` — Correctly matches the string `'true'` stored by `metadata["is_archived"] = bool(...)` at project.py:841.
- `delete_missing_items()`: `DELETE FROM agentic.kb_project_items WHERE NOT (project_item_node_id = ANY(%s))` — Correctly deletes items NOT in the provided set. The `list(valid_node_ids)` conversion is safe for psycopg array parameter handling.

### Deletion Ordering

`sync_project()` (lines 493-495) calls `delete_archived_items()` first, then `delete_missing_items()`. This is correct: archived items should be removed regardless of whether they were "seen" during ingest, then `delete_missing_items()` removes truly-deleted items not in the seen set.

### Edge Case Handling

- **Empty `seen_node_ids` guard**: `if mode == "full" and result.seen_node_ids:` (line 494) correctly prevents calling `delete_missing_items()` when `seen_node_ids` is empty (first sync of empty project).
- **Empty `valid_node_ids` in `delete_missing_items()`**: Correctly deletes all items when passed an empty set (documented behavior for "collect then delete non-seen" approach). The guard in `sync_project()` prevents this from occurring on first sync.
- **`_sync_project_changed()` limitation**: Correctly limits to `delete_archived_items()` only, since cursor continuation cannot detect truly-deleted items. Only full refresh handles truly-deleted items.

### Test Coverage

5 new tests added:
1. `test_in_memory_project_items_store_delete_archived_items` — Correctly verifies archived item deletion
2. `test_in_memory_project_items_store_delete_missing_items` — Correctly verifies non-seen item deletion
3. `test_in_memory_project_items_store_delete_missing_items_empty_set` — Correctly verifies empty set deletes all items
4. `test_sync_project_full_refresh_calls_delete_archived_and_delete_missing` — Correctly verifies both methods called in correct order for full refresh with proper state (node-1 and node-3 kept, node-2 deleted as archived despite being "seen")
5. `test_sync_project_incremental_calls_delete_archived_items` — Correctly verifies only `delete_archived_items()` called in incremental mode

### Test Results

```
Ran 39 tests in 0.736s
OK (skipped=1)
```

All 39 tests pass. The 1 skip is unrelated to task-711 (likely due to optional tree-sitter dependencies).

### Issues Found

None.

### Reviewer Confidence

High. Implementation is straightforward, follows established patterns, and all plan items are satisfied.

### Decision: approved
