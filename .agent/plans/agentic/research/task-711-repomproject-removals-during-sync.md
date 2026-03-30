# Research Note: Repo and Project Removals During Sync

**Task:** task-711  
**Date:** 2026-03-30  
**Status:** Implemented and Approved  

## Problem Statement

The sync commands for docs and code already handled deletions correctly via `delete_documents_for_paths` and `delete_missing_paths`. However, project sync had no mechanism to remove KB records when:

1. Project items were archived in GitHub
2. Project items were deleted from GitHub (not just archived)
3. The only available sync signal was cursor-based pagination

## Key Design Decisions

### 1. Two-Phase Deletion Strategy

Project removal handling uses a two-phase approach:

- **Phase 1:** `delete_archived_items()` — Removes all items flagged as archived during ingest
- **Phase 2:** `delete_missing_items(node_ids)` — Removes all items NOT in the provided set

This ordering is important because archived items may or may not be in the seen set, so they are handled separately first.

### 2. `seen_node_ids` Collection via `ProjectIngestResult`

During full-refresh ingest, node IDs are collected into a `frozenset[str]` as pages are iterated:

```python
@dataclass
class ProjectIngestResult:
    upserted_count: int
    archived_count: int
    seen_node_ids: frozenset[str] = frozenset()  # NEW: collects IDs seen during ingest
```

A `frozenset` is used because:
- It is immutable and hashable (safe for frozen dataclass)
- Set membership checks are O(1)
- Memory overhead is similar to a regular set

### 3. Empty-Set Guard

`delete_missing_items()` is only called when `seen_node_ids` is non-empty:

```python
if mode == "full" and result.seen_node_ids:
    project_store.delete_missing_items(result.seen_node_ids)
```

This prevents accidental deletion on first sync (empty KB scenario) where no items are "seen" during ingest.

### 4. Incremental vs Full Sync Handling

| Sync Mode | `delete_archived_items()` | `delete_missing_items()` |
|-----------|--------------------------|--------------------------|
| Full (`sync project --full`) | Always | Only when `seen_node_ids` non-empty |
| Incremental (`sync project`) | Always | Never (cursor limitation) |

The incremental path cannot detect truly-deleted items (only archived) because cursor-based pagination only returns items that still exist in GitHub.

## SQL Patterns

### Deleting Archived Items

```sql
DELETE FROM agentic.kb_project_items WHERE metadata->>'is_archived' = 'true'
```

The `is_archived` field is stored as a boolean converted to string `'true'` or `'false'` during item preparation.

### Deleting Missing Items

```sql
DELETE FROM agentic.kb_project_items WHERE NOT (project_item_node_id = ANY(%s))
```

Uses PostgreSQL's `ANY()` array containment for efficient set membership check.

## Edge Cases

1. **Empty GitHub Project:** First sync of an empty project calls neither deletion method (guard prevents `delete_missing_items([])` call).

2. **All Items Archived:** `delete_archived_items()` removes everything; `delete_missing_items()` is not called since `seen_node_ids` is empty.

3. **Empty `valid_node_ids` passed to `delete_missing_items()`:** Would delete ALL items. This is correct documented behavior for the "collect then delete non-seen" approach, but the sync command guard prevents this from occurring.

## Limitations

- **Incremental sync cannot detect truly-deleted items:** Only archived items can be removed in incremental mode. Full refresh is required to reconcile items deleted from GitHub.
- **GitHub history is append-only:** The PRD specifies that GitHub issues and PRs are not subject to removal reconciliation (v1 contract).

## Files Modified

| File | Changes |
|------|---------|
| `agentic/src/agentic_kb/ingest/project.py` | Added `seen_node_ids` to `ProjectIngestResult`, added `delete_archived_items()` and `delete_missing_items()` to both store implementations |
| `agentic/src/agentic_kb/commands/sync.py` | Modified `sync_project()` and `_sync_project_changed()` to call deletion methods |
| `agentic/tests/test_sync_command.py` | Added 5 unit tests covering deletion method behavior |

## Verification

- All 39 unit tests pass (`PYTHONPATH="agentic/src" python3 -m unittest agentic.tests.test_sync_command`)
- `python3 -m py_compile` passes on all modified files
- Code review approved without issues
