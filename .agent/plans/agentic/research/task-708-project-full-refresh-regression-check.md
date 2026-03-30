# Research Brain: task-708 Project Full-Refresh Regression Check

## Key Implementation Decisions

### `side_effect` vs Fake Class Pattern
Chose to patch `ingest_project_items` directly with a `side_effect` function rather than creating a custom fake class. This matches the established pattern in `test_sync_command_db.py` and avoids introducing a new fake class that would need its own maintenance.

### `bounds.after_cursor` as the Decision Point
The `side_effect` function uses `bounds.after_cursor` to determine what page to return:
- `after_cursor=None` → returns ITEM_5 v2 (simulates initial/full refresh fetch from beginning)
- `after_cursor=stored_cursor` → returns empty page (simulates incremental fetch past ITEM_5)

This correctly reproduces the real GitHub Projects pagination behavior where cursors advance through pages.

### Direct psycopg Queries for Assertions
Using direct SQL queries against `agentic.kb_project_items` rather than relying on domain-layer query methods. This provides:
- Direct proof that the DB state matches expectations
- No coupling to query-layer APIs that might change
- Clear visibility into exactly what rows exist

### No Workspace/Git Initialization
Unlike `test_sync_command_db.py` which exercises the full CLI with workspace git state, this test only calls `sync_project()` directly. This simplifies the test and removes git-workspace dependencies that aren't relevant to the project full-refresh behavior.

## Durable Findings

1. **The `SyncCommandDbTests` base class** provides `_bootstrap_database()` which handles schema creation and provides DB isolation between tests via `setUp()`.

2. **`FakeEmbeddingClient`** from `test_sync_command_db.py` can be reused without modification.

3. **The `ProjectFetchBounds` object** is passed as `bounds` keyword arg to `ingest_project_items` and contains `after_cursor` which is the key pagination state.

4. **`project_item_node_id` uniqueness constraint** enables upsert-in-place behavior - rows for the same item are updated, not duplicated.

5. **`mode="full"`** is returned in the result dict from `sync_project(full_refresh=True)` confirming the full-refresh path is exercised.

## Constraints and Gotchas

1. **task-707 dependency**: The test requires `derive_project_cursor_for_explicit_sync()` to return `(None, "full")` when `full_refresh=True`. If task-707 implementation changes, this test will break.

2. **Cursor string comparison**: The `side_effect` function must compare `bounds.after_cursor` with the stored cursor using exact string equality. Any mismatch in cursor format will cause the test to fail silently.

3. **`body_text` column name**: The column name was confirmed from `PreparedProjectItem.body_text` in `project.py:339`. If the schema changes, this test will fail.

4. **DB cleanup**: Each test method relies on `_bootstrap_database()` in `setUp()` for isolation. If tests are run in different order or without proper setup, state can leak.

5. **No test for `--full` CLI flag parsing**: The test exercises `sync_project(full_refresh=True)` directly. Flag parsing is covered by `test_sync_command.py` (task-707).
