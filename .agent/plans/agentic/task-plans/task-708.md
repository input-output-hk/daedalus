# Task Plan: task-708 Add Project full-refresh replay regression check

- Task ID: `task-708`
- Title: `Add Project full-refresh replay regression check`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-707` delivered `sync project --full` which forces `after_cursor=None` to re-ingest all Project items.
- `task-708` is the regression check proving this actually works: that an edit to a previously-seen Project item becomes visible after running `sync project --full`.
- Without this regression check, there is no automated proof that the full-refresh path re-converges project state for already-seen items.
- This test also serves as a living contract: if someone inadvertently breaks the full-refresh behavior, the test will catch it.

## Scope

- Create a new DB-backed test file at `agentic/tests/test_project_full_refresh.py` using the `SyncCommandDbTests` pattern.
- The test proves end-to-end that an edit to a previously-seen Project item (identified by `project_item_node_id`) becomes visible in the KB after running `sync project --full`.
- The test sequence: seed baseline with item at v1 → simulate edit to v2 → run non-full sync → assert v1 is still visible → run full sync → assert v2 is visible.
- The test must use `FakeGithubProjectApiClient` to simulate paginated project items with controlled body content.

## Non-Goals

- Do not add new source code; this task is test-only.
- Do not modify the existing `test_sync_command_db.py` or `test_project_ingest.py` files.
- Do not test `sync changed` interaction with project items; task-708 is scoped to `sync project --full` only.
- Do not test the `--full` flag parsing; that is already covered by `test_sync_command.py` for task-707.

## Relevant Dependencies

- `task-707` - delivered `sync project --full` with:
  - `--full` flag on `project_parser` in `add_sync_subcommands()`
  - `sync_project()` accepts `full_refresh: bool = False` parameter
  - `derive_project_cursor_for_explicit_sync()` returns `(None, "full")` when `full_refresh=True`
  - `format_sync_source_output()` emits mode-specific notes: "full", "incremental", "initial"
  - Rows are updated in place through natural-key uniqueness constraint on `project_item_node_id`
- Primary references:
  - `agentic/src/agentic_kb/commands/sync.py` (task-707 implementation)
  - `agentic/tests/test_sync_command_db.py` (DB-backed sync test pattern)
  - `agentic/tests/test_project_ingest.py` (Project ingest test pattern with `FakeGithubProjectApiClient`)

## Files Expected To Change

- `agentic/tests/test_project_full_refresh.py` - new file: DB-backed regression check for project full-refresh replay

## Implementation Approach

- **Test class**: Use `SyncCommandDbTests` as the base class, decorated with `@unittest.skipUnless` for the same environment requirements as `test_sync_command_db.py`.
- **Patching pattern**: Follow the exact same pattern as `test_sync_command_db.py` — patch `agentic_kb.commands.sync.ingest_project_items` directly with a `side_effect` function (not a custom fake class). The `side_effect` function receives `**kwargs` including `bounds` (a `ProjectFetchBounds` object), reads `bounds.after_cursor`, and returns the appropriate `ProjectIngestResult`.
- **FakeEmbeddingClient**: Same `FakeEmbeddingClient` used in `test_sync_command_db.py` (defined at line 39 of that file).
- **Test sequence**:
  1. Bootstrap an empty KB via `_bootstrap_database()`.
  2. Call `sync_project()` with `ingest_project_items` patched to return ITEM_5 v1 (`body_text="first body"`) when called with `after_cursor=None`. Verify ITEM_5 is stored with `body_text="first body"` via direct psycopg query.
  3. After step 2, the `sync_state` table stores the `final_cursor` returned in the `ProjectIngestResult` (e.g., `"cursor-after-item-5"`).
  4. Call `sync_project()` again with `ingest_project_items` patched to return a page containing ITEM_5 v2 when called with `after_cursor=None`, but return an empty page (no ITEM_5) when called with `after_cursor` equal to the stored cursor from step 3. This simulates the real behavior where an incremental sync fetches from the stored cursor position, which is past ITEM_5.
  5. Verify ITEM_5 still has `body_text="first body"` — the non-full sync's patched `ingest_project_items` was called with `after_cursor=stored_cursor` (not `None`), so it returned no ITEM_5, leaving the existing row untouched.
  6. Call `sync_project(full_refresh=True)` — this passes `after_cursor=None` to `ingest_project_items`, so the patch returns ITEM_5 v2.
  7. Verify ITEM_5 now has `body_text="updated body"` — proving the edit became visible after full-refresh.
- **Assertions**: Direct psycopg queries against `agentic.kb_project_items` to verify the stored `body_text` values at each step. The `body_text` column name is confirmed from `PreparedProjectItem.body_text` in `project.py:339` and the `ON CONFLICT DO UPDATE SET body_text = EXCLUDED.body_text` clause in `upsert_project_items()` at `project.py:1263`.
- **No workspace/git needed**: Unlike `test_sync_command_db.py` tests that require a git workspace, this test only exercises `sync_project()` directly without a workspace, so no `_git_init`, `_git_commit_all`, or `_seed_imported_baseline` is needed.

## Acceptance Criteria

- `agentic/tests/test_project_full_refresh.py` exists and runs in the same environment as `test_sync_command_db.py`.
- The test uses `unittest.mock.patch` to patch `agentic_kb.commands.sync.ingest_project_items` directly with a `side_effect` function (no custom fake class), matching the established pattern in `test_sync_command_db.py`.
- The `side_effect` function distinguishes calls by `bounds.after_cursor`: returns ITEM_5 v2 when `after_cursor=None`, returns an empty page when `after_cursor` matches the stored incremental-sync cursor.
- The test proves that a non-full `sync_project()` call does NOT refresh an already-seen item (item retains its original `body_text`).
- The test proves that a `sync_project(full_refresh=True)` call DOES refresh an already-seen item (item `body_text` is updated to the new value).
- The test verifies `mode="full"` in the result dict from the full-refresh call.
- The test verifies via direct DB query that only one row exists for ITEM_5 (upsert-in-place behavior via `project_item_node_id` uniqueness constraint).
- All modified Python modules pass `python3 -m py_compile`.

## Verification Plan

- Run `python3 -m py_compile` on the new test file.
- Run the new test in isolation with `python -m pytest agentic/tests/test_project_full_refresh.py -v` (or the project's unittest runner).
- Verify the test passes in the full test suite after `task-707` implementation is present.
- Run lint/typecheck if the project has configured commands (check `package.json` or `Makefile` for lint/test commands).

## Risks / Open Questions

- **Dependency on task-707 implementation**: This test requires the task-707 implementation to be present and correct. If `derive_project_cursor_for_explicit_sync()` does not return `(None, "full")` when `full_refresh=True`, the test will fail.
- **Cursor-based pagination**: The `side_effect` function must correctly interpret `bounds.after_cursor` to determine what page to return. The test is only valid if the actual `sync_project()` passes the stored cursor as `after_cursor` during incremental syncs.
- **DB cleanup**: Each test method must clean up its KB state to avoid polluting other tests. The `SyncCommandDbTests.setUp()` calls `_bootstrap_database()` which recreates the schema, providing isolation.

## Required Docs / Tracking / Research Updates

- This canonical task plan is created at `.agent/plans/agentic/task-plans/task-708.md`.
- Planning review history lives at `.agent/plans/agentic/task-plans/task-708-plan-review.md`.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` will be updated to mark `task-708` when implementation lands.

## Implementation Notes

- Created `agentic/tests/test_project_full_refresh.py` using `SyncCommandDbTests` base class
- Used `@unittest.skipUnless` decorator matching `test_sync_command_db.py` environment requirements
- Patched `agentic_kb.commands.sync.ingest_project_items` directly with a `side_effect` function (no custom fake class)
- `side_effect` function distinguishes calls by `bounds.after_cursor`: returns ITEM_5 v2 when `after_cursor=None`, returns empty page when `after_cursor` matches stored incremental-sync cursor
- Direct psycopg queries against `agentic.kb_project_items` verify stored `body_text` values at each step
- Test sequence: seed v1 → incremental sync (no change) → full refresh (v2 visible)
- `FakeEmbeddingClient` reused from `test_sync_command_db.py`
- No workspace/git initialization needed; `sync_project()` called directly

## Outcome

- Test file `agentic/tests/test_project_full_refresh.py` created and passes
- Proves non-full `sync_project()` does NOT refresh already-seen items (item retains original `body_text`)
- Proves `sync_project(full_refresh=True)` DOES refresh already-seen items (`body_text` updated to new value)
- Confirms `mode="full"` in result dict from full-refresh call
- Confirms upsert-in-place behavior via `project_item_node_id` uniqueness constraint (single row for ITEM_5)
- All acceptance criteria met

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-708-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-708-impl-review.md`
