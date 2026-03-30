Planner: Iteration 1
Timestamp: 2026-03-30T19:00:00Z
Outcome: initial_plan

Drafted the initial canonical `task-708` plan for the Project full-refresh replay regression check.

Scope: Create `agentic/tests/test_project_full_refresh.py` using `SyncCommandDbTests` pattern. The test proves end-to-end that an edit to a previously-seen Project item becomes visible after `sync project --full`.

Key design decisions:
- New test file (not modifying existing test files per task description)
- Uses `FakeEmbeddingClient` and a custom `FakeProjectItemsFetcher` to simulate paginated project items
- Test sequence: seed v1 → non-full sync (verify v1 retained) → full-sync (verify v2 visible)
- Direct psycopg queries against `agentic.kb_project_items` to verify body_text at each step
- No workspace/git needed since we call `sync_project()` directly

Acceptance criteria cover:
- Non-full sync does NOT refresh already-seen items
- Full-sync DOES refresh already-seen items  
- `mode="full"` is present in the full-refresh result
- Only one row exists per `project_item_node_id` (upsert-in-place)

Identified risks:
- Dependency on task-707 implementation being correct
- Fake client must faithfully mimic `ingest_project_items()` signature
- `_bootstrap_database()` in setUp provides test isolation

Critiquer: Iteration 1
Timestamp: 2026-03-30T19:45:00Z
Outcome: gaps_found

**Gap 1: Fake client architecture is underspecified and leads to incorrect test sequence**

The plan describes a `FakeProjectItemsFetcher` that "implements `ingest_project_items()`-compatible signature, returning `ProjectIngestResult` with paginated items." But `ingest_project_items` is a function, not a class/interface. The real question is WHERE in the call chain this fake sits. Based on existing `test_sync_command_db.py` patterns, the correct approach is to patch `ingest_project_items` directly with a side-effect function that returns controlled `ProjectIngestResult` values. The plan's `FakeProjectItemsFetcher` terminology creates confusion about whether this is meant to replace `GithubProjectApiClient` (the lower-level paginated API client) or `ingest_project_items` (the higher-level function).

**Gap 2: Test sequence step 4 is logically inconsistent with cursor behavior**

Step 4 says: "Call `sync_project()` again with a fake client that returns ITEM_5 at v2 (body='updated body') — simulating an edit to an already-seen item."

After step 2, a sync_state record is stored with `cursor_text=cursor-1` (or whatever the fake's final_cursor is). When step 4 calls `sync_project(full_refresh=False)`, `derive_project_cursor_for_explicit_sync()` returns `(stored_cursor, "incremental")`, which is passed as `after_cursor` to `ingest_project_items`. The fake, when called with `after_cursor=cursor-1`, would return the NEXT page of items — NOT ITEM_5 again. ITEM_5 was on page 1 (with `after_cursor=None`), not on the page returned for `after_cursor=cursor-1`.

The plan's claim that "non-full sync uses cursor continuation and does not re-fetch" is correct, but the plan incorrectly assumes the fake client would still return ITEM_5 v2 on the subsequent call. The test sequence should verify that ITEM_5's body_text is NOT v2 after the non-full sync, but this happens implicitly because ITEM_5 is not in the subsequent page(s) returned by the fake for `after_cursor=stored_cursor`. The plan should make this cursor→page mapping explicit.

**Gap 3: `body_text` column name is asserted but not verified against schema**

The acceptance criteria say "verify ITEM_5 is stored with body_text='first body'" but the plan never confirms that `kb_project_items` has a `body_text` column. The actual column name should be confirmed from `schema/kb_project_items.sql` or the `ProjectItem` model.

**Complexity issue: `FakeProjectItemsFetcher` adds unnecessary abstraction**

The plan invents a new fake class when the existing `test_sync_command_db.py` pattern patches `ingest_project_items` directly with a `side_effect` lambda/function. A simpler approach: create a helper that returns v1 `ProjectIngestResult` for step 2 and v2 `ProjectIngestResult` for step 4, patching `ingest_project_items` directly. This matches established patterns and avoids introducing a new fake class with ambiguous interface.

**Scope: Approved**
The non-goals are clear and correctly scoped. The test does not creep into testing `sync changed` or flag parsing.

**Feasibility: Approved with minor clarification needed**
The test can verify the behavior with existing infrastructure. The approach of patching `ingest_project_items` and querying `kb_project_items` directly is sound and matches existing test patterns.

**Correctness: Requires clarification on fake client behavior**
The fundamental test logic (seed v1 → non-full sync (v1 retained) → full-sync (v2 visible)) is correct. However, the plan must clarify that the non-full sync's fake is called with a non-None `after_cursor` and returns subsequent pages that do NOT contain ITEM_5, while the full-sync's fake is called with `after_cursor=None` and returns all items including ITEM_5 v2.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-30T20:30:00Z
Outcome: revised_plan_addresses_review_feedback

Changes made:
- Replaced `FakeProjectItemsFetcher` custom class with direct `side_effect` patching of `ingest_project_items`, matching the established pattern in `test_sync_command_db.py` (see lines 95-108 and 112 in that file).
- Clarified the test sequence to correctly describe how cursor-based pagination works: after step 2 stores `final_cursor` in `sync_state`, step 4's non-full sync calls `ingest_project_items` with `after_cursor=stored_cursor`, so the fake returns the next page (no ITEM_5). Only step 6 (full refresh) calls with `after_cursor=None`, returning ITEM_5 v2.
- Verified `body_text` column name from `PreparedProjectItem.body_text` at `project.py:339` and the `ON CONFLICT DO UPDATE SET body_text = EXCLUDED.body_text` clause at `project.py:1263`.
- Updated Acceptance Criteria to explicitly require that the `side_effect` function distinguish calls by `bounds.after_cursor` (None vs stored cursor).
- Updated Risks section to replace the "FakeProjectItemsFetcher complexity" concern with "Cursor-based pagination" — the need for the `side_effect` function to correctly interpret `bounds.after_cursor`.

Critique points addressed:
1. Gap - Fake client architecture: Now patches `ingest_project_items` directly with `side_effect` function, not a custom fake class.
2. Gap - Test sequence step 4: Corrected to show that non-full sync uses `after_cursor=stored_cursor`, so the fake returns the NEXT page (not ITEM_5 v2). ITEM_5 v2 is only returned when `after_cursor=None` (full refresh).
3. Gap - body_text column name: Confirmed from schema in `project.py`.
4. Complexity issue: Custom `FakeProjectItemsFetcher` removed in favor of direct `side_effect` patching.

Critiquer: Iteration 2
Timestamp: 2026-03-30T21:00:00Z
Outcome: approved

All four Iteration 1 critique points have been adequately addressed:

1. **Fake client architecture (Gap 1)**: The revised plan correctly abandons `FakeProjectItemsFetcher` and instead patches `agentic_kb.commands.sync.ingest_project_items` directly with a `side_effect` function. This matches the established pattern in `test_sync_command_db.py` (lines 95-108, 112). The patch target is verified correct: `ingest_project_items` is imported at `sync.py:35` (`from agentic_kb.ingest.project import ... ingest_project_items`) and called directly at `sync.py:483`, so `patch("agentic_kb.commands.sync.ingest_project_items")` will intercept it.

2. **Test sequence cursor logic (Gap 2)**: The revised 7-step sequence is logically consistent. After step 2 stores `final_cursor` in `sync_state`, step 4's non-full `sync_project()` calls `derive_project_cursor_for_explicit_sync()` which returns `(stored_cursor, "incremental")`. This cursor is passed as `bounds.after_cursor` to the patched `ingest_project_items`, which returns an empty page (simulating cursor continuation past ITEM_5). Step 6's `sync_project(full_refresh=True)` triggers `derive_project_cursor_for_explicit_sync()` to return `(None, "full")`, so the patch returns ITEM_5 v2. The cursor→page mapping is now explicit and correct.

3. **body_text column name (Gap 3)**: Verified from `PreparedProjectItem.body_text` at `project.py:339` and the `ON CONFLICT DO UPDATE SET body_text = EXCLUDED.body_text` clause at `project.py:1263` in `upsert_project_items()`. The column name is confirmed.

4. **Unnecessary custom class (Complexity issue)**: `FakeProjectItemsFetcher` has been removed. The plan uses a `side_effect` function that receives `bounds` (a `ProjectFetchBounds` object), reads `bounds.after_cursor`, and returns a `ProjectIngestResult`. This is the minimal approach.

**Verification of patching approach feasibility**: The `side_effect` function signature `def fake_ingest_project_items(**kwargs)` matches how `ingest_project_items` is called from `sync_project()` at `sync.py:483-492` (embedding_client, project_store, bounds, github_token as kwargs). The plan correctly has the side_effect function access `kwargs["bounds"].after_cursor` to distinguish between full-refresh (`None`) and incremental (stored cursor) calls.

**Acceptance criteria completeness**: All criteria are present and testable:
- AC1-2: File existence and patching pattern
- AC3: `side_effect` distinguishes by `bounds.after_cursor`
- AC4-5: Non-full vs full sync behavior for already-seen items
- AC6: `mode="full"` in result dict
- AC7: Single-row uniqueness via upsert
- AC8: `py_compile` check

**One minor note**: The acceptance criteria mention "All modified Python modules pass `python3 -m py_compile`" but since this is a test-only task adding a new file, the criteria should perhaps say "The new test module passes `python3 -m py_compile`". This is a minor wording issue and does not require changes.

**New gaps identified**: None identified.

Decision: approved
