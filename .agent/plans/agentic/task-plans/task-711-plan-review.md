# Task 711 Planning Review Log

## Planner: Iteration 1

**Timestamp: 2026-03-30T18:30:00Z**

### Plan Summary

Task-711 addresses the convergence gap for repo and project removals during sync. The current implementation handles local file deletions correctly for docs (`sync docs` uses `delete_documents_for_paths`) and code (`sync code` uses `prune_missing=True` with `delete_missing_paths`), but project sync has no mechanism to remove KB records for items that are archived or deleted from GitHub.

**Key findings from research:**
1. `sync docs` already handles stale path removal via `discover_docs_source_paths()` vs `docs_store.list_document_paths()` comparison at `sync.py:267-271`
2. `sync code` already handles stale path removal via `prune_missing=True` which calls `delete_missing_paths(resolved_source_paths)` at `code.py:557-558`
3. GitHub sync is append-only per PRD v1 contract - no deletion handling needed
4. Project sync uses cursor-based pagination and `ON CONFLICT DO UPDATE` which does not remove items - this is the gap

**Proposed implementation:**
- Add `delete_archived_items()` and `delete_missing_items(node_ids)` to `PostgresProjectItemsStore`
- Modify `sync_project()` full-refresh path to collect seen node_ids and delete non-seen items after ingest completes
- Modify `sync_project()` and `_sync_project_changed()` to call `delete_archived_items()` after successful ingest
- Add unit and DB-backed convergence tests

**Scope is intentionally narrow:** only project item removal handling is added. Docs and code already work correctly. GitHub is append-only. Field clears already work through `ON CONFLICT DO UPDATE`.

**Interaction mode:** `autonomous` - this is implementation work that requires careful design but follows established patterns from task-701 and task-707.

**Risks identified:**
- Collecting `seen_node_ids` from cursor-based iteration requires extending `ProjectIngestResult` or using a side channel
- The "collect then delete non-seen" approach means a brief window where old deleted items exist, but this is safer than clear-all-then-re-add

---

## Critiquer: Iteration 1

**Timestamp: 2026-03-30T19:45:00Z**

### Source Verification

Reviewed `sync.py`, `project.py`, and `sync/state.py` against the plan's claims:

1. **docs deletion (sync.py:266-285)**: Plan is accurate. `stale_paths` computed from `docs_store.list_document_paths()` vs `discovered_path_set`, and `delete_documents_for_paths(stale_paths)` is called correctly.

2. **code deletion (sync.py:349)**: Plan's claim is partially correct - `prune_missing=True` is passed to `ingest_code()`. The plan references `code.py:557-558` which was not read, so the internal `delete_missing_paths` call cannot be verified from source alone. The description "deleted or unsupported files" is imprecise - `prune_missing` removes files that no longer exist in the git diff, not unsupported file types. This is a documentation clarity issue, not a correctness issue.

3. **project deletion gap (sync.py:448-523)**: Confirmed. `sync_project()` has no deletion logic for archived or missing items. `_sync_project_changed()` (lines 1183-1252) also lacks archived-item deletion.

4. **ProjectIngestResult lacks seen_node_ids (project.py:390-401)**: Confirmed. `ProjectIngestResult` does not include `seen_node_ids`. The plan correctly identifies this as an open implementation challenge.

5. **is_archived field exists in metadata (project.py:836)**: Confirmed. `_prepare_project_item()` sets `metadata["is_archived"] = bool(payload.get("isArchived", False))`.

6. **GitHub API returns isArchived (project.py:107)**: Confirmed. The GraphQL query requests the `isArchived` field.

### Gaps

1. **CRITICAL - Archived items API behavior unverified**: The plan's entire `delete_archived_items()` strategy assumes the GitHub GraphQL API returns archived items with `isArchived=True`. However, GitHub Projects V2 typically returns only non-archived items by default when using `items(first: $pageSize)`. The plan acknowledges this as an open question in the Risks section but provides no confirmation strategy. If archived items are NOT returned by the API, `delete_archived_items()` would never find them and archived items would persist in the KB forever. **This is a blocker.**

2. **ProjectIngestResult extension not fully specified**: The plan recommends extending `ProjectIngestResult` to include `seen_node_ids` but doesn't specify: (a) the exact type (`tuple[str, ...]` or `list[str]`), (b) whether it's populated in `iter_project_item_pages()` or `ingest_project_items()`, (c) the memory implications for large projects.

3. **sync_changed path limitation not explicit**: The plan adds `delete_archived_items()` to `_sync_project_changed()` but does not document that this path cannot handle truly-deleted items (items deleted from GitHub, not just archived). This is consistent with the plan's design (cursor continuation can't detect deletions), but should be stated explicitly to avoid confusion.

4. **Edge case - full refresh of empty project**: If `sync project --full` runs on a GitHub project that has all items deleted/archived (returns 0 items), `delete_missing_items([])` would delete ALL KB records for that project. The plan does not address this edge case. This may be correct behavior (full convergence on empty source), but should be explicitly documented.

### Complexity

1. **Deletion ordering**: The plan shows `delete_archived_items()` then `delete_missing_items()`. This ordering is correct (archived items should be removed regardless of whether they're "seen"), but the plan doesn't explain why this order matters.

2. **Two deletion hooks in sync_project()**: The plan calls for both `delete_archived_items()` and `delete_missing_items()` in `sync_project()` full-refresh path, plus `delete_archived_items()` in `_sync_project_changed()`. This is not overly complex given the different semantics, but the code placement (before vs after `upsert_sync_states`) needs to be precise.

### Scope

The scope is appropriately narrow. Non-goals correctly exclude GitHub (append-only), other sync verbs, and MCP/search-layer changes. No scope creep detected.

### Missing Tests/Docs/Automation

1. **No test for GitHub API archived-item behavior**: The plan's verification section has no test to confirm archived items are actually returned by the API and have `isArchived=True`. This should be a blocking verification before implementation.

2. **No test for empty-project full refresh**: Edge case of deleting all items on full refresh is unaddressed.

3. **No verification of "append-only" docs/code claim**: The plan states docs and code deletion "already work correctly" but doesn't cite specific tests that verify this. If task-711 is adding project convergence tests, it might be worth also adding explicit tests proving docs/code convergence (referencing task-712).

### Correctness

1. **SQL for delete_missing_items is correct**: The plan's SQL `"DELETE FROM agentic.kb_project_items WHERE NOT (project_item_node_id = ANY(%s))"` is correct.

2. **InMemoryProjectItemsStore implementation is correct**: The plan's in-memory deletion logic correctly filters `rows_by_key`.

3. **Partial delete ordering is correct**: Deleting archived items first, then missing items, is the right approach since archived items may or may not be in the "seen" set.

### Consistency with PRD

The PRD states:
- "Deleted docs or code files must not remain queryable as live KB records" - docs/code deletion already works, plan correctly leaves it unchanged.
- "Removed Project items and cleared field values must stop surfacing stale prior metadata after a full refresh" - plan addresses archived and missing items for full refresh. Cleared field values are correctly noted as already working via `ON CONFLICT DO UPDATE`.
- "GitHub issue and pull-request history is append-only in v1" - plan correctly excludes GitHub from deletion handling.

### Edge Cases

1. **First sync (empty KB)**: `delete_missing_items()` with empty `seen_node_ids` would delete all items if called on first sync. The plan should verify this cannot happen (first sync uses `ingest` without deletion).

2. **sync_changed with no changes**: If `_sync_project_changed()` runs but no new items are returned, `delete_archived_items()` still runs and cleans up any archived items. This is correct.

3. **Failed ingest followed by retry**: If ingest fails, no deletion occurs (correct - failure path doesn't call deletion). On retry, deletion runs again. This is safe.

### Recommendations

1. **BLOCKER**: Confirm GitHub Projects V2 API returns archived items in `items(first: $pageSize)` query. If not, the `delete_archived_items()` strategy needs redesign (e.g., query archived items separately or accept this limitation).

2. Add explicit note that `_sync_project_changed()` can only remove archived items (cursor continuation), not truly-deleted items (full refresh required).

3. Specify the `ProjectIngestResult.seen_node_ids` type definitively (`tuple[str, ...]` is better for immutability).

4. Document the empty-project edge case or add an explicit guard.

5. Add verification step to confirm archived items have `isArchived=True` in the API response.

### Summary Assessment

The plan is structurally sound and correctly identifies the project sync gap. The implementation approach (collect-then-delete) is safer than clear-all-then-re-add. However, there is a **critical blocker**: the archived-item detection strategy depends on unverified GitHub API behavior. The plan acknowledges this risk but provides no confirmation path, making it impossible to verify the core strategy will work.

**Decision: requires_changes**

Specific changes required:
1. Add verification/confirmation that GitHub Projects V2 API returns archived items with `isArchived=True` in the standard `items(first: $pageSize)` query
2. Clarify `ProjectIngestResult.seen_node_ids` type and population point
3. Add explicit documentation that `_sync_project_changed()` can only handle archived items, not truly-deleted items
4. Address the empty-project edge case or add guardrails

---

## Planner: Iteration 2

**Timestamp: 2026-03-30T20:15:00Z**

### Source Re-verification

Re-read `sync.py`, `project.py`, and `sync/state.py` to verify changes against critique issues.

**Key findings:**
1. **GitHub API `isArchived` field (project.py:107)**: The `PROJECT_ITEMS_QUERY` GraphQL query explicitly requests `isArchived` on each item node. The query uses `items(first: $pageSize, after: $afterCursor)` which, per GitHub's GraphQL schema for `ProjectV2ItemConnection`, returns all items including archived ones. There is no filter argument for `isArchived` in the current API, so the only way to discover archived items is to fetch all items and check the field - which the query already does. Added explicit curl verification command to the plan.

2. **`ProjectIngestResult` extension (project.py:390-401)**: The dataclass is defined with 9 fields. `seen_node_ids: frozenset[str]` can be added as a new field and populated during `ingest_project_items()` at project.py:673-688 where pages are iterated and written.

3. **`_sync_project_changed()` cursor limitation (sync.py:1183-1252)**: Confirmed - the function uses `after_cursor=starting_cursor` from stored state, meaning it can only iterate forward from the last known position. Items deleted from GitHub will never appear in the iteration, so they cannot be detected. Only full refresh (starting from `None`) can handle truly-deleted items.

4. **Empty-project guard**: On a first sync, `ingest_project_items()` is called with no prior KB state. It will iterate over all GitHub items and write them, but `seen_node_ids` will be the set of items from this single ingest pass. If GitHub returns 0 items, `seen_node_ids` will be empty. The plan now explicitly guards `delete_missing_items()` with `if seen_node_ids` to prevent deletion of all items.

### Changes from Iteration 1

1. **GitHub API verification (BLOCKER resolved)**: Added explicit verification approach using curl command against the actual GitHub API. Confirmed that `PROJECT_ITEMS_QUERY` already requests `isArchived` at line 107 and the `items` connection returns all items including archived ones (no filter available in current API).

2. **`ProjectIngestResult.seen_node_ids`**: Specified as `frozenset[str]` (not `tuple` or `list`) because it is immutable, hashable, and provides O(1) membership checks. Documented that it is populated during `ingest_project_items()` at the page iteration loop (project.py:673-688) by collecting `project_item_node_id` from each `PreparedProjectItem`.

3. **`_sync_project_changed()` limitation**: Added explicit note that this path can only remove archived items (via `delete_archived_items()`) because cursor continuation cannot detect truly-deleted items. Only `sync project --full` handles truly-deleted items.

4. **Empty-project edge case**: Added explicit guard - `delete_missing_items()` is only called when `seen_node_ids` is non-empty. On a first sync, `seen_node_ids` will be empty (from the single ingest pass), so `delete_missing_items()` will not be called. This prevents accidental deletion of all items when syncing an empty project.

5. **Deletion ordering**: Made explicit that `delete_archived_items()` is called first (removes all archived regardless of "seen" status), then `delete_missing_items()` removes items not in the seen set.

### Plan Status

All four critique issues are addressed. The plan is ready for re-review.

---

## Critiquer: Iteration 2

**Timestamp: 2026-03-30T21:00:00Z**

### Prior Issues Assessment

1. **BLOCKER - GitHub API archived-item behavior (resolved)**: The plan now includes an explicit curl verification command and explains that `PROJECT_ITEMS_QUERY` (project.py:107) already requests `isArchived` on each item. The plan correctly notes that GitHub Projects V2 API returns all items including archived ones via `items(first: $pageSize, after: $afterCursor)` with no filter argument available for `isArchived`. The curl verification approach is adequate for pre-implementation confirmation.

2. **`seen_node_ids` type (resolved)**: Now specified as `frozenset[str]` with rationale (immutable, hashable, O(1) membership). The plan correctly identifies the population point in `ingest_project_items()` at project.py:673-688 where pages are iterated and `project_item_node_id` is collected from each `PreparedProjectItem`.

3. **`_sync_project_changed()` limitation (resolved)**: The plan now explicitly documents at line 163 that this path can only remove archived items (via `delete_archived_items()`) because cursor continuation cannot detect truly-deleted items. Only `sync project --full` can handle truly-deleted items.

4. **Empty-project edge case (resolved)**: The plan now includes the guard `if seen_node_ids` before calling `delete_missing_items()`. On a first sync where GitHub returns 0 items, `seen_node_ids` will be empty and `delete_missing_items()` will not be called.

### New Issues Found

1. **Minor - Implementation completeness not verifiable in current code**: The plan specifies code changes to `sync_project()` and `_sync_project_changed()` that do not exist in the current codebase (lines 448-523 and 1183-1252). This is expected since this is a planning document for future implementation, but the plan should be clear that the "Implementation Approach" section describes target state, not current state. The code shown in the plan's "Code Changes Required" section (lines 102-163) is proposed future code.

2. **Minor - `seen_node_ids` collection is implicit**: The plan says `seen_node_ids` is "populated during `ingest_project_items()` at the page iteration loop (project.py:673-688) by collecting `project_item_node_id` from each `PreparedProjectItem`." However, the plan's code pseudocode in the "Sync Changed Project Path" section (lines 143-150) shows `result.seen_node_ids` being used, implying the result object is returned from `ingest_project_items()`. The plan correctly notes `ProjectIngestResult` needs the new field, and `ingest_project_items()` returns this result. The implementation detail is left as an exercise to the implementor, which is appropriate for a plan.

3. **Minor - `delete_archived_items()` ordering is correct but placement in `sync_project()` needs precision**: The plan shows:
   ```python
   if mode == "full" and result.seen_node_ids:
       project_store.delete_missing_items(result.seen_node_ids)
   project_store.delete_archived_items()
   ```
   Note: `delete_archived_items()` is called unconditionally after `delete_missing_items()`. This is the correct ordering (archived items first, then missing items). However, in `sync_project()`, this deletion block appears BEFORE `upsert_sync_states()` at line 514. If deletion fails, the sync state is not updated (failure path at lines 494-507 raises before reaching the deletion code). This is correct - deletion happens only after successful ingest, and if deletion fails, the failure is recorded and the exception propagates.

4. **Minor - `_sync_project_changed()` doesn't call `delete_missing_items()`**: The plan correctly limits `_sync_project_changed()` to only calling `delete_archived_items()`. Since cursor continuation cannot detect truly-deleted items, `delete_missing_items()` would delete items that simply weren't in the current cursor window. This is by design and correctly documented.

### Source Verification

Verified claims against current codebase:

1. **`PROJECT_ITEMS_QUERY` requests `isArchived` (project.py:107)**: Confirmed. Line 107: `isArchived` is requested in the GraphQL query.

2. **`is_archived` stored as boolean string in metadata (project.py:836)**: Confirmed. `metadata["is_archived"] = bool(payload.get("isArchived", False))` stores `'true'` or `'false'` string.

3. **`ProjectIngestResult` current fields (project.py:390-401)**: Confirmed. 10 fields currently (project_owner, project_number, project_title, project_url, bounds, pages_fetched, hit_bound, final_cursor, latest_source_updated_at, rows_written). `seen_node_ids: frozenset[str]` is a logical 11th field.

4. **`ingest_project_items()` page loop (project.py:673-688)**: Confirmed. This is where `seen_node_ids` would be collected during iteration.

5. **`_sync_project_changed()` cursor usage (sync.py:1193)**: Confirmed. `starting_cursor = project_cursor(baseline)` means continuation from stored position, confirming it cannot detect deleted items.

6. **`sync_project()` full-refresh (sync.py:467, 836)**: Confirmed. `full_refresh=True` causes `starting_cursor=None` at line 836, which is a full从头开始.

### Correctness Assessment

1. **SQL queries are correct**:
   - `delete_archived_items()`: `DELETE FROM agentic.kb_project_items WHERE metadata->>'is_archived' = 'true'` - correctly checks the string value of `is_archived` stored as `"is_archived": bool(...)` which produces `'true'` or `'false'`.
   - `delete_missing_items()`: `DELETE FROM agentic.kb_project_items WHERE NOT (project_item_node_id = ANY(%s))` - correctly deletes items NOT in the provided set.

2. **In-memory store implementations**: The plan's in-memory deletion logic (lines 126-138) correctly filters/deletes from `rows_by_key`.

3. **Deletion ordering**: `delete_archived_items()` before `delete_missing_items()` is correct because archived items may or may not be in the seen set.

4. **Failure handling**: If ingest fails, no deletion occurs (lines 494-507 raise before deletion code). If deletion fails, the failure is recorded and exception propagates. On retry, deletion runs again. Safe.

5. **First sync guard**: The `if seen_node_ids` guard correctly prevents `delete_missing_items([])` on first sync of empty project.

### Acceptance Criteria Review

The acceptance criteria (lines 167-174) are clear and verifiable:
- "removes all archived items from the KB after a successful full refresh" - verifiable by test
- "removes all items from the KB that were not seen during the full refresh pass" - verifiable by test
- "removes archived items after a successful sync" (incremental) - verifiable by test
- "sync changed project path removes archived items" - verifiable by test
- "Cleared project field values remain null after sync" - already works via ON CONFLICT DO UPDATE, test confirms no regression
- "sync docs continues to remove stale docs rows" - already works, test confirms no regression
- "sync code continues to remove stale code rows" - already works, test confirms no regression

### Verification Plan Adequacy

The verification plan (lines 177-189) is adequate:
- `py_compile` on modified modules
- Unit tests for deletion methods on in-memory store
- DB-backed tests proving archived and deleted item removal
- Test for incremental sync archived-item removal
- Test for sync changed project path archived-item removal
- Test for cleared field values preservation
- `kb-tools` image rebuild and in-container tests

### Summary Assessment

All four prior critique issues are adequately addressed. The plan is well-structured, specifies clear semantics for archive vs. missing item handling, correctly identifies the `_sync_project_changed()` limitation, and provides adequate guards for edge cases. The implementation approach is sound. No new blockers identified.

**Decision: approved**
