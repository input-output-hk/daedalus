Planner: Iteration 1
Timestamp: 2026-03-30T18:00:00Z
Outcome: initial_plan_documented

- Drafted the initial canonical `task-707` plan around the promised manual full Project refresh path from the PRD (requirement #43) and Phase 7 description.
- Scoped the task to a narrow CLI/sync-layer addition: add `--full` flag to `sync project`, thread `full_refresh` through to `sync_project()`, modify `derive_project_cursor_for_explicit_sync()` to return `(None, "full")` when `full_refresh=True`, and update operator output formatting.
- Kept the non-goals explicit: no automatic scheduled refresh, no ingestion core redesign, no `--full` for `sync changed` or other sources, no claim that full refresh does field-level diffing.
- Called out the cursor-continuation note replacement as the main output-differentiation concern: the limitation note stays for non-full runs and gets replaced with a full-refresh confirmation note for `--full` runs.
- Noted that `sync all` should NOT embed `--full` internally; the operator selectively applies `--full` to `sync project` directly, and `sync all` continues using default incremental mode for each source.
- Identified that `task-708` (full-refresh replay regression check) depends on this task and will need a real seeded KB environment to prove that edits to already-seen items are reflected after `--full`.

Critiquer: Iteration 1
Timestamp: 2026-03-30T18:05:00Z
Outcome: requires_changes

- The plan is directionally aligned with the PRD and the existing `sync_project()` / `derive_project_cursor_for_explicit_sync()` layout in `agentic/src/agentic_kb/commands/sync.py`, but the `--full` flag threading design needs one more structural decision before implementation: whether `full_refresh` should be threaded through `_run_single_source_command()` or handled entirely at the `run_sync_project()` / `sync_project()` layer. Since `_run_single_source_command()` is shared across all sync verbs, the cleaner approach is to handle `full_refresh` entirely at the `run_sync_project()` layer, passing it through to `sync_project()` without changing the shared command runner.
- The `derive_project_cursor_for_explicit_sync()` signature change needs to be explicit: the function currently takes only `baseline: SyncStateRecord | None` and returns `tuple[str | None, str]`. The plan should state that a new `full_refresh: bool = False` parameter is added and that when `full_refresh=True`, the function returns `(None, "full")` unconditionally, bypassing the stored-cursor logic entirely. This is the cleanest way to force re-ingest from the beginning without modifying the ingestion library.
- The output note distinction needs to be more concrete. The plan says "confirmation note instead of cursor-continuation limitation note" but should specify the exact note text for each case so implementation does not invent wording. Suggested: for `mode="full"` emit "note: Project sync ran in full-refresh mode and re-ingested all items." and for `mode="incremental"` keep "note: Project sync is cursor continuation only and can still miss edits to already-seen items."
- The verification plan covers unit and DB-backed tests but does not explicitly require a test for what happens when `--full` is followed by a non-full `sync project` (i.e., that the stored cursor after `--full` is the new end cursor and subsequent non-full runs continue from there). This is an important regression case because it proves the operator can return to normal incremental mode after forcing a full refresh.
- The plan correctly notes that `sync all` should not embed `--full` internally, but it does not address whether `sync changed` should accept `--full`. The correct answer from the PRD non-goals and task context is that `sync changed` does not get `--full` (it is the incremental command for already-seeded KBs), but the plan should state that explicitly to avoid ambiguity during implementation review.
- The `format_sync_source_output()` change is underspecified for the `changed` formatter path. When `sync changed` calls `_sync_project_changed()`, it formats the project result inside the `changed` output block with its own limitation note. The plan should clarify whether `sync changed --full` is a valid combination (it should not be, based on non-goals), and if not, whether the implementation should reject it with a clear error or silently ignore `--full`.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-03-30T18:15:00Z
Outcome: revised_plan_addresses_review_feedback

- Clarified threading design: `full_refresh` is handled entirely at the `run_sync_project()` layer by reading `getattr(args, "full", False)` directly and passing it to `sync_project()`. The shared `_run_single_source_command()` runner is NOT modified.
- Made `derive_project_cursor_for_explicit_sync()` signature change explicit: added `full_refresh: bool = False` parameter; when `True`, returns `(None, "full")` unconditionally, bypassing all stored-cursor logic.
- Specified exact verbatim output note text in the Implementation Approach section: `mode="full"` emits `"note: Project sync ran in full-refresh mode and re-ingested all items."` and `mode="incremental"` emits `"note: Project sync is cursor continuation only and can still miss edits to already-seen items."`. Initial mode emits no limitation note.
- Added acceptance criterion for `--full` followed by non-full `sync project`: the subsequent run must use the `--full` run's end cursor as its new starting point, proving the operator can return to normal incremental mode.
- Added acceptance criterion and verification test case for `sync changed --full`: argparse rejects it with `error: unrecognized arguments: --full`; no custom error handling needed.
- Clarified `format_sync_source_output()` for `changed` formatter path: project sub-result inside `changed` output always uses `mode=incremental` and emits the cursor-continuation limitation note; `--full` does not apply to `sync changed`.
- Updated Risks section to note that `sync changed --full` rejection is handled for free by argparse and that exact output note text is now locked down.
- Expanded Verification Plan to enumerate specific unit test assertions and DB-backed test scenarios, including the cursor-chain test for `--full` followed by non-full runs.

Critiquer: Iteration 2
Timestamp: 2026-03-30T18:30:00Z
Outcome: approved

- Threading design (critique 1): Line 57 explicitly states `full_refresh` is handled entirely at the `run_sync_project()` layer; `_run_single_source_command()` is NOT modified.
- `derive_project_cursor_for_explicit_sync()` signature (critique 2): Line 60 specifies the new `full_refresh: bool = False` parameter and documents that `full_refresh=True` returns `(None, "full")` unconditionally.
- Output note text (critique 3): Lines 61-64 specify exact verbatim strings for `mode="full"`, `mode="incremental"`, and `mode="initial"` (no note emitted).
- Missing test case for cursor chain (critique 4): Line 73 acceptance criterion and lines 96-98 verification plan cover `--full` followed by non-full `sync project` using the newly stored end cursor.
- `sync changed --full` rejection (critique 5): Line 58 and line 74 explicitly document argparse rejection with `error: unrecognized arguments: --full`.
- `changed` formatter path (critique 6): Line 65 and line 78 clarify project section inside `changed` output always uses `mode=incremental` and emits the cursor-continuation limitation note; `--full` does not apply to `sync changed`.
- The plan is narrow, well-scoped, and all non-goals are correctly documented. No remaining issues identified.

Decision: approved
