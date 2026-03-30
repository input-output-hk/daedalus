# Task Plan: task-707 Add manual full Project refresh path

- Task ID: `task-707`
- Title: `Add manual full Project refresh path`
- Planning Status: `approved`
- Build Status: `completed`

## Why This Task Was Chosen Now

- `task-707` is the next unblocked Phase 7 critical-path task after `task-701` delivered the full sync command family, including `sync project` with cursor-based continuation.
- `task-404` research confirmed cursor-based pagination cannot replay edits to already-seen items; task-701 preserved that limitation in operator output but added no override.
- The PRD (requirement #43) and Phase 7 description explicitly promise a manual full Project refresh path so eventual freshness converges even when edits land on already-seen items.
- `task-708` (pending) depends on this task for the regression check that validates the full-refresh path replays edits to already-seen items.
- The platform design decision from the Freshness section states: "cursor-based incremental sync does not need to guarantee immediate replay of edits to already-seen items as long as the platform also provides a manual full refresh path that re-converges project state."

## Scope

- Add `sync project --full` (or an equivalent explicit full-refresh mode) to the packaged CLI in `agentic/src/agentic_kb/commands/sync.py`.
- When `--full` is set, `sync_project()` must pass `after_cursor=None` to force a full re-ingest from the beginning, bypassing stored cursor continuation.
- Update operator output to reflect the full-refresh mode and remove the "cursor continuation only" limitation note when running in `--full` mode.
- Add focused unit and DB-backed tests for the `--full` flag behavior.
- Keep the change narrow: no redesign of the ingestion core, no automatic scheduled refresh, no staleness detection changes.

## Non-Goals

- Do not add automatic scheduled refresh; that remains out of scope per the PRD non-goals.
- Do not redesign the Project 5 ingestion core; this is a CLI/sync-layer addition only.
- Do not add `--full` to `sync changed`; the incremental contract for already-seeded KBs remains unchanged.
- Do not add full-refresh paths for other sources (docs, code, github); this task is scoped to Project 5 only.
- Do not claim that `--full` detects individual field-level edits and updates only those; full refresh re-ingests all pages.

## Relevant Dependencies

- Completed upstream tasks:
  - `task-404` - established the Project 5 ingestor at `agentic/src/agentic_kb/ingest/project.py` using cursor-based pagination through `iter_project_item_pages(...)`.
  - `task-701` - delivered `sync project` with `derive_project_cursor_for_explicit_sync()` that returns `(None, "initial")` for first run and `(stored_cursor, "incremental")` for later runs; the function currently has no `--full` override.
- Direct downstream task unblocked by this work:
  - `task-708` - adds the Project full-refresh replay regression check that proves an edit to a previously seen Project item is reflected after explicit full refresh.
- Primary references reviewed for this plan:
  - `.agent/plans/agentic/knowledge-base-platform-prd.md`
  - `.agent/plans/agentic/knowledge-base-platform-tasks.json`
  - `.agent/plans/agentic/task-plans/task-404.md`
  - `.agent/plans/agentic/task-plans/task-701.md`
  - `agentic/src/agentic_kb/commands/sync.py`
  - `agentic/src/agentic_kb/cli.py`

## Files Expected To Change

- `agentic/src/agentic_kb/commands/sync.py` - add `--full` flag to `project_parser`, modify `sync_project()` to accept a `full_refresh: bool` parameter, modify `derive_project_cursor_for_explicit_sync()` to handle full refresh mode, update output formatting to reflect "full" mode
- `agentic/tests/test_sync_command.py` - extend unit coverage for `--full` flag routing and `sync_project` with `full_refresh=True`
- `agentic/tests/test_sync_command_db.py` - extend DB-backed coverage for full-refresh behavior against an already-seeded KB
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` - update `task-707` status when implementation lands

## Implementation Approach

- **`sync project --full` flag placement**: in `add_sync_subcommands()`, add `project_parser.add_argument("--full", action="store_true")`. This is the ONLY sync verb that accepts `--full`. The `sync changed` subparser does NOT get a `--full` flag.
- **`full_refresh` handling is entirely at the `run_sync_project()` layer**: `run_sync_project(args)` reads `full_refresh=getattr(args, "full", False)` directly and passes it to `sync_project()`. The shared `_run_single_source_command()` runner is NOT modified; it remains generic and knows nothing about `--full`.
- **`sync_changed` rejects `--full`**: if an operator runs `sync changed --full`, it is a usage error. Since `changed_parser` has no `--full` argument, argparse will reject the combination with a clear error: `error: unrecognized arguments: --full`. No custom error handling is needed; argparse provides this rejection for free.
- **`sync_project()` signature change**: update the function signature from `def sync_project(workspace_root: str | Path, *, config: AgenticConfig) -> dict[str, Any]:` to `def sync_project(workspace_root: str | Path, *, config: AgenticConfig, full_refresh: bool = False) -> dict[str, Any]:`.
- **`derive_project_cursor_for_explicit_sync()` signature change**: update the function signature from `def derive_project_cursor_for_explicit_sync(baseline: SyncStateRecord | None) -> tuple[str | None, str]:` to `def derive_project_cursor_for_explicit_sync(baseline: SyncStateRecord | None, full_refresh: bool = False) -> tuple[str | None, str]:`. When `full_refresh=True`, the function returns `(None, "full")` unconditionally, bypassing all stored-cursor logic and forcing re-ingest from `after_cursor=None`.
- **Exact output note text** for `format_sync_source_output()` when `command_name == "project"`:
  - When `result["mode"] == "full"`: `"note: Project sync ran in full-refresh mode and re-ingested all items."`
  - When `result["mode"] == "incremental"`: `"note: Project sync is cursor continuation only and can still miss edits to already-seen items."`
  - When `result["mode"] == "initial"`: no note is emitted (first-run behavior is self-evident).
- **Project section inside `sync changed` output**: the `format_sync_source_output()` for `command_name == "changed"` formats the project sub-result with the incremental limitation note because `sync changed` always uses cursor continuation internally; it never runs in full-refresh mode. The `--full` flag does not apply to `sync changed`, so the project section inside `changed` output always uses the `"incremental"` mode string and emits the cursor-continuation limitation note.
- **Preserve existing behavior**: when `--full` is not set, behavior is unchanged: first run seeds from `after_cursor=None`, later runs continue from stored cursor.
- **Keep changes minimal**: do not modify `ingest_project_items()`, `iter_project_item_pages()`, or any ingestion library code; the full-refresh semantics are achieved entirely through cursor override at the sync command layer.

## Acceptance Criteria

- `agentic-kb sync project --full` forces a full re-ingest from `after_cursor=None` even when a stored project cursor already exists.
- `agentic-kb sync project` (without `--full`) continues to use the stored cursor when one exists, preserving the existing incremental behavior.
- After a `sync project --full` run, a subsequent `sync project` (without `--full`) uses the newly stored end cursor from the `--full` run as its starting point, proving the operator can return to normal incremental mode.
- `sync changed --full` is rejected by argparse with `error: unrecognized arguments: --full`; no custom error handling is needed.
- The `--full` flag is documented in the CLI help text for the `project` subcommand.
- Operator output for `sync project --full` shows `mode=full` and emits: `"note: Project sync ran in full-refresh mode and re-ingested all items."`.
- Operator output for `sync project` (non-full, incremental) shows `mode=incremental` and emits: `"note: Project sync is cursor continuation only and can still miss edits to already-seen items."`.
- The project section inside `sync changed` output always uses `mode=incremental` and emits the cursor-continuation limitation note, never a full-refresh note.
- Focused unit tests cover `--full` flag routing, `sync_project()` with `full_refresh=True`, `derive_project_cursor_for_explicit_sync()` returning `(None, "full")` when `full_refresh=True`, and exact output note text for both modes.
- Focused DB-backed tests cover: (a) `sync project --full` against an already-seeded KB re-ingests from `after_cursor=None`; (b) rows are updated in place through the natural-key uniqueness constraint; (c) subsequent non-full `sync project` uses the `--full` run's end cursor as its new starting point.

## Verification Plan

- Run `python3 -m py_compile` on all touched Python modules and tests.
- Extend unit coverage in `agentic/tests/test_sync_command.py` for:
  - `sync project --full` parser routing: verify `args.full == True` when `--full` is passed
  - `sync project` without `--full`: verify `args.full == False` (default)
  - `sync changed --full`: verify argparse rejects it with `error: unrecognized arguments: --full`
  - `derive_project_cursor_for_explicit_sync()` returning `(None, "full")` when `full_refresh=True`
  - `derive_project_cursor_for_explicit_sync()` returning `(cursor, "incremental")` when `full_refresh=False` and cursor exists
  - `sync_project()` with `full_refresh=True` producing `mode="full"` in output result dict
  - `format_sync_source_output()` for project with `mode="full"`: verify exact note text is `"note: Project sync ran in full-refresh mode and re-ingested all items."`
  - `format_sync_source_output()` for project with `mode="incremental"`: verify exact note text is `"note: Project sync is cursor continuation only and can still miss edits to already-seen items."`
  - `format_sync_source_output()` for project with `mode="initial"`: verify no limitation note is emitted
  - `format_sync_source_output()` for `changed` command: verify project sub-result always uses incremental note regardless of prior `--full` runs
- Extend DB-backed coverage in `agentic/tests/test_sync_command_db.py` for:
  - `sync project --full` against an already-seeded KB: verify `after_cursor=None` is passed to `ingest_project_items`, rows are updated in place through the natural-key uniqueness constraint on `project_item_node_id`, and the stored cursor after the run is the new end cursor
  - `sync project` (non-full) after a prior `--full` run: verify the subsequent run uses the `--full` run's end cursor as its starting point, proving the operator can return to normal incremental mode
- Rebuild `kb-tools` with `docker compose -f docker-compose.agentic.yml build kb-tools` so packaged runtime behavior matches local source.
- Run the focused test suites inside `kb-tools` using the in-container unittest pattern.
- If live GitHub credentials are available, run a bounded live smoke verifying that a `sync project --full` run re-ingests from the beginning and that a subsequent non-full run continues from the new end cursor.

## Risks / Open Questions

- **Minimal scope risk**: the implementation is intentionally narrow and touches only the CLI layer and cursor-derivation logic. The main risk is inadvertently widening into ingestion redesign or adding `--full` to other sync verbs; the plan explicitly excludes those.
- **`sync changed --full` rejection**: since `changed_parser` has no `--full` argument, argparse provides the rejection for free. No custom error handling is needed; implementation just does not add the flag to that subparser.
- **Output note text is now locked down**: exact verbatim strings are specified in the Implementation Approach section. Any deviation during implementation review should be flagged.
- **Interaction with `sync all`**: `sync all` calls `sync project` internally but does not pass `--full`. This is correct by design: `sync all` is the explicit full-refresh orchestrator for all sources, but each source sync within it uses its own default incremental mode. The `--full` flag is operator-selective and should not be embedded inside `sync all`.

## Required Docs / Tracking / Research Updates

- This canonical task plan is created at `.agent/plans/agentic/task-plans/task-707.md`.
- Planning review history lives at `.agent/plans/agentic/task-plans/task-707-plan-review.md`.
- Implementation review history will be preserved at `.agent/plans/agentic/task-plans/task-707-impl-review.md` once implementation begins.
- `.agent/plans/agentic/knowledge-base-platform-tasks.json` will be updated to mark `task-707` `completed` when implementation lands.
- A durable research note will be added at `.agent/plans/agentic/research/task-707-full-project-refresh.md` if implementation discovers durable findings worth capturing.

## Implementation Notes

- Added `--full` flag to the `project` subparser in `add_sync_subcommands()`.
- Modified `run_sync_project()` to read `full_refresh=getattr(args, "full", False)` and pass it to `sync_project()`.
- Updated `sync_project()` signature to accept `full_refresh: bool = False` parameter.
- Updated `derive_project_cursor_for_explicit_sync()` signature to accept `full_refresh: bool = False`; when `True`, returns `(None, "full")` unconditionally, bypassing stored cursor logic.
- Updated `format_sync_source_output()` to emit mode-specific notes: `"full"` mode emits `"note: Project sync ran in full-refresh mode and re-ingested all items."`, `"incremental"` mode emits `"note: Project sync is cursor continuation only and can still miss edits to already-seen items."`, `"initial"` mode emits no note.
- `sync changed` does not accept `--full`; argparse rejects `sync changed --full` with `error: unrecognized arguments: --full` by design.
- Files changed: `agentic/src/agentic_kb/commands/sync.py`, `agentic/tests/test_sync_command.py`, `agentic/tests/test_sync_command_db.py`.
- No deviations from the approved plan; implementation followed the plan exactly.

## Verification Notes

- All modified Python modules passed `python3 -m py_compile`.
- Unit tests in `test_sync_command.py` verified: `--full` flag routing, `sync_project()` with `full_refresh=True` producing `mode="full"`, `derive_project_cursor_for_explicit_sync()` returning `(None, "full")` when `full_refresh=True`, and exact output note text for all three modes.
- DB-backed tests in `test_sync_command_db.py` verified: `sync project --full` against an already-seeded KB re-ingests from `after_cursor=None`, rows are updated in place through the natural-key uniqueness constraint, and subsequent non-full `sync project` uses the `--full` run's end cursor as its new starting point.
- Code review approved; build status marked `completed`.

## Outcome

Task-707 is complete. The `sync project --full` command forces a full re-ingest from `after_cursor=None` even when a stored project cursor already exists, enabling manual convergence for edits to already-seen Project items. This unblocks task-708 (Project full-refresh replay regression check).

## Review Log Paths

- Planning review log: `.agent/plans/agentic/task-plans/task-707-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-707-impl-review.md`
