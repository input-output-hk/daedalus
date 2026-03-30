# Research Brain: task-707 Full Project Refresh

## Key Implementation Decisions

1. **`--full` flag is scoped to `sync project` only**: `changed_parser` does not receive the flag; argparse rejects `sync changed --full` with `error: unrecognized arguments: --full` for free.

2. **Full refresh handled entirely at `run_sync_project()` layer**: `full_refresh` is read from args and passed to `sync_project()`; the shared `_run_single_source_command()` runner remains generic and knows nothing about `--full`.

3. **`derive_project_cursor_for_explicit_sync()` handles the override**: When `full_refresh=True`, returns `(None, "full")` unconditionally, bypassing all stored-cursor logic and forcing `after_cursor=None` in the ingestor.

4. **Mode string drives operator output**: `result["mode"]` is `"full"`, `"incremental"`, or `"initial"` and `format_sync_source_output()` emits mode-specific notes verbatim.

## Design Notes

- The full-refresh semantics are achieved entirely through cursor override at the sync command layer; no changes to `ingest_project_items()`, `iter_project_item_pages()`, or any ingestion library code.
- `sync all` does not pass `--full` internally; it remains the explicit full-refresh orchestrator for all sources but each source sync within it uses its own default incremental mode.
- Rows are updated in place through the natural-key uniqueness constraint on `project_item_node_id`, so re-ingested items replace existing rows rather than duplicating.

## Durable Findings

- Cursor-based pagination cannot replay edits to already-seen items; the only remedy is a manual full refresh that forces `after_cursor=None`.
- The `--full` flag design is intentionally narrow and operator-selective; it should not be embedded inside `sync all` because that would make full refresh the default for all sources simultaneously.
- Exact output note text was locked down in the plan and preserved verbatim in implementation to avoid review-surprise divergence.
