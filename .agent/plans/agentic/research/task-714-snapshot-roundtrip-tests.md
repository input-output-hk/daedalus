# Task 714: Snapshot Round-Trip Regression Tests

## Decisions
- 3 test classes in single file: `test_full_round_trip_all_entities`, `test_multi_manifest_round_trip`, `test_round_trip_with_sparse_entities`
- All tests opt-in via `AGENTIC_TEST_DATABASE_URL` env var (same as `test_snapshot_command_db.py`)
- FK seeding order: parent tables first (documents, code_chunks, github_issues, github_prs, project_items), then child tables (github_issue_comments → issues, github_pr_comments → prs)
- TRUNCATE uses single CASCADE statement with all 9 tables to avoid FK ordering issues
- Deterministic queries use `ORDER BY id` with columns that exist per schema (not `body_hash` which doesn't exist on comment tables)
- Sparse-entity test proves zero-row tables round-trip correctly (not missing, not NULL)
- Multi-manifest test proves only 1 manifest record exists post-import (import drops schema and restores, then persists one new manifest record)
- `OLLAMA_EMBED_MODEL` uses `addCleanup` to prevent env var leakage

## Patterns
- Reuse `_bootstrap_database()` and `_sanitized_sql()` from `test_snapshot_command_db.py`
- `status.inspect_database()` for row counts and sync summaries
- Direct SQL queries for deterministic comparison and sync state verification
- `_capture_sync_state()` helper compares full sync state before/after round-trip

## Constraints
- Comment tables (`kb_github_issue_comments`, `kb_github_pr_comments`) have `body_text` column, NOT `body_hash` or `content_hash`
- `kb_snapshot_manifest` is dropped and recreated during import; only the imported snapshot's manifest record persists post-import
- Sync state uses `source_name='github'` with `scope_key='stream:<stream>:repo:DripDropz/daedalus'` for 4 GitHub streams

## No new research beyond implementation details above
