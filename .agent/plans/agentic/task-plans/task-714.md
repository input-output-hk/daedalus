# Task 714: Add Snapshot Round-Trip Regression Tests

## Task ID
task-714

## Title
Add snapshot round-trip regression tests

## Why This Task Now
Tasks 602 (snapshot export/import) and 612 (disposable import target safety) are both complete. The existing `test_snapshot_command_db.py` covers a minimal 1-document round-trip but leaves significant surface area untested: multi-entity seeding, multi-manifest records, deterministic query comparison, manifest schema validation post-import, and sync-state watermark preservation across all 6 source streams (docs, code, 4 GitHub streams, project). This test file becomes the canonical regression guardrail for the snapshot pipeline.

## Scope and Non-Goals

### In Scope
- New file: `agentic/tests/test_snapshot_roundtrip.py`
- Single integration test class using `unittest.TestCase`
- Opt-in via `AGENTIC_TEST_DATABASE_URL` env var (same pattern as `test_snapshot_command_db.py`)
- Seed ALL 7 entity types with realistic data before export
- Verify row counts match exactly before and after import
- Verify manifest schema validity post-import
- Verify sync state watermarks preserved across all sources
- Verify deterministic BM25 query returns same results before and after
- Second test for multi-entity round-trip with multiple snapshot manifest records

### Non-Goals
- No changes to `snapshot.py` implementation
- No changes to existing `test_snapshot_command_db.py`
- No new fixtures or helper modules beyond what's needed in the test file
- No E2E/Cucumber tests (this is a Python DB integration test)

## Relevant Dependencies
- task-602 ✅ (snapshot export/import commands)
- task-612 ✅ (disposable import target safety)
- task-203 (search indexes, BM25)
- task-713 (preceding task in the 7xx series)

## Files Expected to Change
| File | Change Type |
|------|-------------|
| `agentic/tests/test_snapshot_roundtrip.py` | New file |

## Implementation Approach

### Test Class: `SnapshotRoundTripTests`
- Inherits from `unittest.TestCase`
- Decorated with `@unittest.skipUnless(...)` checking for `AGENTIC_TEST_DATABASE_URL`, `psycopg`, `pg_dump`, `pg_restore`, `psql`
- `setUp()`: bootstrap DB schema, seed all 7 entity types, set `OLLAMA_EMBED_MODEL`

### Test 1: `test_full_round_trip_all_entities`
1. Capture pre-export state via `status.inspect_database()`:
   - Row counts for all 7 entity tables
   - Sync state summaries for docs, code, github, project
2. Capture deterministic query results for all 7 entity tables:
   - `SELECT id, title, content_hash FROM agentic.kb_documents ORDER BY id`
   - `SELECT id, symbol_name, content_hash FROM agentic.kb_code_chunks ORDER BY id`
   - `SELECT id, title, state FROM agentic.kb_github_issues ORDER BY id`
   - `SELECT id, body_text FROM agentic.kb_github_issue_comments ORDER BY id`
   - `SELECT id, title, state FROM agentic.kb_github_prs ORDER BY id`
   - `SELECT id, body_text FROM agentic.kb_github_pr_comments ORDER BY id`
   - `SELECT id, title, status FROM agentic.kb_project_items ORDER BY id`
3. Export snapshot via `snapshot.export_snapshot()`
4. Validate export manifest:
   - Entity counts match seeded data for all 7 types
   - Sync state watermarks present for all sources (docs, code, 4 GitHub streams, project)
   - Artifact hash/size correct, embedding contract correct
5. Destructive import: `TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, agentic.kb_github_issue_comments, agentic.kb_github_prs, agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE` in a single statement
6. Import snapshot via `snapshot.import_snapshot(db_url, manifest_path, confirmed=True)`
7. Post-import assertions:
   - Row counts match pre-export exactly for all 7 tables
   - Sync state summaries restored (docs, code, github 4-streams, project)
   - Direct `kb_sync_state` queries verify `repo_commit_hash` and `watermark_timestamp` preserved per source
   - Manifest record persisted in `kb_snapshot_manifest` with matching `content_hash`, `entity_counts`, and `github_watermarks` JSONB
   - Applied migration versions preserved (1, 2, 3)
8. Deterministic query comparison:
   - Re-run all 7 entity table queries from step 2
   - Assert identical result sets (same rows, same column values, same order)

### Test 2: `test_multi_manifest_round_trip`
1. Seed baseline data (all 7 entities)
2. Export snapshot #1 → manifest #1
3. Seed additional data (more rows in documents, code_chunks, github_issues)
4. Export snapshot #2 → manifest #2
5. Verify 2 manifest records exist in `kb_snapshot_manifest`
6. TRUNCATE everything (single CASCADE statement as in Test 1)
7. Import snapshot #2 (latest, superset)
8. Verify:
   - Row counts match snapshot #2 entity counts (superset of baseline)
   - Only ONE manifest record for the imported snapshot #2 exists (import drops schema and restores, then persists one new manifest record)
   - The persisted manifest record matches the imported snapshot #2's content_hash and entity_counts
   - Deterministic queries return the superset of data (baseline + additional rows)
9. Env var safety: `setUp` uses `addCleanup` for `OLLAMA_EMBED_MODEL` restoration so no leak between iterations

### Test 3: `test_round_trip_with_sparse_entities`
1. Seed only documents (3 rows) and project items (2 rows); leave code_chunks, github_issues, github_issue_comments, github_prs, github_pr_comments all empty (0 rows)
2. Seed sync state for ALL sources (docs, code, github 4-streams, project) even though entity tables are empty — this proves sync state round-trips independently of entity data
3. Export snapshot
4. Verify manifest entity_counts shows 0 for empty tables
5. TRUNCATE everything
6. Import snapshot
7. Verify:
   - Documents and project items restored with correct row counts
   - All other entity tables have 0 rows (not missing, not NULL)
   - Manifest entity_counts preserved with zeros
   - `inspect_database().row_counts` includes all 7 tables with correct counts
   - Sync state for all sources preserved (docs, code, github 4-streams, project)

### Seeding Strategy
Reuse `_bootstrap_database()` and `_sanitized_sql()` from `test_snapshot_command_db.py`. Add comprehensive `_seed_all_entities()` helper that inserts in FK-safe order:
1. Parent tables first: `kb_documents`, `kb_code_chunks`, `kb_github_issues`, `kb_github_prs`, `kb_project_items`
2. Child tables second: `kb_github_issue_comments` (FK → issues), `kb_github_pr_comments` (FK → prs)
3. Sync state: docs, code, github (4 streams), project with realistic watermarks

Seeding data:
- 3 documents (different doc_kinds: plan, code, architecture)
- 2 code chunks (different languages: python, typescript)
- 2 github issues (open, closed)
- 2 github issue comments (one per issue)
- 2 github PRs (open, merged)
- 2 github PR comments (one per PR)
- 2 project items (different statuses)
- Sync state for docs: `source_name='docs'`, `scope_key='repo:DripDropz/daedalus'`, `repo_commit_hash='cafebabe'`, `watermark_timestamp`
- Sync state for code: `source_name='code'`, `scope_key='repo:DripDropz/daedalus'`, `repo_commit_hash='deadbeef'`, `watermark_timestamp`
- Sync state for github: 4 rows with `source_name='github'`, `scope_key='stream:<stream_name>:repo:DripDropz/daedalus'` for streams `issues`, `pulls`, `issue_comments`, `review_comments`, each with distinct `watermark_timestamp`
- Sync state for project: `source_name='project'`, `scope_key='project:DripDropz:5'`, `cursor_text='cursor-abc'`, `watermark_timestamp`

### Query Comparison
Use deterministic ORDER BY id queries that don't depend on BM25 scoring order. Use only columns that exist per the schema:
```sql
SELECT id, title, content_hash FROM agentic.kb_documents ORDER BY id
SELECT id, symbol_name, content_hash FROM agentic.kb_code_chunks ORDER BY id
SELECT id, title, state FROM agentic.kb_github_issues ORDER BY id
SELECT id, body_text FROM agentic.kb_github_issue_comments ORDER BY id
SELECT id, title, state FROM agentic.kb_github_prs ORDER BY id
SELECT id, body_text FROM agentic.kb_github_pr_comments ORDER BY id
SELECT id, title, status FROM agentic.kb_project_items ORDER BY id
```

## Acceptance Criteria
- [ ] `agentic/tests/test_snapshot_roundtrip.py` exists and is importable
- [ ] `test_full_round_trip_all_entities` passes against a live PostgreSQL with `AGENTIC_TEST_DATABASE_URL`
- [ ] `test_multi_manifest_round_trip` passes against a live PostgreSQL with `AGENTIC_TEST_DATABASE_URL`
- [ ] `test_round_trip_with_sparse_entities` passes against a live PostgreSQL with `AGENTIC_TEST_DATABASE_URL`
- [ ] All tests are skipped gracefully when `AGENTIC_TEST_DATABASE_URL` is not set
- [ ] All 7 entity types are seeded and verified in the full round-trip test
- [ ] Sparse-entity test proves zero-row tables round-trip correctly (not missing, not NULL)
- [ ] Sync state watermarks verified via direct `kb_sync_state` queries for all sources
- [ ] Manifest JSONB fields (`content_hash`, `entity_counts`, `github_watermarks`) verified post-import
- [ ] Deterministic query results match before/after round-trip for all 7 entity tables
- [ ] FK seeding order is parent-first, child-second (issues before comments, PRs before PR comments)
- [ ] `OLLAMA_EMBED_MODEL` uses `addCleanup` to prevent env var leakage

## Verification Plan
```bash
# Without DB URL (should skip)
python -m pytest agentic/tests/test_snapshot_roundtrip.py -v

# With DB URL (should run and pass)
AGENTIC_TEST_DATABASE_URL="postgresql://agentic:agentic@db:5432/agentic_kb" \
  python -m pytest agentic/tests/test_snapshot_roundtrip.py -v

# Also verify via unittest runner
AGENTIC_TEST_DATABASE_URL="postgresql://agentic:agentic@db:5432/agentic_kb" \
  python -m unittest agentic.tests.test_snapshot_roundtrip -v
```

## Risks / Open Questions
- BM25 index rebuild after import: pg_restore should restore indexes, but verify that search queries work post-import
- Embedding vectors: the dump includes VECTOR(384) columns; verify pg_restore handles them correctly
- Foreign key constraints: seeding order is parent-first, child-second to satisfy FK constraints
- The `pg_restore` with `--use-list` filtering: verify all 7 entity tables are included in the agentic restore list
- TRUNCATE CASCADE: use a single TRUNCATE statement with all tables to avoid FK ordering issues during cleanup
- CI gate: this test requires a live PostgreSQL instance. It should be noted as an open question whether to add a CI step with a disposable PostgreSQL container (e.g., via `docker compose -f docker-compose.agentic.yml run --rm kb-tools python -m unittest agentic.tests.test_snapshot_roundtrip`)

## Required Docs / Tracking / Research Updates
- Plan doc: `.agent/plans/agentic/task-plans/task-714.md` (this file)
- Planning review log: `.agent/plans/agentic/task-plans/task-714-plan-review.md`
- Implementation review log: `.agent/plans/agentic/task-plans/task-714-impl-review.md` (created during build loop)
- Tasks JSON: update `status` to `completed`, set `completedAt` in `.agent/plans/agentic/knowledge-base-platform-tasks.json`
- PRD: mark the corresponding checkbox in `.agent/plans/agentic/knowledge-base-platform-prd.md`
- Research brain: update or create `.agent/plans/agentic/research/task-714-snapshot-roundtrip-tests.md`

## Review Log
- `.agent/plans/agentic/task-plans/task-714-plan-review.md`

## Planning Status
`approved`

## Build Status
`completed`

## Final Outcome
All 3 tests implemented and approved in code review. File created at `agentic/tests/test_snapshot_roundtrip.py` (518 lines). Tests are discoverable by unittest and skip gracefully without `AGENTIC_TEST_DATABASE_URL`.

## Review Log Paths
- Planning review log: `.agent/plans/agentic/task-plans/task-714-plan-review.md` (4 iterations: Planner×3, Critiquer×4, approved at iteration 4)
- Implementation review log: `.agent/plans/agentic/task-plans/task-714-impl-review.md` (1 iteration: Implementation×1, Code Review×1, approved)
